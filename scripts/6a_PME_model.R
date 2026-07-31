
################################################################################
#
# Article title: Mapping landowners' willingness to allow recreational deer 
#                harvest on their property for managing chronic wasting disease 
#
# Article DOI: https://doi.org/10.1002/jwmg.70254
#
# Journal: The Journal of Wildlife Management
#
# Article authors: Eric Palm, Matthew Williamson, Nathan Snow, Sonja Christensen
#
# Script description: 6a. Wrapper around PMEDMrcpp::pmedm_solve. Adapted from 
#                     examples by Nagle (2013, 2015): 
#                     https://www.rpubs.com/nnnagle/PMEDM_Example
#
# Script authors: Joe Tuccillo (https://github.com/jvtcl/pmedmize) and Nicholas 
#                 Nagle (https://bitbucket.org/nnnagle/pmedmrcpp/src/master/), 
#                 with small changes by Eric Palm
#
################################################################################

# Load packages
sapply(
  c("PMEDMrcpp", "Matrix"),
  require,
  character.only = TRUE
)

assign_person_ids <- function(pums) {

  # Helper function. Generates a unique serial ID for every member of a
  # household (needed for IPUMS-style PUMS, which share one SERIAL per
  # household rather than a unique ID per person).

  unlist(sapply(unique(pums$SERIALNO), function(s) {
    ts <- pums$SERIALNO[pums$SERIALNO == s]
    if (length(ts) > 1) {
      paste0(s, letters[1:length(ts)])
    } else {
      s
    }
  }))
}

pmedm <- function(pums, pums_style = "acs", pums_in, geo_lookup, datch, datpt,
                   q = NULL, lambda = NULL, type = "household", output_minimal = TRUE) {

  # Wrapper for PMEDMrcpp::pmedm_solve.
  #
  # ARGUMENTS
  #   pums            : PUMS microdata
  #   pums_style      : "acs" or "ipums" - determines which weight/serial
  #                      columns are used
  #   pums_in         : individual-level P-MEDM constraint matrix (from
  #                      prepare_constraints_ind())
  #   geo_lookup      : two-column lookup of child geography -> parent
  #                      geography
  #   datch           : child summary-level constraints, estimates AND SEs
  #   datpt           : parent summary-level constraints, estimates AND SEs
  #   type            : design weights to use, one of "person" or "household"
  #   output_minimal  : if TRUE, excludes intermediate variables from the
  #                      output. If FALSE, outputs all P-MEDM inputs/outputs.
  #
  # VALUE
  #   A list containing the P-MEDM solution and model variables.

  if (!type %in% c("person", "household")) {
    stop('Argument `type` must be one of: `person`, `household`.')
  }

  if (!pums_style %in% c("ipums", "acs")) {
    stop('Argument `pums_style` must be one of: `ipums`, `acs`.')
  }

  # -----------------------------------------------------------------------
  # Microdata inputs
  # -----------------------------------------------------------------------
  if (type == "person") {
    if (pums_style == "ipums") {
      wt <- pums$PERWT
      serial <- assign_person_ids(pums)
    } else if (pums_style == "acs") {
      wt <- pums$PWGTP
      serial <- pums$SERIALNO
    }
  } else {
    # subset to household head, limit to occupied housing units
    if (pums_style == "ipums") {
      hhsub <- (pums$RELATED == 101) & (pums$GQ %in% c(1:2))
      pums <- pums[hhsub, ]
      pums_in <- pums_in[hhsub, ]
      wt <- pums$HHWT
      serial <- pums$SERIAL
    } else if (pums_style == "acs") {
      hhsub <- pums$SPORDER == 1
      pums <- pums[hhsub, ]
      pums_in <- pums_in[hhsub, ]
      wt <- pums$WGTP
      serial <- pums$SERIALNO
    }
  }

  pums_in <- as.matrix(pums_in)
  pX <- list(Matrix::drop0(pums_in), Matrix::drop0(pums_in))

  # -----------------------------------------------------------------------
  # Geographies: A1 = child-geography-in-parent-geography incidence matrix,
  # A2 = child-geography identity matrix
  # -----------------------------------------------------------------------
  A1 <- do.call("rbind", lapply(unique(geo_lookup[, 2]), function(g) {
    nbt <- geo_lookup[geo_lookup[, 2] == g, ][, 1]
    ifelse(geo_lookup[, 1] %in% nbt, 1, 0)
  }))
  rownames(A1) <- unique(geo_lookup[, 2])
  colnames(A1) <- geo_lookup[, 1]
  A1 <- methods::as(A1, "dgCMatrix")

  A2 <- do.call("rbind", lapply(geo_lookup[, 1], function(g) {
    ifelse(geo_lookup[, 1] %in% g, 1, 0)
  }))
  rownames(A2) <- geo_lookup[, 1]
  colnames(A2) <- geo_lookup[, 1]
  A2 <- methods::as(A2, "dtCMatrix")

  A <- list(A1, A2)

  # -----------------------------------------------------------------------
  # Prep summary-level constraints
  # -----------------------------------------------------------------------
  datch <- datch[datch[, 1] %in% geo_lookup[, 1], ]
  datch <- datch[match(row.names(A2), datch[, 1]), ] # NOTE: order-sensitive join; verify no NAs introduced here

  # ensure geoid, standard error values included
  tempch <- data.frame(GEOID = datch$GEOID, datch[, na.omit(match(colnames(pums_in), colnames(datch)))])
  datch <- data.frame(tempch, datch[, paste0(names(tempch)[-1], "_se")])

  datpt <- datpt[datpt[, 1] %in% geo_lookup[, 2], ]
  datpt <- datpt[match(row.names(A1), datpt[, 1]), ]

  # ensure geoid, standard error values included
  temppt <- data.frame(GEOID = datpt$GEOID, datpt[, na.omit(match(colnames(pums_in), colnames(datpt)))])
  datpt <- data.frame(temppt, datpt[, paste0(names(temppt)[-1], "_se")])

  # -----------------------------------------------------------------------
  # Generate summary-level inputs
  # -----------------------------------------------------------------------
  sumpt <- datpt[, -1]
  sumpt <- sumpt[!endsWith(names(sumpt), "_se")]
  rownames(sumpt) <- datpt[, 1]

  sumch <- datch[, names(sumpt)]
  rownames(sumch) <- as.character(datch[, 1])

  Y <- list(as.matrix(sumpt), as.matrix(sumch))

  # variances
  vpt <- datpt[, paste0(names(sumpt), "_se")]
  rownames(vpt) <- rownames(sumpt)
  vch <- datch[, paste0(names(sumch), "_se")]
  rownames(vch) <- rownames(sumch)
  V <- list(as.matrix(vpt^2), as.matrix(vch^2))

  # -----------------------------------------------------------------------
  # Check constraint columns match between PUMS and summary-level data
  # -----------------------------------------------------------------------
  check_nb_cols <- ncol(pums_in) == ncol(sumpt)
  check_trt_cols <- ncol(pums_in) == ncol(sumch)

  sumpt.check <- sapply(colnames(sumpt), function(x) paste(unlist(strsplit(x, "")), collapse = ""))
  sumch.check <- sapply(colnames(sumch), function(x) paste(unlist(strsplit(x, "")), collapse = ""))

  check_nb_cols_len <- sum(colnames(pums_in) == sumpt.check) == ncol(sumpt)
  check_trt_cols_len <- sum(colnames(pums_in) == sumch.check) == ncol(sumch)

  checks <- c(check_nb_cols, check_nb_cols_len, check_trt_cols, check_trt_cols_len)

  if (sum(checks) != length(checks)) {
    stop("PUMS and Summary Level columns do not match.")
  }

  # -----------------------------------------------------------------------
  # Generate PUMS solver inputs
  # -----------------------------------------------------------------------
  N <- sum(Y[[1]][, 1]) # population size
  n <- NROW(pX[[1]]) # sample size

  # Since we are optimizing probabilities p rather than weights w,
  # normalize Y (tract/bg data) by N (pop size) and V (tract/bg variances)
  # by n/N^2
  Y_vec <- do.call("c", lapply(Y, function(x) as.vector(as.matrix(x)))) / N
  V_vec <- do.call("c", lapply(V, function(x) as.vector(as.matrix(x)))) * n / N^2

  # variance-covariance matrix, as a sparse diagonal matrix
  sV <- Matrix::.sparseDiagonal(n = length(V_vec), x = V_vec)

  # all possible PUMS allocations
  X <- t(rbind(kronecker(t(pX[[1]]), A[[1]]), kronecker(t(pX[[2]]), A[[2]])))

  # design weights, normalized
  if (is.null(q)) {
    q <- matrix(wt, n, dim(A[[1]])[2])
    q <- q / sum(as.numeric(q))
    q <- as.vector(t(q))
  }

  X <- methods::as(X, "dgCMatrix")
  sV <- methods::as(sV, "dgCMatrix")

  # -----------------------------------------------------------------------
  # Solve P-MEDM problem
  # -----------------------------------------------------------------------
  t <- PMEDMrcpp::PMEDM_solve(X, Y_vec, sV, q, lambda)

  # allocation matrix
  wt_matrix <- matrix(t$p, nrow(pums_in), dim(A[[1]])[2], byrow = TRUE) * N
  dimnames(wt_matrix) <- list(serial, rownames(Y[[2]]))

  # -----------------------------------------------------------------------
  # Return inputs/outputs
  # -----------------------------------------------------------------------
  if (output_minimal) {
    out <- list(datpt, datch, pums_in, pX, Y, V, A, wt, t, wt_matrix)
    names(out) <- c("parent.data", "child.data", "pums_in", "pX", "Y", "V", "A", "wt", "t", "wt_matrix")
  } else {
    out <- list(n, N, datpt, datch, pums_in, pX, X, Y, V, sV, A, wt, t, q, wt_matrix)
    names(out) <- c("n", "N", "datpt", "datch", "pums_in", "pX", "X", "Y", "V", "sV", "A", "wt", "t", "q", "wt_matrix")
  }

  out
}
