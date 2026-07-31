
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
# Script description: 6b. Helper functions for preparing P-MEDM constraints 
#                     (individual and area level) from raw ACS Summary File 
#                     tables.
#
# Script authors: Joe Tuccillo (https://github.com/jvtcl/pmedmize)  
#                 with small changes by Eric Palm
#
################################################################################


prepare_constraints_ind <- function(schema, pums) {

  # Prepares P-MEDM constraints for individuals/PUMS.
  #
  # schema : data.frame with columns `code` (ACS variable code) and
  #          `constraint` (the P-MEDM constraint name it belongs to)
  # pums   : PUMS microdata data.frame
  #
  # Returns a matrix of individual-level constraint indicators, one column
  # per unique value of `schema$constraint`.

  cid <- unique(substr(schema$code, 1, 6)) # constraint table IDs, e.g. "B25013"

  ctable <- lapply(cid, function(v) {
    do.call(v, args = list(pums = pums))
  })
  ctable <- do.call(cbind, ctable)

  sc <- unique(schema$constraint)

  cind <- lapply(sc, function(x) {

    xc <- schema$code[schema$constraint == x]
    cout <- ctable[, xc]

    # collapse multi-column constraints into a single 0/1 indicator
    if (length(xc) > 1) {
      cout <- ifelse(rowSums(cout) >= 1, 1, 0)
    }

    cout
  })
  names(cind) <- sc

  do.call(cbind, cind)
}


prepare_constraints_geo <- function(dat, schema) {

  # Prepares P-MEDM constraints for areas/Summary File.
  #
  # dat    : data.frame of ACS estimates + standard errors (from
  #          build_constraints()/build_constraints_super()), with GEOID in
  #          column 1
  # schema : data.frame with columns `code` and `constraint`
  #
  # Returns a data.frame of area-level constraint estimates and standard
  # errors, one estimate/SE pair per unique value of `schema$constraint`.

  sc <- unique(schema$constraint)

  pmedm_constraints_geo <- lapply(sc, function(x) {

    xc <- schema$code[schema$constraint == x]

    cout    <- dat[, xc]
    se.cout <- dat[, paste0(xc, "_se")]

    # collapse multi-column constraints by summing estimates and combining
    # standard errors in quadrature
    if (length(xc) > 1) {
      cout    <- rowSums(cout)
      se.cout <- sqrt(rowSums(se.cout^2))
    }

    cout <- cbind(cout, se.cout)
    colnames(cout) <- c(x, paste0(x, "_se"))

    cout
  })

  data.frame(GEOID = dat[, 1], do.call(cbind, pmedm_constraints_geo))
}
