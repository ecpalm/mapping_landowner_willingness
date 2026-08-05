
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
# Script description: 6d. Builds PME model area-level constraints from the ACS 
#                         Summary File using the Census API.
#
# Script authors: Eric Palm, with code heavily borrowed from Joe Tuccillo 
#                 (https://github.com/jvtcl/pmedmize)  
#
################################################################################

# Load packages
sapply(
  c("censusapi", "dplyr", "stringr", "tidycensus"),
  require,
  character.only = TRUE
)

get_table <- function(v, table, key, name, year, level, geo) {

  # Gets/processes a single table from censusapi for estimate, margin of
  # error, and standard error, to match SocialExplorer table conventions.

  table_vars <- v$name[v$group == table]
  moe_vars <- gsub("E$", "M", table_vars)

  a <- censusapi::getCensus(
    name = name, vintage = year, key = key,
    vars = table_vars, region = level, regionin = geo
  )

  # create GEOIDs from the leading geography columns
  endGeo <- which(startsWith(names(a), table))[1] - 1
  geoid <- apply(a[, 1:endGeo], 1, function(x) paste(x, collapse = ""))
  a <- a[, -c(1:endGeo)] # remove geographic vars

  b <- censusapi::getCensus(
    name = name, vintage = year, key = key,
    vars = moe_vars, region = level, regionin = geo
  )
  b <- b[, -c(1:endGeo)] # remove geographic vars

  # rename variables
  a_rename <- gsub("_", "", table_vars)
  a_rename <- gsub("E$", "", a_rename)
  b_rename <- gsub("_", "", moe_vars)
  b_rename <- gsub("M$", "", b_rename)

  # safeguard for one-column tables
  if (length(table_vars) > 1) {
    est <- a[, order(a_rename)]
    names(est) <- sort(a_rename)
    moe <- b[, order(b_rename)]
    names(moe) <- sort(b_rename)
  } else {
    est <- data.frame(a)
    moe <- data.frame(b)
    names(est) <- paste0(table, "001")
    names(moe) <- paste0(table, "001")
  }

  # MOE to SE
  if (length(moe_vars) > 1) {
    se <- do.call(cbind.data.frame, lapply(1:ncol(moe), function(i) moe[, i] / 1.645))
  } else {
    se <- moe / 1.645
  }
  names(se) <- paste0(names(moe), "_se")

  list(geoid = geoid, est = est, moe = moe, se = se)
}

build_constraints <- function(v, tables, key, name, year, level, geo, verbose = TRUE) {

  # Builds a table of ACS estimates + standard errors, matched to P-MEDM
  # constraint naming conventions, for one or more Census tables.

  table_ests <- lapply(tables, function(t) {
    if (verbose) cat("Table", t, "\n")
    get_table(v = v, table = t, key = key, name = name, year = year, level = level, geo = geo)
  })

  ests_combined <- do.call(cbind.data.frame, lapply(table_ests, function(x) x$est))
  ses_combined <- do.call(cbind.data.frame, lapply(table_ests, function(x) x$se))

  out <- data.frame(GEOID = table_ests[[1]]$geoid, ests_combined, ses_combined) %>%
    dplyr::filter(rowSums(dplyr::select(., -c(GEOID, dplyr::contains("se")))) > 0)

  out
}

# -----------------------------------------------------------------------
# "Supertract" (PUMA-nested pairs of tracts) versions of the above, used
# for the parent-level P-MEDM constraints.
# -----------------------------------------------------------------------

get_table_super <- function(v, table, key, name, year, level, geo, puma_tract) {

  # Gets/processes a single table from censusapi, aggregated to "supertracts" 
  # (paired tracts nested within a PUMA), for estimate, margin of
  # error, and standard error, to match SocialExplorer table conventions.

  table_vars <- v$name[v$group == table]
  moe_vars <- gsub("E$", "M", table_vars)

  a <- censusapi::getCensus(
    name = name, vintage = year, key = key,
    vars = table_vars, region = level, regionin = geo
  ) %>%
    dplyr::filter(rowSums(dplyr::select(., -dplyr::contains(c("state", "county", "tract")))) > 0)

  b <- censusapi::getCensus(
    name = name, vintage = year, key = key,
    vars = moe_vars, region = level, regionin = geo
  )

  puma_tract_joined <- a %>%
    tidyr::pivot_longer(cols = -c(1:3), values_to = "estimate") %>%
    dplyr::mutate(name = stringr::str_sub(name, end = -2)) %>%
    dplyr::inner_join(
      b %>%
        tidyr::pivot_longer(cols = -c(1:3), values_to = "moe") %>%
        dplyr::mutate(name = stringr::str_sub(name, end = -2))
    ) %>%
    dplyr::inner_join(
      puma_tract %>%
        dplyr::filter(stringr::str_c(STATEFP, COUNTYFP, TRACTCE) %in% stringr::str_c(a$state, a$county, a$tract)) %>%
        dplyr::arrange(STATEFP, PUMA5CE, trt_id) %>%
        dplyr::group_by(STATEFP, PUMA5CE) %>%
        # pair adjacent tracts (by trt_id order) within each PUMA into
        # "super tracts" of ~2 tracts each
        dplyr::mutate(tempie = as.character(dplyr::ntile(trt_id, floor(dplyr::n() / 2)))) %>%
        dplyr::group_by(STATEFP, PUMA5CE, tempie) %>%
        dplyr::mutate(super_tract = paste(unique(trt_id), collapse = " ")) %>%
        dplyr::ungroup() %>%
        dplyr::select(PUMA = PUMA5CE, county = COUNTYFP, tract = TRACTCE, super_tract)
    ) %>%
    dplyr::group_by(state, PUMA, name, super_tract) %>%
    dplyr::summarize(
      E = sum(estimate, na.rm = TRUE),
      M = tidycensus::moe_sum(moe = moe, estimate = estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = name, values_from = c(E, M), names_glue = "{name}{.value}") %>%
    `colnames<-`(stringr::str_replace_all(colnames(.), "_", ""))

  list(
    geoid = puma_tract_joined$supertract,
    est = puma_tract_joined %>%
      dplyr::select(dplyr::ends_with("E", ignore.case = FALSE)) %>%
      `colnames<-`(stringr::str_replace_all(colnames(.), "E", "")),
    moe = puma_tract_joined %>%
      dplyr::select(dplyr::ends_with("M", ignore.case = FALSE)) %>%
      `colnames<-`(stringr::str_replace_all(colnames(.), "M", "")),
    se = puma_tract_joined %>%
      dplyr::select(dplyr::ends_with("M", ignore.case = FALSE)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ . / 1.645)) %>%
      `colnames<-`(stringr::str_replace_all(colnames(.), "M", "_se"))
  )
}

build_constraints_super <- function(v, tables, key, name, year, level, geo, puma_tract, verbose = TRUE) {

  # Builds a table of "super tract" ACS estimates + standard errors, matched
  # to P-MEDM constraint naming conventions, for one or more Census tables.

  table_ests <- lapply(tables, function(t) {
    if (verbose) cat("Table", t, "\n")
    get_table_super(
      v = v, table = t, key = key, name = name, year = year,
      level = level, geo = geo, puma_tract = puma_tract
    )
  })

  ests_combined <- do.call(cbind.data.frame, lapply(table_ests, function(x) x$est))
  ses_combined <- do.call(cbind.data.frame, lapply(table_ests, function(x) x$se))

  data.frame(GEOID = table_ests[[1]]$geoid, ests_combined, ses_combined)
}
