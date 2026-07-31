
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
# Script description: 6. Runs the full P-MEDM pipeline to probabilistically 
#                     allocate individual microdata (PUMS records) to census
#                     tracts within a single state.
#
# Script author: Eric Palm, with code borrowed heavily from Joe Tuccillo 
#                (https://github.com/jvtcl/pmedmize) and Nicholas Nagle
#                (https://bitbucket.org/nnnagle/pmedmrcpp/src/master/)
#
################################################################################

# Load packages
sapply(
  c("dplyr", "tidyr", "purrr", "tibble", "stringr", "readr",
    "tigris", "censusapi", "tidycensus", "matrixStats", "tictoc"),
  require,
  character.only = TRUE
)

# -----------------------------------------------------------------------
# Census API key
# -----------------------------------------------------------------------
# NOTE: do not hard-code API keys in source files. Store the key in your
# .Renviron (e.g. `CENSUS_API_KEY=...`) and load it with Sys.getenv().
# If a key was ever committed to a script or shared elsewhere, request a
# new one at https://api.census.gov/data/key_signup.html and revoke the
# old one.
census_api_key <- Sys.getenv("CENSUS_API_KEY")
if (identical(census_api_key, "")) {
  stop("Set the CENSUS_API_KEY environment variable (e.g. in .Renviron) before running this script.")
}

# -----------------------------------------------------------------------
# State to process
# -----------------------------------------------------------------------

focal_state <- "WY"

source("scripts/6a_pmedm.R")
source("scripts/6b_constraints.R")
source("scripts/6c_intermediates.R")
source("scripts/6d_build_constraints_ind.R")
source("scripts/6e_build_constraints_geo.R")
source("scripts/6f_build_puma_lookup.R")

fips <- tigris::fips_codes %>%
  dplyr::distinct(state, state_code)

fips_state <- fips %>%
  dplyr::filter(state == focal_state) %>%
  dplyr::pull(state_code)

# -----------------------------------------------------------------------
# PUMS constraints
# -----------------------------------------------------------------------
pums_vars <- c("HINCP", "TEN", "AGEP", "SCHL", "PUMA")

pums <- tidycensus::get_pums(
  variables = pums_vars,
  state = focal_state,
  year = 2023,
  survey = "acs5",
  rep_weights = NULL,
  variables_filter = list(SPORDER = 1)
) %>%
  dplyr::filter(WGTP != 0)

tp <- sort(unique(pums$PUMA))

## Generate tables for building constraints

schema <- readr::read_csv("data/constraints.csv") 

cid <- unique(substr(schema$code, 1, 6)) # constraint table IDs

pmedm_constraints_ind <- prepare_constraints_ind(schema, pums)

# split by PUMA, in case the state has more than one
pmedm_constraints_ind <- split(data.frame(pmedm_constraints_ind), pums$PUMA)
pums <- split(pums, pums$PUMA)
names(pmedm_constraints_ind) <- tp
names(pums) <- tp

# sanity check: implied constraint proportions per PUMA, weighted by WGTP
lapply(tp, function(p) {
  colSums(pmedm_constraints_ind[[p]] * pums[[p]]$WGTP) / sum(pums[[p]]$WGTP)
})

# -----------------------------------------------------------------------
# Summary-level constraints
# -----------------------------------------------------------------------
v <- censusapi::listCensusMetadata(name = "acs/acs5", vintage = 2023, type = "variables")

puma_lookup <- build_puma_lookup(state = fips_state) %>%
  dplyr::filter(PUMA5CE %in% tp)

constraints_trt <- build_constraints(
  v = v,
  tables = cid,
  key = census_api_key,
  name = "acs/acs5",
  year = 2023,
  level = "tract:*",
  geo = stringr::str_c("state:", fips_state),
  verbose = FALSE
)

constraints_spr_trt <- build_constraints_super(
  v = v,
  tables = cid,
  key = census_api_key,
  name = "acs/acs5",
  year = 2023,
  level = "tract:*",
  geo = stringr::str_c("state:", fips_state),
  puma_tract = puma_lookup,
  verbose = FALSE
)

pmedm_constraints_spr_trt <- prepare_constraints_geo(constraints_spr_trt, schema)
pmedm_constraints_trt <- prepare_constraints_geo(constraints_trt, schema)

saveRDS(pmedm_constraints_trt, stringr::str_c("results/pmedm_constraints_tract_", focal_state, ".rds"))
saveRDS(pmedm_constraints_spr_trt, stringr::str_c("results/pmedm_constraints_super_tract_", focal_state, ".rds"))

geo_lookup <- puma_lookup %>%
  dplyr::filter(trt_id %in% pmedm_constraints_trt$GEOID) %>%
  dplyr::arrange(STATEFP, PUMA5CE, trt_id) %>%
  dplyr::group_by(STATEFP, PUMA5CE) %>%
  dplyr::mutate(tempie = as.character(dplyr::ntile(trt_id, floor(dplyr::n() / 2)))) %>%
  dplyr::group_by(PUMA5CE, tempie) %>%
  dplyr::mutate(spr_trt = paste(unique(trt_id), collapse = " ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(trt = trt_id, spr_trt) %>%
  as.data.frame()

puma_lookup_super <- puma_lookup %>%
  dplyr::inner_join(
    geo_lookup %>% dplyr::select(trt_id = trt, spr_trt_id = spr_trt)
  )

# -----------------------------------------------------------------------
# Run P-MEDM solver, by PUMA
# -----------------------------------------------------------------------
tictoc::tic()

res <- lapply(tp, function(p) {

  print(p)

  G <- puma_lookup_super$spr_trt_id[puma_lookup_super$PUMA5CE == p]
  g <- geo_lookup$trt[geo_lookup$spr_trt %in% G]

  pmedm(
    pums = pums[[p]],
    pums_in = pmedm_constraints_ind[[p]],
    datch = pmedm_constraints_trt[pmedm_constraints_trt$GEOID %in% g, ],
    datpt = pmedm_constraints_spr_trt[pmedm_constraints_spr_trt$GEOID %in% G, ],
    geo_lookup = geo_lookup[geo_lookup$trt %in% g, ],
    output_minimal = FALSE
  )
})
names(res) <- tp

tictoc::toc()

# -----------------------------------------------------------------------
# Summarize owner-occupied household estimates by education x age x
# income segment, at the tract level
# -----------------------------------------------------------------------
df_segs <- tidyr::expand_grid(
  edu = c("edu_no_hs", "edu_hs", "edu_some_col", "edu_bach_plus"),
  age = c("age_18_34", "age_35_44", "age_45_64", "age_65_plus"),
  inc = c("inc_under_50k", "inc_50k_99k", "inc_100k_149k", "inc_150k_plus")
) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_c("own_", .))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(seg = list(stringr::str_split_1(stringr::str_c(edu, age, inc, sep = " "), " "))) %>%
  dplyr::ungroup()

results <- tibble::tibble(
  PUMA = tp,
  res_tp = res,
  df = purrr::map(seq_along(tp), ~df_segs)
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    syp = list(res_tp$wt_matrix / res_tp$N),
    hh_est = list(colSums(syp * res_tp$N)),
    GEOID = list(colnames(res_tp$wt_matrix))
  ) %>%
  tidyr::unnest(cols = df) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    est = list(colSums(matrixStats::rowProds(res_tp$pums_in[, seg]) * syp * res_tp$N) / colSums(syp * res_tp$N))
  ) %>%
  dplyr::select(-c(res_tp, seg, syp)) %>%
  tidyr::unnest(cols = hh_est:est) %>%
  dplyr::inner_join(
    pmedm_constraints_trt %>% dplyr::select(GEOID, hh, own)
  ) %>%
  dplyr::mutate(cell_est = hh_est * est)

# Save the outputs
saveRDS(results, stringr::str_c("results/allocation_", focal_state, ".rds"))
