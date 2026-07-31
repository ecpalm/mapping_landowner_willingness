
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
# Script description: 6d. Builds a Census tract-to-PUMA lookup table for a given
#                     state, handling the Connecticut 2022 crosswalk.
#
# Script authors: Eric Palm, with code borrowed from Joe Tuccillo 
#                 (https://github.com/jvtcl/pmedmize)  
#
################################################################################

# Load packages
sapply(
  c("dplyr", "readr", "stringr"),
  require,
  character.only = TRUE
)

build_puma_lookup <- function(state) {

  puma_lookup <- read.table(
    "data/2020_Census_Tract_to_2020_PUMA.txt",
    sep = ",", header = TRUE
  )

  puma_lookup$STATEFP <- sprintf("%02d", puma_lookup$STATEFP)
  puma_lookup$COUNTYFP <- sprintf("%03d", puma_lookup$COUNTYFP)
  puma_lookup$TRACTCE <- sprintf("%06d", puma_lookup$TRACTCE)
  puma_lookup$PUMA5CE <- sprintf("%05d", puma_lookup$PUMA5CE)

  # Connecticut redefined its tract/county boundaries in 2022 (transitioning
  # from counties to planning regions); swap in the 2022 codes for CT records
  # so they align with vintages using the new geography.
  puma_lookup <- puma_lookup %>%
    dplyr::left_join(
      readr::read_csv("data/2022tractcrosswalk_ct.csv") %>%
        dplyr::select(county_fips_2020, ce_fips_2022, tract_fips_2020, Tract_fips_2022) %>%
        dplyr::mutate(
          TRACTCE = stringr::str_sub(tract_fips_2020, start = 6),
          TRACTCE_CT = stringr::str_sub(Tract_fips_2022, start = 6),
          COUNTYFP = stringr::str_sub(county_fips_2020, start = 3),
          COUNTYFP_CT = stringr::str_sub(ce_fips_2022, start = 3),
          STATEFP = "09"
        ) %>%
        dplyr::select(COUNTYFP, COUNTYFP_CT, STATEFP, TRACTCE, TRACTCE_CT)
    ) %>%
    dplyr::mutate(
      TRACTCE = dplyr::if_else(STATEFP == "09", TRACTCE_CT, TRACTCE),
      COUNTYFP = dplyr::if_else(STATEFP == "09", COUNTYFP_CT, COUNTYFP)
    ) %>%
    dplyr::select(STATEFP, COUNTYFP, TRACTCE, PUMA5CE)

  puma_lookup$trt_id <- with(puma_lookup, paste0(STATEFP, COUNTYFP, TRACTCE))

  # subset to target state
  puma_lookup[puma_lookup$STATEFP %in% state, ]
}
