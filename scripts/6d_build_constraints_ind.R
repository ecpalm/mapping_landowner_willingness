
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
# Script description: 6d. Definitions for ACS Summary File constraints at the 
#                     individual (PUMS) level. Each function reconstructs one 
#                     ACS table's cell structure from PUMS microdata, using the 
#                     intermediates defined in 6c_Intermediates.R. 
#                     Tables: B25013 (tenure x education), B25007 
#                     (tenure x age), B25118 (tenure x income).
#
# Script authors: Joe Tuccillo (https://github.com/jvtcl/pmedmize)  
#                 with modifications by Eric Palm
#
################################################################################


build_intermediates <- function(pums, intermediates) {

  # Helper function. Builds a named list of intermediate PUMS variables used
  # to construct P-MEDM constraints, based on the function names supplied in
  # `intermediates` (see intermediates.R for definitions).

  its <- sapply(intermediates, function(i) {
    do.call(i, args = list(pums = pums))
  })
  names(its) <- intermediates

  its
}

B25013 <- function(pums) {

  # B25013: Tenure by Educational Attainment of Householder (TRACT)

  its <- build_intermediates(pums, c("tenr", "edutenr"))

  occ.hu <- rep(1, nrow(pums)) # Total 001

  occ.hu.own           <- its$tenr[, 1] # Owner occupied 002
  occ.hu.own.no.hs     <- occ.hu.own * its$edutenr[, 1]
  occ.hu.own.hs        <- occ.hu.own * its$edutenr[, 2]
  occ.hu.own.some.col  <- occ.hu.own * its$edutenr[, 3]
  occ.hu.own.bach.plus <- occ.hu.own * its$edutenr[, 4]

  occ.hu.rent           <- its$tenr[, 2] # Renter occupied 012
  occ.hu.rent.no.hs     <- occ.hu.rent * its$edutenr[, 1]
  occ.hu.rent.hs        <- occ.hu.rent * its$edutenr[, 2]
  occ.hu.rent.some.col  <- occ.hu.rent * its$edutenr[, 3]
  occ.hu.rent.bach.plus <- occ.hu.rent * its$edutenr[, 4]

  pums.B25013 <- cbind(
    occ.hu,
    occ.hu.own, occ.hu.own.no.hs, occ.hu.own.hs, occ.hu.own.some.col, occ.hu.own.bach.plus,
    occ.hu.rent, occ.hu.rent.no.hs, occ.hu.rent.hs, occ.hu.rent.some.col, occ.hu.rent.bach.plus
  )

  pums.B25013 <- occ.hu * pums.B25013 # limit to occupied (non-GQ) units

  colnames(pums.B25013) <- paste0("B25013", sprintf("%03d", 1:ncol(pums.B25013)))

  pums.B25013
}

B25007 <- function(pums) {

  # B25007: Tenure by Age of Householder (Households) (BLOCK)

  its <- build_intermediates(pums, c("tenr", "agetenr"))

  occ.hu <- rep(1, nrow(pums)) # Total 001

  occ.hu.own       <- its$tenr[, 1] # Owner occupied 002
  occ.hu.own.15.24 <- occ.hu.own * its$agetenr[, 1]
  occ.hu.own.25.34 <- occ.hu.own * its$agetenr[, 2]
  occ.hu.own.35.44 <- occ.hu.own * its$agetenr[, 3]
  occ.hu.own.45.54 <- occ.hu.own * its$agetenr[, 4]
  occ.hu.own.55.59 <- occ.hu.own * its$agetenr[, 5]
  occ.hu.own.60.64 <- occ.hu.own * its$agetenr[, 6]
  occ.hu.own.65.74 <- occ.hu.own * its$agetenr[, 7]
  occ.hu.own.75.84 <- occ.hu.own * its$agetenr[, 8]
  occ.hu.own.85o   <- occ.hu.own * its$agetenr[, 9]

  occ.hu.rent       <- its$tenr[, 2] # Renter occupied 012
  occ.hu.rent.15.24 <- occ.hu.rent * its$agetenr[, 1]
  occ.hu.rent.25.34 <- occ.hu.rent * its$agetenr[, 2]
  occ.hu.rent.35.44 <- occ.hu.rent * its$agetenr[, 3]
  occ.hu.rent.45.54 <- occ.hu.rent * its$agetenr[, 4]
  occ.hu.rent.55.59 <- occ.hu.rent * its$agetenr[, 5]
  occ.hu.rent.60.64 <- occ.hu.rent * its$agetenr[, 6]
  occ.hu.rent.65.74 <- occ.hu.rent * its$agetenr[, 7]
  occ.hu.rent.75.84 <- occ.hu.rent * its$agetenr[, 8]
  occ.hu.rent.85o   <- occ.hu.rent * its$agetenr[, 9]

  pums.B25007 <- cbind(
    occ.hu,
    occ.hu.own, occ.hu.own.15.24, occ.hu.own.25.34, occ.hu.own.35.44, occ.hu.own.45.54,
    occ.hu.own.55.59, occ.hu.own.60.64, occ.hu.own.65.74, occ.hu.own.75.84, occ.hu.own.85o,
    occ.hu.rent, occ.hu.rent.15.24, occ.hu.rent.25.34, occ.hu.rent.35.44, occ.hu.rent.45.54,
    occ.hu.rent.55.59, occ.hu.rent.60.64, occ.hu.rent.65.74, occ.hu.rent.75.84, occ.hu.rent.85o
  )

  pums.B25007 <- occ.hu * pums.B25007 # limit to occupied (non-GQ) units

  colnames(pums.B25007) <- paste0("B25007", sprintf("%03d", 1:ncol(pums.B25007)))

  pums.B25007
}

B25118 <- function(pums) {

  # B25118: Tenure by Income of Householder (Households) (TRACT)

  its <- build_intermediates(pums, c("tenr", "hhinctenr"))

  occ.hu <- rep(1, nrow(pums)) # Total 001

  occ.hu.own              <- its$tenr[, 1] # Owner occupied 002
  occ.hu.own.less.than.5k <- occ.hu.own * its$hhinctenr[, 1]
  occ.hu.own.5k.9k        <- occ.hu.own * its$hhinctenr[, 2]
  occ.hu.own.10k.14k      <- occ.hu.own * its$hhinctenr[, 3]
  occ.hu.own.15k.19k      <- occ.hu.own * its$hhinctenr[, 4]
  occ.hu.own.20k.24k      <- occ.hu.own * its$hhinctenr[, 5]
  occ.hu.own.25k.34k      <- occ.hu.own * its$hhinctenr[, 6]
  occ.hu.own.35k.49k      <- occ.hu.own * its$hhinctenr[, 7]
  occ.hu.own.50k.74k      <- occ.hu.own * its$hhinctenr[, 8]
  occ.hu.own.75k.99k      <- occ.hu.own * its$hhinctenr[, 9]
  occ.hu.own.100k.149k    <- occ.hu.own * its$hhinctenr[, 10]
  occ.hu.own.150k.more    <- occ.hu.own * its$hhinctenr[, 11]

  occ.hu.rent              <- its$tenr[, 2] # Renter occupied 012
  occ.hu.rent.less.than.5k <- occ.hu.rent * its$hhinctenr[, 1]
  occ.hu.rent.5k.9k        <- occ.hu.rent * its$hhinctenr[, 2]
  occ.hu.rent.10k.14k      <- occ.hu.rent * its$hhinctenr[, 3]
  occ.hu.rent.15k.19k      <- occ.hu.rent * its$hhinctenr[, 4]
  occ.hu.rent.20k.24k      <- occ.hu.rent * its$hhinctenr[, 5]
  occ.hu.rent.25k.34k      <- occ.hu.rent * its$hhinctenr[, 6]
  occ.hu.rent.35k.49k      <- occ.hu.rent * its$hhinctenr[, 7]
  occ.hu.rent.50k.74k      <- occ.hu.rent * its$hhinctenr[, 8]
  occ.hu.rent.75k.99k      <- occ.hu.rent * its$hhinctenr[, 9]
  occ.hu.rent.100k.149k    <- occ.hu.rent * its$hhinctenr[, 10]
  occ.hu.rent.150k.more    <- occ.hu.rent * its$hhinctenr[, 11]

  pums.B25118 <- cbind(
    occ.hu,
    occ.hu.own, occ.hu.own.less.than.5k, occ.hu.own.5k.9k, occ.hu.own.10k.14k,
    occ.hu.own.15k.19k, occ.hu.own.20k.24k, occ.hu.own.25k.34k, occ.hu.own.35k.49k,
    occ.hu.own.50k.74k, occ.hu.own.75k.99k, occ.hu.own.100k.149k, occ.hu.own.150k.more,
    occ.hu.rent, occ.hu.rent.less.than.5k, occ.hu.rent.5k.9k, occ.hu.rent.10k.14k,
    occ.hu.rent.15k.19k, occ.hu.rent.20k.24k, occ.hu.rent.25k.34k, occ.hu.rent.35k.49k,
    occ.hu.rent.50k.74k, occ.hu.rent.75k.99k, occ.hu.rent.100k.149k, occ.hu.rent.150k.more
  )

  pums.B25118 <- occ.hu * pums.B25118 # limit to occupied (non-GQ) units

  colnames(pums.B25118) <- paste0("B25118", sprintf("%03d", 1:ncol(pums.B25118)))

  pums.B25118
}
