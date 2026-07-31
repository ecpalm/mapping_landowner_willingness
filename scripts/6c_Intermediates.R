
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
# Script description: 6c. Definitions for intermediate variables used to build
#                     individual-level PME model constraints from PUMS 
#                     microdata: age, income, education, and tenure.
#
# Script authors: Joe Tuccillo (https://github.com/jvtcl/pmedmize)  
#                 with small changes by Eric Palm
#
################################################################################

assign_householder_item_level <- function(pums, v) {

  # Helper function. Assigns the item-level value of the householder to all
  # members of the household (used for building household-level
  # intermediates from person-level PUMS records).

  v <- lapply(split(v, pums$SERIALNO), function(s) {
    rep(s[1], length(s))
  })

  factor(unlist(v))
}

# -----------------------------------------------------------------------
# Household / person demographic intermediates
# -----------------------------------------------------------------------

edutenr <- function(pums) {

  # Person: Education (Tenure table breaks)

  edu.brks <- c(0, 16, 18, 21, Inf)
  edu.labels <- c(paste(edu.brks[1:3], edu.brks[2:4] - 1, sep = " - "), "Bachelor+")

  v <- cut(
    as.numeric(pums$SCHL),
    breaks = edu.brks,
    include.lowest = TRUE,
    right = FALSE,
    labels = edu.labels
  )

  model.matrix(~ v - 1)
}

agehhincr <- function(pums) {

  # Household: Householder Age (Income table breaks)

  age.brks.hhinc <- c(0, 25, 45, 65, Inf)
  age.hhinc.labels <- c(paste(age.brks.hhinc[1:3], age.brks.hhinc[2:4] - 1, sep = "-"), "65+")

  v <- cut(
    as.numeric(pums$AGEP),
    breaks = age.brks.hhinc,
    include.lowest = TRUE,
    right = FALSE,
    labels = age.hhinc.labels
  )

  v <- assign_householder_item_level(pums, v)

  model.matrix(~ v - 1)
}

agetenr <- function(pums) {

  # Household: Householder Age (Tenure table breaks)

  age.brks.tenure <- c(0, 15, 25, 35, 45, 55, 60, 65, 75, 85, Inf)
  age.tenure.labels <- c(paste(age.brks.tenure[1:9], age.brks.tenure[2:10] - 1, sep = "-"), "85+")

  v <- cut(
    as.numeric(pums$AGEP),
    breaks = age.brks.tenure,
    include.lowest = TRUE,
    right = FALSE,
    labels = age.tenure.labels
  )

  v <- assign_householder_item_level(pums, v)

  model.matrix(~ v - 1)
}

hheadr <- function(pums) {

  # Person: Head of Household (yes/no)

  v <- factor(ifelse(pums$SPORDER == 1, 1, 0))

  model.matrix(~ v - 1)
}

hhincr <- function(pums) {

  # Household: Income

  hhinc.brks <- c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200, Inf)
  hhinc.labels <- c(paste0(paste(hhinc.brks[1:15], hhinc.brks[2:16] - 0.1, sep = "-"), "k"), "200k+")

  # safeguard - treat negative income as 0 income
  HHINCOME.safe <- pums$HINCP
  HHINCOME.safe[HHINCOME.safe < 0] <- 0

  v <- cut(
    as.numeric(HHINCOME.safe),
    breaks = 1000 * hhinc.brks,
    include.lowest = TRUE,
    right = FALSE,
    labels = hhinc.labels
  )

  model.matrix(~ v - 1)
}

hhinctenr <- function(pums) {

  # Household: Income (Tenure table breaks)

  hhinc.brks <- c(0, 5, 10, 15, 20, 25, 35, 50, 75, 100, 150, Inf)
  hhinc.labels <- c(paste0(paste(hhinc.brks[1:10], hhinc.brks[2:11] - 0.1, sep = "-"), "k"), "150k+")

  # safeguard - treat negative income as 0 income
  HHINCOME.safe <- pums$HINCP
  HHINCOME.safe[HHINCOME.safe < 0] <- 0

  v <- cut(
    as.numeric(HHINCOME.safe),
    breaks = 1000 * hhinc.brks,
    include.lowest = TRUE,
    right = FALSE,
    labels = hhinc.labels
  )

  model.matrix(~ v - 1)
}

tenr <- function(pums) {

  # Household: Tenure

  v <- factor(with(pums, ifelse(TEN %in% c("1", "2"), "Own", "Rent")))

  model.matrix(~ v - 1)
}

