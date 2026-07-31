
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
# Script description: 1. Fit logistic regression to survey data
#
# Script author: Eric Palm
#
################################################################################

# Load packages
sapply(
  c("dplyr", "brms", "mice"), 
  require, 
  character.only = T)

# Import survey data
df <- readRDS("data/survey_with_land_cover.rds")

# To impute missing values for Age, Education, Income, and Allow, and then
# fit separate models for each imputed dataset, follow code below. The model 
# fitting code is commented out because the fully imputed model is too large 
# to store in a github repository. Therefore, the remaining code in this 
# repository is based on the model below that was fitted without imputed data.

# Number of datasets with imputed missing values
m <- 20

# Set a seed for reproducibility
set.seed(1234)

# Use defaults for multiple imputation in 'mice'
imp <- mice::mice(df, m = m, print = F)

# Fit separate models using imputed datasets using brms::brm_multiple. This
# will take a long time and generates a very large model object. This is the 
# final model used in the manuscript.

# fit_imp <-
#   brms::brm_multiple(data = imp,
#                      family = brms::bernoulli(),
#                      Allow ~ 0 + Intercept +
#                        mo(Age) +
#                        mo(Education) +
#                        mo(Income) +
#                        mo(Property_size) +
#                        (1 | Land_cover) +
#                        (1 | State),
#                      prior = c(prior(normal(0, 2), class = b, coef = "Intercept"),
#                                prior(dirichlet(1, 1, 1), class = "simo", coef = "moAge1"),
#                                prior(dirichlet(1, 1), class = "simo", coef = "moEducation1"),
#                                prior(dirichlet(1, 1, 1), class = "simo", coef = "moIncome1"),
#                                prior(dirichlet(1, 1, 1, 1, 1, 1), class = "simo", coef = "moProperty_size1"),
#                                prior(exponential(0.5), class = sd)),
#                      control = list(adapt_delta = 0.995,
#                                     max_treedepth = 10),
#                      seed = 1234,
#                      cores = 4)

# Save model output
# saveRDS(fit_imp, "models/fit_logistic_multiple.rds")

# Fit a model without imputing missing values for use in the rest of this 
# repository. This model is fitted using a total of 1129 survey responses.
fit <-
  brms::brm(data = df,
            family = brms::bernoulli(),
            Allow ~ 0 + Intercept +
              mo(Age) +
              mo(Education) +
              mo(Income) +
              mo(Property_size) +
              (1 | Land_cover) +
              (1 | State),
            prior = c(prior(normal(0, 2), class = b, coef = "Intercept"),
                      prior(dirichlet(1, 1, 1), class = "simo", coef = "moAge1"),
                      prior(dirichlet(1, 1), class = "simo", coef = "moEducation1"),
                      prior(dirichlet(1, 1, 1), class = "simo", coef = "moIncome1"),
                      prior(dirichlet(1, 1, 1, 1, 1, 1), class = "simo", coef = "moProperty_size1"),
                      prior(exponential(0.5), class = sd)),
            control = list(adapt_delta = 0.995,
                           max_treedepth = 10),
            seed = 1234, 
            cores = 4)

# Save model output
saveRDS(fit, "models/fit_logistic.rds")
