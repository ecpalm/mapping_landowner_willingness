# Mapping landowners' willingness to allow management actions on their property
This repository includes data and code for the following article published in *The Journal of Wildlife Management*: 

**"Mapping landowners' willingness to allow recreational deer harvest on their property for managing chronic wasting disease"**
by Eric Palm, Matthew Williamson, Nathan Snow, and Sonja Christensen

Article DOI: https://doi.org/10.1002/jwmg.70254

# Note 
- We do not include survey respondent addresses in the survey dataset.
- The final model used in this dataset was fitted using 20 datasets with imputed values for all missing data in the survey dataset. We include to fit this fully imputed model, but because the resulting model object is far too large for a github repository, all subsequent code in the repository uses a model fitted to a dataset with all missing values ommitted.
- Please open the `mapping_landowner_willingness.Rproj` file to start RStudio before opening individual scripts to ensure that relative file paths in the code work correctly.


# Data and code DOI: 

https://doi.org/10.5281/zenodo.21479375


# List of R scripts

Here is a list of scripts with descriptions (**additional scripts are forthcoming**):

`1. Fit_logistic_regression.R` – Fit logistic regression to survey data.

`2. Marginal_posterior_predictions.R` – Plot marginal posterior predicted probabilities from fitted logistic regression.
