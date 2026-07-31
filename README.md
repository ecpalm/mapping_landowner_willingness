# Mapping landowners' willingness to allow management actions on their property
This repository includes data and code for the following article published in *The Journal of Wildlife Management*: 

**"Mapping landowners' willingness to allow recreational deer harvest on their property for managing chronic wasting disease"**
by Eric Palm, Matthew Williamson, Nathan Snow, and Sonja Christensen

Article DOI: https://doi.org/10.1002/jwmg.70254

# Note 
- We do not include survey respondent addresses in the survey dataset.
- The final model used in this analysis was fitted using 20 datasets with imputed values for all missing data in the survey dataset. We include to fit this fully imputed model, but because the resulting model object is far too large for a github repository, all subsequent code in the repository uses a model fitted to a dataset with all missing values ommitted.
- Please open the `mapping_landowner_willingness.Rproj` file to start RStudio before opening individual scripts to ensure that relative file paths in the code work correctly.


# Data and code DOI: 

https://doi.org/10.5281/zenodo.21479375


# List of R scripts

Here is a list of scripts with descriptions (**additional scripts are forthcoming**):

`1. Fit_logistic_regression.R` – Fit logistic regression to survey data.

`2. Marginal_posterior_predictions.R` – Plot marginal posterior predicted probabilities from fitted logistic regression.

`3. Plot_elpd_diff_by_variable.R` – Plot ELPD values to assess variables' ability to predict withheld data in LOO CV.

`4. Sensitivity_nmar.R` – Sensitivity analysis assessing assumption of survey data responses missing at random.

`5. Download_prep_parcel_data.R` – Download and prepare spatial data for tax parcels, protected areas, and census tracts to be used for mapping predictions in a single state (Wyoming).

`6. Download_prep_parcel_data.R` – Run the full penalized maximum entropy model pipeline to probabilistically allocate individual microdata (PUMS) records to census tracts within a single state.

`6a_PME_model.R` – Wrapper around PMEDMrcpp::pmedm_solve. Written by Joe Tuccillo (https://github.com/jvtcl/pmedmize) and Nicholas Nagle (https://bitbucket.org/nnnagle/pmedmrcpp/src/master/).

`6b_Constraints.R` – Helper functions for preparing PME model constraints (individual and area level) from raw US Census ACS Summary File tables.

`6c_Intermediates.R` – Definitions for intermediate variables used to build individual-level PME model constraints from PUMS microdata: age, income, education, and tenure.

`6d_Build_constraints_ind.R` – Definitions for ACS Summary File constraints at the individual (PUMS) level. Each function reconstructs one ACS table's cell structure from PUMS microdata, using the intermediates defined in 6c_intermediates.R. Tables: B25013 (tenure x education), B25007 (tenure x age), B25118 (tenure x income).

`6e_Build_constraints_geo.R` – Builds PME model area-level constraints from the ACS Summary File using the US Census API.

`6f_Build_puma_lookup.R` – Builds a Census tract-to-PUMA lookup table for a given state, handling the Connecticut 2022 crosswalk.

`7. Map_predictions_single_state.R` – Map predictions from fitted logistic regression model in a single state (Wyoming), poststratifying using US Census Data (PUMS records allocated into census tracts).
