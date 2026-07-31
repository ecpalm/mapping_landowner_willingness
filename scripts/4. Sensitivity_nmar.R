
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
# Script description: 4. Run sensitivity analysis to assess the degree to which 
#                     missing survey data responses occurred not at random.
#
# Script author: Eric Palm
#
################################################################################

# Load packages

sapply(
  c("brms", "mice", "dplyr", "purrr", "tibble", "forcats", 
    "ggplot2", "ggokabeito"),
  require,
  character.only = T
)

# Import survey data
df <- readRDS("data/surver_with_land_cover.rds") 

# Define variables with missing data
missing_idx <- list(
  Allow         = is.na(df$Allow),
  Age           = is.na(df$Age),
  Education     = is.na(df$Education),
  Income        = is.na(df$Income)
)

# Number of imputations for mice
m <- 20

tictoc::tic()

# Set seed for reproducibility
set.seed(1234)

# Create imputed datasets with mice
imp_base <- mice::mice(df, m = m, print = FALSE)

tictoc::toc()

# Variable-specific delta ranges
# Max shift in ordinal levels should not exceed (n_levels - 1),
# and for variables with few levels, cap at ±1

delta_specs <- list(
  Age           = c(-2, -1, 0, 1, 2),  # 4 levels, ±2 is boundary but defensible
  Education     = c(-1, 0, 1),          # 3 levels, ±2 would collapse distribution
  Income        = c(-2, -1, 0, 1, 2),  # 4 levels, same as Age
  Allow         = c(-1, 0, 1)           # binary, ±1 ordinal unit is the only option
)

# Build a long grid varying ONE variable at a time,
# all others held at delta = 0
sensitivity_grid <- purrr::imap_dfr(delta_specs, function(deltas, var) {
  tibble::tibble(shifted_var = var,
                 delta       = deltas)
}) %>%
  # Remove redundant baseline rows (delta=0 appears once per variable;
  # collapse to a single baseline scenario)
  dplyr::filter(!(delta == 0))  %>%
  # Add back a single true baseline row
  dplyr::bind_rows(tibble::tibble(shifted_var = "baseline", delta = 0))

# Function that shifts ordinal levels by specified amount

apply_delta_shift <- function(imp_obj, var, delta, missing_idx, level_bounds) {
  # level_bounds: named list, e.g. list(Age = c(1,4), Education = c(1,3), ...)
  # imp_obj: mids object from mice
  # var: variable name as string
  # delta: integer shift (e.g., -1, 0, +1, +2)
  # missing_idx: logical vector, TRUE where original data was NA
  
  lapply(1:imp_obj$m, function(i) {
    df_i <- mice::complete(imp_obj, i)
    
    bounds   <- level_bounds[[var]]
    min_level <- bounds[1]
    max_level <- bounds[2]
    
    orig_vals <- as.integer(df_i[[var]])
    shifted   <- orig_vals
    
    # Only shift the originally-missing cases
    miss_rows <- missing_idx[[var]]
    shifted[miss_rows] <- pmin(
      pmax(orig_vals[miss_rows] + delta, min_level),
      max_level
    )
    
    # Preserve original factor levels
    df_i[[var]] <- factor(shifted,
                          levels = min_level:max_level,
                          labels = levels(df[[var]]),
                          ordered = T)
    df_i
  })
}

# Define bounds for each variable so shifts don't create unobserved levels
level_bounds <- list(
  Age           = c(1, 4),
  Education     = c(1, 3),
  Income        = c(1, 4),
  Allow         = c(0, 1)   # binary response variable, handled separately below
)

# Fit function, handling baseline and per-variable shifts

fit_sensitivity_scenario <- function(shifted_var, delta, imp_base, missing_idx) {
  
  cat("Fitting: shifted_var =", shifted_var, "| delta =", delta, "\n")
  
  if (shifted_var == "baseline" || delta == 0) {
    imp_list <- lapply(1:imp_base$m, function (i) mice::complete(imp_base, i))
    
  } else if (shifted_var == "Allow") {
    # Binary: flip imputed 0s to 1s or vice versa based on delta direction.
    # delta = +1: shift missing allow toward 1 (more willing to allow)
    # delta = -1: shift missing allow toward 0 (less willing to allow)
    imp_list <- lapply(1:imp_base$m, function(i) {
      df_i     <- complete(imp_base, i)
      miss_rows <- missing_idx[["Allow"]]
      if (delta > 0) {
        # Replace imputed 0s with 1s among missing cases
        df_i$Allow[miss_rows & df_i$Allow == 0] <- 1L
      } else {
        # Replace imputed 1s with 0s among missing cases
        df_i$Allow[miss_rows & df_i$Allow == 1] <- 0L
      }
      df_i
    })
    
  } else {
    imp_list <- apply_delta_shift(
      imp_obj     = imp_base,
      var         = shifted_var,
      delta       = delta,
      missing_idx = missing_idx,
      level_bounds = level_bounds
    )
  }

  # Actual model fitting with formula and priors  
  fit <- brms::brm_multiple(
    formula = brms::bf(Allow ~ 0 + Intercept +
                         brms::mo(Age) +
                         brms::mo(Education) +
                         brms::mo(Income) +
                         brms::mo(Property_size) +
                         (1 | Land_cover) +
                         (1 | State)),
    data    = imp_list,
    family  = brms::bernoulli(),
    prior   = c(brms::prior(brms::dirichlet(1, 1), class = "simo", coef = "moEducation1"),
                brms::prior(brms::exponential(.5), class = sd),
                brms::prior(brms::dirichlet(1, 1, 1), class = "simo", coef = "moAge1"),
                brms::prior(brms::dirichlet(1, 1, 1), class = "simo", coef = "moIncome1"),
                brms::prior(brms::dirichlet(1, 1, 1, 1, 1, 1), class = "simo", coef = "moProperty_size1"),
                brms::prior(brms::normal(0, 2), class = b, coef = "Intercept"),
                brms::prior(brms::normal(0, 1), class = b)),
    chains  = 4,
    iter    = 2000,
    cores   = 4,
    control = list(adapt_delta = 0.995, max_treedepth  = 10),
    combine = TRUE,
    file    = paste0("fit_", shifted_var, "_delta", delta),
    silent  = 2
  )
  
  list(shifted_var = shifted_var, delta = delta, fit = fit)
}

# Run all scenarios 
tictoc::tic()

# Store the results
sensitivity_results <- purrr::pmap(
  sensitivity_grid,
  fit_sensitivity_scenario,
  imp_base    = imp_base,
  missing_idx = missing_idx
)

tictoc::toc()

# Count model fits to confirm there are as many models as you expected
nrow(sensitivity_grid)
# baseline:  1
# Age:       4 (delta -2,-1,1,2)
# Education: 2 (delta -1,1)
# Income:    4 (delta -2,-1,1,2)
# allow:     2 (delta -1,1)
# Total:    13 model fits, each across 20 imputed datasets


# Extract posteriors for summarizing in a plot
extract_key_params <- function(result) {
  
  fit <- result$fit
  
  summ <- posterior_summary(
    fit,
    pars = c("bsp_moAge", "bsp_moEducation",
             "bsp_moIncome")
  )
  
  as.data.frame(summ) %>%
    tibble::rownames_to_column("parameter") %>%
    dplyr::mutate(
      shifted_var = result$shifted_var,
      delta       = result$delta
    )
}

sensitivity_table <- purrr::map_dfr(sensitivity_results, extract_key_params)

# Clean up for plotting 

# Define labels for facets and legend
param_labels <- c(
  bsp_moAge           = "Age",
  bsp_moEducation     = "Education",
  bsp_moIncome        = "Income"
)

var_labels <- c(
  baseline  = "Baseline imputation",
  Age       = "Age",
  Education = "Education",
  Income    = "Income",
  Allow     = "Allow (binary response)"
)

plot_data <- sensitivity_table %>%
  dplyr::mutate(
    parameter   = param_labels[parameter],
    shifted_var = factor(var_labels[shifted_var],
                         levels = var_labels),  
    scenario = dplyr::case_when(
      shifted_var == "Baseline imputation" ~ "Baseline\n(MAR)",
      TRUE ~ paste0(shifted_var, "\nδ=", delta)
    )
  )

# Plot

# Extract baseline estimates for reference lines
baseline_vals <- plot_data %>%
  dplyr::filter(shifted_var == "Baseline imputation") %>%
  dplyr::select(parameter, Estimate) %>%
  dplyr::rename(baseline_est = Estimate)

plot_data %>%
  dplyr::left_join(baseline_vals, by = "parameter") %>%
  ggplot2::ggplot(ggplot2::aes(x = delta, y = Estimate,
                               ymin = Q2.5, ymax = Q97.5,
                               colour = shifted_var,
                               group  = shifted_var)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = baseline_est),
                      linetype = "dashed", colour = "black") +
  ggplot2::geom_hline(yintercept = 0, colour = "white", linewidth = .3) +
  ggplot2::geom_hline(yintercept = 0,
                      linetype = "dotted", colour = "black", linewidth = .4) +
  ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), expand = ggplot2::expansion(mult = c(.01, .01))) +
  ggplot2::scale_color_manual(values = c("black", ggokabeito::palette_okabe_ito()[c(6,3,5,7)]), name = "Shifted variable") +
  ggplot2::facet_wrap(~parameter, ncol = 2) +
  ggplot2::labs(
    x        = "Delta (ordinal levels shifted)",
    y        = "Posterior mean (95% CI)"
  ) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Helvetica"),
    axis.text = ggplot2::element_text(color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray60", linetype = "dotted", linewidth = .2),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 13),
    axis.title.y = ggplot2::element_text(size = 13),
    legend.position = "inside",
    legend.position.inside = c(.79, .22),
    legend.title.position = "top",
    legend.title = ggplot2::element_text(hjust = .5, size = 13),
    legend.box.margin = ggplot2::margin(l = -30)
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.005, .01)))

# Save final plot
ggplot2::ggsave("figures/nmar_sensitivity.tiff", compression = "lzw", height = 6, width = 7, dpi = 600, bg = "white")
