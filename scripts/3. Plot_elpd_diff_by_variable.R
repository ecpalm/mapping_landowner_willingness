
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
# Script description: Plot differences in expected log-predictive density 
#                     from LOO CV between full model and models missing one 
#                     variable
#
# Script author: Eric Palm
#
################################################################################

# Load packages

sapply(
  c("brms", "loo", "dplyr", "tibble", "purrr", "forcats", "ggplot2"),
  require,
  character.only = T
)

# Load full fitted model
fit <- readr::readRDS("models/fit_logistic.rds")

FIT_CONTROL <- list(adapt_delta = 0.995, max_treedepth = 10)
SEED  <- 1010
CORES <- 4

# For each variable, set the formula term to drop from the full model
drop_terms <- tibble::tibble(
  label = c("State", "Age", "Education", "Income", "Property size", "Land cover"),
  term  = c("(1 | State)", "mo(Age)", "mo(Education)", "mo(Income)", "mo(Property_size)", "(1 | Land_cover)")
)

# Run all models, each missing one variable
fit_reduced <- function(fit, term, control = FIT_CONTROL, seed = SEED, cores = CORES) {
  update(
    fit,
    formula. = stats::as.formula(paste("~ . -", term)),
    control = control, seed = seed, cores = cores
  )
}

reduced_fits <- purrr::map(drop_terms$term, ~ fit_reduced(fit_harvest, .x)) %>%
  purrr::set_names(drop_terms$label)

all_fits <- c(list(Full = fit_harvest), reduced_fits)

# Calculate the ELPDs of all model fits
all_fits_loo <- purrr::map(all_fits, ~ brms::add_criterion(.x, "loo"))

# Compare the ELPDs
elpds_harvest <- loo::loo_compare(x = all_fits_loo)

# Build the labeled and plot-ready tibble 
elpds_harvest_labeled <-
  elpds_harvest %>%
  as.data.frame() %>%
  tibble::rownames_to_column("model") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(dplyr::across(-model, ~ -1 * as.numeric(.)))

# Calculate the difference in ELPD between each model and that of the full model
full_diff <- elpds_harvest_labeled$elpd_diff[elpds_harvest_labeled$model == "Full"]

# Prepare for final plotting
elpd_plot_data <-
  elpds_harvest_labeled %>%
  dplyr::mutate(dplyr::across(-model, ~ . - full_diff)) %>%
  dplyr::filter(model != "Full") %>%
  dplyr::mutate(model = forcats::fct_reorder(model, elpd_diff))

# Plot
elpd_plot_data %>%
  ggplot2::ggplot(ggplot2::aes(x = elpd_diff, y = model)) +
  ggplot2::geom_col(fill = RColorBrewer::brewer.pal(3, "Dark2")[3], alpha = .8) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = .4) +
  ggplot2::labs(
    y = "Variable",
    x = "Improvement in expected log-predictive density\nfrom including variable",
    fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 18) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Helvetica"),
    axis.text = ggplot2::element_text(color = "black"),
    legend.position = "bottom",
    axis.title = ggplot2::element_text(size = 18),
    axis.ticks = ggplot2::element_line(linewidth = .4),
    axis.line = ggplot2::element_line(linewidth = .4)
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.02, .02))) +
  ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(.02, .02)))

# Save the plot
ggplot2::ggsave(
  "figures/loo_elpd_diffs_by_variable.tiff",
  height = 5, width = 7, dpi = 600, compression = "lzw"
)