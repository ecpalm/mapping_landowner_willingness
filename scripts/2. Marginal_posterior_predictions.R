
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
# Script description: 2. Plot marginal posterior predicted probabilities from 
#                     fitted logistic regression.
#
# Script author: Eric Palm
#
################################################################################

# Load packages
sapply(
  c("dplyr", "brms", "tidybayes", "ggplot2", "forcats", "purr", "tibble", "tidytable", "patchwork"), 
  require, 
  character.only = T)

# Load fitted model object
fit <- readRDS("models/fit_logistic.rds")
d   <- fit$data

# Set number of draws for plotting and set seed for reproducibility
NDRAWS <- 4000
SEED   <- 1234

# For each variable, set the other variables to hold at their modal values,
# what the panel title is, and whether to reverse the y-axis order for plotting.
plot_vars <- tibble::tibble(
  var = c("Property_size", "Income", "Education", "Age", "Land_cover", "State"),
  hold_vars = list(
    c("Income", "Education", "Age"),
    c("Property_size", "Education", "Age"),
    c("Property_size", "Income", "Age"),
    c("Property_size", "Education", "Income"),
    c("Property_size", "Income", "Education", "Age"),
    c("Property_size", "Income", "Education", "Age")
  ),
  title = c(
    "Property size",
    "Household income",
    "Highest level of education",
    "Age",
    "Land cover",
    "State"
  ),
  reverse = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)

# Function to generate model draws and prep data for plotting
get_marginal_pred <- function(fit, target_var, hold_vars, ndraws = NDRAWS, seed = SEED) {
  
  d <- fit$data
  n_levels <- nlevels(d[[target_var]])
  
  newdat <- d %>%
    dplyr::count(dplyr::across(dplyr::all_of(hold_vars))) %>%
    dplyr::slice(which.max(n)) %>%
    dplyr::slice(rep(1, n_levels))
  
  newdat[[target_var]] <- levels(d[[target_var]])
  
  newdat %>%
    tidybayes::add_epred_draws(newdata = ., 
                               object = fit, 
                               ndraws = ndraws, 
                               seed = seed, 
                               allow_new_levels = TRUE) %>%
    tidytable::ungroup() %>%
    tidytable::summarize(pred = mean(.epred), .by = c(dplyr::all_of(target_var), .draw)) %>%
    dplyr::mutate(!!target_var := factor(.data[[target_var]], levels = levels(d[[target_var]])))
}

pred_list <- purrr::pmap(
  plot_vars %>% 
    dplyr::select(var, hold_vars),
  function(var, hold_vars) get_marginal_pred(fit, var, hold_vars)) %>% 
  purrr::set_names(plot_vars$var)

# Set the x axis range for all plots to be the same.
x_range <- pred_list %>% 
  purrr::map(~ range(.x$pred)) %>% 
  unlist() %>% 
  range()


# Set theme and labels for ggplot2
x_lab <- "Marginal predicted probability of landowner willingness\nto allow recreational deer harvest"

base_theme <- ggplot2::theme_classic(base_size = 16) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(color = "black"),
    legend.position = "bottom",
    strip.background = ggplot2::element_blank(),
    text = ggplot2::element_text(family = "Helvetica"),
    panel.grid.major.x = ggplot2::element_line(linewidth = .35, color = "gray70", linetype = "dotted"),
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(hjust = 0.5, size = 15, vjust = -.25),
    axis.line = ggplot2::element_line(linewidth = .3),
    axis.ticks = ggplot2::element_line(linewidth = .3),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10))
  )

# Create the plot using ggplot2 and tidybayes packages
make_marginal_plot <- function(data, var, title, x_range, reverse = FALSE) {
  
  data <- data %>%
    dplyr::mutate(.y = if (reverse) forcats::fct_rev(.data[[var]]) else .data[[var]])
  
  ggplot2::ggplot(data, ggplot2::aes(x = pred, y = .y)) +
    tidybayes::stat_slab(alpha = .2, scale = .75, show.legend = FALSE, fill = "#0072b2") +
    tidybayes::stat_pointinterval(
      position = ggplot2::position_dodge(width = .4, preserve = "single"),
      .width = c(.5, .95),
      interval_size_range = c(0.4, 1.1),
      point_size = 2.5, show.legend = FALSE, color = "#0072b2"
    ) +
    base_theme +
    ggplot2::labs(x = x_lab, color = NULL, fill = NULL, y = NULL, title = title) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.01, .01)), limits = x_range)
}

plots <- purrr::pmap(
  plot_vars,
  function(var, hold_vars, title, reverse) make_marginal_plot(pred_list[[var]], var, title, x_range, reverse)) %>% 
  purrr::set_names(plot_vars$var)

# Combine plots with patchwork
# Plotting in order of variable predictive capacity as determined by LOO CV
patchwork::wrap_plots(plots[c("Property_size", "Land_cover", "Income", "Age", "State", "Education")],
                      guides = "collect",
                      axes = "collect") & 
  ggplot2::theme(
    legend.position = "bottom",
    axis.line = ggplot2::element_line(linewidth = .4),
    axis.ticks = ggplot2::element_line(linewidth = .4)
)

# Save the plot
ggplot2::ggsave("figures/plot_marginal_predictions.tiff", compression = "lzw", width = 12.5, height = 7.5, dpi = 600)
