
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
# Script description: Map predictions from fitted logistic regression model, 
#                     poststratifying using US Census Data (PUMS records allocated 
#                     into census tracts)
#
# Script author: Eric Palm
#
################################################################################

# Load packages
sapply(
  c("dplyr", "sf", "terra", "stringr", "ggplot2", "ggnewscale", "ggspatial",
    "tidyr", "patchwork"),
  require,
  character.only = TRUE
)


# Input spatial data and fitted model used for predictions
tracts_no_padus  <- sf::read_sf("spatial/tracts_no_padus.gpkg")
parcels_no_padus <- sf::read_sf("spatial/parcels_no_padus.gpkg")
fit              <- readRDS("models/fit_logistic.rds")

# Constants

# Coordinate reference system
CRS_TARGET  <- "ESRI:102039"
STATE_FOCAL <- "WY"

# Desired spatial resolution for mapping (in meters)
RES         <- 500

# ---- state boundary + raster template ----
state_sf <- tigris::states() %>%
  dplyr::filter(STUSPS == STATE_FOCAL) %>%
  sf::st_transform(CRS_TARGET)

# Create a template raster, first adding a small buffer around the border
# to ensure it covers the entire state
rast_template <- state_sf %>%
  terra::vect() %>%
  terra::buffer(RES / 2 + 1) %>%
  terra::ext() %>%
  as.vector() %>%
  plyr::round_any(RES, f = round) %>%
  terra::ext() %>%
  terra::rast(resolution = RES, crs = CRS_TARGET)

# Reclassify parcels into property-size bins and dissolve all polygons within
# a size class into a massive multipolygon
# This step takes a long time
parcel_reclass <- parcels_no_padus %>%
  dplyr::mutate(value = dplyr::case_when(
    acres < 3                   ~ 0,
    acres >= 3   & acres < 10   ~ 1,
    acres >= 10  & acres < 50   ~ 2,
    acres >= 50  & acres < 100  ~ 3,
    acres >= 100 & acres < 250  ~ 4,
    acres >= 250 & acres < 500  ~ 5,
    acres >= 500 & acres < 1000 ~ 6,
    TRUE                        ~ 7
  )) %>%
  dplyr::select(value) %>%
  dplyr::group_by(value) %>%
  dplyr::summarize()

# Rasterize parcel polygons into template raster, where the resulting pixel
# value is the parcel size that composes the largest area of the pixel
rast_parcel <- exactextractr::rasterize_polygons(parcel_reclass, rast_template, min_coverage = .5) %>%
  terra::app(function(x) x - 1) %>%
  terra::classify(cbind(-Inf, .5, NA)) %>%
  `names<-`("Property_size")

# Create a polygon object of the properties <3 acres for plotting on top in gray
parcels_small_NA <- exactextractr::rasterize_polygons(parcel_reclass, rast_template, min_coverage = .5) %>%
  terra::classify(cbind(1.5, Inf, NA)) %>%
  terra::as.polygons() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(State = STATE_FOCAL) %>%
  sf::st_cast("POLYGON") %>%
  dplyr::select(-lyr.1) %>%
  sf::st_intersection(state_sf)

# Allocate demographic estimates to census tracts
edu_levels <- c("No college", "Some college", "Bachelor's +")
age_levels <- c("18–34 years", "35–44 years", "45–64 years", "≥65 years")
inc_levels <- c("<$50,000", "$50,000–$99,999", "$100,000–$149,999", "≥$150,000")

allocated_ps <- readRDS(stringr::str_c("results/allocation_", STATE_FOCAL, ".rds")) %>%
  dplyr::mutate(State = as.factor(STATE_FOCAL)) %>%
  dplyr::select(State, PUMA, Education = edu, Age = age, Income = inc, GEOID, n = cell_est) %>%
  dplyr::mutate(
    Education = factor(dplyr::case_when(
      stringr::str_detect(Education, "hs")   ~ "No college",
      stringr::str_detect(Education, "some") ~ "Some college",
      TRUE                                   ~ "Bachelor's +"
    ), levels = edu_levels, ordered = TRUE),
    Age = factor(dplyr::case_when(
      stringr::str_detect(Age, "18") ~ "18–34 years",
      stringr::str_detect(Age, "35") ~ "35–44 years",
      stringr::str_detect(Age, "45") ~ "45–64 years",
      TRUE                           ~ "≥65 years"
    ), levels = age_levels, ordered = TRUE),
    Income = factor(dplyr::case_when(
      stringr::str_detect(Income, "under") ~ "<$50,000",
      stringr::str_detect(Income, "99")    ~ "$50,000–$99,999",
      stringr::str_detect(Income, "149")   ~ "$100,000–$149,999",
      TRUE                                 ~ "≥$150,000"
    ), levels = inc_levels, ordered = TRUE)
  ) %>%
  dplyr::group_by(dplyr::across(-n), .drop = FALSE) %>%
  dplyr::summarize(n = sum(n), .groups = "drop") %>%
  dplyr::mutate(prop_demo = n / sum(n), .by = GEOID)

# build target polygons: intersect the property-size raster with tracts 
property_size_levels <- c("3–9 acres", "10–49 acres", "50–99 acres", "100–249 acres",
                          "250–499 acres", "500–999 acres", "≥1,000 acres")

sf_target <- terra::as.polygons(rast_parcel) %>%
  terra::intersect(terra::vect(tracts_no_padus)) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    State = as.factor(STATE_FOCAL),
    Property_size = factor(dplyr::case_when(
      Property_size == 1 ~ "3–9 acres",
      Property_size == 2 ~ "10–49 acres",
      Property_size == 3 ~ "50–99 acres",
      Property_size == 4 ~ "100–249 acres",
      Property_size == 5 ~ "250–499 acres",
      Property_size == 6 ~ "500–999 acres",
      Property_size == 7 ~ "≥1,000 acres"
    ), levels = property_size_levels, ordered = TRUE)
  )

for_predict <- allocated_ps %>%
  dplyr::inner_join(sf::st_drop_geometry(sf_target))

# MRP prediction 
mrp <- for_predict %>%
  tidybayes::add_epred_draws(newdata = ., object = fit, ndraws = 200, seed = 1234, allow_new_levels = TRUE) %>%
  tidytable::mutate(epred_mrp = .epred * prop_demo) %>%
  tidytable::ungroup() %>%
  tidytable::summarize(.epred = sum(epred_mrp), 
                       .by = c(State, GEOID, Property_size, .draw)) %>%
  dplyr::summarize(prob = median(.epred), 
                   sd = stats::sd(.epred), 
                   .by = c(State, GEOID, Property_size)) %>%
  dplyr::inner_join(sf_target, .) %>%
  sf::st_sf()

preds_harvest_mrp <- mrp %>%
  sf::st_intersection(state_sf)

padus_no_private <- sf::read_sf("spatial/padus_no_private.gpkg") %>%
  sf::st_intersection(state_sf)

# Function to map predictions
require(sf)
require(terra)
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(ggnewscale)
require(ggspatial)
require(ggtext)
require(patchwork)
require(ragg)

# ---- inputs ----
tracts_no_padus  <- sf::read_sf("data/tracts_no_padus.gpkg")
parcels_no_padus <- sf::read_sf("data/parcels_no_padus.gpkg")
fit              <- readRDS("models/fit_logistic.rds")

# ---- constants ----
CRS_TARGET  <- "ESRI:102039"
STATE_FOCAL <- "WY"
RES         <- 500

ggplot2::theme_set(ggplot2::theme_void(base_family = "Helvetica", base_size = 17))

# ---- state boundary + raster template ----
state_sf <- tigris::states() %>%
  dplyr::filter(STUSPS == STATE_FOCAL) %>%
  sf::st_transform(CRS_TARGET) %>%
  dplyr::mutate(fill_label = "Public lands, protected areas,\ntribal lands, and properties <3 acres")

rast_template <- state_sf %>%
  terra::vect() %>%
  terra::buffer(RES / 2 + 1) %>%
  terra::ext() %>%
  as.vector() %>%
  plyr::round_any(RES, f = round) %>%
  terra::ext() %>%
  terra::rast(resolution = RES, crs = CRS_TARGET)

# ---- reclassify parcels into property-size bins and rasterize ----
# This step takes a long time
parcel_reclass <- parcels_no_padus %>%
  dplyr::mutate(value = dplyr::case_when(
    acres < 3                   ~ 0,
    acres >= 3   & acres < 10   ~ 1,
    acres >= 10  & acres < 50   ~ 2,
    acres >= 50  & acres < 100  ~ 3,
    acres >= 100 & acres < 250  ~ 4,
    acres >= 250 & acres < 500  ~ 5,
    acres >= 500 & acres < 1000 ~ 6,
    TRUE                        ~ 7
  )) %>%
  dplyr::select(value) %>%
  dplyr::group_by(value) %>%
  dplyr::summarize()

# Rasterize parcel polygons into template raster, where the resulting pixel
# value is the parcel size that composes the largest area of the pixel
rast_parcel <- exactextractr::rasterize_polygons(parcel_reclass, rast_template, min_coverage = .5) %>%
  terra::app(function(x) x - 1) %>%
  terra::classify(cbind(-Inf, .5, NA)) %>%
  `names<-`("Property_size")

parcels_small_NA <- exactextractr::rasterize_polygons(parcel_reclass, rast_template, min_coverage = .5) %>%
  terra::classify(cbind(1.5, Inf, NA)) %>%
  terra::as.polygons() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(State = STATE_FOCAL) %>%
  sf::st_cast("POLYGON") %>%
  dplyr::select(-lyr.1) %>%
  sf::st_intersection(state_sf)

# ---- allocate demographic estimates to census tracts ----
edu_levels <- c("No college", "Some college", "Bachelor's +")
age_levels <- c("18–34 years", "35–44 years", "45–64 years", "≥65 years")
inc_levels <- c("<$50,000", "$50,000–$99,999", "$100,000–$149,999", "≥$150,000")

allocated_ps <- readRDS(stringr::str_c("data/ests_", STATE_FOCAL, ".rds")) %>%
  dplyr::mutate(State = as.factor(STATE_FOCAL)) %>%
  dplyr::select(State, PUMA, Education = edu, Age = age, Income = inc, GEOID, n = cell_est) %>%
  dplyr::mutate(
    Education = factor(dplyr::case_when(
      stringr::str_detect(Education, "hs")   ~ "No college",
      stringr::str_detect(Education, "some") ~ "Some college",
      TRUE                                    ~ "Bachelor's +"
    ), levels = edu_levels, ordered = TRUE),
    Age = factor(dplyr::case_when(
      stringr::str_detect(Age, "18") ~ "18–34 years",
      stringr::str_detect(Age, "35") ~ "35–44 years",
      stringr::str_detect(Age, "45") ~ "45–64 years",
      TRUE                            ~ "≥65 years"
    ), levels = age_levels, ordered = TRUE),
    Income = factor(dplyr::case_when(
      stringr::str_detect(Income, "under") ~ "<$50,000",
      stringr::str_detect(Income, "99")    ~ "$50,000–$99,999",
      stringr::str_detect(Income, "149")   ~ "$100,000–$149,999",
      TRUE                                  ~ "≥$150,000"
    ), levels = inc_levels, ordered = TRUE)
  ) %>%
  dplyr::group_by(dplyr::across(-n), .drop = FALSE) %>%
  dplyr::summarize(n = sum(n), .groups = "drop") %>%
  dplyr::mutate(prop_demo = n / sum(n), .by = GEOID)

# ---- build target polygons: intersect the property-size raster with tracts ----
# (previously wrapped in a rowwise tibble + list-columns to loop over multiple
# states; collapsed to a direct computation now that this is a single-state example)
property_size_levels <- c("3–9 acres", "10–49 acres", "50–99 acres", "100–249 acres",
                          "250–499 acres", "500–999 acres", ">1,000 acres")

sf_target <- terra::as.polygons(rast_parcel) %>%
  terra::intersect(terra::vect(tracts_no_padus)) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    State = as.factor(STATE_FOCAL),
    Property_size = factor(dplyr::case_when(
      Property_size == 1 ~ "3–9 acres",
      Property_size == 2 ~ "10–49 acres",
      Property_size == 3 ~ "50–99 acres",
      Property_size == 4 ~ "100–249 acres",
      Property_size == 5 ~ "250–499 acres",
      Property_size == 6 ~ "500–999 acres",
      Property_size == 7 ~ ">1,000 acres"
    ), levels = property_size_levels, ordered = TRUE)
  )

for_predict <- allocated_ps %>%
  dplyr::inner_join(sf::st_drop_geometry(sf_target))

# ---- MRP prediction ----
mrp <- for_predict %>%
  tidybayes::add_epred_draws(newdata = ., object = fit, ndraws = 200, seed = 1234, allow_new_levels = TRUE) %>%
  tidytable::mutate(epred_mrp = .epred * prop_demo) %>%
  tidytable::ungroup() %>%
  tidytable::summarize(.epred = sum(epred_mrp), .by = c(State, GEOID, Property_size, .draw)) %>%
  dplyr::summarize(prob = median(.epred), sd = stats::sd(.epred), .by = c(State, GEOID, Property_size)) %>%
  dplyr::inner_join(sf_target, .) %>%
  sf::st_sf()

preds_harvest_mrp <- mrp %>%
  sf::st_intersection(state_sf)

padus_no_private <- sf::read_sf("spatial/padus_no_private.gpkg") %>%
  sf::st_intersection(state_sf)

# ---- map-building helper (replaces near-duplicate med/sd ggplot blocks) ----

make_prediction_map <- function(fill_var, legend_name, viridis_option = "viridis",
                                breaks = ggplot2::waiver(), scale_bar = FALSE, north_arrow = FALSE) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = state_sf, ggplot2::aes(fill = fill_label), color = NA) +
    ggplot2::scale_fill_manual(values = c("gray70"), name = " ",
                               guide = ggplot2::guide_legend(order = 2, title.position = "top")) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = preds_harvest_mrp,
                     ggplot2::aes(fill = .data[[fill_var]], color = ggplot2::after_scale(fill))) +
    ggplot2::scale_fill_viridis_c(
      name = legend_name, breaks = breaks,
      guide = ggplot2::guide_colorbar(
        barwidth = 14, title.hjust = .5, title.position = "top", order = 1,
        theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
      ),
      option = viridis_option
    ) +
    ggplot2::geom_sf(data = padus_no_private, fill = "gray70", color = NA) +
    ggplot2::geom_sf(data = parcels_small_NA, fill = "gray70", color = NA) +
    ggplot2::geom_sf(data = state_sf, fill = NA, color = "black", linewidth = .2) +
    ggplot2::theme_void(base_size = 17)
  
  if (scale_bar) {
    p <- p + ggspatial::annotation_scale(
      location = "bl", text_cex = 1.2, text_family = "Helvetica",
      height = ggplot2::unit(.25, "cm"), pad_y = ggplot2::unit(.25, "cm")
    )
  }
  
  if (north_arrow) {
    p <- p + ggspatial::annotation_north_arrow(
      location = "bl", which_north = "true",
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(1, "cm"), width = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(text_family = "Helvetica", text_size = 14)
    )
  }
  
  p + ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    text = ggplot2::element_text(family = "Helvetica"),
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.spacing.x = ggplot2::unit(3, "cm")
  )
}

med_plot <- make_prediction_map("prob", "Predicted probability")
sd_plot  <- make_prediction_map(
  "sd", "Predicted standard deviation",
  viridis_option = "magma",
  scale_bar = TRUE,
  north_arrow = TRUE
)

# Caption with controlled line breaks/sizes via markdown
caption_text <- paste0(
  "<span style='font-size:20pt;'>Landowner willingness to allow recreational deer harvest</span><br>",
  "<span style='font-size:13pt;'>(500-m spatial resolution)</span>"
)

combined_plot <- (med_plot + sd_plot) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(caption = caption_text) &
  ggplot2::theme(
    plot.caption = ggtext::element_markdown(
      hjust = 0.5,
      lineheight = 1.3,
      margin = ggplot2::margin(t = 15)
    ),
    legend.position = "bottom",
    legend.spacing.x = ggplot2::unit(2.5, "cm")
  )

combined_plot

# Save with ragg for crisp output
ggplot2::ggsave(
  stringr::str_c("figures/preds_map_", tolower(STATE_FOCAL), ".png"),
  plot = combined_plot,
  device = ragg::agg_png,
  width = 13, height = 7, units = "in",
  dpi = 300, bg = "white"
)
