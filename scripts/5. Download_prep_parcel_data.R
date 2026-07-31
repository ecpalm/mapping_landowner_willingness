
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
# Script description: 5. Download tax parcel polygons for a focal state and
#                     remove protected and publicly owned areas in preparation 
#                     for spatial prediction
#
# Script author: Eric Palm
#
################################################################################

# Load packages
sapply(
  c("dplyr", "arcpullr", "sf", "rmapshaper"),
  require,
  character.only = TRUE
)

# constants 
CRS_TARGET  <- "ESRI:102039"
STATE_FOCAL <- "WY"

# Function to download, reproject, and validate a polygon layer, keeping only geometry
fetch_polygon_layer <- function(url, geom_col = "geometry", make_valid = TRUE) {
  layer <- arcpullr::get_spatial_layer(url) %>%
    sf::st_set_geometry(geom_col) %>%
    dplyr::select(dplyr::all_of(geom_col)) %>%
    sf::st_transform(CRS_TARGET)
  if (make_valid) layer <- sf::st_make_valid(layer)
  layer
}

# Helper function: download a point layer, reproject, filter to the focal state 
# state_col varies by source (e.g. "STATE" vs "state" vs "STD_ST"); rename_geom_to
# optionally standardizes the geometry column name before later bind_rows() calls
fetch_point_layer <- function(url, state_col, rename_geom_to = NULL) {
  layer <- arcpullr::get_spatial_layer(url) %>%
    sf::st_transform(CRS_TARGET) %>%
    dplyr::filter(.data[[state_col]] == STATE_FOCAL)
  if (!is.null(rename_geom_to)) layer <- sf::st_set_geometry(layer, rename_geom_to)
  layer
}

# ==== Tax parcel polygons ====
url_parcel <- "https://services3.arcgis.com/r0iJ85SKZ4zAzz3P/arcgis/rest/services/Wyoming_Parcels_for_2025/FeatureServer/0"

# This step is very time consuming; other options for downloading parcel data may be much faster
parcels <- fetch_polygon_layer(url_parcel) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::filter(!sf::st_is_empty(.))

# ==== state-owned land polygons ====
url_state_lands <- "https://gis2.statelands.wyo.gov/arcgis/rest/services/StateSurfaceOwnership/MapServer/0"

state_lands <- fetch_polygon_layer(url_state_lands)

# ==== waterbodies (> 1000 acres) ====
url_water <- "https://services.wygisc.org/HostGIS/rest/services/GeoHub/NHDWaterbody/MapServer/0"

water <- fetch_polygon_layer(url_water) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_zm() %>%
  dplyr::mutate(acres = as.numeric(units::set_units(sf::st_area(.), "acres"))) %>%
  dplyr::filter(acres > 1000)

# ==== PAD-US (protected areas + easements) ====
# Download PAD-US by state here: https://www.sciencebase.gov/catalog/item/6759abcfd34edfeb8710a004
# Can download directly through R using sbtools package but it requires a ScienceBase account
# Omit easements with private and unknown ownership
padus <- sf::read_sf("spatial/PADUS4_1_StateWY.gdb", layer = "PADUS4_1Comb_DOD_Trib_NGP_Fee_Desig_Ease_State_WY") %>%
  dplyr::left_join(
    sf::read_sf("spatial/PADUS4_1_StateWY.gdb", layer = "PADUS4_1Easement_State_WY") %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct()
  ) %>%
  dplyr::filter(is.na(Own_Type) | !Own_Type %in% c("PVT", "UNK")) %>%
  dplyr::select(Category, Mang_Type, Unit_Nm) %>%
  sf::st_cast("MULTIPOLYGON") %>% 
  sf::st_make_valid()

# Save padus file without private or unknown easements
sf::st_write(padus, "spatial/padus_no_private.gpkg")

# ==== Public point locations (schools, prisons, public housing, higher ed) ====
url_schools_k12    <- "https://nces.ed.gov/opengis/rest/services/K12_School_Locations/EDGE_GEOCODE_PUBLICSCH_2122/MapServer/0"
url_prisons        <- "https://carto.nationalmap.gov/arcgis/rest/services/structures/mapserver/19"
url_public_housing <- "https://services.arcgis.com/VTyQ9soqVukalItT/ArcGIS/rest/services/Public_Housing_Developments/FeatureServer/0"
urls_higher_ed     <- c(
  "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Structures_Education_v1/FeatureServer/0",
  "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Structures_Education_v1/FeatureServer/1"
)

points_schools <- fetch_point_layer(url_schools_k12, "STATE")
points_prisons <- fetch_point_layer(url_prisons, "state")
points_housing_public  <- fetch_point_layer(url_public_housing, "STD_ST", rename_geom_to = "geoms")

points_higher_ed <- urls_higher_ed %>%
  purrr::map(., arcpullr::get_spatial_layer) %>%
  dplyr::bind_rows() %>%
  sf::st_transform(CRS_TARGET) %>%
  dplyr::filter(STATE == STATE_FOCAL) %>%
  sf::st_set_geometry("geoms")

# Merge all point location files together and join with parcels to get parcel IDs
points_public <- points_schools %>%
  dplyr::bind_rows(points_prisons) %>%
  dplyr::bind_rows(points_housing_public) %>%
  dplyr::bind_rows(points_higher_ed) %>%
  dplyr::mutate(id_delete = dplyr::row_number()) %>%
  dplyr::select(geoms) %>%
  sf::st_join(parcels) %>%
  dplyr::filter(!is.na(id))

# Identify public parcels
parcels_public <- parcels %>%
  dplyr::filter(id %in% points_public$id)

# Download census tract polygons for focal state
tracts <- tigris::tracts(state = STATE_FOCAL) %>%
  sf::st_transform(CRS_TARGET)

######################################################
# Merge public polygon files
padus_public_water <- padus %>%
  sf::st_set_geometry("geom") %>%
  dplyr::bind_rows(state_lands %>% sf::st_set_geometry("geom")) %>%
  dplyr::bind_rows(parcels_public %>% sf::st_set_geometry("geom")) %>%
  dplyr::bind_rows(water %>% sf::st_set_geometry("geom")) %>%
  dplyr::select("geom")

# Erase publicly-owned parcels from census tract polygons and delete tract
# portions likely caused by misaligned polygons (defined as those < 3 acres)
tracts_no_padus <- tracts %>%
  rmapshaper::ms_erase(padus_public_water) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(acres = as.numeric(units::set_units(sf::st_area(.), "acres"))) %>%
  dplyr::filter(acres >= 3) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarize()

# Remove publicly-owned and protected parcels from state parcel file
parcels_no_padus <- parcels %>%
  sf::st_point_on_surface() %>%
  sf::st_join(tracts_no_padus) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!is.na(GEOID)) %>%
  dplyr::inner_join(parcels, .) %>%
  dplyr::mutate(acres = as.numeric(units::set_units(sf::st_area(.), "acres"))) %>%
  dplyr::select(GEOID, acres)

# Save tract and parcel files without protected/public areas
sf::st_write(tracts_no_padus, "spatial/tracts_no_padus.gpkg")
sf::st_write(parcels_no_padus, "spatial/parcels_no_padus.gpkg")

