# PROJECT:  ticket2ride
# PURPOSE:  Intersect sites with buffers and create maps / tables
# AUTHOR:   T. Essam | USAID
# REF ID:   0682e591 
# LICENSE:  MIT
# DATE:     2024-05-16
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(googledrive)
  library(googlesheets4)
  library(sf)
  library(grabr)
  library(crsuggest)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()
  ref_id <- "0682e591"  #a reference to be places in viz captions 
  
  api_results_id <- "1TiNHXVletecX3uCfwUpFHZbR6dDhSMZQDHu-_ZrP0HA"
  
  lobito_cor <- list.files("GIS", pattern = "TAH_.*.shp$", full.names = T)
  
  source("Scripts/00_helpers.r")

# FUNCTIONS ---------------------------------------------------------------

  # Function to check whether or not data are projected
  check_proj <- function(.data){
    if (!is.na(.data$epsg) && grepl("+proj=", .data$proj4string)) {
      print("Data are projected.")
    } else {
      cli::cli_inform(glue::glue_col("Data are not projected or CRS is undefined. See {yellow crsuggest::suggest_crs()} for suggestions."))
    }
  }  
  
  # check for same CRS
  check_crs <- function(df1, df2){
    crs1 <- st_crs(df1)
    crs2 <- st_crs(df2)
    
    return(crs1 == crs2)
  }
  
# IMPORT ------------------------------------------------------------------
  
  # API results with the program info and lat/lon we need
  df_api <- read_sheet(ss = api_results_id)
  
  # PEPFAR polygons to be filtered with gophr
  cntry_list <- c("Angola", "Democratic Republic of the Congo", "Zambia")
  
  # All are at level 3
  get_levels() %>% 
    filter(operatingunit %in% cntry_list) %>% 
    pull(country)
  
  ou_lvl <- 3
  
  # Load the pepfar polygons  
  spdf_pepfar <- get_vcpolygons(folderpath = glamr::si_path("path_vector"))
  
  # Bind them together into a single sf object
  df_boundaries <- map_dfr(cntry_list, ~extract_boundaries(spdf_pepfar,
                                      country = .x,
                                      level = ou_lvl,
                                      username = datim_user(),
                                      password = datim_pwd()))
  
  df_zmb_psnus <- extract_boundaries(spdf_pepfar, country = "Zambia",
                                      level = 5,
                                      username = datim_user(),
                                      password = datim_pwd())
  
  gview(df_boundaries)
  
  # What is the best project for this geography?
  zoi_crs <- suggest_crs(df_boundaries) %>% 
    slice(1) %>% 
    pull(crs_code) %>% 
    as.numeric() 

  
  # Geoprocessed corridor files with buffers included
  # Name each list object using shapefile core name parts
  # Assign the ZOI projection as well so we don't have to do this later
  df_geo <- 
    lobito_cor %>% 
    set_names(~str_remove_all(.x, "GIS/|_Buffer|\\.shp")) %>% 
    imap(~st_read(.x, quiet = T) %>% st_transform(., crs = zoi_crs))
  
  # Check that everythign is alinged
  df_geo$TAH_Corridor_of_interrest %>% 
    ggplot() + 
    geom_sf(data = df_boundaries) +
    geom_sf()


# STANDARDIZE CRS ---------------------------------------------------------

  # Because we are doing geoprocessing, we need to make sure all the data are projected
  # and are in comparable units. 

  check_proj(st_crs(df_boundaries))
  
  df_boundaries <- st_transform(df_boundaries, crs = zoi_crs)
  df_zmb_psnus <- st_transform(df_zmb_psnus, crs = zoi_crs)
  
  df_api_pts <- st_as_sf(df_api %>% filter(!is.na(latitude)),
                     coords = c("longitude", "latitude"), crs = 4326) %>% 
    st_transform(., crs = zoi_crs)
  
  st_crs(df_api_pts)
  check_crs(df_boundaries, df_api_pts)
  check_crs(df_boundaries, df_geo$TAH_Corridor_10KM)

  # Create bounding box for the largest buffer
  bbox <- st_bbox(df_geo$TAH_Corridor_25KM)
  
# INTERSECT DATA -------------------------------------------------------------------

  # Now, let's intersect our api site level data with each of the 3 buffers
  relevel_buffer <- function(x){
    fct_relevel(x %>% as.factor, c("TRUE", "FALSE"))
  }
  
  # Need to remove sites with multiple partners reporting -- use distinct
  fac_count <- function(df, col){
    df %>% 
      filter({{col}} == TRUE) %>% 
      distinct(orgunituid) %>% 
      count() %>% 
      pull(n)
  }
  
  # Create intersected tags at site level for each buffer; Use this to count sites along corridor.
  df_pts_buffered <- df_api_pts %>% 
    filter(is_usaid == TRUE) %>% 
    mutate(within_buffer_5km = st_within(geometry, df_geo$TAH_Corridor_5KM, sparse = F) %>% 
             relevel_buffer(),
           within_buffer_10km = st_within(geometry, df_geo$TAH_Corridor_10KM, sparse = F) %>% 
             relevel_buffer(),
           within_buffer_25km = st_within(geometry, df_geo$TAH_Corridor_25KM, sparse = F) %>% 
             relevel_buffer()
           )
  
  # Get counts for different viz/map products
  fac_count_5km <- fac_count(df_pts_buffered, within_buffer_5km)
  df_pts_buffered %>% 
    filter(within_buffer_5km == TRUE) %>% 
    distinct(orgunituid, operatingunit) %>% 
    count(operatingunit)
  
  fac_count_10km <- fac_count(df_pts_buffered, within_buffer_10km)
  fac_count_25km <- fac_count(df_pts_buffered, within_buffer_25km)


# VIZ ---------------------------------------------------------------------
 
  #Basic Map with different data sources
   df_pts_buffered %>%
   ggplot() +
   geom_sf(data = df_boundaries, fill = grey10k, linewidth = 1) +
   geom_sf(data = df_zmb_psnus %>% filter(orgunit == "Ndola District"), fill = grey20k) +
   geom_sf(data = df_geo$TAH_Corridor_5KM, aes(fill = grey40k), alpha = 0.5) +
   geom_sf(data = df_geo$TAH_Corridor_of_interrest, color = "#7f6e55") +
   geom_sf(aes(color = within_buffer_5km, alpha = ifelse(within_buffer_5km == "TRUE", 1, 0.25)), linewidth = 1) +
   coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
   scale_fill_identity() +
   scale_color_manual(values = c("TRUE" = hw_orchid_bloom, "FALSE" = hw_slate), ) +
   labs(
     title = glue::glue("From Kitwe, Zambia to Lobito, Angola USAID supports {fac_count_5km} facilities through PEPFAR\n that are within a 5 kilometer range of the Lobito Corridor"),
     caption = "Source: PEPFAR DATIM 2024-05-16 | Notes: Count does not include facilities with missing coordinates"
   ) +
   si_style_map()
  si_save("Graphics/Lobito_sites.svg")

  # Of those sites supported, what areas report into DATIM?
  site_list_themes <- df_pts_buffered %>%
    # st_drop_geometry() %>%
    filter(within_buffer_5km == TRUE) %>%
    select(operatingunit, orgunituid, psnu, facility, has_prev_agyw_prev:has_hss_sc_curr) %>%
    rowwise() %>%
    mutate(count = sum(c_across(has_prev_agyw_prev:has_hss_sc_curr), na.rm = T)) %>%
    pivot_longer(has_prev_agyw_prev:has_hss_sc_curr, values_drop_na = TRUE) %>%
    mutate(theme = str_extract(name, "(?<=_)[^_]+(?=_)")) %>%
    distinct(operatingunit, orgunituid, facility, theme) %>%
    mutate(n = 1)

  site_list_themes %>%
    group_by(operatingunit, theme) %>%
    summarise(count = sum(n, na.rm = T)) %>%
    spread(theme, count)

  # wHAT MECHANISMS ARE SUPPORTED IN EACH COUNTRY
  df_pts_buffered %>%
    st_drop_geometry() %>%
    filter(within_buffer_5km == TRUE) %>%
    count(operatingunit)

  df_pts_buffered %>%
    # st_drop_geometry() %>%
    filter(within_buffer_5km == TRUE) %>%
    filter(psnu == "Kitwe District") %>%
    ggplot() +
    geom_sf() +
    geom_sf_label(aes(label = facility), size = 3)
    
