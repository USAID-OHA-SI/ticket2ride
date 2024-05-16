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
  
  plot(df_boundaries)
  
  # What is the best project for this geography?
  zoi_crs <- suggest_crs(df_boundaries) %>% slice(1) %>% pull(crs_code) %>% as.numeric() 

  
  # Geoprocesses corridor files with buffers included
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
  df_geo$TAH_Corridor_5KM


# STANDARDIZE CRS ---------------------------------------------------------

  # Because we are doing geoprocessing, we need to make sure all the data are projected
  # and are in comparable units. 

  check_proj(st_crs(df_boundaries))
  
  df_boundaries <- st_transform(df_boundaries, crs = zoi_crs)
  
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
  
  tmp <- df_api_pts %>% 
    filter(is_usaid == TRUE) %>% 
    mutate(within_buffer = st_within(geometry, df_geo$TAH_Corridor_5KM, sparse = F),
           within_buffer = fct_relevel(within_buffer %>% as.factor, c("TRUE", "FALSE")))
  
  fac_count <- tmp %>% filter(within_5km == TRUE) %>% count() %>% pull(n)
  
  # Prototype map
  tmp %>% 
    ggplot() +
    geom_sf(data = df_boundaries, fill = grey10k) +
    geom_sf(data = df_geo$TAH_Corridor_5KM, aes(fill = grey40k), alpha = 0.5) +
    geom_sf(data = df_geo$TAH_Corridor_of_interrest, color = hw_orchid_bloom) +
    geom_sf(aes(color = within_buffer), alpha = 0.75) +
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
    scale_fill_identity() +
    scale_color_manual(values = c("TRUE" = hw_orchid_bloom, "FALSE" = hw_slate), ) +
    labs(title = glue::glue("{fac_count} facilities fall within 5 kilometers of the Lobito Corridor.")) +
    si_style_map()



# VIZ ---------------------------------------------------------------------


