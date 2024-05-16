# PROJECT: ticket2ride
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Corridor Feature Extraction
# REF ID:  ba555e96 
# LICENSE: MIT
# DATE:    2024-05-16
# UPDATE:  2024-05-16
# NOTES:   SBU - Exclude location

# Libraries ====
  
  library(tidyverse)
  library(gagglr)
  library(sf)
  library(scales, warn.conflicts = FALSE)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(lubridate)
  library(glue)
  
  
# LOCALS & SETUP ====

  # Set Params

  ref_id <- "b8140e7e"
  agency <- "USAID"
  cntry <- "Zambia"
  cntries <- c("Angola","Democratic Republic of the Congo", "Zambia")
  cntry_uid <- get_ouuid(cntry) 
  cntries_uid <- map_chr(cntries, get_ouuid)
  
  gdrive_folder <- as_id("1G334IFbkqo_U_usXTC58jNCZ3wb91-CC")
  
  # Set paths  
  
  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"
  dir_gis <- "GIS"
   
  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")

  # Files 
  
  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY22")
    
  meta <- get_metadata(file_nat)
  
  meta$caption <- glue("{meta$caption} | Updated on {curr_date()}")
    
# Functions  =====
  
# LOAD DATA =====

  ##df_nat <- file_nat %>% read_psd()
  
  ## ZOI - Countries
  
  spdf_pepfar <- dir_shp %>% get_vcpolygons()
  
  cntries_zoi <- spdf_pepfar %>% 
    filter(uid %in% cntries_uid)
  
  cntries_zoi %>% gview
  
  ## ZOI - Psnu & Communities
  
  df_lvls <- get_levels(reshape = T) %>% 
    filter(countryname %in% cntries, 
           label %in% c("prioritization", "community")) %>% 
    select(countryname, label, level)
  
  df_orgs <- df_lvls %>% 
    pmap(function(countryname, label, level) {
      get_ouorgs(
        ouuid = get_ouuid(countryname),
        level = level
      ) %>% 
      mutate(country = countryname, label = label) %>% 
      relocate(country, .before = 1)
    }) %>% 
    bind_rows()
  
  spdf_psnus <- df_orgs %>% 
    filter(label == "prioritization") %>% 
    left_join(spdf_pepfar, ., by = "uid") %>% 
    filter(!is.na(orgunit))
  
  spdf_comms <- df_orgs %>% 
    filter(label == "community") %>% 
    left_join(spdf_pepfar, ., by = "uid") %>% 
    filter(!is.na(orgunit)) %>% 
    mutate(idx = row_number())
  
  ## Corridors 
  
  spdf_corridor <- dir_gis %>% 
    file.path("Corridor") %>% 
    return_latest(pattern = "^T.*_Corridor_of_interrest.shp") %>% 
    read_sf()
  
  cpaths <- c(5, 10, 25)
  
  spdf_cpaths <- cpaths %>% 
    map(function(.x) {
      st_buffer(x = spdf_corridor, dist = .x * 1000) %>% 
        mutate(name = "Lobito", dist = .x * 1000)
    }) %>% 
    bind_rows() 
  
  ## Communities along the corridors
  
  spdf_comms_of_interest <- spdf_comms %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_intersects(spdf_corridor, .) 
  
  spdf_comms_of_interest <- spdf_comms %>% 
    filter(idx %in% (spdf_comms_of_interest %>% as_vector() %>% as.integer()))
  
  
# MUNGE =====
  

  
# VIZ =====

   ggplot() +
    geom_sf(data = spdf_cpaths %>% filter(dist == 25000), 
            color = grey30k, fill = trolley_grey_light, linetype = "dotted") +
    geom_sf(data = spdf_comms_of_interest, fill = NA, color = grey70k) +
    geom_sf(data = spdf_corridor, color = usaid_red) +
    si_style_map()

# OUTPUTS =====

