# PROJECT:  ticket2ride
# PURPOSE:  query DATIM sites
# AUTHOR:   A.Chafetz | USAID
# REF ID:   19669250 
# LICENSE:  MIT
# DATE:     2024-05-16
# UPDATED: 
# NOTES:    adapted from groundhog_day/Scripts/FY21Q4_pepfar-site-count.R
#           https://github.com/USAID-OHA-SI/groundhog_day/blob/main/Scripts/FY21Q4_pepfar-site-count

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(janitor)
  library(googledrive)
  library(googlesheets4)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  library(grabr) 
  library(Wavelength)

  #tidylog needed but not loaded

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "19669250"  #a reference to be places in viz captions 
  
  load_secrets("datim")
  
  v_cntry <- c("Angola","Democratic Republic of the Congo", "Zambia")
  
  meta <- get_metadata()
  
  gdrive_folder <- as_id("1G334IFbkqo_U_usXTC58jNCZ3wb91-CC")

  
# DATIM API FUNCTION ------------------------------------------------------
  
  pull_sites <- function(ou_name, ou_uid, org_type, org_lvl,
                         username, password, 
                         baseurl = "https://final.datim.org/"){
    
    print(paste("Running DATIM API for", ou_name, org_type,  Sys.time(),
                sep = " ... "))
    
    type_uid <- ifelse(org_type == "facility", "POHZmzofoVx", "PvuaP6YALSA") #excludes military & Other organisation unit type
    
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=pe:2022Oct&", #period
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=bw8KHXzxd9i&", #Funding Agency,
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:ELZsYsR89rn;CZplmfCbnv2;vw3VoiA4D0s;NYAJ6QkEKbC;Uo2vBxak9im;RxyNwEV3oQf;Fvs28dwjL6e;pkZRNlMgL89;fRWHMVd6Vq5;gma5vVZgK49;FfxbuFZVAM5;wdoUps1qb3V;qOgXk080fJH;CUblPgOMGaT;twyHxdQVjMC;hGUykTtC0Xm;f5IPTM7mieH;lYTgCwEjUX6;cwZbCmUvjp7;R59aGLjmKBO;ECGbKy8o3FC;BTIqHnjeG7l;rI3JlpiuwEK;bybAqM1Lnba;AaCcy7dVfWw;Z6TU9Os82Yw;MvszPTQrUhy;cSTYDtvP0Nt;udCop657yzi;o8GCardEcYz;tOiM2uxcnkj;bZOF8bon1dD;TYAjnC2isEk;jbyq87W19Qv;scxfIjoA6nt;oCwIxluUXok;lIUE50KyUIH&", #Technical Area
             "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results - results
             "dimension=HWPJnUTMjEq&", #Disaggregation Type
             "dimension=mINJi7rR1a6:", type_uid,"&", #Type of organisational unit
             # "dimension=TWXpUVE2MqL&", #Support Type
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    
    if(org_type == "agyw"){
      core_url <-
        paste0(baseurl,"api/29/analytics?",
               "dimension=pe:2022Oct&", #period
               "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
               # "dimension=SH885jaRe0o&", #Funding Mechanism - not used with AGYW
               "dimension=LxhLO68FcXm:ELZsYsR89rn&", #Technical Area - AGYW
               "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results - results
               "dimension=HWPJnUTMjEq&", #Disaggregation Type
               "dimension=mINJi7rR1a6:", type_uid,"&", #Type of organisational unit
               # "dimension=TWXpUVE2MqL&", #Support Type
               "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    }
    
    df <- grabr::datim_process_query(core_url, username, password)
    
    if(!is.null(df)){
      
      if(!"Funding Agency" %in% names(df))
        df <- dplyr::mutate(df, `Funding Agency` = NA_character_)
      
      if(!"Funding Mechanism" %in% names(df))
        df <- dplyr::mutate(df, `Funding Mechanism` = NA_character_)
      
      df <- df %>%
        dplyr::filter(`Technical Area` %in% c("SC_CURR", "SC_ARVDISP", "HRH_PRE") | Value != 0) %>%
        dplyr::mutate(operatingunit = ifelse(stringr::str_detect(orglvl_3, "Region"), paste0(orglvl_3, "/", orglvl_4), orglvl_3)) %>%
        dplyr::select(-orglvl_1, -orglvl_2) %>% 
        tidyr::unite(orgunit_hierarchy, starts_with("orglvl"), sep = "/", remove = FALSE) %>% 
        dplyr::mutate(orgunit_hierarchy = paste(orgunit_hierarchy, `Organisation unit`, sep = "/")) %>% 
        glamr::convert_datim_pd_to_qtr() %>% 
        dplyr::distinct(period = Period, 
                        operatingunit =orglvl_3, 
                        orgunituid, orgunit_hierarchy, 
                        funding_agency = `Funding Agency`,
                        mechanism = `Funding Mechanism`,
                        sitetype = `Type of organisational unit`, 
                        indicator = `Technical Area`)
      #concatenate funding agency and mechanism
      df <- df %>% 
        gophr::clean_agency() %>% 
        dplyr::arrange(funding_agency, mechanism) %>% 
        dplyr::group_by(period, operatingunit, orgunituid, #orgunit_hierarchy,
                        sitetype, indicator) %>% 
        dplyr::summarise(across(c(funding_agency, mechanism),
                                \(x) paste(x, collapse = "; ")),
                         .groups = "drop")
      
      df <- df %>% 
        dplyr::mutate(x = TRUE) %>% 
        tidyr::pivot_wider(names_from = indicator,
                           names_glue = "has_{tolower(indicator)}",
                           values_from = x)
    }
    
    return(df)
  }

# IDENTIFY INPUTS FOR API -------------------------------------------------
  
  #country and level list
  ctry_list <- get_outable() %>% 
    filter(country %in% v_cntry) %>% 
    select(country, country_uid, 
           community = community_lvl, facility = facility_lvl) %>% 
    pivot_longer(c(community, facility),
                 names_to = "type",
                 values_to = "level") 
  
  #add a separate level for agyw
  ctry_list <- ctry_list %>% 
    bind_rows(ctry_list %>%
                filter(type == "community") %>% 
                mutate(type = "agyw"))  %>% 
    arrange(country, desc(type))
  
# RUN API -----------------------------------------------------------------
  
  df_sites <- ctry_list %>%
    pmap_dfr(~pull_sites(..1, ..2, ..3, ..4, 
                         username = datim_user(), password = datim_pwd()))


# PULL HIERACHY -----------------------------------------------------------

  
  df_orgs <-  map(unique(ctry_list$country_uid),
                  ~pull_hierarchy(.x, datim_user(), datim_pwd())
                  ) %>% 
                    list_rbind()
  
# MERGE HIERARCHY ---------------------------------------------------------

   df_export <- tidylog::right_join(df_orgs, df_sites)
  
   df_export <- df_export %>% 
     mutate(is_usaid = str_detect("USAID", funding_agency), .after = funding_agency)
  
# EXPORT ------------------------------------------------------------------
  
  #indicators from MER 2.6
  df_ind <- tibble::tribble(
    ~category,      ~indicator,
    "Prevention",     "AGYW_PREV",
    "Prevention",    "FPINT_SITE",
    "Prevention",      "GEND_GBV",
    "Prevention",        "KP_MAT",
    "Prevention",       "KP_PREV",
    "Prevention",      "OVC_SERV",
    "Prevention",       "PP_PREV",
    "Prevention",     "PrEP_CURR",
    "Prevention",       "PrEP_CT",
    "Prevention",      "PrEP_NEW",
    "Prevention",       "TB_PREV",
    "Prevention",     "VMMC_CIRC",
    "Testing",     "CXCA_SCRN",
    "Testing",     "HTS_INDEX",
    "Testing",    "HTS_RECENT",
    "Testing",      "HTS_SELF",
    "Testing",       "HTS_TST",
    "Testing",   "OVC_HIVSTAT",
    "Testing",     "PMTCT_EID",
    "Testing",      "PMTCT_FO",
    "Testing", "PMTCT_HEI_POS",
    "Testing",    "PMTCT_STAT",
    "Testing",       "TB_STAT",
    "Treatment",       "CXCA_TX",
    "Treatment",     "PMTCT_ART",
    "Treatment",        "TB_ART",
    "Treatment",       "TX_CURR",
    "Treatment",         "TX_ML",
    "Treatment",        "TX_NEW",
    "Treatment",        "TX_RTT",
    "Treatment",         "TX_TB",
    "Viral Load Suppression",       "TX_PVLS",
    "Health Systems",      "EMR_SITE",
    "Health Systems",       "HRH_PRE",
    "Health Systems",     "LAB_PTCQI",
    "Health Systems",    "SC_ARVDISP",
    "Health Systems",       "SC_CURR"
  )
  
  
  #append prefix to indicator names for matching/ordering
  df_ind_adj <- df_ind %>% 
    mutate(category = recode(category,
                             "Prevention" = "prev",
                             "Testing" = "test",
                             "Treatment" = "treat",
                             "Viral Load Suppression" = "vls",
                             "Health Systems" = "hss"),
           indicator = paste0("has_", tolower(indicator)))
  
  #use as ordered list for export
  ind_order <- df_ind_adj %>% 
    mutate(indicator = str_replace(indicator, "has", paste0("has_", category))) %>% 
    pull(indicator)
  
  #reshape to make wide rather than long
  df_export_wide <- df_export %>% 
    pivot_longer(starts_with("has_"), names_to = "indicator", values_drop_na = TRUE) %>% 
    tidylog::left_join(df_ind_adj) %>% 
    mutate(indicator = str_replace(indicator, "has", paste0("has_", category)),
           indicator = factor(indicator, ind_order)) %>% 
    arrange(indicator) %>% 
    select(-category) %>% 
    pivot_wider(names_from = indicator)
  
  
  #filename
  f_name <- paste0("SBU_", meta$curr_pd, "_PEPFAR-sites-and-types_", 
                   format(Sys.Date(), "%Y%m%d"), "_SBU")
  f_path <- file.path("Dataout", paste0(f_name, ".csv"))
  
  #export as csv
  write_csv(df_export_wide, f_path, na = "")

  #upload to gdrive
  drive_upload(f_path, gdrive_folder, f_name, type = "spreadsheet", overwrite = TRUE)
  