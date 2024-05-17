# PROJECT:  C:/Users/tessam/Documents/Github/ticket2ride
# PURPOSE:  Helper functions
# AUTHOR:   T. Essam | USAID
# REF ID:   3f8f0438 
# LICENSE:  MIT
# DATE:     2024-05-16
# UPDATED: 

# Helpers ------------------------------------------------------------
  
# Dataset for table
df_themes <- tibble::tribble(
  ~theme,      ~Indicator, ~fill_clr,
  "HSS",      "EMR_SITE",  "#cce4ff",
  "HSS",     "LAB_PTCQI",  "#cce4ff",
  "HSS",    "SC_ARVDISP",  "#cce4ff",
  "HSS",       "SC_CURR",  "#cce4ff",
  "Prevention",    "FPINT_SITE",  "#ffd5b5",
  "Prevention",      "GEND_GBV",  "#ffd5b5",
  "Prevention",        "KP_MAT",  "#ffd5b5",
  "Prevention",       "KP_PREV",  "#ffd5b5",
  "Prevention",      "OVC_SERV",  "#ffd5b5",
  "Prevention",       "PP_PREV",  "#ffd5b5",
  "Prevention",       "PREP_CT",  "#ffd5b5",
  "Prevention",      "PREP_NEW",  "#ffd5b5",
  "Prevention",       "TB_PREV",  "#ffd5b5",
  "Prevention",     "VMMC_CIRC",  "#ffd5b5",
  "Testing",     "CXCA_SCRN",  "#e5dcff",
  "Testing",     "HTS_INDEX",  "#e5dcff",
  "Testing",    "HTS_RECENT",  "#e5dcff",
  "Testing",      "HTS_SELF",  "#e5dcff",
  "Testing",       "HTS_TST",  "#e5dcff",
  "Testing",   "OVC_HIVSTAT",  "#e5dcff",
  "Testing",     "PMTCT_EID",  "#e5dcff",
  "Testing",      "PMTCT_FO",  "#e5dcff",
  "Testing", "PMTCT_HEI_POS",  "#e5dcff",
  "Testing",    "PMTCT_STAT",  "#e5dcff",
  "Testing",       "TB_STAT",  "#e5dcff",
  "Treatment",       "CXCA_TX",  "#ffe4b5",
  "Treatment",     "PMTCT_ART",  "#ffe4b5",
  "Treatment",        "TB_ART",  "#ffe4b5",
  "Treatment",       "TX_CURR",  "#ffe4b5",
  "Treatment",         "TX_ML",  "#ffe4b5",
  "Treatment",        "TX_NEW", "#ffe4b5",
  "Treatment",        "TX_RTT", "#ffe4b5",
  "Treatment",         "TX_TB", "#ffe4b5",
  "VLS",       "TX_PVLS",  "#ffa9ae"
)



