############################################
# Health Care Worker Data
# Create Basefile
# 
# 
# Ciara Gribben
# June 2020
#############################################

#+ setup
#' Seting up document for spinning into a markdown file for regression modelling for storage on internal git repository
knitr::opts_knit$set(root.dir = here::here())

knitme <- function(){
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
  knitr::spin("code/01_Create_Basefile.R")
  file.copy("01_Create_Basefile.md", "code/01_Create_Basefile.md", overwrite = TRUE)
  if(!dir.exists("code/figure")) dir.create("code/figure")
  file.remove("01_Create_Basefile.md", "01_Create_Basefile.html")
}

### 1 - Housekeeping
# Load packages
library(here)
library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(purrr)
library(tidyr)
source("code/Functions.R")

# Read in cleaned medical and dental and AFC
afc_med <- readRDS(here("data", "afc_med.rds"))
household <- readRDS(here("data", "household_afc_med_gpcd.rds"))

# Apply ethnicity
household <- household %>% 
  mutate(ethnic5 = MakeEthnicOnomap(onolytic = onolytics_type, geographic = geographical_area)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, ~ str_replace_all(.x, "\\&", "and") %>% 
            str_to_lower()) %>% 
  select(-onolytics_type, -geographical_area)

# Where ethnicity is missing for an individual, use the ethnicity from the household
# Note is usually missing because ONOMAP could not resolve
ethnic_mis <- household %>% 
  filter(is.na(ethnic5)) %>% 
  distinct(hid)
ethnic_mis2 <- household %>% 
  select(hid, ethnic5) %>% 
  semi_join(ethnic_mis) 
ethnic_mis3 <- ethnic_mis2 %>% 
  filter(!is.na(ethnic5)) %>% 
  group_by(hid) %>% 
  count(ethnic5, sort = T) %>% 
  slice(1) %>% 
  ungroup()
## dropped from 1293 households to 263 with missing data
ethnic_mis %>% 
  anti_join(ethnic_mis3)
ethnic_replace <- ethnic_mis %>% 
  left_join(ethnic_mis3) %>% 
  mutate(ethnic_impute = if_else(is.na(ethnic5), "impute household", "impute scotland"),
         ethnic5 = if_else(is.na(ethnic5), "white", ethnic5))
  
household <- household %>% 
  left_join(ethnic_replace %>% rename(ethnic5_replace = ethnic5)) %>% 
  mutate(ethnic5 = if_else(is.na(ethnic5), ethnic5_replace, ethnic5)) %>% 
  select(-ethnic5_replace)
rm(ethnic_mis, ethnic_mis2, ethnic_mis3, ethnic_replace)

## Add in variables for household. and create separate houshold dataset, lose no staff
afc_med <- afc_med %>% 
  distinct()
hcw_data <- afc_med %>% 
  inner_join(household %>% select(-n)) %>% 
  distinct()
hcw_no_data <- afc_med %>% 
  anti_join(household %>% select(-n))
household <- household %>% 
  anti_join(afc_med %>% select(anon_id))

#' Assign health boards to strata for modelling
hb_pop <- read_delim("hb_name_full|hbres_name|hb_name_lcase|pop
                     Ayrshire and Arran|AA|nhs ayrshire and arran|369360
                     Borders|BR|nhs borders|115510
                     Dumfries and Galloway|DG|nhs dumfries and galloway|148860
                     Fife|FF|nhs fife|373550
                     Forth Valley|FV|nhs forth valley|306640
                     Grampian|GR|nhs grampian|585700
                     Greater Glasgow and Clyde|GC|nhs greater glasgow and clyde|1183120
                     Highland|HG|nhs highland|321700
                     Lanarkshire|LN|nhs lanarkshire|661900
                     Lothian|LO|nhs lothian|907580
                     Orkney|OR|nhs orkney|22270
                     Shetland|SH|nhs shetland|22920
                     Tayside|TY|nhs tayside|417470
                     Western Isles|WI|nhs western isles|26720", delim = "|")

#' On basis of whole population analysis of rates undertaken previously assign the following strata
strats_lst <- list(a = "GC",
                   b = "LN",
                   c = c("BR", "TY", "AA", "FF", "FV", "LO","DG"),
                   d = c("GR", "HG", "SH", "WI", "OR")) 
strats <-  stack(strats_lst)
names(strats) <- c("hbres_name", "stratum")
hb_pop <- hb_pop %>% 
  inner_join(strats) %>% 
  arrange(stratum)

strats_lst2 <- tapply(hb_pop$hb_name_lcase, hb_pop$stratum, function(x) paste(x, collapse = "|"), simplify = FALSE)

#' append national boards based on location of main offices. Assign NSS to Glasgow as this is where HPS was based
strats_lst3 <- list(a = paste("healthcare improvement scotland", "nhs health scotland","national waiting times centre", "nhs 24",
                              "nhs national services scotland", sep = "|"),
                    b = paste("the state hospital", sep = "|"),
                    c = paste("nhs education for scotland", "scottish ambulance service", sep = "|"))

#' Staff can have multiple affiliations, take the highest incidence ones first
#' Also assign to territorial boards in preference to special boards
hcw_data <- hcw_data %>% 
  mutate(stratum = case_when(
    ## Territorial boards
    str_detect(board_name, strats_lst2$a) ~ "a",
    str_detect(board_name, strats_lst2$b) ~ "b",
    str_detect(board_name, strats_lst2$c) ~ "c",
    str_detect(board_name, strats_lst2$d) ~ "d",
    ## Special boards
    str_detect(board_name, strats_lst3$a) ~ "a",
    str_detect(board_name, strats_lst3$b) ~ "b",
    str_detect(board_name, strats_lst3$c) ~ "c"
  ))

#' Assign job families and group job families into broader categories
hcw_data <- hcw_data %>% 
  mutate(job_family_grp = case_when(
    str_detect(job_family, "nursing and midwifery") ~ "nursing and midwifery",
    str_detect(job_family, "medical and dental")    ~ "medical and dental",
    str_detect(job_family, "allied health profession") ~ "allied health profession",
    str_detect(job_family, "support services") ~ "support services",
    str_detect(job_family, "administrative services") ~ "administrative services",
    TRUE ~ "Other"),
    job_family_grp = factor(job_family_grp,
                            levels = c("nursing and midwifery", "medical and dental", "allied health profession",
                                       "support services",  
                                       "administrative services", "Other")
    ),
    nurs_med_ahp = if_else(
      job_family_grp %in% c("nursing and midwifery", "medical and dental", 
                            "allied health profession"), 1L, 0L))

#' Further transformations
hcw_data <- hcw_data %>% 
  mutate(age10 = age/10,
         male = if_else(sex ==1, 1L, 0L),
         white = if_else(ethnic5 == "white", 1L, 0L),
         role = case_when(
           (pf_any ==1) ~ "pf_any",
           (undetermined ==1) ~ "undetermined",
           (npf ==1) ~ "npf"
         ),
         role_sub = case_when(
           pf_front ==1 ~ "pf_front",
           pf_resp_oro_agp ==1 ~ "pf_resp_oro_agp",
           pf_icu == 1 ~ "pf_icu",
           pf_any == 0 ~ role, 
           pf_any == 1 ~ "pf_other"),
         role2 = case_when(
           role_sub == "pf_front" ~ "pf_front",
           role == "npf" ~ "npf",
           TRUE ~ "pf_other"
         ),
         # set SIMD to median where not known
         simd = if_else(simd ==6, 3L, as.integer(simd))) 


household <- household %>% 
  mutate(age10 = age/10,
         male = if_else(sex ==1, 1L, 0L),
         white = if_else(ethnic5 == "white", 1L, 0L),
         # set SIMD to median where not known
         simd = if_else(simd ==6, 3L, as.integer(simd)))

### 3 - Clean environment ----
rm(afc_med)
rm(strats, strats_lst, strats_lst2, strats_lst3)

### 4 - Save Basefile ----
write_rds(hb_pop, here("data", "hb_popn.rds"))
write_rds(hcw_data, here("data", "hcw_basefile.rds"))
write_rds(household, here("data", "hcw_household_basefile.rds"))

#### END OF SCRIPT ###