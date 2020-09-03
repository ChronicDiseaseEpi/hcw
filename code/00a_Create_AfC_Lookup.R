############################################
# Health Care Worker Data
# Create AfC Lookup
# 
# 
# Ciara Gribben
# June 2020
#############################################

### 1 - Housekeeping
# Load packages
library(here)
library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(purrr)
library(tidyr)

# Spin to count reduction in cases
#+ setup
#' Seting up document for spinning into a markdown file for regression modelling for storage on internal git repository
knitr::opts_knit$set(root.dir = here::here())

#' Need to run this function to spin (ie knit from r file) the document. Would be more elegant to store this in another script.
knitme <- function(){
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
  knitr::spin("code/00a_Create_AfC_Lookup.R")
  file.copy("00a_Create_AfC_Lookup.md", "code/00a_Create_AfC_Lookup.md", overwrite = TRUE)
  if(!dir.exists("code/figure")) dir.create("code/figure")
  file.remove("00a_Create_AfC_Lookup.md", "00a_Create_AfC_Lookup.html")
}

# Functions
CountInclusions <- function(mydf = hcw_data, varname = "unique_id"){
  x <- sum(!duplicated(mydf[[varname]]))
  paste0(x, " uniue staff members")
}

ReviewFileContents <- function(x){
  ## simple function for printing variables for review
  varnames <- setdiff(names(x), "unique_id")
  x_smry <- map(varnames, ~ count_(x[,c("unique_id", .x)], .x, sort = TRUE))
  names(x_smry) <- varnames
  x_smry
}

# Read in linked data, clean variable names and data
hcw_data <- read.csv(here("../../data", "SwissExtract_anon.csv")) %>% 
  clean_names() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, ~ str_replace_all(.x, "\\&", "and") %>% 
              str_to_lower()) %>% 
  mutate(job_sub_family = if_else(is.na(job_sub_family), "unspecified", job_sub_family))
CountInclusions()

# Read in lookups and clean
afc_paeds <- read_delim(here("reference_files", "afc_paediatric_dental.txt"), "\t") %>% 
  clean_names() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, ~ str_replace_all(.x, "\\&", "and") %>% 
              str_to_lower()) %>% 
  rename(job_family = jobfamily,
         job_sub_family = jobsubfamily)

afc_lookup <- read.delim(here("reference_files", "afc_patient_facing.txt")) %>% 
  clean_names() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, ~ str_replace_all(.x, "\\&", "and") %>% 
              str_to_lower()) %>% 
  rename(job_family = jobfamily,
         job_sub_family = jobsubfamily) %>% 
  arrange(job_family, job_sub_family) %>% 
  select(-original_dict) %>% 
  mutate(npf = if_else(npf == "yes", 1, 0), 
         pf_any = if_else(pf_any == "yes", 1, 0),
         pf_front = if_else(pf_front == "yes", 1, 0),
         pf_resp_oro_agp = if_else(pf_resp_oro_agp == "yes", 1, 0),
         # pf_icu blank - fill with 0s
         pf_icu = 0,
         job_sub_family = if_else(is.na(job_sub_family), "unspecified", job_sub_family))

### 2 - Create AFC patient facing lookup ----
# Remove medical and dental job families
hcw_data_afc <- hcw_data %>% 
  filter(!(registration_body %in% c("gmc", "gdc"))) %>% 
  filter(job_family != "medical and dental")
CountInclusions(hcw_data_afc)

# Flag paediatrics and dental
hcw_data_afc <- hcw_data_afc %>% 
  left_join(afc_paeds, by = c("job_family", "job_sub_family")) %>% 
  replace_na(list(child_health = 0, dental = 0))
CountInclusions(hcw_data_afc %>% filter(child_health == 0 & dental ==0))

# Count ones with missing job family and or sub job family, there are none
hcw_data_afc %>% 
  anti_join(afc_lookup, by = c("job_family", "job_sub_family"))%>% 
  count(job_family, job_sub_family)

# Match on AfC job family/sub family lookup
check_match1 <- nrow(hcw_data_afc)
hcw_data_afc <- hcw_data_afc %>% 
  inner_join(afc_lookup, by = c("job_family", "job_sub_family")) 
check_match2 <- nrow(hcw_data_afc)
# should be true
check_match1 == check_match2
rm(check_match1, check_match2)

CountInclusions(hcw_data_afc)

# For selected boards and job families further define role  
hcw_data_afc <- hcw_data_afc %>% 
  mutate(pf_front = if_else(!(board_name %in% c("nhs lanarkshire ", 
                                                "nhs lothian")) &
                              job_sub_family %in% c("general acute nursing", 
                                                    "specialist nursing", 
                                                    "bank nursing") &
                              service_area %in% c("accident and emergency", 
                                                  "general medicine", 
                                                  "infectious diseases"), 1, pf_front), 
         pf_resp_oro_agp = if_else(!(board_name %in% c("nhs lanarkshire ", 
                                                       "nhs lothian")) &
                                     job_sub_family %in% c("general acute nursing", 
                                                           "specialist nursing", 
                                                           "bank nursing") &
                                     service_area %in% c("ear nose and throat", 
                                                         "oral and maxillofacial", 
                                                         "general dental practice", 
                                                         "restorative dentistry", 
                                                         "surgical dentistry",
                                                         "respiratory"), 1, 
                                    pf_resp_oro_agp), 
         pf_icu = if_else(!(board_name %in% c("nhs lanarkshire ", 
                                              "nhs lothian")) &
                            job_sub_family %in% c("general acute nursing", 
                                                  "specialist nursing", 
                                                  "bank nursing") &
                            service_area %in% c("intensive care"), 1, 
                            pf_icu)) 

# Further classify npf if working in npf boards
hcw_data_afc <- hcw_data_afc %>% 
  mutate(npf = if_else(board_name %in% c("healthcare improvement scotland", 
                                          "nhs 24", 
                                          "nhs national services scotland", 
                                           "nhs education for scotland"), 1,npf))  
  
# Do not classify senior staff as patient facing, band 8a staff are undetermined
hcw_data_afc <-  hcw_data_afc %>% 
  mutate(npf = if_else(job_family == "nursing and midwifery" &
                                      stringr::str_extract(post_descriptor, 
                                      "(8a)|(8b)|(8c)|(8d)|([1-9])+") %in% c("8a"), 
                                        0, npf), 
          pf_any = if_else(job_family == "nursing and midwifery" &
                                        stringr::str_extract(post_descriptor, 
                                        "(8a)|(8b)|(8c)|(8d)|([1-9])+") %in% c("8a"), 
                                            0, pf_any), 
          pf_front = if_else(job_family == "nursing and midwifery" &
                                            stringr::str_extract(post_descriptor, 
                                            "(8a)|(8b)|(8c)|(8d)|([1-9])+") %in% c("8a"), 
                                           0, pf_front), 
          pf_resp_oro_agp = if_else(job_family == "nursing and midwifery" &
                                          stringr::str_extract(post_descriptor, 
                                          "(8a)|(8b)|(8c)|(8d)|([1-9])+") %in% c("8a"), 
                                            0, pf_resp_oro_agp), 
          pf_icu = if_else(job_family == "nursing and midwifery" &
                                        stringr::str_extract(post_descriptor, 
                                        "(8a)|(8b)|(8c)|(8d)|([1-9])+") %in% c("8a"), 
                                          0, pf_icu)) %>% 
  mutate(npf = if_else(job_family == "nursing and midwifery" &
                                      stringr::str_extract(post_descriptor, 
                                      "(8a)|(8b)|(8c)|(8d)|([1-9])+") %in%
                                      c("8b", "8c", "8d"), 1, npf))

# Classify patient facing
hcw_data_afc <- hcw_data_afc %>% 
  # If flagged in any pf sub category also flag in pf_any
  mutate(pf_any = if_else(is.na(pf_any) == T, 0, 
                          if_else(pf_front == 1 | pf_resp_oro_agp == 1 | 
                                    pf_icu == 1, 1, pf_any))) 
  
# If flagged as npf do not flag as patient facing
hcw_data_afc <- hcw_data_afc %>% 
  mutate(pf_any = if_else(npf == 1, 0, pf_any), 
         pf_front = if_else(npf == 1, 0, pf_front), 
         pf_resp_oro_agp = if_else(npf == 1, 0, pf_resp_oro_agp), 
         pf_icu = if_else(npf == 1, 0, pf_icu)) 
  
# If flagged as patient facing don't flag as non patient facing
hcw_data_afc <- hcw_data_afc %>% 
  mutate(npf = if_else(is.na(npf) == T | pf_any == 1, 0, npf)) 

# Add grade data 
hcw_data_afc <- hcw_data_afc %>%
  mutate(afc_grade = stringr::str_extract(post_descriptor, "(8a)|(8b)|(8c)|(8d)|([1-9])+")) %>% 
  mutate(afc_1_4 = case_when(afc_grade == "1" | afc_grade == "2" |
                               afc_grade == "3" | afc_grade == "4" ~ 1,
                             TRUE ~ 0),
         afc_5_7 = case_when(afc_grade == "5" | afc_grade == "6" | afc_grade == "7" ~ 1,
                             TRUE ~ 0),
         afc_8 = case_when(afc_grade == "8a" | afc_grade == "8b" | afc_grade == "8c" |
                             afc_grade == "8d" | afc_grade == "8" ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(grade = case_when(
    afc_1_4 == 1 ~ "1-4",
    afc_5_7 == 1 ~ "5-7",
    afc_8   == 1 ~ "8+",
    TRUE ~ NA_character_
  )) %>% 
  select(-afc_grade, -afc_1_4, -afc_5_7, -afc_8)


# Create lookup
CountInclusions(hcw_data_afc)
lookup <- hcw_data_afc %>% 
  select(unique_id, npf, pf_any, pf_front, pf_resp_oro_agp, pf_icu, child_health, 
         dental, grade) %>% 
  group_by(unique_id) %>% 
  summarise(npf = max(npf), 
            pf_any = max(pf_any), 
            pf_front = max(pf_front), 
            pf_resp_oro_agp = max(pf_resp_oro_agp), 
            pf_icu = max(pf_icu), 
            child_health = max(child_health), 
            dental = max(dental),
            grade = max(grade, na.rm = TRUE)) %>% 
  
  # if a person has any pf role they are pf
  mutate(npf = if_else(pf_any == 1, 0, npf)) %>% 
  
  # replace NAs
  replace_na(list(npf = 0, pf_any = 0, pf_front = 0, pf_resp_oro_agp = 0, 
                  pf_icu = 0, undetermined = 0, child_health = 0, dental = 0)) %>% 
  
  # flag those whose role is undetermined
  mutate(undetermined = if_else(npf == 0 & pf_any == 0 & pf_front == 0 &
                                  pf_resp_oro_agp == 0 & pf_icu == 0, 1, 0))
  
CountInclusions(lookup)
## Review missingness
# service_area_missingness
hcw_data_afc %>% 
  filter(!(board_name %in% c("nhs lanarkshire ", 
                             "nhs lothian")) &
           job_sub_family %in% c("general acute nursing", 
                                 "specialist nursing", 
                                 "bank nursing")) %>% 
  mutate(service_area = case_when(
    service_area %in% c("accident and emergency", 
                        "general medicine", 
                        "infectious diseases",
                        "ear nose and throat", 
                        "oral and maxillofacial", 
                        "general dental practice", 
                        "restorative dentistry", 
                        "surgical dentistry",
                        "respiratory",
                        "intensive care") ~ service_area,
    is.na(service_area) ~ "Missing",
    TRUE ~ "other"
  )) %>% 
  count(service_area, sort = TRUE) %>% 
  mutate(prcnt = 100*n/sum(n)) %>% 
  knitr::kable()

## 36 values with single characters, and 2 missing, highly complete
hcw_data_afc %>% 
  filter(!npf ==1) %>% 
  mutate(post_descriptor2 = str_remove_all(post_descriptor, "[0-9]") %>% 
           str_remove_all("\\b([a-z]{1,1})\\b") %>% 
           str_trim())  %>% 
  count(post_descriptor2) %>% 
  knitr::kable()


# Save out
write_rds(lookup, here("data", "afc_lookup_pre_id.rds"))

### 3 - Clean environment ----
rm(afc_lookup)
rm(afc_paeds)
rm(hcw_data)
rm(hcw_data_afc)
rm(lookup)

#### END OF SCRIPT ####