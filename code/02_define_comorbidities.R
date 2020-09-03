# define_comorbidities
# Uses code from PM github repository to define comorbidities last updated on 1st June 2020
# see https://github.com/pmckeigue/covid-scotland/blob/master/comorbidity.R



library(tidyverse)
library(janitor)
library(here)

ComorbidityMakeFunction <- function(cc.severe, 
                                    diagnoses,
                                    procedures,
                                    diabetes, 
                                    scrips, 
                                    fileout){

# please contact https://github.com/pmckeigue for permission to access the BNF pseudocdes of this function
}

## define comorbidity for SWISS and GPCD data ----
diagnoses <- readRDS(here("../../data/", "HCW_SMR01_ICD10_x25.rds")) %>% 
  as_tibble() %>% 
  mutate(ANON_ID = paste(ANON_ID, GP_FLAG, sep = "_"))
procedures <- readRDS(here("../../data/", "HCW_SMR01_OPCS4_MAIN.x25.rds")) %>% 
  as_tibble() %>% 
  mutate(ANON_ID = paste(ANON_ID, GP_FLAG, sep = "_"))
diabetes <- readRDS(here("../../data", "HCW_diabetes.rds")) %>% 
  as_tibble() %>% 
  mutate(ANON_ID = paste(ANON_ID, GP_FLAG, sep = "_"))
## Note that the following transformations match those in PM script casecontrol.R
scrips <- readRDS(here("../../data/", 
                       "HCW_nine_month_excl_15_day_PIS_data_20200610.rds")) %>% 
  as_tibble() %>% 
  mutate(ANON_ID = paste(ANON_ID, GP_FLAG, sep = "_")) 

scrips <- scrips %>% 
  mutate(sectioncode = str_sub(bnf_paragraph_code, 1,4),
         paracode = str_sub(bnf_paragraph_code, 1, 6)) %>% 
  select(ANON_ID, bnf_paragraph_code, paracode)
scrips_n <- scrips %>% 
  clean_names() %>% 
  filter(as.integer(str_sub(bnf_paragraph_code, 1, 2)) %in% 1:13) %>% 
  distinct(anon_id, bnf_paragraph_code) %>% 
  group_by(anon_id) %>% 
  count()
scrips_n <- scrips_n %>% 
  filter(n >=3) %>% 
  pull(anon_id)
saveRDS(scrips_n, "data/ge3_drugs.rds")
# call swiss cc.severe so can easily update this code if PM updtates his
hcw <- readRDS(here("data", "hcw_basefile.rds"))
rely <- readRDS(here("data", "hcw_household_basefile.rds"))
hcw_rely <- bind_rows(hcw  %>% select(ANON_ID = anon_id),
                       rely %>% select(ANON_ID = anon_id))
rm(hcw, rely)
res <- ComorbidityMakeFunction(hcw_rely, 
                               diagnoses,
                               procedures,
                               diabetes, 
                               scrips, 
                               fileout = "comorbidity_smr_pis_dm.rds")
rm(hcw_rely, diagnoses, procedures, diabetes, scrips)

## define comorbidity for case control data ----
cc <- readRDS(here("../../../Case_control/data", "CC_linked_ANON_2020-06-18.rds"))
diagnoses_cc <- readRDS("../../../Case_control/data/CC_SMR01_ICD10_x25_ANON_2020-06-18.rds")
procedures_cc <- readRDS("../../../Case_control/data/CC_SMR01_OPCS4_MAIN.x25_ANON_2020-06-18.rds")
diabetes_cc <- cc %>% 
  select(ANON_ID, dm.type, diab.reg) 
scrips_cc <- readRDS("../../../Case_control/data/CC_PIS_x15_ANON_2020-06-18.rds")
res_cc <- ComorbidityMakeFunction(cc, diagnoses_cc, procedures_cc, diabetes_cc, scrips_cc, 
                                  fileout = "comorbidity_smr_pis_dm_casecontrol.rds")

## define comorbidity for case control data updated ----
cc <- readRDS(here("../../../Case_control/data/2020-07-09", "CC_linked_ANON_2020-07-23.rds"))
diagnoses_cc <- readRDS("../../../Case_control/data/2020-07-09/CC_SMR01_ICD10_x25_ANON_2020-07-23.rds")
procedures_cc <- readRDS("../../../Case_control/data/2020-07-09/CC_SMR01_OPCS4_MAIN.x25_ANON_2020-07-23.rds")
diabetes_cc <- cc %>% 
  select(ANON_ID, dm.type, diab.reg) 
scrips_cc <- readRDS("../../../Case_control/data/2020-07-09/CC_PIS_x15_ANON_2020-07-23.rds")

scrips_cc <- scrips_cc[ , c("bnf_paragraph_code",
             "ANON_ID")] %>% 
  distinct()
scrips_cc <- scrips_cc %>% 
  mutate(sectioncode = str_sub(bnf_paragraph_code, 1,4),
         paracode = str_sub(bnf_paragraph_code, 1, 6))

res_cc <- ComorbidityMakeFunction(cc, diagnoses_cc, procedures_cc, diabetes_cc, scrips_cc, 
                                  fileout = "comorbidity_smr_pis_dm_casecontrol_new.rds")

