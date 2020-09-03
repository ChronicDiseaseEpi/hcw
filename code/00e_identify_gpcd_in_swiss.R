# combine SWISS and GPCD

library(tidyverse)
library(here)
library(janitor)

#+ setup
#' Seting up document for spinning into a markdown file for regression modelling for storage on internal git repository
knitr::opts_knit$set(root.dir = here::here())

## Note that the linkage ID is the anonymous ID and the GPFLAG
swiss <- readRDS(here("../../data/", "HCW_non_GP_base_file_anon.rds")) %>% 
  as_tibble() %>% 
  mutate(ANON_ID = paste(ANON_ID, GP_FLAG, sep = "_"),
         HID = paste(HID, GP_FLAG, sep = "_")) %>% 
  select(ANON_ID, UPI = WF_UPI, HID) %>% 
  clean_names()
gpcd <- readRDS(here("../../data/", "HCW_GP_base_file_anon.rds"))%>% 
  as_tibble() %>% 
  mutate(ANON_ID = paste(ANON_ID, GP_FLAG, sep = "_"),
         HID = paste(HID, GP_FLAG, sep = "_"),
         GP_UPI = as.character(GP_UPI),
         GP = if_else(is.na(GP), 0, GP)) %>% 
  select(ANON_ID, UPI = GP_UPI, GP, HID, age, sex) %>% 
  clean_names() 
gpcd <- gpcd %>% 
  filter(gp ==1) %>% 
  select(-gp) %>% 
  mutate(med_afc = "gpcd",
         unique_id = 5000000 + seq_along(anon_id),
         npf = 0,
         pf_any = 1,
         pf_front = 1,
         pf_resp_oro_agp = 0,
         pf_icu = 0,
         child_health = 0,
         dental = 0,
         grade  = "consultant",
         undetermined = 0,
         adult = "adult",
         country_qual = "gpcd not included",
         anon_id_multiple = "gpcd not included",
         registration_body = "gmc",
         base_location_description = "gpcd not included",
         marital_status = "gp_unknown",
         region = "gpcd not included",
         gp_flag = "1",
         immigration = NA_integer_,
         gpcd_gp = 1L,
         job_family = "medical and dental",
         specialty = "general practice",
         second_specialty = "general practice",
         whole_or_part_time = "general practice",
         ## assume 25 years for length of service
         length_of_service = 25) 

afc_med <- readRDS(here("data", "afc_med_pre_gp.rds")) %>% 
  mutate(hid = paste0(hid, "_0"))
swiss <- swiss %>% 
  filter(anon_id %in% afc_med$anon_id)
hhgpswiss_df <- read_csv("../../data/GPs_on_SWISS.csv") %>% 
  clean_names() 

## Read in household data, limit to staff have already selected and separate into staff and non staff
swiss_household <- readRDS(here("../../data/", "HCW_non_GP_base_file_anon.rds")) %>% 
  clean_names()  %>% 
  mutate(hid = paste0(hid, "_0"),
         anon_id = paste0(anon_id, "_0")) %>% 
  semi_join(swiss %>% select(hid)) 
swiss_staff_age_sex <- swiss_household %>% 
  semi_join(swiss)
swiss_household <- swiss_household %>% 
  anti_join(swiss)
gpcd_household <- readRDS(here("../../data/", "HCW_GP_base_file_anon.rds")) %>% 
  clean_names() %>% 
  mutate(hid = paste0(hid, "_1"),
         anon_id = paste0(anon_id, "_1")) %>% 
  semi_join(gpcd %>% select(hid)) 
gpcd_staff_age_sex <- gpcd_household %>% 
  semi_join(gpcd %>% select(anon_id))
gpcd_household <- gpcd_household %>% 
  anti_join(gpcd %>% select(anon_id))

## Read in eariler data to get postcode to map across to
## health board using files in "\\\\nssstats01/cl-out/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_1"
gp_earlier <- readRDS(here("../../data/", "Linked_GP_anon_20200507.rds")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anonid, "_1")) %>% 
  select(anon_id, pc7, age, sex)

## Clearly is the same anon id, use postcode to get healthboard
gp_check_same_anon <- gp_earlier %>% 
  inner_join(gpcd %>% select(anon_id, age_new = age, sex_new = sex))
gp_check_same_anon %>% 
  mutate(age_same = age == age_new,
         sex_same = sex == sex_new) %>% 
  count(age_same, sex_same)
gp_postcode <- gp_earlier %>% 
  select(anon_id, pc7)  %>% 
  distinct()
gpcd <- gpcd %>% 
  inner_join(gp_postcode)
rm(gp_earlier, gp_check_same_anon, gp_postcode)
postcode2hb <- readRDS("reference_files/pc7tohb.rds")
# postcodes 99.7% complete, leave as missing if is not present
mean(gpcd$pc7 %in% postcode2hb$pc7)
gpcd <- gpcd %>% 
  left_join(postcode2hb) %>% 
  mutate(board_name = str_to_lower(hb2019name)) %>% 
  select(-hb2019name) 
rm(postcode2hb)

# 5043 GPs
gpcd %>% count()

#' Want to identify :-
#' 1. GPs who are also staff in SWISS - remove from GPCD (with all HID), add to SWISS
#' 2. GPs who are also household members in SWISS, remove from GPCD (with all HID) add to SWISS houshold, then move to SWISS
#' It is not possible for GP household members to be in SWISS after 1 and 2 completed
#' Nor is it possible for GP household members to be SWISS household members
## 1. GPs who are staff in swiss
gpcdinswiss <- gpcd %>% 
  distinct(anon_id, upi, hid, board_name) %>% 
  filter(upi %in% swiss$upi)

## 667 GPs in SWISS
swiss_gp <- swiss %>% 
  filter(upi %in% gpcd$upi)
## 162965 not in swiss
swiss_not <- swiss %>% 
  filter(!upi %in% gpcd$upi)
## Append GP variables to SWISS anon_id and hid using UPI
swiss_gp <- swiss_gp %>% 
  inner_join(gpcd %>% rename(anon_id_gp = anon_id, hid_gp = hid) %>% mutate(gp = 1L))
intersect(swiss_gp$anon_id, paste0(hhgpswiss_df$swiss_anon_id, "_0")) %>% 
  length()
## Drop GP who are SWISS staff and household members of GPCD whoa re SWISS staff
gp_drop_swiss <- swiss_gp %>% 
  select(-anon_id, -hid) %>% 
  select(anon_id = anon_id_gp, hid = hid_gp)
gpcd <- gpcd %>% 
  anti_join(gp_drop_swiss)
gpcd_household <- gpcd_household %>%
  anti_join(gp_drop_swiss %>% select(hid))

##  2. GPs who are also household members in SWISS
hhgpswiss <- paste0(hhgpswiss_df$swiss_anon_id, "_0")
hhgpswiss <- intersect(hhgpswiss, swiss_household$anon_id)
hhgpswiss %>% length()

hhgpswiss <- swiss_household %>% 
  select(anon_id, hid) %>% 
  filter(anon_id %in% hhgpswiss) 

## Identify GPs in GPCD who are swiss household members
gpcd_swiss_hh <- gpcd %>% 
  ## This line should be the anonymous ID merge to identify the correct GPs
  inner_join(hhgpswiss_df %>% 
               mutate(anon_id = paste0(gp_anon_id, "_1")) %>% 
                        select(anon_id, swiss_anon_id)) %>% 
  mutate(swiss_anon_id = paste0(swiss_anon_id, "_0")) %>% 
  rename(anon_id_gp = anon_id,
         anon_id = swiss_anon_id) %>% 
  rename(hid_gp = hid) %>% 
  inner_join(hhgpswiss) %>% 
  mutate(gp = 1L)

## Identify swiss household who are GPs
swiss_household_gp <- swiss_household %>% 
  filter(anon_id %in% gpcd_swiss_hh$anon_id)
## Remove SWISS households who are GPs from the household set
swiss_household_not <- swiss_household %>% 
  filter(!anon_id %in% gpcd_swiss_hh$anon_id)

## Append GP variables to SWISS anon_id and hid using GPCD anon_id
swiss_gpcd_hh <- swiss_household_gp %>% 
  select(anon_id, hid) %>% 
  inner_join(gpcd_swiss_hh) 
rm(gp_drop_swiss, swiss_household, swiss, hhgpswiss)

## Drop GPs and households who are in SWISS households
gpcd <- gpcd %>% 
  anti_join(gpcd_swiss_hh %>% 
              select(anon_id_gp) %>%
              rename(anon_id = anon_id_gp))
gpcd_household <- gpcd_household %>% 
  anti_join(gpcd_swiss_hh %>% 
            select(hid_gp) %>% 
            rename(hid = hid_gp))

## Add #1 and #2 to SWISS table, #1 first requires an anti_join
## This should all use the swiss anon_id, ie "123456_0"
afc_med2 <- afc_med %>% 
  mutate(gp = 0L) %>% 
  anti_join(swiss_gp %>% select(anon_id)) %>% 
  bind_rows(swiss_gp,
            gpcd_swiss_hh)
#' There are `r nrow(afc_med2) - nrow(afc_med)` extra staff added to swiss 
#' and `r sum(afc_med2$gp)` GPs from GPCD in total.
#' If the anon ids are correct then the following conditions should be TRUE
all(afc_med2$anon_id %>% str_detect("_0$"))
all(afc_med2$hid %>% str_detect("_0$"))
afc_med <- afc_med2
rm(afc_med2)

## Add afcmed and residual GPCD together
afc_med <- bind_rows(swiss = afc_med,
                      gpcd = gpcd %>% mutate(gp = 1L),
                      .id = "datasource") %>% 
  select(-age, -sex)

afc_med %>% 
  mutate(anon_id_type = if_else(str_detect(anon_id, "_1$"), "GPCD", "SWISS"),
         hid_type     = if_else(str_detect(hid,     "_1$"), "GPCD", "SWISS")) %>% 
  count(datasource, anon_id_type, hid_type, gp)

## Rejoin household data
swiss_staff_age_sex
gpcd_staff_age_sex
swiss_household_not
gpcd_household

swiss_household_not <- bind_rows(swiss_household_not,
                                 swiss_staff_age_sex)
gpcd_household <- bind_rows(gpcd_household,
                            gpcd_staff_age_sex)

swiss_household_not <- swiss_household_not %>% 
  mutate_if(is.factor, as.character) %>% 
  select(anon_id, hid, onolytics_type, geographical_area, simd, age, sex, specdate, ncov_result) %>% 
  filter(hid %in% afc_med$hid)
gpcd_household <- gpcd_household %>% 
  mutate_if(is.factor, as.character) %>% 
  select(anon_id, hid, onolytics_type, geographical_area, simd, age, sex, specdate, ncov_result) %>% 
  filter(hid %in% afc_med$hid)

## Need to add back in SWISS household for the GPs
swiss_household_gpcd <- readRDS(here("../../data/", "HCW_non_GP_base_file_anon.rds")) %>% 
  clean_names()  %>% 
  mutate(hid = paste0(hid, "_0"),
         anon_id = paste0(anon_id, "_0")) %>% 
  semi_join(swiss_gp %>% select(hid)) 
swiss_household_gpcd <- swiss_household_gpcd [ , names(swiss_household_not)]
swiss_household_gp <- swiss_household_gp[ , names(swiss_household_not)]

household <- bind_rows(swiss_household_not,
                       gpcd_household,
                       swiss_household_gpcd,
                       swiss_household_gp) %>% 
  distinct()

# Duplicates due to duplicate HIDs already a known issue, take the first
dups <- household$anon_id[duplicated(household$anon_id)]
dups2 <- household %>% 
  filter(anon_id %in% dups) %>% 
  arrange(anon_id)
dups2 %>% 
  distinct(anon_id)
dups2 %>% 
  select(-hid) %>% 
  distinct()

household <- household %>% 
  arrange(anon_id, hid) %>% 
  distinct(anon_id, .keep_all = TRUE)

rm(swiss_household_not, gpcd_household, swiss_household_gpcd, dups,dups2)

saveRDS(afc_med, "data/afc_med.rds")
saveRDS(household, "data/household_afc_med_gpcd.rds")
