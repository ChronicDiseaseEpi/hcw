#03_summarise_household
library(tidyverse)
library(here)

## Functions
source("code/Functions.R")

## Data
hcw <- readRDS(here("data", "hcw_basefile.rds"))
hhold <- readRDS(here("data", "hcw_household_basefile.rds"))
como <- readRDS(here("data", "comorbidity_smr_pis_dm.rds"))

hhold_smry <- hhold %>% 
  group_by(hid) %>% 
  summarise(hh_nonsf = sum(!duplicated(anon_id)),
            hh_age_m = mean(age),
            hh_age_s = sd(age),
            hh_male = sum(sex ==1),
            hh_female = sum(sex ==2),
            hh_00to05 = sum(age<5),
            hh_05to11 = sum(age >=5 & age <12),
            hh_12to17 = sum(age >=12 & age <18),
            hh_00to11 = sum(age <12),
            hh_18to64 = sum(age >= 18 & age <65),
            hh_65to74 = sum(age >= 65 & age <75),
            hh_75plus = sum(age >= 75)) %>% 
  ungroup() 
  
hhold_como_smry <- hhold %>% 
  inner_join(como) %>% 
  mutate(hh_como = case_when(
    como_count ==0 & age <75 ~ "0",
    como_count ==1 & age <75 ~ "1",
    como_count >=2  & age < 75~ "2",
    age>=75 ~ "older")
  ) %>% 
  count(hid, hh_como) %>% 
  mutate(hh_como = paste0("hh_como_", hh_como)) %>% 
  spread(hh_como, n, fill = 0L)

hhold_smry <- hhold_smry %>% 
  inner_join(hhold_como_smry)
rm(hhold_como_smry)

## Summarise staff
hcw_smry <- hcw %>%
  group_by(hid) %>% 
  summarise(hh_staff = sum(!duplicated(anon_id))) %>% 
  ungroup()

## Test function for selecting roles etc, performs same task as case_when
hcw_multi_role_test <- hcw %>% 
  semi_join(hcw_smry %>% filter(hh_staff >=2) %>% select(hid)) %>% 
  group_by(hid) %>% 
  summarise(
    role_test1 = case_when(
      "pf_any" %in% role ~ "pf_any",
      "undetermined" %in% role ~ "undetermined",
      "npf" %in% role ~ "npf"),
    role_test2 = Prioritise(role, c("pf_any", "undetermined", "npf"))) %>% 
  ungroup()
hcw_multi_role_test %>% 
  count(role_test1, role_test2)
rm(hcw_multi_role_test)

# HCW health board stratum and occupation take highest risk boards and occupations
# case_when performs this function
hcw_multi_stratum <- hcw %>% 
  semi_join(hcw_smry %>% filter(hh_staff >=2) %>% select(hid)) %>% 
  group_by(hid) %>% 
  summarise(stratum = min(stratum),
            role = Prioritise(role, c("pf_any", "undetermined", "npf")),
            role_sub = Prioritise(role_sub, c("pf_front",
                                             "pf_other", 
                                             "undetermined", 
                                             "pf_resp_oro_agp",
                                             "pf_icu",
                                             "npf")),
            role2 = Prioritise(role2, c("pf_front", "pf_other", "npf")),
            job_family_grp = Prioritise(job_family_grp, c("nursing and midwifery",
                                                          "medical and dental",
                                                          "allied health profession",
                                                          "support services",
                                                          "administrative services",
                                                          "Other")),
            nurs_med_ahp = max(nurs_med_ahp),
            grade = Prioritise(grade, c("consultant",
                                        "specialty_assoc_spec",
                                        "training_grade",
                                        "8+",
                                        "5-7",
                                        "1-4")),
            dental_grade = any(grade == "dental"),
            adult = Prioritise(adult, c("adult", "non adult")),
            dental = max(dental, na.rm = TRUE),
            gp = max(gp, na.rm = TRUE),
            length_of_service = max(length_of_service, na.rm=TRUE),
            whole_or_part_time = Prioritise(whole_or_part_time, c("w", "p", "general practice"))) %>% 
  ungroup()

hcw_singl_stratum <-  hcw %>% 
  mutate(dental_grade = grade == "dental") %>% 
  semi_join(hcw_smry %>% filter(hh_staff ==1) %>% select(hid)) %>% 
  select(hid, stratum, role, role_sub, role2, job_family_grp, nurs_med_ahp, grade, dental_grade,
         adult, dental, gp, length_of_service, whole_or_part_time) 
hcw_stratum <- bind_rows(hcw_multi_stratum, hcw_singl_stratum)
rm(hcw_multi_stratum, hcw_singl_stratum)

hcw_smry <- hcw_smry %>% 
  left_join(hhold_smry) %>% 
  mutate_at(vars(-hid, -hh_staff, -hh_age_m, -hh_age_s), ~ if_else(is.na(.x), 0L, .x)) %>% 
  mutate(hh_tot = hh_staff + hh_nonsf) %>% 
  left_join(hcw_stratum)

saveRDS(hcw_smry, "data/household_summary.Rds")

a <- hcw_smry %>% 
  filter(hh_tot <=6) %>% 
  filter(dental ==0, adult == "adult", dental_grade ==0) %>% 
  group_by(role) %>% 
  summarise_at(vars(-hid, -hh_age_m, -hh_age_s, -stratum, -job_family_grp, -role2, -role_sub,
                    -nurs_med_ahp, -grade, -dental_grade, -adult, -gp, 
                    -length_of_service, -whole_or_part_time), sum) %>% 
  ungroup() 
a_col <- a$role
a$role <- NULL
a_row <- names(a)
a <- t(a)
a_tbl <- tibble(rowname = a_row)
colnames(a) <- a_col
a <- as_tibble(a, .name_repair = "minimal")
a <- bind_cols(a_tbl, a) # %>% 
  # rename(Missing = V1)

b <- hcw_smry %>% 
  filter(dental ==0, adult == "adult", dental_grade ==0) %>% 
  mutate(rowname = if_else(hh_staff >=2, "Two or more staff housholds", "One staff households")) %>% 
  count(rowname, role) %>% 
  spread(role, n) # %>% 
  # rename(Missing = `<NA>`)

tbl1_3 <-  bind_rows(b, a)

tbl1_3 <- tbl1_3 %>% 
  mutate(rowname = factor(rowname,
                          levels = c("One staff households", "Two or more staff housholds",
                                     "hh_tot", "hh_staff","hh_nonsf",
                                     "hh_male", "hh_female", 
                                     "hh_00to05", "hh_05to11", "hh_12to17", "hh_18to64", 
                                     "hh_65to74", "hh_75plus", 
                                     "hh_como_0", "hh_como_1", "hh_como_2"
                                     ))) %>% 
  filter(!is.na(rowname)) %>% 
  arrange(rowname)

rowname_rename <- c("One staff households" = "One staff households",
                    "Two or more staff housholds" = "Two or more staff housholds",
                    "hh_tot" = "Total household members",
                    "hh_staff" = "Total staff in household",
                    "hh_nonsf" = "Total non-staff in household",
                    "hh_male" = "Total male",
                    "hh_female" = "Total female", 
                    "hh_00to05" = "Under 5", 
                    "hh_05to11" = "5 to 11", 
                    "hh_12to17" = "12 to 17", 
                    "hh_18to64" = "18 to 64", 
                    "hh_65to74" = "65 to 74",
                    "hh_75plus" = "75 and older", 
                    "hh_como_0" = "Aged <75 and no comorbidities",
                    "hh_como_1" = "Aged <75 and 1 comorbidity",
                    "hh_como_2" = "Aged <75 and 2 or more comorbidities"
)

tbl1_3 <- tbl1_3 %>% 
  mutate(rowname = as.character(rowname),
         rowname = rowname_rename[rowname])
tbl1_3

# Compare census, produce similar stats
census2011 <- read_delim("age|total|kids
All ages|5,196,386|2,055,981
0 to 15|913,173|858,331
16 to 24|595,568|241,913
25 to 49|1,796,397|841,161
50 to 64|1,037,096|109,867
65 and over|854,152|4,709", delim = "|")

census2011 <- census2011 %>% 
  mutate(prcnt = 100* kids/total)
hh_both <- bind_rows(hcw,
                     hhold) %>% 
  select(hid, anon_id, age, male) %>% 
  mutate(age_cat = Hmisc::cut2(age, cuts = c(15, 25, 50, 65))) 

hh_both <- hh_both %>% 
  group_by(hid) %>% 
  mutate(onefamilykids = if_else(length(hid) >=2 & any(age <=18), "kids", "no kids")) %>% 
  ungroup()

smry <- hh_both %>% 
  count(age_cat, onefamilykids) %>% 
  spread(onefamilykids, n, fill = 0L) %>% 
  mutate(prcnt = 100*kids/(kids+`no kids`))

census2011
smry
