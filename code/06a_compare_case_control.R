# 08_compare_case_control

library(tidyverse)
library(here)
library(janitor)
library(survival)


source("code/Functions.R")

lkp2cc <- read_csv(here("../../data/", "SWISS_GP_CC_ANON_ID_2020_06_08.csv"))
lkp2cc <- lkp2cc %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_", gp_flag)) %>% 
  select(-gp_flag)

cc <- readRDS(here("../../../Case_control/data", "CC_linked_ANON_2020-06-18.rds"))%>% 
  clean_names() 
onomap <- readRDS(here("../../../Case_control/data", "ONOMAP_ANON_2020-06-18.rds"))
onomap <- onomap %>% 
  mutate(ethnic5 = MakeEthnicOnomap(OnolyticsType, GeographicalArea),
         white = if_else(ethnic5 == "white" | is.na(ethnic5), 1L, 0L)) %>% 
  clean_names()

cc <- cc  %>% 
  mutate(sex = if_else(sex ==1, "Males", "Females"),
         hosptlsd = if_else((adm28|inhosp|dead28) & is_case & ecoss_positive == "Positive", 1L, 0L)) %>% 
  rename(age = age_year) 
cc <- cc %>% 
  inner_join(onomap %>% select(anon_id, white))

## Select cases from HCWs
hcw <- readRDS("data/hcw_for_cc.rds")
hhd <- readRDS("data/hhd_for_cc.rds") 

## Combine HCW and HHD
hcw_cases <- bind_rows(staff = hcw,
                       hhold = hhd,
                       .id = "staff") %>% 
  filter(ncov_result == "positive")
hcw_cases %>% summarise(hosptlsd = sum(hosptlsd))

## Compare cc
cc_cases <- cc %>% 
  filter(is_case)
cc_cases %>% 
  summarise(hosptlsd = sum(hosptlsd))

## hcw cases
hcw_cases <- hcw_cases %>% 
  select(anon_id, hosptlsd)
## case control cases
cc_cases <- cc_cases %>% 
  select(anon_id, hosptlsd)
## Append anon_id_cc to hcw_cases
hcw_cases <- hcw_cases %>% 
  left_join(lkp2cc)

hcw_cases %>% filter(is.na(cc_anon_id))
## Rename cc anon id to cc_anon_id
cc_cases <- cc_cases %>% 
  rename(cc_anon_id = anon_id,
         cc_hosptlsd = hosptlsd)
## examine how many cases are in either or both
compare_cases <- lkp2cc %>% 
  mutate(hcw_case = anon_id %in% hcw_cases$anon_id,
         hcw_hosp = anon_id %in% hcw_cases$anon_id[hcw_cases$hosptlsd ==1],
         ccs_case = cc_anon_id %in% cc_cases$cc_anon_id,
         ccs_hosp = cc_anon_id %in% cc_cases$cc_anon_id[cc_cases$cc_hosptlsd ==1])
## Cases in healthcare worker and in case control set
compare_cases %>% 
  count(hcw_case, ccs_case)
## Hospitalised cases in healthcare worker and in case control set
compare_cases %>% 
  count(hcw_hosp, ccs_hosp)

## Similar numbers, differences readily accounted for by differences in first positivity date in ECOSS
rm(cc_cases, compare_cases)

hcwhh <- bind_rows(staff = hcw, hhold = hhd, .id = "staff") %>% 
  distinct(anon_id, role, staff) 
como <- readRDS("data/comorbidity_smr_pis_dm_casecontrol.rds") %>% 
  select(anon_id, ihd_any:unkdm) %>% 
  as_tibble() %>% 
  mutate_at(vars(-anon_id), ~ .x %>% as.character() %>% as.integer())
como_count <- como %>%
  select(-anon_id) %>% 
  mutate_all(as.integer) %>% 
  as.matrix() 
como_count <- rowSums(como_count)
como <- como %>% 
  select(anon_id) %>% 
  mutate(como_count = como_count)
rm(hcw, hhd)

hcwhh <- hcwhh %>% 
  left_join(lkp2cc)
hcwhh %>% 
  count(role, !is.na(cc_anon_id)) %>% 
  spread(`!is.na(cc_anon_id)`, n) %>% 
  mutate(`TRUE`/`FALSE`)
hcwhh <- hcwhh %>% 
  rename(anon_id_hcw = anon_id, anon_id = cc_anon_id) %>% 
  filter(!is.na(anon_id))
rm(hcw_cases, lkp2cc)

## Join to cc
cc <- cc %>% 
  left_join(hcwhh) %>% 
  mutate(role = if_else(is.na(role), "population", role),
         staff = if_else(is.na(staff), "population", staff)) %>% 
  distinct(anon_id, .keep_all = TRUE)

cc <- cc %>% 
  left_join(como)
rm(como, como_count)

## Select hospitalised strata for staff
cc <- cc %>% 
  mutate(simd = if_else(simd2020_sc_quintile ==9, NA_real_, simd2020_sc_quintile))

hospstrat <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, role %in% c("npf", "population"), staff %in% c("staff", "population")) %>% 
  distinct(stratum)
hospstrat_m <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, sex == "Males") %>% 
  distinct(stratum)
hospstrat_f <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, sex == "Females") %>% 
  distinct(stratum)
cc_hosp <- cc %>% 
  semi_join(hospstrat) %>% 
  filter(role %in% c("npf", "population"), staff %in% c("staff", "population"))

summary(mod1)

## Select hospitalised strata for staff
cc <- cc %>% 
  mutate(simd = if_else(simd2020_sc_quintile ==9, NA_real_, simd2020_sc_quintile))

hospstrat <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, role %in% c("npf", "population"), staff %in% c("staff", "population")) %>% 
  distinct(stratum)
hospstrat_m <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, sex == "Males") %>% 
  distinct(stratum)
hospstrat_f <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, sex == "Females") %>% 
  distinct(stratum)
cc_hosp <- cc %>% 
  semi_join(hospstrat) %>% 
  filter(role %in% c("npf", "population"), staff %in% c("staff", "population"))
cc_hosp_m <- cc %>% 
  semi_join(hospstrat_m) %>% 
  filter(role %in% c("npf", "population"), staff %in% c("staff", "population"))
cc_hosp_f <- cc %>% 
  semi_join(hospstrat_f) %>% 
  filter(role %in% c("npf", "population"), staff %in% c("staff", "population"))

mod1 <- clogit(hosptlsd ~ role + strata(stratum),
               data = cc_hosp)
summary(mod1)
mod1_m <- clogit(hosptlsd ~ role + strata(stratum),
                 data = cc_hosp_m)
summary(mod1_m)
mod1_f <-clogit(hosptlsd ~ role + strata(stratum),
                data = cc_hosp_f)
summary(mod1_f)
## HRs for men and women somewhat different, but CIs wide, treat as single
modellist <- list("agesex", "ethnicsimd","como")
names(modellist) <- c("agesex", "ethnicsimd","como")
modellist$agesex <- ExtractEst(mod1)

mod2 <- update(mod1, . ~ . + white + simd)
summary(mod2)
modellist$ethnicsimd <- ExtractEst(mod2)

mod3 <- update(mod2, . ~ . + como_count)
summary(mod3)
modellist$como <- ExtractEst(mod3)

cmpr_npf_pop <- ConvertListModelSmriesDf(modellist)
write_csv(cmpr_npf_pop, "outputs/model_pop_staff.csv")

## Repeat for households, similar
hospstrat_hhd <- cc %>% 
  filter(hosptlsd ==1, age >=18 & age <= 65, role %in% c("npf", "population"), staff %in% c("hhold", "population")) %>% 
  distinct(stratum)
cc_hosp_hhd <- cc %>% 
  semi_join(hospstrat_hhd) %>% 
  filter(role %in% c("npf", "population"), staff %in% c("hhold", "population")) 

mod1 <- clogit(hosptlsd ~ role + strata(stratum),
                             data = cc_hosp_hhd)
summary(mod1)

modellist_hhd <- list("agesex", "ethnicsimd","como")
names(modellist_hhd) <- c("agesex", "ethnicsimd","como")
modellist_hhd$agesex <- ExtractEst(mod1)

mod2 <- update(mod1, . ~ . + white + simd)
summary(mod2)
modellist_hhd$ethnicsimd <- ExtractEst(mod2)

mod3 <- update(mod2, . ~ . + como_count)
summary(mod3)
modellist_hhd$como <- ExtractEst(mod3)

## Compare household to population with age interaction
hospstrat_both <- cc %>% 
  filter(hosptlsd ==1, staff %in% c("hhold", "population")) %>% 
  distinct(stratum)
cc_hosp_hhd_both <- cc %>% 
  semi_join(hospstrat_both) %>% 
  filter( staff %in% c("hhold", "population"), !role == "npf") %>% 
  mutate(hhold_both = if_else(role == "population", 0L, 1L),
         age_cat = case_when(
           age <18 ~ "b_kids",
           age>65 ~ "c_older",
           TRUE ~ "a_working_age"
         ))
mod1_both <- clogit(hosptlsd ~ hhold_both + strata(stratum),
                    data = cc_hosp_hhd_both)
mod1_both_inter <- clogit(hosptlsd ~  hhold_both:age_cat + strata(stratum),
                    data = cc_hosp_hhd_both)
anova_hhld_age <- anova(mod1_both_inter, mod1_both)
summary(mod1_both)
summary(mod1_both_inter)

## examine risk for household by age in non-NPF staff

## Examine risk in working age
hospstrat_hhd_wa <- cc %>% 
  filter(hosptlsd ==1, age >=18, age <=65, staff %in% c("hhold", "population"), !role == "npf") %>% 
  distinct(stratum)
cc_hosp_hhd_wa <- cc %>% 
  semi_join(hospstrat_hhd_wa) %>% 
  filter( staff %in% c("hhold", "population"), !role == "npf") %>% 
  mutate(hhold_wa = if_else(role == "population", 0L, 1L))
mod1_wa <- clogit(hosptlsd ~ hhold_wa + strata(stratum),
                    data = cc_hosp_hhd_wa)
summary(mod1_wa)
modellist_hhd$wa <- ExtractEst(mod1_wa)

## Examine risk in children
hospstrat_hhd_kids <- cc %>% 
  filter(hosptlsd ==1, age <18, staff %in% c("hhold", "population"), role %in% c("npf", "population")) %>% 
  distinct(stratum)
cc_hosp_hhd_kids <- cc %>% 
  semi_join(hospstrat_hhd_kids) %>% 
  filter( staff %in% c("hhold", "population"), role %in% c("npf", "population")) %>% 
  mutate(hhold_kids = if_else(role == "population", 0L, 1L))
mod1_kids <- clogit(hosptlsd ~ role + strata(stratum),
                    data = cc_hosp_hhd_kids)
summary(mod1_kids)
modellist_hhd$modkids <- ExtractEst(mod1_kids)

## Examine risk in older people
hospstrat_hhd_older <- cc %>% 
  filter(hosptlsd ==1, age >65, staff %in% c("hhold", "population"), role %in% c("npf", "population")) %>% 
  distinct(stratum)
cc_hosp_hhd_older <- cc %>% 
  semi_join(hospstrat_hhd_older) %>% 
  filter( staff %in% c("hhold", "population"), role %in% c("npf", "population")) %>% 
  mutate(hhold_older = if_else(role == "population", 0L, 1L))
mod1_older <- clogit(hosptlsd ~ role + strata(stratum),
                    data = cc_hosp_hhd_older)
summary(mod1_older)
modellist_hhd$modolder <- ExtractEst(mod1_older)
cmpr_npf_pop_hhd <- ConvertListModelSmriesDf(modellist_hhd)
write_csv(cmpr_npf_pop_hhd, "outputs/model_pop_hhd.csv")

## estimate interactions
cc_tv <- cc %>% 
  filter(hosptlsd ==1, staff %in% c("staff", "population"), role %in% c("pf_any", "population")) %>% 
  distinct(stratum)
cc_tv <- cc %>% 
  semi_join(cc_tv) %>% 
  filter( staff %in% c("staff", "population"), role %in% c("pf_any", "population")) 
## check single date for each stratum, there is
cc_tv %>% 
  group_by(stratum) %>% 
  summarise(n_dates = length(unique(specimendate))) %>% 
  ungroup() %>% 
  count(n_dates)

date_cuts <- as.Date(paste0(""))
cc_tv <- cc_tv %>% 
  mutate(post = Hmisc::cut2(as.integer(specimendate - as.Date("2020-03-01")), cuts = c(30,60)),
         role_bin = if_else(role == "pf_any", 1L, 0L))

mod_no_inter <- clogit(hosptlsd ~ role_bin + strata(stratum),
       data = cc_tv)
summary(mod_no_inter)
mod_inter <- clogit(hosptlsd ~ role_bin + strata(stratum) + role_bin:post,
                       data = cc_tv)
summary(mod_inter)
anova(mod_no_inter, mod_inter)
## Examine cases for both
library(tableone)
hosp_cases <- cc %>% 
  filter(is_case & ecoss_positive == "Positive" & hosptlsd ==1, 
         (age >=18 & age <=65) | staff == "hhold", 
         staff %in% c("staff", "population", "hhold")) %>% 
  mutate(Age = case_when(
    age < 18 ~ "<18",
    age <66 ~ "18-65",
    age >=66 ~ ">65"),
    Age = factor(Age, levels = c("<18", "18-65", ">65"))) %>% 
  rename(`High dependency` = hdu,
         `Intensive care` = icu,
         Male = sex,
         `Died` = dead28)
hosp_cases <- hosp_cases %>% 
  mutate(`Any comorbidity` = como_count >=1,
         Staff = case_when(
           staff == "staff" ~ "Healthcare workers",
           staff == "population" ~ "Population",
           staff == "hhold" ~ "Household members"))

cc_hosp_names <- names(hosp_cases)
cc_hosp_names = case_when(
  cc_hosp_names == "circulatory_other" ~ "Other circulatory system diseases", 
  cc_hosp_names == "ckd_any" ~ "Advanced chronic kidney disease", 
  cc_hosp_names == "esoph_stomach_duod" ~ "Disorders of esophagus, stomach and duodenum", 
  cc_hosp_names == "heart_other_any" ~ "Other heart disease", 
  cc_hosp_names == "ihd_any" ~ "Ischaemic heart disease", 
  cc_hosp_names == "liver_any" ~ "Decompensated liver disease", 
  cc_hosp_names == "neoplasm_any" ~ "Malignant Neoplasms", 
  cc_hosp_names == "neuro_any" ~ "Neurological disorders", 
  cc_hosp_names == "oad_any" ~ "Asthma and chronic lower respiratory disease", 
  cc_hosp_names == "t1dm" ~ "Diabetes, type 1", 
  cc_hosp_names == "t2dm" ~ "Diabetes, type 2", 
  cc_hosp_names == "unkdm" ~ "Diabetes, unknown type", 
  TRUE ~ cc_hosp_names)
names(hosp_cases) <- cc_hosp_names

# drop "liver_any", "immune_any", and "ckd_any"
vars <- c("Age", "age", "Male", "Ischaemic heart disease", "Other heart disease", "Other circulatory system diseases", 
          #"Advanced chronic kidney disease", 
          "Asthma and chronic lower respiratory disease", 
          "Neurological disorders", 
          # "Decompensated liver disease", 
          # "immune_any", 
          "Malignant Neoplasms", "Disorders of esophagus, stomach and duodenum", 
          "Diabetes, type 1", "Diabetes, type 2", "Diabetes, unknown type",
          "Any comorbidity",
          "High dependency", "Intensive care", 
          "Died")
vars_f <- setdiff(vars, c("age", "como_count"))
norm_no <- "Any comorbidity"
t3_paper <- CreateTableOne(vars = vars, factorVars = vars_f, strata = "Staff",
                           data = hosp_cases,   test = FALSE)
t3_paper2 <- t3_paper %>% 
  print(contDigist = 1,
        noSpaces = TRUE, dropEqual = TRUE, nonnormal = "como_count")

tab3Mat <- print(t3_paper, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, dropEqual = TRUE)
tab3Mat[tab3Mat == "0 (0.0)"] <- "0"
rownames(tab3Mat) <- str_to_sentence(rownames(tab3Mat))
saveRDS(tab3Mat, "outputs/table3_paper.rds")
knitr::kable(tab3Mat)
write.csv(tab3Mat, "outputs/table3_paper.csv")




