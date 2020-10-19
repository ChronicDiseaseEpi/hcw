#07_household_factors_on_staff
library(survival)
library(tidyverse)
library(survminer)
library(here)
library(janitor)
## run this as a comment
# library(butcher)

#+ setup
#' Seting up document for spinning into a markdown file for regression modelling for storage on internal git repository
knitr::opts_knit$set(root.dir = here::here())

## Switches ----
rndmspl <- FALSE

## functions ----
source("code/Functions.R")

## remove residuals from model to make object smaller
ChopSize <- function(x) {
  x$residuals <- NULL
  x$y <- NULL
  x$linear.predictors <- NULL
  x
}

## Function to capture warnings from coxph
update_modify <- function(...) {
  update(...) %>% 
    ChopSize()
}
quiet_update <- quietly(update_modify)

## read data ----
hcw <- readRDS(here("data/", "hcw_for_cc.rds")) 
hhold <- readRDS(here("data", "hhd_for_cc.rds"))

adults <- bind_rows(staff = hcw,
                    non_staff = hhold %>% filter(age >=18), .id = "staff") %>% 
  select(anon_id, hid, age10, male, white, simd, specdate, ncov_result, staff, gp)
rm(hcw, hhold)

## identify people without recent meds or comorbidities
ge3 <- readRDS("data/ge3_drugs.rds")
adults <- adults %>% 
  mutate(ge3scripts = if_else(anon_id %in% ge3, "ge3", "not ge3"))

rapid <- readRDS(here("../../data/", "HCW_RAPID.rds")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_", gp_flag)) %>% 
  select(-gp_flag) %>% 
  distinct()
deaths <- readRDS(here("../../data/", "HCW_deaths.rds")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_", gp_flag)) %>% 
  select(anon_id, covid_cod, covid_ucod, dead28) %>% 
  distinct()
como <- readRDS(here("data", "comorbidity_smr_pis_dm.rds")) %>% 
  distinct()
sicsag <- readRDS(here("../../data/", "HCW_SICSAG_ANON.rds")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_", gp_flag)) %>% 
  select(anon_id, covid_ic_uor_hdu, admit_unit) %>%
  distinct() 
sicsag %>% count(covid_ic_uor_hdu)
sicsag <- sicsag %>% 
  inner_join(adults %>% filter(ncov_result == "positive") %>% select(anon_id, specdate)) %>% 
  mutate(icu = if_else(covid_ic_uor_hdu == 1 , 1L, 0L),
         hdu = if_else(covid_ic_uor_hdu == 3, 1L, 0L)) %>% 
  mutate_at(vars(icu, hdu), ~ if_else( as.integer(admit_unit - specdate) <=28 , .x, 0L)) %>% 
  group_by(anon_id) %>% 
  summarise_at(vars(icu, hdu), max) %>% 
  ungroup()
adults <- adults %>% 
  inner_join(rapid) %>% 
  inner_join(deaths) %>% 
  inner_join(como)  %>% 
  left_join(sicsag) %>% 
  mutate_at(vars(icu, hdu), ~ if_else(is.na(.x), 0L, .x))
rm(rapid, como, deaths, sicsag)

#' add in household data
hhold_smry <- readRDS(here("data", "household_and_staff_household_summary.Rds"))
hhold_smry  <- hhold_smry %>% 
  select(hid, hh_00to11, hh_12to17, hh_18to64, hh_65to74, hh_75plus, role, nurs_med_ahp, 
         stratum, hh_00to05,
         job_family_grp, hh_staff, grade, length_of_service, whole_or_part_time) %>% 
  mutate(hh_adults = hh_18to64 + hh_65to74 + hh_75plus,
         hh06to11 = hh_00to11 - hh_00to05) 
adults_final <- adults %>% 
  inner_join(hhold_smry %>% distinct())
rm(adults, hhold_smry)

#' add in new cases from July data update, see following block to see how added
new_cases <- readRDS("data/new_events.rds")

#' Create time variable, note set to censoring where there is no hospitalisation 
adults_final <- adults_final %>% 
  mutate(time = as.double(specdate - as.Date("2020-03-01")),
         case = if_else(ncov_result == "positive", 1L, 0L),
         case = if_else(anon_id %in% new_cases$anycs$anon_id, 1L, case),
         time_any = if_else(case == 0, max(time), time),
         hosptlsd = if_else((adm28|inhosp|dead28) & ncov_result == "positive", 1L, 0L),
         hosptlsd = if_else(anon_id %in% new_cases$cases$anon_id, 1L, hosptlsd),
         time = if_else(hosptlsd == 0, max(time), time ),
         severe = if_else(hosptlsd ==1 & (icu|hdu|dead28), 1L, 0L),
         severe = if_else(anon_id %in% new_cases$serious$anon_id, 1L, severe),
         time_severe = if_else(hosptlsd ==0, max(time), time))
adults_final <- adults_final %>% 
  left_join(new_cases$cases %>% rename(time_new = time)) %>% 
  mutate(time = if_else(!is.na(time_new), as.double(time_new), time)) %>% 
  select(-time_new)
adults_final <- adults_final %>% 
  left_join(new_cases$serious %>% rename(time_new = time)) %>% 
  mutate(time_severe = if_else(!is.na(time_new), as.double(time_new), time)) %>% 
  select(-time_new)

## deal with missing LOS data
adults_final$length_of_service <- if_else(adults_final$length_of_service == -Inf,
                                          NA_real_,
                                          adults_final$length_of_service)
adults_final <- adults_final %>% 
  mutate(length_of_service = if_else(length_of_service %>% is.na(),
                                     median(length_of_service, na.rm= TRUE),
                                     length_of_service))

adults_final <- adults_final %>% 
  mutate(hh_00to11_c = case_when(
    hh_00to11 == 0 ~ "a_zero",
    hh_00to11 == 1 ~ "b_one",
    hh_00to11 == 2 ~ "c_two",
    hh_00to11 >= 3 ~ "d_three"
  ))

adults_final <- adults_final %>% 
  mutate(pt = factor(whole_or_part_time, levels = c("w", "p", "general practice")))
saveRDS(adults_final, "Scratch_data/adults_final.Rds")
## Models for all combinations. Takes time to run. If no changes skip to "Read in models again after saving" ----
## Create each set then "c" list together

Runmodels(){
## Primary outcome, full dataset, basic models ----
if(rndmspl == FALSE){
  # 0 Unadjusted
  mod0 <- list(result = coxph(Surv(time, hosptlsd) ~  hh_00to11 +
                                strata(stratum) + cluster(hid), data = adults_final))
  # 1 Age
  mod1 <- quiet_update(mod0$result, . ~ . +  pspline(age10))
  map(mod1[-1], print)
  # 2 As above plus 
  mod2 <- quiet_update(mod1$result, . ~ . + male +  simd +
                         role + staff + hh_12to17 + 
                         nurs_med_ahp + hh_adults + length_of_service)
  # 3 As above plus 
  mod3 <- quiet_update(mod2$result, . ~ . + como_count + ihd_any + heart_other_any + circulatory_other + ckd_any + 
                         oad_any + neuro_any +
                         liver_any + immune_any + neoplasm_any + esoph_stomach_duod + t1dm + t2dm )
  mod4 <- quiet_update(mod3$result, . ~ . + pt)
  mod0_adoles <- list(result = coxph(Surv(time, hosptlsd) ~  hh_12to17 +
                                       strata(stratum) + cluster(hid), data = adults_final))
  
  mod0_adults <- list(result = coxph(Surv(time, hosptlsd) ~  hh_adults +
                                       strata(stratum) + cluster(hid), data = adults_final))
}

## Run with random sample to speed up anlysis, do not use for final analysis
if(rndmspl == TRUE){
  smpl_frac <- 0.2
  adults_final_case <- adults_final %>% 
    filter(hosptlsd==1)
  adults_final_non_case <- adults_final %>% 
    filter(hosptlsd ==0)
  adults_final_non_case <- adults_final_non_case %>% 
    sample_frac(smpl_frac)
  adults_final <- bind_rows(adults_final_case %>% mutate(mywt = 1), 
                            adults_final_non_case %>% mutate(mywt = 1/smpl_frac))
  rm(adults_final_case, adults_final_non_case)
  mod0 <- list(result = coxph(Surv(time, hosptlsd) ~  hh_00to11 +
                                strata(stratum) + cluster(hid), data = adults_final, weights = mywt))
  mod1 <- quiet_update(mod0$result, . ~ . +  pspline(age10))
  mod2 <- quiet_update(mod1$result, . ~ . + male +  simd +
                         role + staff + hh_12to17 + 
                         nurs_med_ahp + hh_adults + length_of_service)
  mod3 <- quiet_update(mod2$result, . ~ . + como_count + ihd_any + heart_other_any + circulatory_other + ckd_any + oad_any + neuro_any +
                         liver_any + immune_any + neoplasm_any + esoph_stomach_duod + t1dm + t2dm )
  mod4 <- quiet_update(mod3$result, . ~ . + pt)
  
  mod0_adoles <- list(result = coxph(Surv(time, hosptlsd) ~  hh_12to17 +
                                       strata(stratum) + cluster(hid), data = adults_final, weights = mywt))
  
  mod0_adults <- list(result = coxph(Surv(time, hosptlsd) ~  hh_adults +
                                       strata(stratum) + cluster(hid), data = adults_final, weights = mywt))
  
}

mdls <- list(mod0 = mod0, mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
rm(mod0, mod1, mod2, mod3, mod4)

## Re-run for different exposure groups ----
mdls_c <- map(mdls, ~ quiet_update(.x$result, . ~ . - hh_00to11 + hh_00to11_c))
names(mdls_c) <- paste0(names(mdls), "_catg")

# Re-run with under 5s and 5 to 11s as separate variables
# 5 to 11s and udner 5s mdls_f = "fine"
mdls_f <- map(mdls, ~ quiet_update(.x$result, . ~ . - hh_00to11 + hh_00to05 + hh06to11) )
names(mdls_f) <- paste0(names(mdls_f), "_fine")
## Create a list of models with different exposure types
names(mdls) <- paste0(names(mdls), "_cont")
mdls <- c(mdls, mdls_c, mdls_f)
rm(mdls_c, mdls_f)

## Re-run all models for different outcomes ----
# severe case (ICU, HDU or death)
# mdls_s for models_severe
mdls_s <- map(mdls, ~ quiet_update(.x$result, Surv(time_severe, severe) ~ .) )
names(mdls_s) <- paste0(names(mdls), "_svre")

# Any case
mdls_m <- map(mdls, ~ quiet_update(.x$result, Surv(time_any, case) ~ .) )
names(mdls_m) <- paste0(names(mdls), "_any")
names(mdls) <- paste0(names(mdls), "_hosp")
mdls <- c(mdls, mdls_s, mdls_m)
rm(mdls_s, mdls_m)

## Re-run models for different populations ----
# ge2 staff per household
mdls_sf <- map(mdls, ~ quiet_update(.x$result, . ~ . - staff, data = adults_final %>% filter(staff == "staff")) )
names(mdls_sf) <- paste0(names(mdls_sf), "_stff")
# patient-facing
mdls_pf <- map(mdls, ~ quiet_update(.x$result, . ~ . - role, data = adults_final %>% filter(role == "pf_any") ) )
names(mdls_pf) <- paste0(names(mdls_pf), "_pf")
# non patient-facing
mdls_npf <- map(mdls, ~ quiet_update(.x$result, . ~ . - role, data = adults_final %>% filter(role == "npf") ) )
names(mdls_npf) <- paste0(names(mdls_npf), "_npf")
# Men
mdls_m  <- map(mdls, ~ quiet_update(.x$result, . ~ . - male, data = adults_final %>% filter(male ==1) ) )
names(mdls_m) <- paste0(names(mdls_m), "_men")
# Women
mdls_w  <- map(mdls, ~ quiet_update(.x$result, . ~ . - male, data = adults_final %>% filter(male ==0) ) )
names(mdls_w) <- paste0(names(mdls_w), "_wom")
# Restrict to healthy
hlthy <- mdls[str_detect(names(mdls), "^mod[0-2]")]
hlthy  <- map(hlthy, ~ quiet_update(.x$result, data = adults_final %>% filter(como_count ==0)) )
names(hlthy) <- paste0(names(hlthy), "_hlthy")
vhlthy <- map(hlthy, ~ quiet_update(.x$result, data = adults_final %>% filter(!ge3scripts == "ge3")) )
names(vhlthy) <- paste0(names(vhlthy), "_vhlthy")
# Part-timeness analysis, add part time variable to models other than model 4
ptmdls_p <- map(mdls[!str_detect(names(mdls), "^mod4")], ~ quiet_update(.x$result, data = adults_final %>% filter(pt == "p")) )
ptmdls_w <- map(mdls[!str_detect(names(mdls), "^mod4")], ~ quiet_update(.x$result, data = adults_final %>% filter(pt == "w")) )
names(ptmdls_p) <- paste0(names(ptmdls_p), "_ptstrat")
names(ptmdls_w) <- paste0(names(ptmdls_w), "_ptwhole")
names(mdls) <- paste0(names(mdls), "_all")
mdls <- c(mdls, mdls_m, mdls_w, mdls_sf, mdls_pf, mdls_npf, hlthy, vhlthy, ptmdls_p, ptmdls_w)
rm(             mdls_m, mdls_w, mdls_sf, mdls_pf, mdls_npf, hlthy, vhlthy, ptmdls_p, ptmdls_w)

saveRDS(mdls, "Scratch_data/mdls_hh_concise.Rds")
}

## Read in models again after saving ----
mdls <- readRDS("Scratch_data/mdls_hh_concise.Rds")

## Unadjusted and age-sex adjusted for adolescents and adults, for any, severe and hospitalised outcomes ----
mod0_adoles_hosp <- coxph(Surv(time, hosptlsd) ~  hh_12to17 +
                            strata(stratum) + cluster(hid), data = adults_final)

mod0_adults_hosp <- coxph(Surv(time, hosptlsd) ~  hh_adults +
                            strata(stratum) + cluster(hid), data = adults_final)
mod1_adoles_hosp <- update(mod0_adoles_hosp, . ~ . + pspline(age10))
mod1_adults_hosp <- update(mod0_adults_hosp, . ~ . + pspline(age10))

mdls_aa_hosp <- list(mod0_adoles = mod0_adoles_hosp,
                     mod1_adoles = mod1_adoles_hosp,
                     mod0_adults = mod0_adults_hosp,
                     mod1_adults = mod1_adults_hosp)
rm(mod0_adoles_hosp, mod1_adoles_hosp, mod0_adults_hosp, mod1_adults_hosp)
mdls_aa_svre <- vector(length = 4, mode = "list")
names(mdls_aa_svre) <- names(mdls_aa_hosp)
for(i in 1:4){
  mdls_aa_svre[[i]] <- update(mdls_aa_hosp[[i]], Surv(time_severe, severe) ~ .)
}

mdls_aa_any <- vector(length = 4, mode = "list")
names(mdls_aa_any) <- names(mdls_aa_hosp)
for(i in 1:4){
  mdls_aa_any[[i]] <- update(mdls_aa_hosp[[i]], Surv(time_any, case) ~ .)
}

adul_adol <- list(hosp = mdls_aa_hosp,
                  svre = mdls_aa_svre,
                  case = mdls_aa_any)
rm(mdls_aa_any, mdls_aa_hosp, mdls_aa_svre)
adol_adol <- map(adol_adol, ~ map(.x, ChopSize))

saveRDS(adol_adol,
        "Scratch_data/adolescents_adults.Rds")


## interactions ----
# check interaction by role
mod3_role_no_interact <- update(mdls$mod3_cont_hosp_all$result, data = adults_final)
mod3_role_interact    <- update(mdls$mod3_cont_hosp_all$result, . ~ . + role:hh_00to11, data = adults_final)
summary(mod3_role_interact)
interact_res <- anova(mod3_role_no_interact, mod3_role_interact)
mod3_role_interact <- ChopSize(mod3_role_interact)
saveRDS(list(mdl = mod3_role_interact, anova = interact_res), "Scratch_data/interact_mod3.Rds")

# Compare model fit in children split into pre-school and school aged
lumped <- update(mdls$mod2_cont_any_all$result, data = adults_final)
split  <- update(mdls$mod2_fine_any_all$result, data = adults_final)
interact_res <- anova(lumped, split)
rm(lumped, split)
saveRDS(interact_res, "Scratch_data/preschool_interaction.Rds")

# Compare part-time interaction hosputalised cases
lumped <- update(mdls$mod4_cont_hosp_all$result, . ~ . , data = adults_final %>% filter(pt != "general practice"))
split  <- update(lumped, . ~ . + pt:hh_00to11)
interact_res <- anova(lumped, split)
saveRDS(interact_res, "Scratch_data/parttime_interaction_hosp.Rds")

# Compare part-time interaction all cases
lumped <- update(mdls$mod4_cont_any_all$result, . ~ . , data = adults_final %>% filter(pt != "general practice"))
split  <- update(lumped, . ~ . + pt:hh_00to11)
interact_res <- anova(lumped, split)
saveRDS(interact_res, "Scratch_data/parttime_interaction_all_cases.Rds")

# Compare part-time interaction severe cases
lumped <- update(mdls$mod4_cont_svre_all$result, . ~ . , data = adults_final %>% filter(pt != "general practice"))
split  <- update(lumped, . ~ . + pt:hh_00to11)
split$df[is.na(split$df)] <- 0
lumped$df[is.na(lumped$df)] <- 0

interact_res <- anova(lumped, split)
saveRDS(interact_res, "Scratch_data/parttime_interaction_severe_cases.Rds")

# restrict to households where all adults are healthcare workers
all_staff <- mdls[ names(mdls) %>% str_detect("_all$")]
mdls_all_staff <- map(all_staff, ~ quiet_update(.x$result, . ~ . -staff ,data = adults_final %>% filter(hh_staff == hh_adults)))
saveRDS(mdls_all_staff, "Scratch_data/mdls_hh_concise_all_staff.Rds")


## Analyse risk conditional on hospitalisation and case status ----
adults_final_cases <- adults_final %>% 
  filter(case ==1)
adults_final_hosp <- adults_final %>% 
  filter(hosptlsd ==1)

# Examine risk of death or ICU among those hospitalised by presence/absence of chilren
# and risk of death or ICU among any estimating positive with/without children
mdl_forms <- map(mdls, ~ .x$result %>% formula)
mdl_forms <- map(mdl_forms, ~ update.formula(.x, severe ~ .))
mdl_forms <- mdl_forms[!duplicated(mdl_forms)]

severe_on_case <- map(mdl_forms, ~ glm(.x, data = adults_final_cases, family = "binomial"))
severe_on_case_res <- map(severe_on_case, broom::tidy)
severe_on_case_res <- bind_rows(severe_on_case_res, .id = "modelname")
severe_on_case_res_rv <- severe_on_case_res %>% 
  filter(term %>% str_detect("^hh_00to11_c"))

# hospitalised_on_case
mdl_forms <- map(mdl_forms, ~ update.formula(.x, hosptlsd ~ .))
hospitalised_on_case <- map(mdl_forms, ~ glm(.x, data = adults_final_cases, family = "binomial"))
hospitalised_on_case <- map(hospitalised_on_case, broom::tidy)
hospitalised_on_case_res <- bind_rows(hospitalised_on_case, .id = "modelname")
hospitalised_on_case_res_rv <- hospitalised_on_case_res %>% 
  filter(term %>% str_detect("^hh_00to11_c")) %>% 
  select(modelname, term, estimate) %>% 
  spread(term, estimate)
hospitalised_on_case_res_rv2 <- hospitalised_on_case_res %>% 
  filter(term %>% str_detect("^hh_00to11$"))

# Severe on hospitalised
mdl_forms <- map(mdls, ~ .x$result %>% formula)
mdl_forms <- map(mdl_forms, ~ update.formula(.x, severe ~ .))
mdl_forms <- mdl_forms[!duplicated(mdl_forms)]
mdl_forms <- mdl_forms[str_detect(names(mdl_forms), "^mod[0-2]") & str_detect(names(mdl_forms), "cont")]
severe_on_hospitalised <- map(mdl_forms, ~ {
  print(.x)
  glm(.x, data = adults_final_hosp, family = "binomial")
})
severe_on_hospitalised <- map(severe_on_hospitalised, broom::tidy)
severe_on_hospitalised_res <- bind_rows(severe_on_hospitalised, .id = "modelname")
severe_on_hospitalised_res_rv <- severe_on_hospitalised_res %>% 
  filter(term %>% str_detect("^hh_00to11$"))

## Plots and summary statistics ----
PerOutcome <- function(myoutcome, mydf = adults_final){
  # select outcome
  # browser()
  mydf$outcome <- mydf[ , myoutcome, drop = TRUE]
  
  ci_events <- mydf %>% 
    arrange(hh_00to11_c, specdate) %>% 
    group_by(hh_00to11_c) %>% 
    mutate(cum_events = cumsum(outcome)) %>% 
    ungroup() %>% 
    select(hh_00to11_c, specdate, cum_events)
  ci_n <- mydf %>% 
    group_by(hh_00to11_c) %>% 
    count() %>% 
    ungroup()
  ci_events <- ci_events %>% 
    inner_join(ci_n) %>% 
    mutate(ci_prop = 10000*cum_events/n,
           hh_00to11_c = factor(hh_00to11_c, ordered = TRUE))
  
  ci_events2 <- ci_events %>% 
    group_by(hh_00to11_c) %>% 
    mutate(lag_same = if_else(cum_events == lag(cum_events, default = 1), "todrop", "keep")) %>% 
    ungroup() %>% 
    filter(lag_same == "keep") %>% 
    select(-lag_same)
  ci_events3 <- ci_events2 %>% 
    group_by(hh_00to11_c) %>% 
    filter(specdate == max(specdate), cum_events == max(cum_events)) %>% 
    ungroup() %>% 
    distinct()  
  list(ci_events2, ci_events3)
}
a <- PerOutcome("hosptlsd")
b <- PerOutcome("case")
ci_events4 <- bind_rows(a) 

ci_events4 <- ci_events4 %>% 
  mutate(hh_00to11_c = factor(hh_00to11_c,
                              levels = c("a_zero", "b_one", "c_two", "d_three"),
                              labels = c("0", "1", "2", "3 or above")))
ci_events_max <-  ci_events4 %>% 
  group_by(hh_00to11_c) %>% 
  summarise(cum_events = max(cum_events),
            ci_prop = max(ci_prop),
            n = max(n))%>% 
  ungroup() %>% 
  mutate(specdate = max(ci_events4$specdate))

ci_events5 <- bind_rows(ci_events4,
                        ci_events_max)

plot1 <- ggplot(ci_events5, aes(x = specdate, y = ci_prop, colour = hh_00to11_c)) +
  geom_step() +
  scale_y_continuous("Risk of hospitalisation with COVID-19 per 10,000 population") +
  scale_x_date("Date of first positive test", date_breaks = "2 weeks") +
  scale_color_ordinal("Number of children under 12 in household")
plot1
png("outputs/Plot of risk in children.png", height = 8, width = 12, units = "in", res = 300)
plot1
dev.off()

## Summary results ----
SmryCounts <- function(events_counts){
  ci_smry <- events_counts %>% 
    mutate(hh_00to11_c = factor(hh_00to11_c,
                                levels = c("a_zero", "b_one", "c_two", "d_three"),
                                labels = c("0", "1", "2", "3 or above")),
           ci_prop = formatC(ci_prop, format = "f", digits = 1)) %>% 
    select(`No. of children in household` = hh_00to11_c, `Cases in adults` = cum_events,
           `Number of adults` = n, `Cases per 10,000 adults` = ci_prop) %>% 
    t() %>% 
    # as_tibble(.name_repair = "minimal")
    as.data.frame()
  ci_smry_names <- ci_smry[1,, drop = TRUE]
  names(ci_smry) <- ci_smry_names
  ci_smry <- ci_smry[-1,]
}
ci_smry_hosp <- SmryCounts(a[[2]])

## All cases
ci_smry_case <- PerOutcome(myoutcome = "case")
ci_smry_case <- SmryCounts(ci_smry_case[[2]])
ci_smry_severe <- PerOutcome(myoutcome = "severe")
ci_smry_severe <- SmryCounts(ci_smry_severe[[2]])

ci_smry_hosp_pf <- PerOutcome(myoutcome = "hosptlsd", adults_final %>% filter(role == "pf_any"))
ci_smry_hosp_pf <- SmryCounts(ci_smry_hosp_pf[[2]])

ci_smry_hosp_npf <- PerOutcome(myoutcome = "hosptlsd", adults_final %>% filter(role == "npf"))
ci_smry_hosp_npf <- SmryCounts(ci_smry_hosp_npf[[2]])

ci_smry %>% knitr::kable()
saveRDS(list(hosp = ci_smry_hosp,
             case = ci_smry_case,
             svre  = ci_smry_severe,
             hosp_pf = ci_smry_hosp_pf,
             hosp_npf = ci_smry_hosp_npf), "outputs/chips_smry_tbl.Rds")