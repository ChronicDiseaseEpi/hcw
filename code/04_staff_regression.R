# regression models, just age and se to begin with
library(janitor)
library(tidyverse)
library(survival)
library(survminer)
library(here)

#+ setup
#' Seting up document for spinning into a markdown file for regression modelling for storage on internal git repository
knitr::opts_knit$set(root.dir = here::here())

#' Need to run this function to spin (ie knit from r file) the document. Would be more elegant to store this in another script.
knitme <- function(){
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
  knitr::spin("code/04_staff_regression.R")
  file.copy("04_staff_regression.md", "code/04_staff_regression.md", overwrite = TRUE)
  if(!dir.exists("code/figure")) dir.create("code/figure")
  file.remove("04_staff_regression.md", "04_staff_regression.html")
}

## functions ----
source("code/Functions.R")

## read data ----
hcw <- readRDS(here("data/", "hcw_basefile.rds")) 
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
sicsag <- readRDS(here("../../data/", "HCW_SICSAG_ANON.rds")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_", gp_flag)) %>% 
  select(anon_id, covid_ic_uor_hdu, admit_unit) %>%
  distinct() 
sicsag %>% count(covid_ic_uor_hdu)
sicsag <- sicsag %>% 
  inner_join(hcw %>% filter(ncov_result == "positive") %>% select(anon_id, specdate)) %>% 
  mutate(icu = if_else(covid_ic_uor_hdu == 1 , 1L, 0L),
         hdu = if_else(covid_ic_uor_hdu == 3, 1L, 0L)) %>% 
  mutate_at(vars(icu, hdu), ~ if_else( as.integer(admit_unit - specdate) <=28 , .x, 0L)) %>% 
  group_by(anon_id) %>% 
  summarise_at(vars(icu, hdu), max) %>% 
  ungroup()

como <- readRDS(here("data", "comorbidity_smr_pis_dm.rds")) %>% 
  distinct()

hcw <- hcw %>% 
  inner_join(rapid ) %>% 
  inner_join(deaths ) %>% 
  inner_join(como) %>% 
  left_join(sicsag)  %>% 
  mutate_at(vars(icu, hdu), ~ if_else(is.na(.x), 0L, .x))
rm(rapid, como, deaths, sicsag)

#' Create time variable, note set to censoring where there is no hosptialisation 
hcw <- hcw %>% 
  mutate(time = as.double(specdate - as.Date("2020-03-01")),
         hosptlsd = if_else((adm28|inhosp|dead28) & ncov_result == "positive", 1L, 0L),
         time = if_else(hosptlsd == 0, max(time), time ),
         part_time = factor(whole_or_part_time, levels = c("w", "p", "general practice")))

#' restrict to adults and non-dental
nrow(hcw)
hcw <- hcw %>%
  filter(dental == 0  & adult == "adult")
nrow(hcw)

#' # Summary statistics on counts for different groups
hcw %>% 
  filter(age >=18 & age <=65) %>% 
  count(gp, hosptlsd)
sum(is.na(hcw$time))
stem(hcw$time)
hcw %>% 
  filter(job_family_grp == "medical and dental") %>% 
  count(gp, role, hosptlsd)
hcw %>% 
  count(gp, role, dead28) %>% 
  spread(dead28, n, fill = 0L) %>% 
  mutate(prcnt = 100*`1` / (`1` + `0`))
hcw %>% 
  count(gp, role, icu) %>% 
  spread(icu, n, fill = 0L) %>% 
  mutate(prcnt = 100*`1` / (`1` + `0`))
hcw %>% 
  count(gp, role, hdu) %>% 
  spread(hdu, n, fill = 0L) %>% 
  mutate(prcnt = 100*`1` / (`1` + `0`))
hcw %>% 
  count(!ncov_result == "positive" & covid_cod | covid_ucod)
hcw %>% 
  filter(hosptlsd ==1, age >=18 & age <=65) %>% 
  count(dead28 | icu | hdu) %>% 
  mutate(prcnt = 100* n/sum(n))

#' Calcualte survival for subsequent plots, output these as dataframes for comparison with population
km1 <- survfit(Surv(time, hosptlsd) ~ sex +  role, data = hcw %>% filter(age >=18 & age<=65))
a <- survminer::surv_summary(km1) %>% 
  as_tibble()
a <- a %>% 
  mutate(per100000 = 100000*(1 - surv)) %>% 
  select(sex, role, time, per100000)
astart <- a %>% 
  distinct(sex, role)
astart <- astart %>% 
  mutate(per100000 = 0,
         time = 0)
a <- bind_rows(astart, a)
saveRDS(a, "outputs/risk_by_role.rds")

#' Plot by sub-role
km2 <- survfit(Surv(time, hosptlsd) ~  role_sub, data = hcw %>% filter(age >=18 & age<=65))
a <- survminer::surv_summary(km2) %>% 
  as_tibble()
a <- a %>% 
  mutate(per100000 = 100000*(1 - surv)) %>% 
  select(role_sub, time, per100000)
astart <- a %>% 
  distinct(role_sub)
astart <- astart %>% 
  mutate(per100000 = 0,
         time = 0)
a <- bind_rows(astart, a)
saveRDS(a, "outputs/risk_by_sub_role.rds")

## plots by health board
hcw_hb <- hcw %>% 
  filter(!str_detect(board_name, "\\|")) %>% 
  mutate(test_time = if_else(!ncov_result == "positive", max(time), as.double(specdate - as.Date("2020-03-01")))) 
km_hb <- survfit(Surv(test_time, ncov_result == "positive") ~ role + board_name, data = hcw_hb %>% filter(age >=18 & age<=65))
a <- survminer::surv_summary(km_hb) %>% 
  as_tibble()
a <- a %>% 
  mutate(per100=  100*(1 - surv)) %>% 
  select(role, time, per100, board_name, n.risk)
astart <- a %>% 
  distinct(role, board_name)
astart <- astart %>% 
  mutate(per100 = 0,
         time = 0)
a <- bind_rows(astart, a) %>% 
  group_by(board_name, role) %>% 
  mutate(n.risk = max(n.risk, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(events = n.risk * per100/100)

saveRDS(a, "outputs/risk_by_role_by_hb.rds")
a <- a %>% 
  filter(board_name %in%
           c("nhs ayrshire and arran", 
             "nhs borders", 
             "nhs dumfries and galloway", 
             "nhs fife", 
             "nhs forth valley", 
             "nhs grampian", 
             "nhs highland",
             "nhs greater glasgow and clyde", 
             "nhs lanarkshire",
             "nhs lothian", 
             "nhs tayside"))

a <- a %>% 
  mutate(board_name = board_name %>% 
           str_to_title() %>% 
           str_replace("Nhs", "") %>% 
           str_replace("And", "and"))
board_order <- a %>%
  filter(role == "pf_any") %>% 
  arrange(desc(n.risk)) %>% 
  distinct(board_name) %>% 
  pull()

a <- a %>% 
  mutate(board_name_f = factor(board_name, levels = board_order),
         mydate = as.Date("2020-03-01") + time,
         role_f = factor(role, 
                         levels = c("npf", "undetermined", "pf_any"),
                         labels = c("Non-patient facing",
                                    "Undetermined",
                                    "Patient facing")))
  
## do ggplot of trends over time by healthboard
plot_hb <- ggplot(a, aes(x = mydate, y = events, colour = role_f)) + geom_step() +
  facet_wrap(~board_name_f, scales = "free_y") +
  scale_y_continuous("Number of healthcare workers testing positive") +
  scale_x_date("Date of first positive test", date_breaks = "3 weeks", date_labels = "%b-%d") +
  scale_color_discrete("") +
  theme_bw()
plot_hb
saveRDS(plot_hb, "outputs/test_pos.rds")

# Limit to age 18 to 65
hcw <- hcw %>% 
  filter(age >=18 & age <=65)

#' # Modelling
#' Model age
#' 
#' Save HCW data for subsequent comparison with population
saveRDS(hcw, "data/hcw_for_cc.rds")
mod1 <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + cluster(hid) + strata(stratum) + role, 
              data = hcw)
modellist <- list(agesex = ExtractEst(mod1))

#' Better AIC with SIMD as a continuous variable
mod2 <- update(mod1, . ~ . + white + simd)
AIC(mod2)
mod2_f <- update(mod1, . ~ . + white + factor(simd))
AIC(mod2_f)
summary(mod2)
modellist$ethnicsimd <- ExtractEst(mod2)

mod3 <- update(mod2, . ~ . + como_count + t2dm )
summary(mod3)
modellist$como <- ExtractEst(mod3)


#' Ethnicity
mod_ethnic <- update(mod1, . ~ . -role + white)
summary(mod_ethnic)
modellist$ethnicalone <- ExtractEst(mod_ethnic)

#' occupation; very similar whether or not adjust for role
mod_occ  <- update(mod3, . ~ . + job_family_grp )
mod_occ_chk <- update(mod3, . ~ . + job_family_grp - role)
summary(mod_occ)
summary(mod_occ_chk)

#' On basis of similar coefficents collapse occupation into med_nurs_ahp and other
mod_occ_smpl <- update(mod3, . ~ . + nurs_med_ahp)
summary(mod_occ_smpl)
#' Better AIC with simplification and makes sense in terms of occupational activities 
AIC(mod_occ)
AIC(mod_occ_smpl)
rm(mod_ethnic, mod_occ, mod_occ_chk)
modellist$occupation <- ExtractEst(mod_occ_smpl)

mod_pt <- update(mod_occ_smpl, . ~ . + part_time)
summary(mod_pt)
modellist$parttime <- ExtractEst(mod_pt)
#' No evidence of a difference across AFC grades conditional on role and occupation
mod_grade_afc <- update(mod_pt, . ~ . + grade, data = hcw %>% filter(grade %in% c("1-4", "5-7", "8+")))
summary(mod_grade_afc)
modellist$gradeafc <- ExtractEst(mod_grade_afc)
#' Difficult to interpret grade for medical staff as numbers low
mod_grade_mnd <- update(mod_pt, . ~ pspline(age) + male + simd + grade, data = hcw %>% 
                          filter(grade %in% c("training_grade", "consultant", "specialty_assoc_spec"),
                                 role == "pf_any"))
summary(mod_grade_mnd)
modellist$grademed <- ExtractEst(mod_grade_mnd)

#' Examine comorbidities, use step function to identify best set of comorbidities
#' note that this choice was incorporated into model 3 above

#' hcw_nomis <- hcw %>% 
#'   select(time, hosptlsd, age10, male, hid, stratum, role, white, como_count,
#'          ihd_any,
#'          heart_other_any,
#'          circulatory_other,
#'          ckd_any,
#'          oad_any,
#'          neuro_any,
#'          liver_any,
#'          immune_any,
#'          neoplasm_any,
#'          esoph_stomach_duod,
#'          t1dm,
#'          t2dm,
#'          unkdm,
#'          simd,
#'          nurs_med_ahp) %>% 
#'   drop_na()
#' mod_como <- update(mod2, . ~ . + como_count, data = hcw_nomis)
#' mod_como2 <- update(mod_como, . ~ . + ihd_any + heart_other_any + circulatory_other + ckd_any + oad_any + 
#'                       neuro_any + liver_any + immune_any + neoplasm_any +  t1dm + t2dm + unkdm)
#' #' drop esoph_stomach_duod as model wont converge with this in
#' summary(mod_como)
#' summary(mod_como2)
#' 
#' #' Specific comorbidities selected as follows
#' # res <- MASS::stepAIC(mod_como, scope = list(
#' #   lower = formula(mod_como),
#' #   upper = formula(mod_como2) ))
#' # formula(res)
#' Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) +
#'   role + white + simd + nurs_med_ahp + como_count + t2dm
#' 
#' mod_final <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male +  
#'                      role + white + simd + nurs_med_ahp + como_count + t2dm + 
#'                      strata(stratum) + cluster(hid), data = hcw)
#' summary(mod_final)
# modellist$final <- ExtractEst(mod_final)

#' hcw_nomis <- hcw %>% 
#'   select(time, hosptlsd, age10, male, hid, stratum, role, white, como_count,
#'          ihd_any,
#'          heart_other_any,
#'          circulatory_other,
#'          ckd_any,
#'          oad_any,
#'          neuro_any,
#'          liver_any,
#'          immune_any,
#'          neoplasm_any,
#'          esoph_stomach_duod,
#'          t1dm,
#'          t2dm,
#'          unkdm,
#'          simd,
#'          nurs_med_ahp) %>% 
#'   drop_na()
#' mod_como <- update(mod_occ_smpl, . ~ . + como_count, data = hcw_nomis)
#' mod_como2 <- update(mod_como, . ~ . + ihd_any + heart_other_any + circulatory_other + ckd_any + oad_any + 
#'                       neuro_any + liver_any + immune_any + neoplasm_any +  t1dm + t2dm + unkdm)
#' #' drop esoph_stomach_duod as model wont converge with this in
#' summary(mod_como)
#' summary(mod_como2)

#' Specific comorbidities selected as follows
# res <- MASS::stepAIC(mod_como, scope = list(
#   lower = formula(mod_como),
#   upper = formula(mod_como2) ))
# formula(res)
# Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) +
#   role + white + simd + nurs_med_ahp + como_count + t2dm
# 
# mod_final <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male +
#                      role + white + simd + nurs_med_ahp + como_count + t2dm +
#                      strata(stratum) + cluster(hid), data = hcw)
# summary(mod_final)
# modellist$como <- ExtractEst(mod_final)
mod_final <- update(mod_pt, data = hcw %>% filter(!is.na(stratum)))
modellist$pt <- ExtractEst(mod_pt)

## Examine just GPs
hcw %>% 
  filter(pf_any ==1, str_detect(job_family, "medical and dental")) %>% 
  count(gp, grade, hosptlsd) %>% 
  spread(hosptlsd, `n`, fill = 0L) %>% 
  mutate(prcnt = 100* `1` / (`1` + `0`))

#' examine immigration status, not statsitically significant
#' at the conventional level. Need to mention this and consider getting better data.
mod_imm <- update(mod_final, . ~ . -white  + immigration)
summary(mod_imm)
modellist$immigration <- ExtractEst(mod_imm)
rm(mod_imm)

#' Marital status, lots of missingess, and married is not signficant
marital <- update(mod_final, . ~ . + I(marital_status == "married"))
summary(marital)
rm(marital)
#' country qualified small effect only
country <- update(mod_final, . ~ . + I(country_qual == "united kingdom"))
summary(country)
rm(country)

## Check for interactions by age, sex and comorbidity
#' All p > 0.1
modageinter <- update(mod_pt, . ~ . + role:age10)
summary(modageinter)
modellist$ageroleinter <- ExtractEst(modageinter)
mod_pt_m <- update(mod_pt, . ~ . - male, data = hcw %>% filter(male == 1))
mod_pt_f <- update(mod_pt, . ~ . - male, data = hcw %>% filter(male == 0))

modsexinter <- update(mod_pt, . ~ . + role:male)
summary(modsexinter)
modellist$sexroleinter  <- ExtractEst(modsexinter)
modellist$sexrolemale   <- ExtractEst(mod_pt_m)
modellist$sexrolefemale <- ExtractEst(mod_pt_f)

modnocomo <- update(mod_pt, data = hcw %>% filter(como_count == 0))
summary(modnocomo)
modcomo <- update(mod_pt, data = hcw %>% filter(!como_count == 0))
summary(modcomo)
modcomointer <- update(mod_pt, . ~ . + como_count:role)
modellist$modnocomo <- ExtractEst(modnocomo)
modellist$modcomo <- ExtractEst(modcomo)

## Compare model fit with and without interactions
anova(modsexinter, mod_pt)
anova(modcomointer, mod_pt)
anova(modageinter, mod_pt)

### Model risk within patient-facing staff
hcw_sub <- hcw %>% 
  filter(role == "pf_any") %>% 
  mutate(role_sub = factor(role_sub, levels = c("pf_other", "pf_front", "pf_resp_oro_agp", 
                                                "pf_icu")))
mod_subrole <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) + 
                       role_sub + white + simd + como_count + t2dm + nurs_med_ahp + 
                       part_time, data = hcw_sub)
summary(mod_subrole)
modellist$subrole <- ExtractEst(mod_subrole)

modeldfs <- ConvertListModelSmriesDf(modellist)
write_csv(modeldfs, "outputs/model_staff.csv")

## Calculate expected risk by role, age, sex and comorbidity
hcw <- hcw %>% 
  mutate(como = case_when(
    como_count == 0 ~ "0",
    como_count == 1 ~ "1",
    TRUE ~ "2pls"
  ))
mod_pars <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) + 
                    role + como, data = hcw %>% filter(!is.na(stratum)))
mod_pars_inter <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) + 
                          role + como +  male:role + como:role, data = hcw %>% filter(!is.na(stratum)))
MakePredData <- function(mymod){
  new_data <- expand_grid(age10 = (18:65)/10, 
                          male = 0:1, 
                          role = c("npf", "pf_any", "undetermined"),
                          como = c("0", "1", "2pls"),
                          hosptlsd = 1L, stratum = "a", time = 31*3)
  
  pres <- predict(mod_pars_inter, newdata = new_data, type = "expected", se.fit = TRUE)
  new_data$uci <- pres$fit + 1.96*pres$se.fit
  new_data$lci <- pres$fit - 1.96*pres$se.fit
  new_data$est <- pres$fit
  
  new_data <- new_data %>% 
    mutate(Sex = factor(male, levels = c(0, 1), labels = c("Women", "Men")),
           Role = factor(role, 
                         levels = c("npf", "undetermined", "pf_any"),
                         labels = c("Non patient-facing", "Undetermined", "Patient-facing")),
           Como = factor(como, levels = c("0", "1", "2pls"),
                         labels = c("None", "One", "Two or more")))
  new_data
}
new_data_main <- MakePredData(mod_pars)
new_data_inter <- MakePredData(mod_pars_inter)

plot_abs_ribbon <- ggplot(new_data_main %>% filter(!role == "undetermined"),
                          aes(x = age10*10, y = 100*est, ymin = 100*lci, 
                              ymax = 100*uci,
                              colour = Role,
                              fill = Role)) +
  geom_line() +
  geom_ribbon(alpha = 0.1, colour = NA) +
  facet_grid(Como ~ Sex) +
  scale_y_continuous("Risk of COVID-19 hospitalisation (%)") +
  scale_x_continuous("Age (years)") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() 
plot_abs_ribbon_inter <- plot_abs_ribbon %+% (new_data_inter %>% filter(!role == "undetermined"))

pdf("outputs/absolute_risk_model.pdf")
plot_abs_ribbon
plot_abs_ribbon_inter
dev.off()

tiff("outputs/absolute_risk_model_staff.tiff", compression = "lzw", res = 300, width = 8, height = 10, unit = "in")
plot_abs_ribbon
dev.off()
