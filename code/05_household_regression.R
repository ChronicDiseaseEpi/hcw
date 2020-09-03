#05_household_regression
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
  knitr::spin("code/05_household_regression.R")
  file.copy("05_household_regression.md", "code/05_household_regression.md", overwrite = TRUE)
  if(!dir.exists("code/figure")) dir.create("code/figure")
  file.remove("05_household_regression.md", "05_household_regression.html")
}

source("code/Functions.R")

## read data ----
hh_staff <- readRDS("data/household_summary.Rds")
hholdmem <- readRDS(here("data", "hcw_household_basefile.rds"))
hholdmem <- hholdmem %>% 
  inner_join(hh_staff %>% 
               select(hid, hh_staff, role, role_sub, role2, stratum, job_family_grp, nurs_med_ahp,
                      part_time = whole_or_part_time) %>% 
               distinct())
rm(hh_staff)

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

sicsag <- sicsag %>% 
  inner_join(hholdmem %>% filter(ncov_result == "positive") %>% select(anon_id, specdate)) %>% 
  mutate(icu = if_else(covid_ic_uor_hdu == 1 , 1L, 0L),
         hdu = if_else(covid_ic_uor_hdu == 3, 1L, 0L)) %>% 
  mutate_at(vars(icu, hdu), ~ if_else( as.integer(admit_unit - specdate) <=28 , .x, 0L)) %>% 
  group_by(anon_id) %>% 
  summarise_at(vars(icu, hdu), max) %>% 
  ungroup()


hholdmem <- hholdmem %>% 
  inner_join(rapid) %>% 
  inner_join(deaths) %>% 
  inner_join(como) %>% 
  left_join(sicsag) %>% 
  mutate_at(vars(icu, hdu), ~ if_else(is.na(.x), 0L, .x))
rm(rapid, como, deaths, sicsag)

## Limit to staff included in main regression
hcw <- readRDS("data/hcw_for_cc.rds")
hholdmem <- hholdmem %>% 
  semi_join(hcw %>% select(hid))
rm(hcw)

#' Create time variable, note set to censoring where there is no hosptialisation 
#' One household member died within 28 days of testing positive, without apparently being hospitalised
hholdmem <- hholdmem %>% 
  mutate(time = as.double(specdate - as.Date("2020-03-01")),
         hosptlsd = if_else((adm28|inhosp|dead28) & ncov_result == "positive", 1L, 0L),
         time = if_else(hosptlsd == 0, max(time), time ),
         job_family_grp = factor(job_family_grp, levels = c("medical and dental",
                                                            "allied health profession", 
                                                            "administrative services",
                                                            "Other",
                                                            "nursing and midwifery",
                                                            "support services"
                                                            )))
hholdmem %>% 
  count(hosptlsd)
sum(is.na(hholdmem$time))
stem(hholdmem$time)

hholdmem %>% 
  count(dead28)

hholdmem %>% 
  count(icu)

hholdmem %>% 
  count(hdu)

hholdmem %>% 
  count(!ncov_result == "positive" & covid_cod | covid_ucod)

#' Plot by role
km1 <- survfit(Surv(time, hosptlsd) ~  role, data = hholdmem %>% filter(age >=18 & age <=65))
a <- survminer::surv_summary(km1) %>% 
  as_tibble()
a <- a %>% 
  mutate(per100000 = 100000*(1 - surv)) %>% 
  select(role, time, per100000)
astart <- a %>% 
  distinct(role)
astart <- astart %>% 
  mutate(per100000 = 0,
         time = 0)
a <- bind_rows(astart, a)
saveRDS(a, "outputs/risk_by_role_household.rds")

#' Plot by role2
km2 <- survfit(Surv(time, hosptlsd) ~  role2, data = hholdmem)
#' Plot by sub-role
km3 <- survfit(Surv(time, hosptlsd) ~  role_sub, data = hholdmem)
#' Plot by occupation
km1 <- survfit(Surv(time, hosptlsd) ~  job_family_grp, data = hholdmem)
a <- survminer::surv_summary(km1) %>% 
  as_tibble()
a <- a %>% 
  mutate(per100000 = 100000*(1 - surv)) %>% 
  select(job_family_grp, time, per100000)
astart <- a %>% 
  distinct(job_family_grp)
astart <- astart %>% 
  mutate(per100000 = 0,
         time = 0)
a <- bind_rows(astart, a)
saveRDS(a, "outputs/risk_by_occ_household.rds")

#' Age, sex
saveRDS(hholdmem, "data/hhd_for_cc.rds") 
mod1 <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + cluster(hid) + strata(stratum) + role, 
              data = hholdmem)
summary(mod1)
modellist <- list(agesex = ExtractEst(mod1))
mod1_kids <- update(mod1, data = hholdmem %>% filter(age <18))
summary(mod1_kids)
mod1_older <- update(mod1, data = hholdmem %>% filter(age >65))
summary(mod1_older)
modellist$agesex_kids <- ExtractEst(mod1_kids)
modellist$agesex_gt65 <- ExtractEst(mod1_older)


#' ethnicity and SIMD, as with staff, AIC similar/slightly lower with SIMD as continuous variable
mod2 <- update(mod1, . ~ . + white + simd)
AIC(mod2)
mod2_f <- update(mod1, . ~ . + white + factor(simd))
AIC(mod2_f)
summary(mod2_f)
rm(mod2_f)
modellist$ethnicsimd <- ExtractEst(mod2)

#' Ethnicity
mod_ethnic <- update(mod1, . ~ . -role + white)
summary(mod_ethnic)
modellist$ethnicalone <- ExtractEst(mod_ethnic)

mod3 <- update(mod2, . ~ . + como_count + heart_other_any + t2dm + 
                 unkdm + ckd_any + oad_any)
summary(mod3)
modellist$como <- ExtractEst(mod3)

#' occupation; as per staff, very similar whether or not adjust for role
mod_occ  <- update(mod3, . ~ . + job_family_grp )
mod_occ_chk <- update(mod3, . ~ . + job_family_grp -role)
summary(mod_occ)
summary(mod_occ_chk)
modellist$occupation <- ExtractEst(mod_occ)

#' Part time
hholdmem <- hholdmem %>% 
  mutate(part_time = factor(part_time, levels = c("w", "p", "general practice" )))
mod_pt <- update(mod_occ, . ~ . + part_time)
summary(mod_pt)
modellist$pt <- ExtractEst(mod_pt)

#' Occupation
mod_occ_smpl <- update(mod_pt, . ~ . - job_family_grp + nurs_med_ahp)
AIC(mod_occ)
AIC(mod_occ_smpl)

## Convert model summaries into a single dataframe
modeldfs <- ConvertListModelSmriesDf(modellist)
write_csv(modeldfs, "outputs/model_household.csv")

## Calculate expected risk by role, age, sex and comorbidity
hholdmem <- hholdmem %>% 
  mutate(como = case_when(
    como_count == 0 ~ "0",
    como_count == 1 ~ "1",
    TRUE ~ "2pls"
  ))
mod_pars <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) + 
                    role + como, data = hholdmem %>% filter(!is.na(stratum)))
mod_pars_inter <- coxph(Surv(time, hosptlsd) ~ pspline(age10) + male + strata(stratum) + 
                          role + como +  male:role + como:role, data = hholdmem %>% filter(!is.na(stratum)))
MakePredData <- function(mymod){
  new_data <- expand_grid(age10 = (0:85)/10, 
                          male = 0:1, 
                          role = c("npf", "pf_any", "undetermined"),
                          como = c("0", "1", "2pls"),
                          hosptlsd = 1L, stratum = "a", time = 31*3)
  
  pres <- predict(mod_pars_inter, newdata = new_data, type = "expected", se.fit = TRUE)
  new_data$uci <- pres$fit + 1.96*pres$se.fit
  new_data$lci <- pres$fit - 1.96*pres$se.fit
  new_data$est <- pres$fit
  
  new_data <- new_data %>% 
    mutate(Sex = factor(male, levels = c(0, 1), labels = c("Female", "Male")),
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

pdf("outputs/absolute_risk_model_hhold.pdf")
plot_abs_ribbon
plot_abs_ribbon_inter
dev.off()

tiff("outputs/absolute_risk_model_hhold.tiff", compression = "lzw", res = 300, width = 8, height = 10, unit = "in")
plot_abs_ribbon
dev.off()
