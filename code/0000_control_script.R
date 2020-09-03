#00000
library(here)

scripts <- list(
  ## AFC role, occ etc
  "00a_Create_AfC_Lookup.R",
  ## Medical role, occ etc
  "00b_Create_Medical_lookup.R",
  ## Add anon_id and hid to data
  "00c_uniqueid_anonid_create_lkp.R",
  ## Process fields common to medical and afc in swiss
  "00d_common2afcmed.R",
  ## Read in data to exclude GPCD staff from swiss analysis
  "00e_identify_gpcd_in_swiss.R",
  ## Add age, sex, ethnicity etc
  "01_Create_Basefile.R",
  ## Add comorbidity
  "02_define_comorbidities.R",
  ## Create summary for household level data for subsequent analysis and for table
  "03_summarise_household.R",
  ## Run regressions to examine effect on outcome in staff
  "04_staff_regression.R",
  ## As above but with households
  "05_household_regression.R",
  ## Do case-control comparison with general population
  "06a_compare_case_control.R",
  ## Compare population rates to HCWs
  "06a_compare_population.R")

lapply(scripts, function(x){
  print(x)
  source(here("code", x))
  rm(list = ls())
})

scripts_knit <- scripts[c(1, 2, 6, 9, 10, 12, 13)]

## need to check this function for the copying and deleting, might need to use here
knitme <- function(script_choose){
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
  knitr::spin(here::here("code", script_choose))
  file.copy(paste0(str_sub(script_choose, 1, -1), "md"),
            paste0("code/",str_sub(script_choose, 1, -1), "md"), overwrite = TRUE)
  if(!dir.exists("code/figure")) dir.create("code/figure")
  file.remove(paste0(str_sub(script_choose, 1, -1), "md"),
              paste0(str_sub(script_choose, 1, -1), "html"))
}
lapply(scripts_knit, knitme)
