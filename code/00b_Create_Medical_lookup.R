#' # Processing of medical and dental data to define exposure groups

# Spin to count reduction in cases
#+ setup
#' Seting up document for spinning into a markdown file for regression modelling for storage on internal git repository
knitr::opts_knit$set(root.dir = here::here())

#' Need to run this function to spin (ie knit from r file) the document. Would be more elegant to store this in another script.
knitme <- function(){
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
  knitr::spin("code/00b_Create_Medical_lookup.R")
  file.copy("00b_Create_Medical_lookup.md", "code/00b_Create_Medical_lookup.md", overwrite = TRUE)
  if(!dir.exists("code/figure")) dir.create("code/figure")
  file.remove("00b_Create_Medical_lookup.md", "00b_Create_Medical_lookup.html")
}

#+ Packages ----
library(tidyverse)
library(janitor)

#+ Functions ----
ReviewFileContents <- function(x){
  ## simple function for printing variables for review
  varnames <- setdiff(names(x), "uniqueid")
  x_smry <- map(varnames, ~ count_(x[,c("uniqueid", .x)], .x, sort = TRUE))
  names(x_smry) <- varnames
  x_smry
}
MissingAll <- function(varname, mydf = medical){
  if(!varname %in% names(mydf)) return("Variable not included in data")
  medical_unq <- sum(!duplicated(mydf$uniqueid))
  notallmis <- sum(!duplicated(mydf$uniqueid)[!is.na(mydf[[varname]])])
  medical_unq - notallmis
}

CountInclusions <- function(mydf = medical, varname = "uniqueid", lbl = ""){
  # report the inclusions and exclusions and create a table with this for printing
  # at the end
  # Note uses super-assignment
  x <- sum(!duplicated(mydf[[varname]]))
  print(paste0(x, " unique staff members"))
  summarise_all_incl_excl <<- bind_rows(summarise_all_incl_excl,
                                        tibble(n = x, label = lbl)) 
  # print(summarise_all_incl_excl)
}

#+ Read in data and initial clean-up ----
swiss <- read_csv("//data/HPS/Covid19/HCW/data/SwissExtract_anon.csv", guess_max = 50000)
summarise_all_incl_excl <- tibble(n = nrow(swiss), label = "All Swiss")

# Read in supporting information
abbrev_regst <- read_delim("reference_files/abbreviations_registration.txt", delim = "|")
spec2gmclkp <- read_csv("reference_files/med_spec_lkp.csv") 
pfac <- read_tsv("reference_files/medical_patient_facing.txt")
grade <- read_csv("reference_files/grade_lkp.csv") %>% 
  select(grade, grade_r) %>% 
  distinct()
rslv_grade_manl <- read_csv("reference_files/resolve_grade_mixups.csv") 
multi_spec <- read_csv("reference_files/review_multi_spec.csv") 
country_region <- read_tsv("reference_files/country_geo_code.txt")
gmccountryofqualification <- read_csv("reference_files/gmccountryofqualification.csv")

# merge country data to a single object
country_region <- gmccountryofqualification %>% 
  inner_join(country_region %>% select(alpha_3, region, sub_region)) %>% 
  mutate(sub_region = if_else(alpha_3 == "GBR", "United Kingdom", sub_region)) 
rm(gmccountryofqualification)

# Rename data
names(swiss) <- names(swiss) %>% 
  str_to_lower() %>% 
  str_trim() %>% 
  str_replace_all("\\.{1,}", "_")

# remove ampersands and change to lower case for all character data and lookup files
for(i in c("swiss", "spec2gmclkp", "pfac", "grade", "multi_spec",
           "rslv_grade_manl", "country_region", "abbrev_regst")){
  assign(i, get(i) %>% 
           mutate_if(is.character, ~ str_replace_all(.x, "\\&", "and") %>% 
                       str_to_lower()))
}

# Drop a range of variables which are predominantly NA
swiss <- swiss[, setdiff(names(swiss), c("professionalbody1", "professionalbody2", "professionalbody3", 
                                         "professionalbody4", "professionalbody5", "professionalbody6", 
                                         "body", "specialistarea", "consultantnmahpspecialty", "consultantnmahpindicator", "initials"))]

#' There are `r sum(is.na(swiss$jobfamily))` missing records for AFC jobfamily.

#+ Review registration organisations ----
#' # Review registration
swiss %>% 
  count(registrationbody) %>% 
  left_join(abbrev_regst %>% rename(registrationbody = abbrev)) %>% 
  knitr::kable()

#' # Select staff based on registration OR job family as medical and dental
medical <- swiss %>% 
  filter(registrationbody %in% c("gmc", "gdc") | jobfamily == "medical and dental")
CountInclusions(lbl = "Selection of medical and dental from SWISS")

#' There are `r nrow(medical)` records and `r sum(!duplicated(medical$uniqueid))` staff members.
medical_mis <- medical %>%
  mutate_at(vars(grade, medicalgrade), function(x) if_else(is.na(x) | x == "Other", NA_character_, x))

#+ Add Country of qualification ---- 
#' # Allocate country of qualification to a WHO region
#' Note that there are no duplicates
medical_country <- medical %>% 
  select(uniqueid, gmccountryofqualification) %>% 
  inner_join(country_region) %>% 
  distinct(uniqueid, sub_region)
medical_no_country <- medical %>% 
  distinct(uniqueid) %>% 
  anti_join(medical_country) %>% 
  mutate(sub_region = "no country recorded")
medical_country <- bind_rows(medical_country,
                             medical_no_country) %>% 
  arrange(uniqueid)
rm(medical_no_country)

#+ Review grade and medicalgrade ---- 
#' # Review grade of staff using the general grade and the medical grade variables
#' 
#' There are `r sum(is.na(medical_mis$grade))` rows with missing grade and
#' `r sum(is.na(medical_mis$medicalgrade))` rows with missing medical grade.
#' `r MissingAll("grade", medical_mis)` staff have no grade information and 
#' `r MissingAll("medicalgrade", medical_mis)` staff have no medical grade information.
rm(medical_mis)

medical_grade <- medical %>% 
  distinct(uniqueid, grade, medicalgrade) 
medical_grade <- medical_grade %>% 
  left_join(grade %>% select(grade, grade_r)) 

#' Compare grade and medical grade variables manualy.
#' Treat as identical automatically where are identical, where one is just an "other" variable
#' or where both grade and medicalgrade are GP or dental.
cmpr_medical_grade <- medical_grade %>%
  filter(grade_r != medicalgrade, 
         !grade_r == "other", 
         !medicalgrade == "other",
         ! (str_detect(grade_r %>% str_to_lower(), "\\bdent") &
              str_detect(medicalgrade %>% str_to_lower(),"\\bdent")),
         ! (grade_r == "gp" & medicalgrade == "salaried gp")) %>%
  count(grade, grade_r, medicalgrade) 
# write_csv(cmpr_medical_grade, "")

#' Note that all staff with with missing grade also having missing medicalgrade
medical %>% 
  group_by(uniqueid) %>% 
  summarise_at(vars(grade, medicalgrade), function(x) if_else(all(is.na(x)),
                                                              "missing",
                                                              "not missing")) %>% 
  ungroup() %>% 
  count(grade, medicalgrade) %>% 
  knitr::kable()

rslv_grade_auto <- medical_grade %>%
  filter(is.na(medicalgrade) | 
           grade_r == medicalgrade| 
           grade_r == "Other"| 
           medicalgrade == "Other"|
           (str_detect(grade_r %>% str_to_lower(), "\\bdent") &
              str_detect(medicalgrade %>% str_to_lower(),"\\bdent"))|
           (grade_r == "gp" & medicalgrade == "salaried gp")) %>% 
  mutate(grade_slct = case_when(
    grade_r == "Other" ~ medicalgrade,
    medicalgrade == "Other" ~ grade_r,
    TRUE ~ grade_r)
  ) %>% 
  distinct(grade, grade_r, medicalgrade, grade_slct)

rslv_grade_manl <- rslv_grade_manl %>% 
  select(-n) %>% 
  mutate(grade_slct = case_when(
    slct == "drop" ~ "inconsistent",
    slct == "medicalgrade" ~ medicalgrade,
    slct == "grade_r" ~ grade_r
  )) %>% 
  select(-slct) %>% 
  distinct()

rslv_grade <- bind_rows(rslv_grade_manl,
                        rslv_grade_auto)

## Expected number of missing grades
sum(medical$grade %>% is.na())
medical %>% 
  anti_join(rslv_grade %>% select(grade)) %>% 
  count(grade)

medical_grade <- medical_grade %>% 
  left_join(rslv_grade) %>% 
  select(-grade_r) %>% 
  rename(grade_r = grade_slct)

# Simplify grades and medical grades
medical_grade <- medical_grade %>% 
  mutate(grade_s = case_when(
    grade_r %in% c("clinical fellow",
                   "dental core training - grade 1", 
                   "dental core training - grade 2",
                   "fixed term specialist training appointment", 
                   "foundation house officer year 1", 
                   "foundation house officer year 2", 
                   "foundation year 1", 
                   "general practice specialty training",
                   "locum appointment service",
                   "locum appointment training",
                   "senior registrar", 
                   "sessional gp out of hours",
                   "specialist registrar",
                   "specialty registrar",
                   "specialty registrar (core training)",
                   "sho") ~ "training_grade",
    grade_r %in% c("consultant", "assistant clinical director", "clinical director", "clinical lead") ~ "consultant",
    ## non training grade gps (see reference file for original designations)
    grade_r %in% c("gp", "salaried gp") ~ "gp",
    grade_r %in% c("specialty doctor", "associate specialist", "staff grade") ~ "specialty_assoc_spec",
    is.na(grade_r) | grade_r == "other" ~ "not recorded or other", 
    grade_r %in% c("dental officer", "senior dental officer", "salaried gdp", "dental") ~ "dental",
    grade_r %in% c("medical director", "medical director") ~ "medical director",
    TRUE ~ "not recorded or other"))

#' ## Summary of grades at doctor and row level
medical_grade %>% 
  group_by(grade_s, grade_r) %>% 
  summarise(rows = length(uniqueid),
            staff = sum(!duplicated(uniqueid))) %>% 
  knitr::kable()

#' ## Review clashes within new grade variable
medical_grade_clash <- medical_grade %>% 
  distinct(uniqueid, grade_s) %>% 
  filter(!is.na(grade_s), !grade_s == "not recorded or other") %>% 
  arrange(grade_s) %>% 
  group_by(uniqueid) %>% 
  mutate(myseq = seq_along(uniqueid),
         mylen = length(uniqueid)) %>% 
  ungroup() %>% 
  filter(mylen >=2) 

#' Some apparent grade clashes make sense eg, a medical director who is a consultant, but others do not.
#' Given the small numbers (`r sum(!duplicated(medical_grade_clash$uniqueid))`) it 
#' makes most sense to drop all of these staff,
#' even those which appear to make sense. 
#' Particularly as the classification of dual-role staff will be difficult.
a <- medical_grade_clash %>% 
  spread(myseq, grade_s, fill = "") %>% 
  count(`1`, `2`, `3`)
bind_rows(a, tibble(`1` = "Total", n = sum(a$n)))

#' The number of medical staff before excluding those with clashing grades
medical <- medical %>% 
  anti_join(medical_grade_clash)
CountInclusions(lbl = "Drop staff where the grades clash")
#' The number of medical staff after excluding tnhose with clashing grades.
medical_grade <- medical_grade %>% 
  anti_join(medical_grade_clash %>% select(uniqueid))

#' ## Final grade allocation
#' 
#' This allows us to reduce the medical grade to single row per doctor
#' Slightly higher missingness now than at the onset because we excluded staff where there was a mismatch between recorded grade on grade
#' and recorded on medicalgrade
medical_grade <- medical_grade %>%
  select(uniqueid, grade_r, grade_s) %>% 
  mutate(grade_s = if_else(grade_s == "not recorded or other", NA_character_, grade_s)) %>% 
  group_by(uniqueid) %>% 
  summarise_at(vars(grade_s, grade_r), ~ paste(.x %>% unique() %>% na.omit() %>% sort(), collapse = "|")) %>% 
  ungroup()

# Append medical grade to medical
medical <- medical %>% 
  select(-grade, -medicalgrade) %>% 
  inner_join(medical_grade %>% select(uniqueid, grade_s))

#+ Identify dental ----
#' # Dental staff
#' Define dental staff as belonging to a dental grade or belonging to one fo the selected specialties.
#' Based on advice from Consultant in Dental Public Health on accuracy of SWISS specialty data 
#' for dentistry do not try and split dental further.
#' For similar reasons do not include dental public health, which is implausibly common in the non-patient
#'  facing group but leave as undetermined.
dental <-  medical %>% 
  mutate(dental_spec = if_else(specialty %in% c("community dentistry",
                                                "oral surgery",
                                                "special care dentistry",
                                                "endodontics"),
                               "dental specialty", "not dental specialty"),
         dental_grade = if_else(grade_s == "dental", "dental grade", "non dental grade"))
dental_smry <- dental %>% 
  group_by(dental_spec, dental_grade) %>% 
  summarise(records = length(uniqueid),
            staff = sum(!duplicated(uniqueid))) %>% 
  ungroup()
dental_smry %>% 
  knitr::kable()

dental <- dental %>% 
  filter(dental_spec == "dental specialty" |
           dental_grade == "dental grade") %>% 
  pull(uniqueid) %>% 
  unique()
#' There are `r length(dental)` dentists.

#+ Identify paediatrics ----
#' # Paediatric staff
# Note there are no paediatric specialties in secondspecialty variable
paeds <- medical %>% 
  mutate(paed_spec = if_else(specialty %in% c("community child health",
                                              "child and adolescent psychiatry",
                                              "paediatric and perinatal pathology",
                                              "paediatric cardiology",
                                              "paediatric dentistry",
                                              "paediatric surgery",
                                              "paediatrics") |
                               subspecialty %in% "neonatal medicine",
                             "paed specialty", "not paed specialty"),
         paed_age = case_when(
           ageofpopulationtreated %in% c("0 - 5", "under 18") ~ "paed age",
           ageofpopulationtreated %in% c("all", "over 18", "over 65s") ~ "not paed age",
           TRUE ~ NA_character_))

paeds_smry <- paeds %>% 
  group_by(paed_spec, paed_age) %>% 
  summarise(records = length(uniqueid),
            staff = sum(!duplicated(uniqueid))) %>% 
  ungroup()

#' Paediatrics classification
#' Classify as adult only if neither
#' specialty data nor age ofpopulation indicates children.
#' Classify as paediatrics only if specialty indicates paediatrics (regardless of age of population).
#' Following table shows classification of specialty by age of population and specialty
paeds_smry %>% 
  select(-records) %>% 
  spread(paed_spec, staff) %>% 
  mutate(prcnt = 100 * `paed specialty`/(`not paed specialty` + `paed specialty`))

medical_adult <- medical %>% 
  filter(!specialty %in% c("community child health",
                           "child and adolescent psychiatry",
                           "paediatric and perinatal pathology",
                           "paediatric cardiology",
                           "paediatric dentistry",
                           "paediatric surgery",
                           "paediatrics"),
         !ageofpopulationtreated %in% c("0 - 5", "under 18")) %>% 
  distinct(uniqueid) 

#' There are `r sum(!duplicated(medical_adult$uniqueid))` doctors working in non-paediatric roles.
medical_paed <- medical %>% 
  filter(specialty %in% c("community child health",
                          "child and adolescent psychiatry",
                          "paediatric and perinatal pathology",
                          "paediatric cardiology",
                          "paediatric dentistry",
                          "paediatric surgery",
                          "paediatrics")) %>% 
  distinct(uniqueid)
#' There are `r nrow(medical_paed)` doctors working in paediatric specialties.
#+ specialty data review ----
#' # Review specialty data
#' 
#' There are `r MissingAll("specialty", medical)` and `r MissingAll("secondspecialty", medical)` staff 
#' with missing specialty and secondspecialty variables respectively.
#' 
#' ## GPs with apparently discrepant specialty results
#' There are large numbers of salaried GPs ostensibly working in other specialties; these 
#' are most likely
#' GPs doing specialty-interest clinics.
medical %>% 
  filter(str_detect(grade_s, "gp")) %>% 
  inner_join(medical %>% select(uniqueid, specialty)) %>% 
  count(specialty, grade_s) %>% 
  spread(grade_s, n, fill = "") %>% 
  as.data.frame()

#' ## Registration bodies for first specialty
#' Registration across specialties looks largely plausible, although in some cases there must be errors in the data as
#' some of the speciality/registration body combinations are implausible. Also, the number of dental public health doctors seems improbably large.
medical %>% 
  count(specialty, registrationbody) %>% 
  spread(registrationbody, n, fill = "") %>% 
  knitr::kable()
# Registration bodies for second specialty
medical %>% 
  count(secondspecialty, registrationbody) %>% 
  spread(registrationbody, n, fill = "") %>% 
  knitr::kable()

#' ## Specialty and GMC specialty
#' The database is also populated by the GMC specialty. This allows us a check on the accuracy of the SWISS specialty variables.
#' 
#' Count number of staff who have a GMC record. This varies by medical grade, being highest for consultant staff.
gmc_spec_recorded <- medical %>% 
  mutate(gotgmcspec = if_else(  !is.na(gmcspecialty1)|
                                  !is.na(gmcspecialty2)|
                                  !is.na(gmcspecialty3)|
                                  !is.na(gmcspecialty4)|
                                  !is.na(gmcspecialty5)|
                                  !is.na(gmcspecialty6)|
                                  !is.na(gmcspecialty7), "gmc_spec_recorded", "no_gmc_spec")) %>% 
  arrange(gotgmcspec) %>% 
  select(grade_s, gotgmcspec, uniqueid) %>% 
  distinct(uniqueid, .keep_all = TRUE) %>% 
  count(grade_s, gotgmcspec) %>% 
  spread(gotgmcspec, n, fill = 0L) %>% 
  mutate(prcnt_gmc_spec =  gmc_spec_recorded /( gmc_spec_recorded  + no_gmc_spec))
gmc_spec_recorded

# rename GMC specialty to same as specialty in SWISS 
medical_spec_compare <- medical %>% 
  select(uniqueid, gmcspecialty1, gmcspecialty2, gmcspecialty3, gmcspecialty4, 
         gmcspecialty5, gmcspecialty6, gmcspecialty7) %>% 
  gather("orderdrop", "gmcspecialty", -uniqueid, na.rm = TRUE) %>% 
  left_join(spec2gmclkp %>% select(specialty, gmcspecialty, match_type)) %>% 
  select(-gmcspecialty, -orderdrop) %>% 
  rename(gmcspecialty = specialty)

#' Examine frequency with which first specialties have a matching specialty in the GMC specialty
#' Take any match within individuals, ignoring GMC order.
medical_spec_compare_smry <- medical %>% 
  select(uniqueid, specialty) %>% 
  filter(!is.na(specialty)) %>% 
  inner_join(medical_spec_compare) %>% 
  group_by(uniqueid) %>% 
  summarise(specialty_in       = any(specialty %in% gmcspecialty),
            specialty_in_exact = any( (specialty %in% gmcspecialty) & match_type == "exact")) %>% 
  ungroup()

#' ## Of staff with any GMC specialty, the following had a match between a GMC specialty 
#' and the specialty variable
#' 
Quicksmry <- function(x) {
  res <- c(sum(x), 100*mean(x))
  res <- round(res, 1)
  paste0(res[1], " (", res[2], "%)")
}

#' There were `r Quicksmry(medical_spec_compare_smry$specialty_in)` staff where the GMC specialty matched approximately and 
#' `r Quicksmry(medical_spec_compare_smry$specialty_in_exact)` where the GMC specialty matched exactly.
not_in_spec <- medical_spec_compare_smry %>% 
  filter(!specialty_in) %>% 
  distinct(uniqueid)
rv_mismatch_spec <- medical %>% 
  semi_join(not_in_spec) %>% 
  select(uniqueid, specialty, starts_with("gmcspecialty")) %>% 
  count(specialty, gmcspecialty1, gmcspecialty2, sort = TRUE)

#' The following is a random sample for specialties which do not have a match within GMC.
knitr::kable(rv_mismatch_spec %>%  sample_n(20))

#' ## Examine second specialties which do not have a match within GMC
#' Note that all of the mismatches on second specialty do have a match for the first specialty
medical_spec_compare_smry2nd <- medical %>% 
  select(uniqueid, secondspecialty) %>% 
  filter(!is.na(secondspecialty)) %>% 
  inner_join(medical_spec_compare) %>% 
  group_by(uniqueid) %>% 
  summarise(specialty_in       = any(secondspecialty %in% gmcspecialty),
            specialty_in_exact = any( (secondspecialty %in% gmcspecialty) & match_type == "exact")) %>% 
  ungroup()

## Of staff with any GMC specialty, the following had a match between a GMC specialty and the *second* specialty variable
#' For the second specialty there were `r Quicksmry(medical_spec_compare_smry2nd$specialty_in)` staff where the GMC specialty matched approximately and 
#' `r Quicksmry(medical_spec_compare_smry2nd$specialty_in_exact)` where the GMC specialty matched exactly.
not_in_spec <- medical_spec_compare_smry2nd %>% 
  filter(!specialty_in) %>% 
  distinct(uniqueid)
rv_mismatch_spec <- medical %>% 
  semi_join(not_in_spec) %>% 
  select(uniqueid, specialty, secondspecialty, starts_with("gmcspecialty")) %>% 
  count(specialty, secondspecialty, gmcspecialty1, gmcspecialty2, sort = TRUE)
knitr::kable(rv_mismatch_spec)

#' ## Comparison of specialty data within SWISS database
#' In the following we examine whether multiple specialties (either multiple rows or specialty and secondspecialty) make sense in
#' terms of plausible combinations.
#' There can be multiple specialties either because of duplicate rows, or because the secondspecialty variable is completed.
medical_lng <- medical %>% 
  select(uniqueid, specialty, secondspecialty) %>% 
  gather("spec_ignore", "specialty", - uniqueid, na.rm = TRUE) %>% 
  mutate(specialty = str_to_lower(specialty)) %>% 
  select(-spec_ignore) %>% 
  filter(!is.na(specialty)) %>% 
  distinct(uniqueid, specialty)

#' Count number of staff with different numbers of specialties. 
#' Only a small proportion of the staff has more than one specialty recorded.
medical_lng %>% 
  group_by(uniqueid) %>% 
  count() %>% 
  ungroup() %>% 
  count(n) %>% 
  arrange(desc(n)) %>%
  mutate(cumsum = cumsum(nn),
         cumprop = cumsum/max(cumsum)) %>% 
  arrange(n)

#' ## Check for incongruent combinations
medical_chk <- medical_lng %>% 
  arrange(specialty) %>% 
  group_by(uniqueid) %>% 
  mutate(n_spec = seq_along(uniqueid)) %>% 
  ungroup() %>% 
  spread(n_spec, specialty, fill = "") 

medical_chk <- medical_chk %>% 
  left_join(medical_grade %>% select(uniqueid, grade_s)) 
names(medical_chk) <- c("uniqueid", "spec1", "spec2", "spec3", "spec4", "grades")
#' Wrote the above file to a csv and then used this to judge the congruence
#' of specialties and recorded this in "reference_files/review_multi_spec.csv".

#' ## Review of plausible specialties
#' For each member of staff, I reviewed whether different specialties found
#' within individuals were plausible and recorded this in
#' “reference_files/review_multi_spec.csv”. Where is plausible, left this as
#' blank on the csv file. Where is a problem, identified this as such. Where is
#' a medical director will want to exclude in any case, so summarising this
#' separately below. Where the grade can help interpret multiple specialties (eg
#' salaried GP or training grade) this is also included in the following
#' summary.
#' 
multi_spec <- multi_spec %>% 
  mutate(outcome = case_when(
    is.na(ptnl_prblm) ~ "1. Appears plausible combination",
    ptnl_prblm %in% c("poss_gp_special_interest", "salaried_gp") ~ "2. Probably a GP with an interest, include GP plus one other plausible specialty",
    str_detect(grades, "medical director") ~ "3. Medical director, difficult to code specialty, will ignore in any case in analyses",
    TRUE ~ "4. Implausible specialties"),
    grades_relevant = if_else(outcome == "4. Implausible specialties", grades, "irrelevant")
  )
multi_spec %>% 
  group_by(outcome, grades_relevant) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()

#' ## Drop medical director posts, salaried GPs (not GPs in training posts) and implausible specialty combinations based on SWISS
#' 
# First need to identify which 265 doctors the combinations apply to
medical_chked <- medical_chk  %>% 
  filter(!spec2 == "") %>% 
  inner_join(multi_spec %>% 
               mutate_at(vars(spec1:spec4), ~ if_else(is.na(.x), "", .x)) %>% 
               select(-grades_relevant)) %>% 
  select(uniqueid, outcome)
medical_drop_spec <- medical_chked %>% 
  filter(!outcome == "1. Appears plausible combination")

#' Dropped the following on the basis of incongruent specialties.
medical_drop_spec %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!duplicated(uniqueid)))
sum(!duplicated(medical_drop_spec$uniqueid))

# Drop from main medical dataset, and medical dataset rearranged to long format
#' Prior to this have  `r sum(!duplicated(medical$uniqueid))` staff members.
gpdrop <- sum(!duplicated(medical$uniqueid[medical$grade_s == "gp"]))
mddrop <- sum(!duplicated(medical$uniqueid[medical$grade_s == "medical director"]))
medical <- medical %>% 
  anti_join(medical_drop_spec) 
CountInclusions(medical, lbl = "Drop implausible specialty combinations as per lookup file")
medical <- medical %>% 
  filter(!grade_s %in% "medical director")
CountInclusions(medical, lbl = "Drop medical directors")
medical <- medical %>%
  filter(!grade_s %in% "gp")
CountInclusions(medical, lbl = "Drop GPs")

#' After excluding this  leaves `r sum(!duplicated(medical$uniqueid))` staff members. Most of this fall is due to
#' the exclusion of GPs in non-training roles (`r gpdrop`) with
#' a much smaller drop due to the exclusion of medical directors (`r mddrop`). 
#' 
medical_lng <- medical_lng %>% 
  semi_join(medical %>% select(uniqueid))

rm(abbrev_regst, gmc_spec_recorded, medical_chk, medical_chked, 
   medical_drop_spec, medical_spec_compare, medical_spec_compare_smry, 
   medical_spec_compare_smry2nd, multi_spec, 
   not_in_spec, ReviewFileContents, rv_mismatch_spec, spec2gmclkp, swiss)


#' ## Examine number of specialties by grade 
#' 
#' Where the grade is not recorded the specialty is also commonly not recorded.
#' As per the analysis plan those without specialties are not included among the
#' patient facing exposure group. We will need to do a sensitivity analysis where we exclude
#' those without grade recorded in case this is an overall marker of poor data quality.
medical_spec_grade <- medical %>% 
  select(uniqueid, grade_s) %>% 
  left_join(medical_lng %>% 
              group_by(uniqueid) %>% 
              summarise(n_spec = length(specialty),
                        specialties = specialty %>% unique() %>% sort() %>% paste(collapse = "|")) %>% 
              ungroup()) %>% 
  mutate(n_spec = if_else(n_spec %>% is.na(), 0L, n_spec),
         specialties = if_else(is.na(specialties), "", specialties))

#' ### Number of rows with none, or one more specialties recorded
medical_spec_grade %>% 
  count(grade_s, n_spec) %>% 
  spread(n_spec, n, fill = 0L) %>% 
  knitr::kable()

count0spec_grade1 <- medical_spec_grade %>% 
  count(grade_s, n_spec) %>% 
  group_by(grade_s) %>% 
  mutate(tot = sum(n),
         prcnt = 100*n/tot) %>% 
  ungroup() %>% 
  filter(n_spec ==0)
count0spec_grade2 <- medical_spec_grade %>% 
  anti_join(count0spec_grade1 %>% select(grade_s)) %>% 
  group_by(grade_s) %>%
  summarise(n = 0L,
            n_spec = 0L,
            tot = length(specialties),
            prcnt = 0) %>% 
  ungroup()

#' ### Proportion of rows with no specialties recorded
bind_rows(count0spec_grade1,
          count0spec_grade2) %>% 
  arrange(grade_s) %>% 
  select(-n_spec) %>% 
  knitr::kable()

rm(count0spec_grade1,
   count0spec_grade2)

# Drop medical and dental where there is no specialty data
medical <- medical %>% 
  semi_join(medical_lng %>% select(uniqueid))
CountInclusions(medical, lbl = "Drop where there is no specialty data")

#+ apply exposure group definitions ---- 
#' # Apply definitions to create categories as per protocol
# Patient facing definitions
pfac <- pfac %>% 
  mutate(specialty = str_to_lower(specialty))
pfac_defines <- map(pfac %>% select(-specialty), ~ pfac$specialty[as.logical(.x)])
pfac_defines_apply <- map(pfac_defines, function(spec_list){
  if_else(medical_lng$specialty %in% spec_list, 1L, 0L)})
records_exposure <- map_int(pfac_defines_apply, sum) 
pfac_defines_tbl <- bind_cols(pfac_defines_apply)
medical_lng <- bind_cols(medical_lng, pfac_defines_tbl)

#+ Summary specialty counts within each exposure group
#' # Summary of number of doctors within each specialty for each exposure group. 
#' Note that doctors can appear multiple times for this table, but not in the final definitions 
#' which are mutually exclusive (npf vs pf and subgroups within pf).
medical_lng_smry <- medical_lng %>% 
  group_by(specialty, npf, pf_any, pf_front, pf_resp_oro_agp, pf_icu) %>% 
  summarise(n_doctors = sum(!duplicated(uniqueid))) %>% 
  ungroup()
medical_lng_smry[ , c("npf", "pf_any", "pf_front", "pf_resp_oro_agp", "pf_icu")]  <- 
  map(medical_lng_smry[ , c("npf", "pf_any", "pf_front", "pf_resp_oro_agp", "pf_icu")], function(x){
    if_else(x == 1, medical_lng_smry$n_doctors %>% as.character(), "") 
  })

#' ### Non patient facing
medical_lng_smry %>% 
  select(-n_doctors) %>% 
  filter(!npf == "") %>% 
  select(specialty, npf ) %>% 
  knitr::kable()

#' ### Patient facing, no further sub-type
medical_lng_smry %>% 
  select(-n_doctors) %>% 
  filter(!pf_any == "", pf_front == "", pf_resp_oro_agp =="", pf_icu =="") %>% 
  select(specialty, pf_any ) %>% 
  knitr::kable()

#' ### Patient facing sub-types
medical_lng_smry %>% 
  select(-n_doctors) %>% 
  filter(!(pf_front == "" & pf_resp_oro_agp =="" & pf_icu =="")) %>% 
  select(specialty, pf_any, pf_front, pf_resp_oro_agp, pf_icu ) %>% 
  arrange(pf_front, pf_resp_oro_agp, pf_icu ) %>% 
  knitr::kable()

#' ### Indeterminate specialties
medical_lng_smry %>% 
  mutate(indeterminate = n_doctors) %>% 
  select(-n_doctors) %>% 
  filter(pf_any == "" , npf == "") %>% 
  select(specialty, indeterminate) %>% 
  knitr::kable()

# Unique definitions for non patient facing, do not include in non-patient facing if has any patient facing role
medical_roles_pf <- medical_lng %>% 
  group_by(uniqueid) %>% 
  summarise_at(vars(pf_any, pf_front, pf_resp_oro_agp, pf_icu), ~ if_else(any(.x), 1L, 0L)) %>% 
  ungroup()
medical_roles_non_pf <- medical_lng %>% 
  group_by(uniqueid) %>% 
  summarise(npf = if_else(any(npf) & all(!pf_any), 1L, 0L )) %>% 
  ungroup()
medical_roles <- medical_roles_non_pf %>% 
  inner_join(medical_roles_pf)

#' ### Number of records and number of staff for each exposure group
staff_exposure <- map_int(medical_roles %>% select(-uniqueid),  sum) 
a <- list(Records = records_exposure,
          Staff = staff_exposure) 
do.call(cbind, a) %>% 
  knitr::kable()

#' ### Number of staff for each exposure group
medical_roles %>% 
  count(npf, pf_any, pf_front, pf_resp_oro_agp, pf_icu) %>% 
  knitr::kable()

#' Note very few in front door are in agp or pf_icu roles as well as front door;
#' for these five seems feasible that is trained in anaesthetics and emergency medicine or general medicine.
#' For final analysis classify these as pf_icu
medical %>% 
  select(uniqueid, specialty, secondspecialty, gmcspecialty1, gmcspecialty2, gmcspecialty3, 
         gmcspecialty4, gmcspecialty5, gmcspecialty6, gmcspecialty7) %>% 
  gather("specialties_lbl", "value", -uniqueid, na.rm = TRUE) %>% 
  semi_join(medical_roles %>% 
              filter(pf_front ==1 & pf_icu  ==1) %>% 
              select(uniqueid)) %>% 
  arrange(uniqueid) %>% 
  select(-specialties_lbl) %>% 
  distinct() %>% 
  group_by(uniqueid) %>% 
  mutate(candrop = seq_along(uniqueid)) %>% 
  ungroup() %>% 
  spread(candrop, value, fill = "")  %>% 
  knitr::kable()

#' Small number of front door are also in resp/oropharnygeal/agp roles, all are respiratory medicine also listed as general medicine
#' For final analysis classify these as pf_resp_oro_agp
medical %>% 
  select(uniqueid, specialty, secondspecialty, gmcspecialty1, gmcspecialty2, gmcspecialty3, gmcspecialty4, gmcspecialty5, gmcspecialty6, gmcspecialty7) %>% 
  gather("specialties_lbl", "value", -uniqueid, na.rm = TRUE) %>% 
  semi_join(medical_roles %>% 
              filter(pf_front ==1 & pf_resp_oro_agp  ==1) %>% 
              select(uniqueid)) %>% 
  select(-specialties_lbl) %>% 
  distinct() %>% 
  group_by(uniqueid) %>% 
  mutate(candrop = seq_along(uniqueid)) %>% 
  ungroup() %>% 
  spread(candrop, value, fill = "")  %>% 
  knitr::kable()

medical_roles <- medical_roles %>%
  mutate(pf_front = if_else(pf_icu == 1 | pf_resp_oro_agp ==1, 0L, pf_front)) 

#' Final allocation of medical staff by role is as follows
medical_roles %>% 
  count(npf, pf_any, pf_front, pf_resp_oro_agp, pf_icu)  %>% 
  knitr::kable()

# Assign staff into medical, dental and grade for reporting
medical_roles_grade <- medical_roles %>% 
  inner_join(medical %>% 
               distinct(uniqueid, grade_s))

#+ Summary exposure group by Grade and Country of qualification ----
#' Summarise exposure group by medical grade and country of GMC registration
#' 
#'  non patient facing
medical_roles_grade %>% 
  filter(npf ==1) %>% 
  count(grade_s) %>% 
  knitr::kable()
#' any patient facing
medical_roles_grade %>% 
  filter(pf_any ==1) %>% 
  count(grade_s) %>% 
  knitr::kable()
#' patient facing subtypes
med_rol_grd_smry <- medical_roles_grade %>% 
  filter(pf_any ==1) %>% 
  count(pf_front, pf_resp_oro_agp, pf_icu, grade_s) 
med_rol_grd_smry %>% 
  knitr::kable()
map(c("pf_front", "pf_resp_oro_agp", "pf_icu"), function(mycol){
  med_rol_grd_smry[med_rol_grd_smry[[mycol]] ==1, c(mycol,"grade_s", "n")]  %>% 
    knitr::kable()
})


medical_roles_grade <- medical_roles_grade %>% 
  inner_join(medical_country)

SmryFunction <- function(arg, mydf = medical_roles_grade){
  mydf %>% 
    count(sub_region, !!enquo(arg))  %>% 
    spread(!!enquo(arg), n, fill = 0L) %>% 
    mutate(prp = 100 * `1` / (`1` + `0`)) %>% 
    knitr::kable()
}
#'  non patient facing
SmryFunction(npf)
#' any patient facing
SmryFunction(pf_any)
#' Front door
SmryFunction(pf_front, medical_roles_grade %>% filter(pf_any ==1) )
#' AGP
SmryFunction(pf_resp_oro_agp, medical_roles_grade %>% filter(pf_any ==1) )
#' ICU
SmryFunction(pf_icu, medical_roles_grade %>% filter(pf_any ==1) )

#' # Create summary dataframe with variables required
medical_final <- medical_roles_grade %>% 
  mutate(undetermined = if_else(npf ==0 & pf_any ==0, 1L, 0L),
         dentl = if_else(uniqueid %in% dental, "dental", "non dental"),
         adult = if_else(uniqueid %in% medical_adult$uniqueid, "adult", "non adult"),
         paeds = if_else(uniqueid %in% medical_paed$uniqueid, "paeds", "non paeds")) %>% 
  rename(country_qual = sub_region, grade = grade_s)
CountInclusions(medical_final, lbl = "Final medical dataset")
saveRDS(medical_final, "data/med_lookup_pre_id.Rds")

summarise_all_incl_excl %>% 
  knitr::kable()
