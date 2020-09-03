#00d_common2afcmed
library(tidyverse)
library(here)
library(janitor)

# Functions
CountInclusions <- function(mydf = swiss_anon, varname = "unique_id", lbl = ""){
  # report the inclusions and exclusions and create a table with this for printing
  # at the end
  # Note uses super-assignment
  x <- sum(!duplicated(mydf[[varname]]))
  print(paste0(x, " unique staff members"))
  summarise_all_incl_excl <<- bind_rows(summarise_all_incl_excl,
                                        tibble(n = x, label = lbl)) 
  # print(summarise_all_incl_excl)
}

# Read in swiss anon data, clean variable names and data ----
med <- readRDS(here("data", "med_lookup.rds"))
afc <- readRDS(here("data", "afc_lookup.rds"))

swiss_anon <- read.csv(here("../../data", "SwissExtract_anon.csv")) %>% 
  clean_names() %>% 
  filter(unique_id %in% c(med$unique_id, afc$unique_id)) %>% 
  as_tibble() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, ~ str_replace_all(.x, "\\&", "and") %>% 
              str_to_lower()) %>% 
  rename(anon_id = recid) %>% 
  mutate(anon_id = as.character(anon_id),
         gp_flag = "0")

summarise_all_incl_excl <- tibble(n = sum(!duplicated(swiss_anon$unique_id)),
                                  label = "Swiss dataset in medical and afc with linkage")

swiss_anon <- swiss_anon %>% 
  select(anon_id,
         registration_body,
         base_location_description,
         job_family,
         specialty,
         second_specialty,
         marital_status,
         immigration_status,
         length_of_service,
         whole_or_part_time,
         board_name,
         region,
         unique_id,
         gp_flag)

swiss_anon <- swiss_anon %>% 
  mutate(immigration = if_else(immigration_status %in% c("asylum seeker", "dependant", "fresh talent working in scotland scheme", 
                                                         "highly skilled migrant programme", "indefinite leave to remain", 
                                                         "other/unidentified immigration status", "tier 1 general", "tier 2 skilled worker", 
                                                         "tier 4 student", "tier 5 temporary worker", "work permit"),
                               "1",
                               "0"),
         length_of_service = if_else(length_of_service >= 50, NA_integer_, length_of_service)) %>% 
  select(-immigration_status)

## deduplicate swiss anon
swiss_dups <- swiss_anon %>% 
  group_by(unique_id) %>% 
  mutate(anydup = length(anon_id)) %>% 
  ungroup() %>% 
  filter(anydup >=2) %>% 
  select(-anydup)
swiss_dups <- swiss_dups %>% 
  group_by(unique_id) %>% 
  mutate(length_of_service = max(length_of_service, na.rm = TRUE),
         ## The following takes a person as whole time IF they are whole time for any file
         whole_or_part_time = max(whole_or_part_time)) %>% 
  summarise_all(function(x) unique(x[!is.na(x)]) %>% sort() %>% paste(collapse = "|")) %>% 
  ungroup() 
swiss_nodup <- swiss_anon %>% 
  group_by(unique_id) %>% 
  mutate(anydup = length(anon_id),
         length_of_service = as.character(length_of_service)) %>% 
  ungroup() %>% 
  filter(anydup == 1) %>% 
  select(-anydup)

swiss_anon2 <- bind_rows(swiss_nodup,
                         swiss_dups) %>% 
  mutate(immigration = as.integer(immigration),
         length_of_service = as.double(length_of_service)) %>% 
  rename(anon_id_multiple = anon_id) %>% 
  select(unique_id, everything())
swiss_anon <- swiss_anon2
rm(swiss_anon2, swiss_dups, swiss_nodup)
CountInclusions(swiss_anon, lbl = "Remove duplicates")

# Read in lookups and modify so same for medical and afc
afc <- afc %>% 
  mutate(adult = if_else(child_health ==0, "adult", "non adult"),
         country_qual = "Non medical")
med <- med %>% 
  mutate(dental = if_else(dentl == "dental", 1L, 0L)) %>% 
  select(-dentl)

afc_med <- bind_rows(afc = afc,
                     med = med %>% 
                       mutate(child_health = if_else(paeds == "paeds", 1L, 0L)) %>% 
                       select(-paeds),
                     .id = "med_afc")
rm(afc, med)
CountInclusions(afc_med, lbl = "AFC and Medical")

afc_med <- afc_med %>%
  inner_join(swiss_anon)
CountInclusions(afc_med, lbl = "Merge generic SWISS to AFC and Medical")

saveRDS(afc_med, here("data", "afc_med_pre_gp.rds"))
