#Check joining of previous SWISS data to current data (not requred for GP data)
library(tidyverse)
library(janitor)
library(here)

## Processed SWISS data
afc <- readRDS(here("data", "afc_lookup_pre_id.rds"))
med <- readRDS(here("data", "med_lookup_pre_id.Rds")) %>% 
  rename(unique_id = uniqueid)
## one person in both AFC and medical, drop from both
inboth <- intersect(afc$unique_id, med$unique_id)
afc <- afc %>% 
  filter(!unique_id %in% inboth)
med <- med %>% 
  filter(!unique_id %in% inboth)

slctd <- bind_rows(afc %>% select(unique_id), med %>% select(unique_id))
slctd %>% 
  summarise_all(function(x) sum(!duplicated(x)))

## SWISS staff and households linkages
swiss_base <- readRDS(here("../../data/", "HCW_non_GP_base_file_anon.rds")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_0"))
swiss_base %>% 
  select(anon_id) %>% 
  summarise_all(function(x) sum(!duplicated(x)))

## Read in original swiss data to perform RECID to uniqueid expansion 
swiss_orig <- read_csv(here("../../data/", "SwissExtract_anon.csv"), guess_max = 50000) %>%
  clean_names() %>% 
  select(recid, unique_id) %>% 
  mutate(anon_id = paste0(recid, "_0")) %>% 
  distinct(unique_id, anon_id) %>% 
  semi_join(slctd)
# All med and afc in SWISS original, as expected
swiss_orig %>% 
  summarise_all(function(x) sum(!duplicated(x)))

# Check all unique patients are in swiss base
swiss_orig <- swiss_orig %>% 
  mutate(inbase = anon_id %in% swiss_base$anon_id) 

swiss_check_in <- swiss_orig %>% 
  group_by(unique_id) %>% 
  summarise(inbase = any(inbase),
            anon_id = paste(anon_id, collapse = "|")) %>% 
  ungroup()

# 1605 of 164551 that do not exist in swiss base, as expected 1% without a match
swiss_check_in %>% 
  filter(!inbase) %>% 
  pull(anon_id) %>% 
  sample(100) %>% 
  sort()
swiss_check_in %>% 
  count(inbase) %>% 
  mutate(prcnt = 100 *n/sum(n))

# If just go by RECID, 3 % dont have a match
# No uniqueid has multiple anon_id matches
swiss_check_multi <- swiss_orig %>% 
  group_by(unique_id) %>% 
  summarise(inbase = sum(inbase),
            nrecid = length(anon_id),
            anon_id = paste(anon_id, collapse = "|")) %>% 
  ungroup()
## 4336 unique ids with multiple recids
swiss_check_multi %>% 
  filter(nrecid >=2)
# But none has more than one match to anonid in the base file
swiss_check_multi %>% 
  filter(inbase >=2)

# So need to append anon_id to our cleaned swiss file as follows:-
# Within a group with same uniqueid ANY recid matches anon_id

# Create uniqueid to anon id lookup
# takes any anon_id where it is in base, otherwise blank
slctd <- slctd %>%
  inner_join(swiss_orig %>% 
               filter(inbase) %>% 
               filter(!is.na(inbase), !is.na(anon_id)) %>% 
               distinct(unique_id, anon_id)) 

slctd %>% 
  anti_join(swiss_base)

## Summarise 
count_matches <- matrix(NA, nrow =2, ncol = 3,dimnames = list(c("AFC", "Med"),
                                                              c("Pre", "Post", "Percentage")))
count_matches[1, "Pre"] <- sum(!duplicated(afc$unique_id))
afc <- afc %>% 
  inner_join(slctd)
count_matches[1, "Post"] <- sum(!duplicated(afc$unique_id))

count_matches[2, "Pre"] <- sum(!duplicated(med$unique_id))
med <- med %>% 
  inner_join(slctd)
count_matches[2, "Post"] <- sum(!duplicated(med$unique_id))
count_matches[, "Percentage"] <- count_matches[,"Post"] / count_matches[,"Pre"]
count_matches

## add hid, 15 have duplicate HID, have referred this to JN
## Who is looking into
swiss_base_dup <- swiss_base %>% 
  distinct(anon_id, hid, .keep_all = TRUE) %>% 
  group_by(anon_id) %>% 
  mutate(n = n()) %>% 
  ungroup()
swiss_base_dup <- swiss_base_dup %>% 
  filter(n >=2) %>% 
  semi_join(slctd)
swiss_base_dup %>% 
  distinct(anon_id)
swiss_base_dup %>% 
  select(anon_id, hid, everything()) %>% 
  arrange(anon_id, hid) 
## For now take the first hid
afc <- afc %>% 
  inner_join(swiss_base %>%
               select(anon_id, hid) %>% 
               distinct(anon_id, .keep_all = TRUE))
med <- med %>% 
  inner_join(swiss_base %>%
               select(anon_id, hid) %>% 
               distinct(anon_id, .keep_all = TRUE))

saveRDS(afc, here("data", "afc_lookup.rds"))
saveRDS(med, here("data", "med_lookup.rds"))
write.csv(count_matches %>% as.data.frame(), "outputs/medical_surgical_matches.csv")


## review swiss base with duplicates