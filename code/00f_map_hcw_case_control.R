#00f Map from HCW to case control ids

library(tidyverse)
library(here)
library(janitor)

lkp2cc <- read_csv(here("../../data/", "SWISS_GP_CC_ANON_ID_2020_07_09.csv"))
lkp2cc <- lkp2cc %>% 
  clean_names() %>% 
  mutate(anon_id = paste0(anon_id, "_", gp_flag)) %>% 
  select(-gp_flag)

hhd <- readRDS("data/hhd_for_cc.rds")
hcw <- readRDS("data/hcw_for_cc.rds")

hcwhhd <- bind_rows(hcw, hhd) %>% 
  mutate(case = if_else(ncov_result == "positive", 1L, 0L)) %>% 
  select(anon_id, case, hosptlsd, icu, hdu, dead28)

## need to convert to character
lkp2cc_vect        <- as.character(lkp2cc$cc_anon_id)
names(lkp2cc_vect) <- as.character(lkp2cc$anon_id)

cc <- readRDS(here("../../../Case_control/data/2020-07-09", "CC_linked_ANON_2020-07-23.rds"))%>% 
  clean_names() %>% 
  as_tibble()

hcwhhd$anon_id_cc <- lkp2cc_vect[as.character(hcwhhd$anon_id)]

hcwhhd2 <- hcwhhd %>% 
  inner_join(cc %>% 
               mutate(anon_id = as.character(anon_id),
                      case_cc = if_else(is_case, 1L, 0L),
                      hosptlsd_cc = is_case & (adm28|inhosp),
                      icu_cc = is_case & (icu==1),
                      hdu_cc = is_case & hdu==1,
                      dead_cc = is_case & (dead28==1),
                      time = as.integer(specimendate - as.Date("2020-03-01"))) %>% 
               select(anon_id_cc = anon_id, case_cc, hosptlsd_cc, icu_cc, hdu_cc, dead_cc, time))

## 479 new cases. Import these
hcwhhd2 %>% 
  count(case, case_cc)

hcwhhd2 %>% 
  count(hosptlsd, hosptlsd_cc)

## 5 new icu cases
hcwhhd2 %>% 
  count(icu,  icu_cc)
## 6 new hdu cases
hcwhhd2 %>% 
  count(hdu,  hdu_cc)
## 7 new deaths
hcwhhd2 %>% 
  count(dead28, dead_cc)

anycs <- hcwhhd2 %>% filter(case_cc ==1 & case ==0) %>% select(anon_id, time)
cases <- hcwhhd2 %>% filter(hosptlsd_cc ==1 & hosptlsd ==0) %>% select(anon_id, time)
icu   <- hcwhhd2 %>% filter(icu_cc ==1 & icu ==0) %>% select(anon_id, time)
hdu   <- hcwhhd2 %>% filter(hdu_cc ==1 & hdu ==0) %>% select(anon_id, time)
dead  <- hcwhhd2 %>% filter(dead_cc ==1 & dead28 ==0) %>% select(anon_id, time)
serious <- bind_rows(icu, hdu, dead) %>% distinct()

saveRDS(list(anycs = anycs, cases = cases, serious = serious, icu = icu, hdu = hdu, dead = dead), "data/new_events.rds")

