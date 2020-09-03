#08 cumulative incidence plots

# compare_population
library(tidyverse)
library(janitor)
library(here)

## read in Scottish population and summary data----
denom <- readRDS("reference_files/HB2019_pop_est_1981_2019.rds")
denom <- denom %>% 
  filter(year == 2019) %>% 
  select(-sex) %>% 
  select(hb2019name, sex = sex_name, age, pop) %>% 
  group_by(hb2019name, sex, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(male = if_else(sex == "M", 1L, 0L))
denom18to65 <- denom %>% 
  filter(age >=18 & age <=65) %>% 
  group_by(male) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()
denomall <- denom %>% 
  group_by(male) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

staff_role <- readRDS("outputs/risk_by_role.rds")
staff_role_sub <- readRDS("outputs/risk_by_sub_role.rds")
hhold_role <- readRDS("outputs/risk_by_role_household.rds") 

# Change to percentage
staff_role <- staff_role %>%
  mutate(prcnt = per100000 /1000) %>% 
  select(-per100000)
staff_role_sub <- staff_role_sub %>%
  mutate(prcnt = per100000 /1000) %>% 
  select(-per100000)
hhold_role <- hhold_role %>%
  mutate(prcnt = per100000 /1000) %>% 
  select(-per100000)

## Read population cases already subtracted
cases_pop <- readRDS("data/cases_no_hcw.rds")

## read in hcw data to allow subtraction from general population ----
hcw <- readRDS(here("data/", "hcw_for_cc.rds")) 
hh <- readRDS(here("data/", "hhd_for_cc.rds"))
hcwhh <- bind_rows(staff = hcw %>% filter(age >=18 & age <=65),
                   non_staff = hh,
                   .id = "staff")
rm(hcw, hh)
## Summarise denominators and delete hhd and hcw from this ----
## 18 to 65
hcwhh_denom_18to65 <- hcwhh %>% 
  filter(age >=18, age<=65) %>% 
  count(male)
denom18to65 <- denom18to65 %>% 
  inner_join(hcwhh_denom_18to65) %>% 
  mutate(pop = pop-n) %>% 
  select(-n)
rm(hcwhh_denom_18to65)
## All ages
hcwhh_denom_all <- hcwhh %>% 
  count(male)
denomall <- denomall %>% 
  inner_join(hcwhh_denom_all) %>% 
  mutate(pop = pop-n) %>% 
  select(-n)
rm(hcwhh_denom_all)
rm(hcwhh)

## Summarise healthcare worker and household cases ----
cases_pop_wa <- cases_pop %>% 
  filter(age>=18 & age <=65) %>% 
  mutate(male = if_else(sex == "Males", 1L, 0L)) %>% 
  count(specdate = specimendate, male) 
cases_pop_all <- cases_pop %>% 
  mutate(male = if_else(sex == "Males", 1L, 0L)) %>% 
  count(specdate = specimendate, male) 


## Functions to calculate cumulative incidences ----
MakeCI <- function(mydf){
  mydf <- mydf %>% 
    arrange(specdate) %>% 
    group_by(male) %>% 
    mutate(cum_n = cumsum(n)) %>% 
    ungroup()
  
  mydf2 <- mydf %>% 
    group_by(male) %>% 
    mutate(lag_same = if_else(cum_n == lag(cum_n, default = 1), "todrop", "keep")) %>% 
    ungroup() %>% 
    filter(lag_same == "keep") %>% 
    select(-lag_same)
  mydf3 <- mydf %>%
    group_by(male) %>% 
    filter(specdate == max(specdate))  %>% 
    mutate(specdate = as.Date("2020-06-08"))  %>% 
    ungroup()
  mydf_initial <- mydf3 %>% 
    mutate(specdate = as.Date("2020-03-01"),
           n = 0,
           cum_n = 0)
  bind_rows(mydf_initial, mydf2, mydf3)
}
MakeCIUnisex <- function(mydf){
  mydf <- mydf %>% 
    arrange(specdate) %>% 
    mutate(cum_n = cumsum(n)) %>% 
    ungroup()
  mydf2 <- mydf %>% 
    mutate(lag_same = if_else(cum_n == lag(cum_n, default = 1), "todrop", "keep")) %>% 
    ungroup() %>% 
    filter(lag_same == "keep") %>% 
    select(-lag_same)
  mydf3 <- mydf %>%
    filter(specdate == max(specdate))  %>% 
    mutate(specdate = as.Date("2020-06-08"))  %>% 
    ungroup()
  mydf_initial <- mydf3 %>% 
    mutate(specdate = as.Date("2020-03-01"),
           n = 0,
           cum_n = 0)
  bind_rows(mydf_initial, mydf2, mydf3)
}

pop_m_f <- MakeCI(cases_pop_wa)
pop_m_f <- pop_m_f %>% 
  inner_join(denom18to65) %>% 
  mutate(prcnt = 100 * cum_n/pop)
pop_uni <- MakeCIUnisex(cases_pop_wa %>% group_by(specdate) %>% summarise(n = sum(n)) %>% ungroup())
pop_uni <- pop_uni %>% 
  mutate(pop = sum(denom18to65$pop),
         prcnt = 100 * cum_n/pop) %>% 
  select(specdate, prcnt)

## Summary stats for inline results
pop_uni_smry_wa <- cases_pop_wa %>% 
  summarise(x = sum(n)) %>% 
  mutate(n = sum(denom18to65$pop),
         p = round(100*x/n,2),
         res = paste0(x, "/", n, " (", p, "%)"))
pop_uni_smry_all <- cases_pop_all %>% 
  summarise(x = sum(n)) %>% 
  mutate(n = sum(denomall$pop),
         p = round(100*x/n,2),
         res = paste0(x, "/", n, " (", p, "%)"))
pop_uni_smry <- bind_rows(wa = pop_uni_smry_wa,
                          all = pop_uni_smry_all,
                          .id = "agelimit")

saveRDS(pop_uni_smry, "outputs/pop_neg_smy.rds")

## Risk in staff by role ----
staff_role <- staff_role %>% 
  mutate(male = if_else(sex ==1, 1L, 0L),
         specdate = as.Date("2020-03-01") + time) %>% 
  select(specdate, role,  male, prcnt)
staff_role_sub <- staff_role_sub %>% 
  mutate(specdate = as.Date("2020-03-01") + time) %>% 
  select(specdate, role_sub, prcnt)
hhold_role <- hhold_role %>% 
  mutate(specdate = as.Date("2020-03-01") + time) %>% 
  select(specdate, role, prcnt)

role_rename <- c("population" = "Comparator population",
                 npf = "Non-patient facing",
                 undetermined = "Undetermined",
                 pf_any = "Patient facing")
staff_role <- bind_rows(staff_role,
                        pop_m_f %>% 
                          select(specdate, male, prcnt) %>% 
                          mutate(role = "population")) %>% 
  mutate(role = role_rename[role],
         male = if_else(male == 1, "Men", "Women"))
rm(cases_pop, cases_pop_wa, denom, denom18to65,
   MakeCI, MakeCIUnisex)

## Append population data to staff data  ----
staff_role_sub <- bind_rows(pop_uni %>% mutate(role_sub = "population"),
                             staff_role_sub) %>% 
  filter(!role_sub %in% c("npf", "undetermined")) %>% 
  mutate(role_sub = factor(role_sub,
                           levels = c("population","pf_front", "pf_resp_oro_agp", "pf_icu", "pf_other"),
                           labels = c("Comparator population", "Front door", "AGP", "ICU", "Other"),
                           ordered = TRUE))

hhold_role <- bind_rows(pop_uni %>% mutate(role = "population"),
                         hhold_role) %>% 
  mutate(forfacet = "Household members, men and women",
         role = role_rename[role]) 
staff_hhold <- staff_role %>% 
  mutate(forfacet = male) %>% 
  bind_rows(hhold_role) %>% 
  select(-male) %>% 
  mutate(forfacet = factor(forfacet, 
                           levels = c("Women", "Men", "Household members, men and women"),
                           labels = c("Healthcare workers (Women)", "Healthcare workers (Men)", "Household members\n(men and women)")))
rm(hhold_role, staff_role)

# ggplotColours <- function(n = 6, h = c(0, 360) + 15){
#   if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
#   hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
# }
# ggplotColours(2)
cbPalette <- c("#999999", "#E69F00", "#F8766D", "#00BFC4")
plot_staff_hhold <- ggplot(staff_hhold,
                            aes(x = specdate, y = prcnt, colour = role)) +
  geom_step() +
  scale_y_continuous("Risk of COVID-19 hospitalisation (%)", breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25), limits = c(0,0.3)) +
  scale_x_date("Date of first positive test", date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_color_manual("", values = cbPalette) +
  facet_wrap(~ forfacet, nrow = 1) +
  theme_bw()
plot_staff_hhold

plot_staff_subrole <- ggplot(staff_role_sub,
                             aes(x = specdate, y = prcnt, colour = role_sub)) +
  geom_step() +
  scale_y_continuous("Risk of COVID-19 hospitalisation (%)", breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25), limits = c(0,0.3)) +
  scale_x_date("Date of first positive test", date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_color_manual("", values = c(cbPalette[1], "Blue4", "Blue2", "Cyan4", "Steelblue2")) +
  theme_bw()
plot_staff_subrole

tiff("outputs/fig1a", res = 600, compression = "lzw", height = 8, width = 16, unit = "in")
plot_staff_hhold
dev.off()
tiff("outputs/fig1b", res = 600, compression = "lzw", height = 8, width = 8, unit = "in")
plot_staff_subrole
dev.off()

saveRDS(list(role = plot_staff_hhold, subrole = plot_staff_subrole), "outputs/cumulative_incidence_plots.rds")
