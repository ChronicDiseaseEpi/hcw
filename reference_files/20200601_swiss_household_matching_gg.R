#*******************************************
#household matching
#graeme 2020-06-01

#based on:
#/chi/(1) Project Folders/20031 GPCD & SWISS inc households/syntax/20031.R
#written by Martin

#run on RStudio Server 3.6.1
#*******************************************

#*******************************************
#load packages####
#*******************************************
library(odbc)
library(tidyverse)
library(tidylog)
library(lubridate)
library(data.table)
library(fuzzyjoin)
library(janitor)
library(phsmethods)
library(glue)

#*******************************************
#connect to SMRA####
#*******************************************
con <- dbConnect(odbc(), dsn = "SMRA",
                 uid = rstudioapi::askForPassword("Database user"),
                 pwd = rstudioapi::askForPassword("Database password"),
                 port = "1527",
                 host = "nssstats01.csa.scot.nhs.uk",
                 SVC = "SMRA.nss.scot.nhs.uk")

#define username for writing
user_name <- unname(toupper(Sys.info()['user']))

#*******************************************
#read and process matches####
#*******************************************

#read data and clean up
cohort <- read_csv("/chi/(1) Project Folders/20031 GPCD & SWISS inc households/Data/Traditional_review_indexed_File_3064_UPI.csv",
                   col_types = "ccccccc") %>% 
          select(SERIAL_NO, UPI) %>% 
          rename(SERIAL = SERIAL_NO, UPI_NUMBER = UPI) %>% 
          filter(!is.na(UPI_NUMBER)) %>%  #remove missing UPIs
          mutate(UPI_NUMBER = chi_pad(UPI_NUMBER)) %>% #pad to 10
          arrange(UPI_NUMBER) #order by UPI

#check CHI validity - all valid
cohort %>% mutate(valid_chi = chi_check(UPI_NUMBER)) %>% count(valid_chi)

#are all serial unique?
n_distinct(cohort$SERIAL)

#show duplicates
cohort %>% 
  mutate(dups = (duplicated(SERIAL) | duplicated(SERIAL, fromLast = TRUE))) %>% 
  filter(dups == TRUE) %>% 
  arrange(SERIAL)

#write up to SMRA for swift join
dbWriteTable(con,
             "COHORT", 
             cohort,
             overwrite = TRUE,
             field.types = c(SERIAL = "VARCHAR2(6)", #had to increase this from 4
                             UPI_NUMBER = "VARCHAR2(10)"))

#collect names for current records
cohort <-(dbGetQuery(con, statement = glue("SELECT COHORT.UPI_NUMBER,
                                           COHORT.SERIAL,
                                           L_UPI_DATA.CHI_STATUS,
                                           L_UPI_DATA.FIRST_FORENAME,
                                           L_UPI_DATA.SURNAME
                                           FROM {user_name}.COHORT COHORT LEFT OUTER JOIN UPIP.L_UPI_DATA L_UPI_DATA
                                           ON (COHORT.UPI_NUMBER = L_UPI_DATA.UPI_NUMBER)
                                           WHERE ( (L_UPI_DATA.CHI_STATUS = 'C') OR (L_UPI_DATA.CHI_STATUS IS NULL))")))

#make tibble and order
cohort <- cohort %>% tibble() %>% arrange(UPI_NUMBER)

#*******************************************
#collect address####
#*******************************************

#remove old table if exists
dbRemoveTable(con, "ADDRESS") 

#create table of addresses
ADDRESS <-(dbGetQuery(con, statement = glue("CREATE TABLE {user_name}.ADDRESS AS SELECT COHORT.UPI_NUMBER,
                                            COHORT.SERIAL,
                                            L_UPI_DATA.CURRENT_LINE1,
                                            L_UPI_DATA.CURRENT_LINE2,
                                            L_UPI_DATA.CURRENT_LINE3,
                                            L_UPI_DATA.CURRENT_POSTCODE,
                                            L_UPI_DATA.DATE_ADDRESS_CHANGED,
                                            L_UPI_DATA.CHI_STATUS
                                            FROM {user_name}.COHORT COHORT LEFT OUTER JOIN UPIP.L_UPI_DATA L_UPI_DATA
                                            ON (COHORT.UPI_NUMBER = L_UPI_DATA.UPI_NUMBER)
                                            WHERE ( (L_UPI_DATA.CHI_STATUS = 'C') OR (L_UPI_DATA.CHI_STATUS IS NULL))")))

#*******************************************
#match households####
#*******************************************

#match SWISS record addresses to other addresses which are equivalent on the CHI file
#renamed ADDRESS UPI as IN_UPI
HOUSEHOLD <- (dbGetQuery(con, statement = glue("SELECT DISTINCT L_UPI_DATA.CURRENT_LINE2,
                                               L_UPI_DATA.CURRENT_LINE3,
                                               L_UPI_DATA.CURRENT_POSTCODE,
                                               L_UPI_DATA.DATE_ADDRESS_CHANGED,
                                               L_UPI_DATA.CHI_STATUS,
                                               ADDRESS.UPI_NUMBER AS IN_UPI,
                                               ADDRESS.SERIAL,
                                               ADDRESS.CURRENT_LINE1 AS IN_1,
                                               ADDRESS.CURRENT_LINE2 AS IN_2,
                                               ADDRESS.CURRENT_LINE3 AS IN_3,
                                               ADDRESS.CURRENT_POSTCODE AS IN_PC,
                                               ADDRESS.DATE_ADDRESS_CHANGED AS IN_DATE,
                                               ADDRESS.CHI_STATUS AS IN_STATUS,
                                               L_UPI_DATA.SURNAME,
                                               L_UPI_DATA.FIRST_FORENAME,
                                               L_UPI_DATA.UPI_NUMBER,
                                               L_UPI_DATA.DATE_OF_BIRTH,
                                               L_UPI_DATA.SEX,
                                               L_UPI_DATA.DATE_OF_DEATH,
                                               L_UPI_DATA.CURRENT_LINE1
                                               FROM    {user_name}.ADDRESS ADDRESS
                                               INNER JOIN
                                               UPIP.L_UPI_DATA L_UPI_DATA
                                               ON     (ADDRESS.CURRENT_POSTCODE = L_UPI_DATA.CURRENT_POSTCODE OR (L_UPI_DATA.CURRENT_POSTCODE IS NULL))
                                               AND (ADDRESS.CURRENT_LINE1 = L_UPI_DATA.CURRENT_LINE1)
                                               AND (ADDRESS.CURRENT_LINE2 = L_UPI_DATA.CURRENT_LINE2)
                                               WHERE ( (L_UPI_DATA.CHI_STATUS = 'C') OR (L_UPI_DATA.CHI_STATUS IS NULL))
                                               ")))

#make tibble and reorder
HOUSEHOLD <- HOUSEHOLD %>% 
              tibble() %>% 
              arrange(IN_UPI) %>% 
              select(IN_UPI, CURRENT_POSTCODE, UPI_NUMBER, SERIAL) %>% 
              distinct()

#assign HID (household ID) for processing each incoming UPI
HOUSEHOLD <- transform(HOUSEHOLD, HID = as.numeric(factor(IN_UPI))) %>% tibble()

#add flag for the incomer (WF) record
all_household <- HOUSEHOLD %>% 
                  mutate(WF = if_else(IN_UPI == UPI_NUMBER, 1, 0)) %>%
                  select(HID, UPI_NUMBER, CURRENT_POSTCODE, WF, SERIAL) %>%
                  distinct() %>% 
                  arrange(HID)

#write this to con
dbWriteTable(con, "HH", 
             all_household,
             overwrite = TRUE,
             field.types = c(SERIAL = "VARCHAR2(6)",
                             UPI_NUMBER = "VARCHAR2(10)",
                             CURRENT_POSTCODE = "VARCHAR2(7)",
                             WF = "VARCHAR2(1)",
                             HID = "VARCHAR2(6)"))

#get names for whole household
HH_NAMES <- (dbGetQuery(con, 
                      statement = glue("SELECT DISTINCT HH.HID,
                                       HH.UPI_NUMBER,
                                       HH.CURRENT_POSTCODE,
                                       HH.WF,
                                       HH.SERIAL,
                                       L_UPI_DATA.CHI_STATUS,
                                       L_UPI_DATA.DATE_OF_DEATH,
                                       L_UPI_DATA.FIRST_FORENAME,
                                       L_UPI_DATA.SURNAME,
                                       L_UPI_DATA.DATE_ADDRESS_CHANGED,
                                       L_UPI_DATA.DOWNLOAD_DATE
                                       FROM {user_name}.HH HH LEFT OUTER JOIN UPIP.L_UPI_DATA L_UPI_DATA
                                       ON (HH.UPI_NUMBER = L_UPI_DATA.UPI_NUMBER)
                                       WHERE ( (L_UPI_DATA.CHI_STATUS = 'C') OR (L_UPI_DATA.CHI_STATUS IS NULL))")))

#remove those household records with a date of death older than March 2020
#keep all WF (incoming records) and those HH members where no date of death or recent date of death
HH_NAMES <- HH_NAMES %>% filter(WF == 1 | (WF == 0 & (is.na(DATE_OF_DEATH) | DATE_OF_DEATH > "2020-03-01")))

#if no date address changed then add download date
HH_NAMES <- HH_NAMES %>% 
            mutate(address_changed = fifelse(is.na(DATE_ADDRESS_CHANGED), DOWNLOAD_DATE, DATE_ADDRESS_CHANGED)) %>% 
            mutate(id = row_number()) %>%
            arrange(HID) %>% 
            tibble()

#*******************************************
#large households####
#*******************************************

#split household where membership is 5+ for checking as many run into 10s, 100s or 1000s
HH_NAMESa <- HH_NAMES %>% 
              add_count(HID, name = "count") %>% 
              filter(count > 5)

#create a dataframe for WF only    
HH_NAMESb <- HH_NAMESa %>% filter(WF == 1)

#create a data frame for household members only
HH_NAMESa <- HH_NAMESa %>% filter(WF == 0) 

#fuzzy join WF to members to cope with hyphenated names in the file
#this matches by HID and SURNAME
#there are some cases where the name is part of a larger one but maybe unlikely to be same
#family - e.g. knight vs mcknight
HH_NAMESc <- fuzzyjoin::fuzzy_left_join(HH_NAMESb,
                                        HH_NAMESa,
                                        by = c("HID", "SURNAME"),
                                        match_fun = stringr::str_detect)

#clean up matched names
HH_NAMESc <- HH_NAMESc %>% 
              distinct() %>% 
              filter(!is.na(UPI_NUMBER.x))

#extract wf members
HH_lead <- HH_NAMESc %>% 
            select(HID.x, UPI_NUMBER.x, CURRENT_POSTCODE.x, WF.x, SERIAL.x, id.x) %>% 
            rename(HID = HID.x, UPI_NUMBER = UPI_NUMBER.x, CURRENT_POSTCODE = CURRENT_POSTCODE.x,
                    WF = WF.x, SERIAL = SERIAL.x, id = id.x) %>% 
            distinct() #added to remove duplicates - where one WF has multiple HH members

#there are two cases where it appears that the person is duplicated
#the input data for seeding was processed to remove exact duplicates
#but these entries had a different UniqueID. So were not removed.
HH_lead %>% 
  mutate(x = duplicated(UPI_NUMBER)|duplicated(UPI_NUMBER, fromLast = TRUE)) %>% 
  filter(x == TRUE)

#extract household members
HH_members <- HH_NAMESc %>% 
                select(HID.y, UPI_NUMBER.y, CURRENT_POSTCODE.y, WF.y, SERIAL.y, id.y) %>% 
                rename(HID = HID.y, UPI_NUMBER = UPI_NUMBER.y, CURRENT_POSTCODE = CURRENT_POSTCODE.y,
                        WF = WF.y, SERIAL = SERIAL.y, id = id.y)

#bind back together large household info and remove blanks
problems <- bind_rows(HH_lead, HH_members) %>% filter(!is.na(UPI_NUMBER))

#*******************************************
#small (<=5) households####
#*******************************************

#rejoin names for ONOmap
HH_small <- HH_NAMES %>% 
              add_count(HID, name = "count") %>% 
              filter(count <= 5) %>% #keep only small households
              select(HID, UPI_NUMBER, CURRENT_POSTCODE, WF, SERIAL, id)

#*******************************************
#combine and process data ####
#*******************************************
HH_final <- bind_rows(HH_small, problems) %>% 
            distinct()

#add back names
add_names <- left_join(HH_final, HH_NAMES, by = "UPI_NUMBER") %>% 
              select(HID.x, UPI_NUMBER, CURRENT_POSTCODE.x, WF.x, SERIAL.x, FIRST_FORENAME, SURNAME) %>% 
              rename(HID = HID.x, CURRENT_POSTCODE = CURRENT_POSTCODE.x, WF = WF.x, SERIAL = SERIAL.x) %>% 
              distinct()

#show largest households
add_names %>% count(HID) %>% arrange(desc(n))

#remove variables for onomap
add_names2 <- add_names %>%
                select(UPI_NUMBER, FIRST_FORENAME, SURNAME) %>% 
                distinct() %>% 
                filter(!(is.na(FIRST_FORENAME) & is.na(SURNAME))) #remove missing fn/sn

#write data for onomap
fwrite(add_names2, 
       "/chi/(1) Project Folders/20031 GPCD & SWISS inc households/Data/SWISS/names_for_onomap.csv")

#*******************************************
#now process file in onomap####
#*******************************************

#*******************************************
#read in onomap results####
#*******************************************

ono <- read_csv("/chi/(1) Project Folders/20031 GPCD & SWISS inc households/Data/SWISS/Onolytics_Export_File.csv", 
                col_types = "ccccccccccc") %>% 
        mutate(UPI_NUMBER = chi_pad(UPI_NUMBER)) %>% 
        select(-FIRST_FORENAME, -SURNAME)

#add onomap output to names, drop names and reorder
final <- left_join(add_names, ono, by = "UPI_NUMBER", keep = T) %>% 
          select(-FIRST_FORENAME, -SURNAME) %>% 
          arrange(HID, WF)

#*******************************************
#remove duplication
#*******************************************
# a is a dataset with a unique identifier, the lead and household varaibles
# remove duplication for households due to GP marriages

multi_hhd <- final %>%  
              distinct(UPI_NUMBER, HID) %>% #keep only UPI and HID
              group_by(UPI_NUMBER) %>% 
              mutate(n_id = seq_along(UPI_NUMBER)) %>% 
              ungroup() %>% 
              spread(n_id, HID) %>% 
              mutate(hid_new = pmin(`1`, `2`, `3`, `4`, na.rm = TRUE)) #added extra cols here, but doesn't change results

multi_hhd2 <- multi_hhd %>% 
              select(-UPI_NUMBER) %>% 
              gather("hid_n", "HID", -hid_new, na.rm = TRUE) %>% 
              select(-hid_n) %>% 
              distinct() %>% 
              arrange(hid_new, HID) %>% 
              mutate(hid_new2 = hid_new)

#add UPI2 to cohort
COHORT <- cohort %>% mutate(UPI2 = as.character(UPI_NUMBER))

#join final data to new household IDs
test <- left_join(final, multi_hhd2, by = c("HID" = "hid_new"), keep = T)
test <- test %>% mutate(UPI2 = UPI_NUMBER)
test <- left_join(test, COHORT, by = "UPI_NUMBER", keep = T)

#clean up
test <- test %>% 
          mutate(WF_final = ifelse(UPI_NUMBER == UPI2.y, 1, 0)) %>%
          select(-HID, -WF, -SERIAL.x, -UPI2.x ,-CHI_STATUS, -FIRST_FORENAME, -SURNAME, -HID.y) %>% 
          distinct() %>% 
          rename(HID = hid_new2, SERIAL = SERIAL.y, WF_UPI = UPI2.y, WF = WF_final) %>% 
          select(UPI_NUMBER, HID, SERIAL, WF_UPI, WF, CURRENT_POSTCODE, 
                  `Onolytics Type Code`, `Onolytics Group`, `Onolytics Subgroup`,
                  `Onolytics Type`, `Geographical Area`, `Religion`, `Personal Score`, `Onolytics coding case`) %>% 
          arrange(HID, WF)

#clean col names
final_wf <- clean_names(test, case = c("screaming_snake"))

#remove blank HID
final_wf <- final_wf %>% filter(!is.na(HID))

#*******************************************
#join counts of HID
#*******************************************
#add original household counts (by address match)
hh_count <- HH_NAMES %>% 
            add_count(HID, name = "count") %>% 
            select(HID, count) %>% 
            distinct()

#join to final dataset
final_wf <- left_join(final_wf, hh_count, by = "HID")

#*******************************************
#export####
#*******************************************
#save output
fwrite(final_wf, "/chi/(1) Project Folders/20031 GPCD & SWISS inc households/Data/SWISS/swiss_wf_output.csv")

