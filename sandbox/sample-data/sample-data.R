library(tidyverse)
library(lubridate)
library(here)

# Instructions ------------------------------------------------------------

# Run this, it will create a data directory here in case you don't already have one

if(dir.exists("sandbox/mini-non-shiny-environment/data/")) {
  "All good"
} else {
  dir.create("sandbox/mini-non-shiny-environment/data/")
}

directory <- "sandbox/mini-non-shiny-environment/"

# Copy a sample dataset into the mini-non-shiny-environment/data directory

if_else(file.exists(paste0(directory, "data/Enrollment.csv")),
        "All good",
        "Please unzip a sample data set into your mini-non-shiny-environment/data
        directory before proceeding.")

# Run the scratch files in order. As you're running them, it's helpful to have
# the Environment tab open so you can see what is going on

# Functions ---------------------------------------------------------------
source(here("global.R"))
source(here("guidance.R"))

source(paste0(directory, "helper_functions.R"))

# Hard codes ---------------------------------------------------------------

source(paste0(directory, "hardcodes.R"))

# Get Export --------------------------------------------------------------

source(paste0(directory, "01_get_Export.R"))

# Export Dates -------------------------------------------------------------------

source(paste0(directory, "02_export_dates.R"))

# Data prep! --------------------------------------------------------------

source(paste0(directory, "04_initial_data_prep.R"))

# Sample data  ---------------------------------------------------------

# Age at Entry -------------------------------------------------------------

small_client <- Client %>% select(PersonalID, DOB)
enrollment_with_age <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate))

# Base DQ Data --------------------------------------------------------------

base_dq_data <- Enrollment %>%
  left_join(Client %>%
              select(-DateCreated), by = "PersonalID") %>%
  left_join(Project %>% select(ProjectID, TrackingMethod, OrganizationName),
            by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    EntryDate,
    HouseholdID,
    MoveInDate,
    ExitDate,
    Destination,
    ExitAdjust,
    DateCreated,
    ProjectType
    ) 

# Project Descriptor Numbers ------------------------------------------------

organizations <- Project0 %>%
  distinct(OrganizationID) %>%
  n_distinct()

projects <- Project0 %>%
  distinct(ProjectID) %>%
  n_distinct()

enrollments <- Enrollment %>%
  distinct(EnrollmentID) %>%
  n_distinct()

# Client information numbers ---------------------------------------------

clients <- Client %>%
  distinct(PersonalID) %>%
  n_distinct()

# Number of enrollments --------------------------------------------

ES_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "1") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

ES_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "1") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

TH_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "2") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

TH_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "2") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PSH_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "3") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PSH_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "3") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SO_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "4") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SO_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "4") %>%
  distinct(EnrollmentID) %>%
  n_distinct()
  
DS_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "11") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

DS_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "11") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PH_Svcs_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "10") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PH_Svcs_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "10") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PH_NoSvcs_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "9") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PH_NoSvcs_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "9") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SSO_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "6") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SSO_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "6") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

HP_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "12") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

HP_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "12") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

RRH_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "13") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

RRH_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "13") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

CE_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "14") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

CE_stayers <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
    ProjectType == "14") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

mult_enrollments <- Enrollment %>%
  group_by(PersonalID) %>%
  summarise(NumEnrollmentID = n()) %>%
  filter(NumEnrollmentID > 1) %>%
  summarise(NumPersonalID = n()) %>%
  pull(NumPersonalID)

# Number of Services -----------------------------------------------

services <- Services %>%
  distinct(ServicesID) %>%
  n_distinct()
print(services)

# Race information -----------------------------------------------

race_df <- Client %>%
  select(AmIndAKNative,
         Asian,
         BlackAfAmerican,
         NativeHIPacific,
         White)
number_races <- as.data.frame(colSums(race_df))
colnames(number_races) <- c("Count")

count_racenone <- sum(Client$RaceNone == "99", na.rm = TRUE)
racenone_df <- data.frame(RaceNone = count_racenone)
colnames(racenone_df) <- c("Count")

number_races_df <- rbind(number_races, racenone_df)
min_race <- min(number_races_df)

mult_race <- Client %>%
  select(PersonalID,
         AmIndAKNative,
         Asian,
         BlackAfAmerican,
         NativeHIPacific,
         White) %>%
  group_by(PersonalID) %>%
  filter(AmIndAKNative + Asian + BlackAfAmerican + NativeHIPacific + White > 1) %>%
  distinct(PersonalID) %>%
  n_distinct()

# Gender information ----------------------------------------------

gender_df <- Client %>%
  select(Female,
         Male,
         NoSingleGender,
         Transgender,
         Questioning)
number_genders <- as.data.frame(colSums(gender_df))
colnames(number_genders) <- c("Count")

count_gendernone <- sum(Client$GenderNone == "99", na.rm = TRUE)
gendernone_df <- data.frame(GenderNone = count_gendernone)
colnames(gendernone_df) <- c("Count")

number_genders_df <- rbind(number_genders, gendernone_df)
min_gender <- min(number_genders_df)

# Ethnicity information --------------------------------------------------

count_ethnicity <- table(Client$Ethnicity)

min_ethnicity <- min(count_ethnicity)

# Age information -------------------------------------------------------

small_client <- Client %>% select(PersonalID, DOB)
enrollment_with_age <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate))

# Veteran information ----------------------------------------------------

count_veteran_status <- table(Client$VeteranStatus)

min_veteran_status <- min(count_veteran_status)

# SSVF funded ------------------------------------------------------------

ssvf_funded <- Funder %>%
  filter(Funder %in% c(ssvf_fund_sources)) %>%
  pull(ProjectID)

ssvf_base_dq_data <- base_dq_data %>%
  filter(ProjectID %in% c(ssvf_funded)) %>%
  select(
    EnrollmentID,
    HouseholdID,
    PersonalID,
    ProjectID,
    ProjectType
  ) %>%
  left_join(
    Enrollment %>%
      select(
        EnrollmentID,
        HouseholdID,
        PersonalID,
        EntryDate,
        ExitDate,
        RelationshipToHoH,
      ),
    by = c("PersonalID", "EnrollmentID", "HouseholdID")
  ) %>%
  left_join(
    Client %>%
      select(
        PersonalID,
        VeteranStatus,
      ),
    by = "PersonalID"
  )

SSVF_HP_enrollments <- ssvf_base_dq_data %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "12") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SSVF_HP_stayers <- ssvf_base_dq_data %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
           ProjectType == "12") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SSVF_RRH_enrollments <- ssvf_base_dq_data %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "13") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SSVF_RRH_stayers <- ssvf_base_dq_data %>%
  group_by(ProjectType) %>%
  filter(is.na(ExitDate) &
           ProjectType == "13") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

# Sample data table ------------------------------------------------------

sample_data <- data.frame(
  Data = c("Number of projects", 
           "Number of organizations", 
           "Number of enrollments",
           "Number of clients",
           "Number of ES enrollments",
           "Number of ES stayers",
           "Number of TH enrollments",
           "Number of TH stayers",
           "Number of PSH enrollments",
           "Number of PSH stayers",
           "Number of SO enrollments",
           "Number of SO stayers",
           "Number of DS enrollments",
           "Number of DS stayers",
           "Number of PH with Svcs enrollments",
           "Number of PH with Svcs stayers",
           "Number of PH No Svcs enrollments",
           "Number of PH No Svcs stayers",
           "Number of SSO enrollments",
           "Number of SSO stayers",
           "Number of HP enrollments",
           "Number of HP stayers",
           "Number of SSVF HP enrollments",
           "Number of SSVF HP stayers",
           "Number of RRH enrollments",
           "Number of RRH stayers",
           "Number of SSVF RRH enrollments",
           "Number of SSVF RRH stayers",
           "Number of CE enrollments",
           "Number of CE stayers",
           "Number of services",
           "Number of clients with multiple enrollments",
           "Number of people in smallest race category",
           "Number of people that selected multiple race categories",
           "Number of people in smallest gender category",
           "Number of people in smallest ethnicity category",
           "Number of people in smallest veteran status category"),
  Number = c(projects, 
             organizations, 
             enrollments,
             clients,
             ES_enrollments,
             ES_stayers,
             TH_enrollments,
             TH_stayers,
             PSH_enrollments,
             PSH_stayers,
             SO_enrollments,
             SO_stayers,
             DS_enrollments,
             DS_stayers,
             PH_Svcs_enrollments,
             PH_Svcs_stayers,
             PH_NoSvcs_enrollments,
             PH_NoSvcs_stayers,
             SSO_enrollments,
             SSO_stayers,
             HP_enrollments,
             HP_stayers,
             SSVF_HP_enrollments,
             SSVF_HP_stayers,
             RRH_enrollments,
             RRH_stayers,
             SSVF_RRH_enrollments,
             SSVF_RRH_stayers,
             CE_enrollments,
             CE_stayers,
             services,
             mult_enrollments,
             min_race,
             mult_race,
             min_gender,
             min_ethnicity,
             min_veteran_status))
print(sample_data)

