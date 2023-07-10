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

# Sample data table ---------------------------------------------------------

organizations <- Project0 %>%
  distinct(OrganizationID) %>%
  n_distinct()

projects <- Project0 %>%
  distinct(ProjectID) %>%
  n_distinct()

enrollments <- Enrollment %>%
  distinct(EnrollmentID) %>%
  n_distinct()

ES_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "1") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

ES_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "1") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

ES_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "1") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

TH_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "2") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PSH_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "3") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SO_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "4") %>%
  distinct(EnrollmentID) %>%
  n_distinct()
  
DS_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "11") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PH_Svcs_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "10") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

PH_NoSvcs_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "9") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

SSO_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "6") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

HP_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "12") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

RRH_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "13") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

CE_enrollments <- Enrollment %>%
  group_by(ProjectType) %>%
  filter(ProjectType == "14") %>%
  distinct(EnrollmentID) %>%
  n_distinct()

services <- Services %>%
  distinct(ServicesID) %>%
  n_distinct()

mult_enrollments <- Enrollment %>%
  group_by(PersonalID) %>%
  summarise(NumEnrollmentID = n()) %>%
  filter(NumEnrollmentID > 1) %>%
  summarise(NumPersonalID = n())
print(mult_enrollments)

sample_data <- data.frame(
  Data = c("Number of projects", 
           "Number of organizations", 
           "Number of enrollments",
           "Number of ES enrollments",
           "Number of TH enrollments",
           "Number of PSH enrollments",
           "Number of SO enrollments",
           "Number of DS enrollments",
           "Number of PH with Services enrollments",
           "Number of PH No Services enrollments",
           "Number of SSO enrollments",
           "Number of HP enrollments",
           "Number of RRH enrollments",
           "Number of CE enrollments",
           "Number of services",
           "Number of clients with multiple enrollments"),
  Number = c(projects, 
             organizations, 
             projects,
             ES_enrollments,
             TH_enrollments,
             PSH_enrollments,
             SO_enrollments,
             DS_enrollments,
             PH_Svcs_enrollments,
             PH_NoSvcs_enrollments,
             SSO_enrollments,
             HP_enrollments,
             RRH_enrollments,
             CE_enrollments,
             services,
             mult_enrollments))
print(sample_data)

