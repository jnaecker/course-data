#### LIBRARIES ####
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)


#### CONSTANTS ####

enrollment_data_path <- "../data/enrollment"

#### SCRIPT ####

# data import 
enrollments <- data_frame(course = list.files(path = enrollment_data_path, pattern = "*.csv")) %>%
  mutate(contents = map(
    course, 
    ~ read_csv(file.path(enrollment_data_path, .), col_types = cols(
      `Class Year` = col_character(), 
      WesPO = col_character()),
      )
    )) %>%
  unnest() %>%
  mutate(
    semester = str_extract(course, "[0-9]{4}"),
    course = str_extract(course, "ECON[0-9]{3}")
  )

# creating other data sets
students <- enrollments %>%
  group_by(WesID, Name, `Class Year`, `E-mail`) %>%
  summarize(
    enrollment_count = n()
  )

courses <- enrollments %>%
  group_by(course) %>%
  summarize(
    enrollment_count = n()
  )

semesters <- enrollments %>%
  group_by(semester) %>%
  summarize(
    enrollment_count = n()
  )
  

# stats
courses
semesters
xtabs(~ course + semester, data = enrollments)
xtabs(~ course + Grade, data = enrollments)
length(unique(enrollments$WesID))

# potential TAs
enrollments %>%
  filter(`Class Year` %in% c(2018, 2019, 2020) & Grade %in% c("A+", "A", "A-", "B+")) %>%
  select(`E-mail`) %>%
  write_csv("../data/ta-emails.csv")
