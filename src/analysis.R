#### LIBRARIES ####
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)


#### CONSTANTS ####

data_path <- "../data"

#### SCRIPT ####

# data import 
raw <- data_frame(course = list.files(path = data_path, pattern = "*.csv")) %>%
  mutate(contents = map(
    course, 
    ~ read_csv(file.path(data_path, .), col_types = cols(
      `Class Year` = col_character(), 
      WesPO = col_character()),
      )
    )) %>%
  unnest() %>%
  mutate(
    semester = str_extract(course, "[0-9]{4}"),
    course = str_extract(course, "ECON[0-9]{3}")
  )

# stats
table(raw$course)
table(raw$semester)
xtabs(~ course + semester, data = raw)
xtabs(~ course + Grade, data = raw)
length(unique(raw$WesID))

# potential TAs
raw %>%
  filter(`Class Year` %in% c(2018, 2019, 2020) & Grade %in% c("A+", "A", "A-", "B+")) %>%
  select(`E-mail`) %>%
  write_csv("../data/ta-emails.csv")
