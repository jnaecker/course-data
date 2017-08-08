#### LIBRARIES ####
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(pdftools)



#### CONSTANTS ####

enrollment_data_path <- "../data/enrollment"
evaluations_data_path <- "../data/evaluations"

#### SCRIPT ####

# import enrollment data
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
  

# enrollment stats
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

# import pdf data
document <- pdf_text(file.path(evaluations_data_path, "Spring 2017 Teaching Evaluation Response Sheet Report for Naecker  Jeffrey (ECON 211) - Faculty View _58f8ed64-b20f-4ae6-ad47-0e2d171ab2b6en-US.pdf"))

n <- (length(document) - 1)/2
out <- list()
for (i in c(1:n)) {
  response <- 
    str_c(document[[i + 1]], document[[i + 2]]) %>%
    str_replace_all("Naecker, Jeffrey", "") %>%
    str_replace_all("\\(ECON [0-9]{3}\\)", "") %>%
    str_replace_all("[0-9]{1,2}/[0-9]{1,2}", "") 
  
  questions <- response %>%
    str_extract_all("(\\W{6,8}[0-9]+\\..*)|(\\W{16,16}.*\n\\W{18,})") %>%
    unlist() %>%
    str_replace_all("[0-9]{1,2}\\. ", "") %>%
    str_trim()
  
  answers <- response %>%
    str_split("(\\W{6,8}[0-9]+\\..*)|(\\W{16,16}.*\n\\W{18,})") %>%
    unlist() %>%
    str_replace_all("Copyright Wesleyan University", "") %>%
    str_trim()
  
  out[[i]] <- data.frame(
    id = i,
    questions = questions,
    answers = unlist(answers)[-1]
  )
}

responses <- do.call("rbind", out)

responses$answers <- gsub("([0-9])-\\w.*$", "\\1", responses$answers)
