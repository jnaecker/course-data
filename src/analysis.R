#### LIBRARIES ####
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(pdftools)
library(reshape2)
library(ggplot2)



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
instructor_first_name <- "Jeffrey"
instructor_last_name <- "Naecker"
master_string <- "(\\W{6,8}[0-9]{1,2}\\..*)|(\\W{16,16}.*\n\\W{20,})"

extract_response_data <- function(response_text){
  response <- response_text %>% 
    str_replace_all("\nCopyright Wesleyan University\\W*[0-9]{1,2}/[0-9]{1,2}\n", "") %>% # remove headers
    str_replace_all(paste(instructor_last_name, ", ", instructor_first_name, " \\(\\s?[A-Z]{3,4} [0-9]{3}\\)", sep = ""), "") # remove footers
  
  questions <- response %>%
    str_extract_all(master_string) %>%
    unlist() %>%
    str_replace_all("[0-9]{1,2}\\. ", "") %>% # remove qestion numbers
    str_replace_all("^(\"|\\.|!|\\])", "") %>% # remove misc punctuation at beginning of line
    str_replace_all("\\[$", "") %>%           # remove misc punctuation at end of line
    str_replace_all("\\.\\\n", "") %>%           # random fix
    str_trim()
  
  answers <- response %>%
    str_split(master_string) %>%
    unlist() %>%
    str_replace_all("Copyright Wesleyan University\\W*[0-9]{1,2}/[0-9]{1,2}", "") %>% # clean up page break
    str_replace_all("\n", "") %>% #remove line breaks
    str_trim() %>%
    gsub("([0-9])-\\w.*$", "\\1", .) # remove text from numerical responses
  
  data.frame(
    questions = questions,
    answers = unlist(answers)[-1]
  ) %>%
    subset(answers != "")
}

extract_evaluation_data <- function(document_name) {
  text <- pdf_text(file.path(evaluations_data_path, document_name))[1]
  data_frame(
    semester = str_extract(text, "Fall|Spring"),
    year = str_extract(text, "20[0-9]{2,2}"),
    department = str_extract(text, "[A-Z]{3,4}"),
    course = str_match(text, "([A-Z]{3,4}) ([0-9]{3,3})")[,3],
    audience = str_match(text, "(Project Audience) ([0-9]+)")[,3],
    responses_received = str_match(text, "(Responses Received) ([0-9]+)")[,3]
    )
}

extract_responses <- function(document_name) {
  responses <- pdf_text(file.path(evaluations_data_path, document_name)) %>%
    paste(., collapse = "") %>% 
    str_split("Course Evaluation Form( \\(continued\\))\n", ) %>%
    unlist()
  data.frame(
    response_number = c(1:length(responses)),
    responses = responses
  )
}

evaluations <- data_frame(
  document_name = list.files(path = evaluations_data_path, pattern = ".*Response Sheet.*.pdf")) %>%
  mutate(
    evaluation_data = map(document_name, ~ extract_evaluation_data(.))
  ) %>% 
  unnest()
  

responses <- evaluations %>% 
  mutate(
    responses = map(document_name, ~ extract_responses(.))  
  ) %>% unnest

responses_long <- responses %>%
  mutate(
    response_data = map(responses, ~ extract_response_data(.))
  ) %>%
  unnest()

responses_wide <- 
  responses_long %>% 
  dcast(semester + year + department + course + response_number ~ questions, value.var = "answers") 

## make variables have right class



#### PLOTS ####

ggplot(aes(x=`The Course`, y=`The Teaching`), data = responses_long) + geom_jitter(width=.1, height=.1) + stat_smooth(method = "lm")

ggplot(aes(x=`The Course`, y=`On average, how many hours per week did you spend on coursework outside of class?`), data = responses_long) + geom_jitter(width=.1, height=.1) + stat_smooth(method = "loess")

ggplot(aes(y=`What is your expected grade in this course?`, x=`On average, how many hours per week did you spend on coursework outside of class?`), 
       data = subset(responses_long, !is.na(`On average, how many hours per week did you spend on coursework outside of class?`))) +
  geom_jitter(width=.1, height=.1)

