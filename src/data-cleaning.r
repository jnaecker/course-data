#### LIBRARIES ####
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(pdftools)
library(reshape2)
library(ggplot2)
library(tibble)
library(xtable)

#### CONSTANTS ####

enrollment_data_path <- "data/enrollment"
evaluations_data_path <- "data/evaluations"
instructor_first_name <- "Jeffrey"
instructor_last_name <- "Naecker"
master_string <- "(\\W{6,8}[0-9]{1,2}\\..*)|(\\W{16,16}.*\n\\W{20,})"


#### FUNCTIONS ####

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
    str_replace_all("comment on anything else not yet addressed\\.", "") %>% #random fix
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

#### DATA IMPORT ####

##### import enrollment data #####
enrollments <- 
  data_frame(
    course = list.files(path = enrollment_data_path, pattern = "*.csv")
  ) %>%
  mutate(contents = map(
    course, 
    ~ read_csv(file.path(enrollment_data_path, .), col_types = cols(
      `Class Year` = col_character(), 
      WesPO = col_character()),
    )
  )) %>%
  unnest() %>%
  mutate(
    department = str_extract(course, "[A-Z]{3,4}"),
    semester_code = str_extract(course, "[0-9]{4}"),
    course = str_extract(course, "[0-9]{3}"),
    year = 2000 + (as.numeric(semester_code) %% 1000) %/% 10,
    semester = ifelse(as.numeric(semester_code)  %% 10 == 9, "Fall", "Spring"),
    semester_year = as.numeric(year) + ifelse(semester == "Fall", 0.5, 0),
    GPA = case_when(
      .$Grade == "A+" ~ 4.3,
      .$Grade == "A"  ~ 4.0,
      .$Grade == "A-" ~ 3.7,
      .$Grade == "B+" ~ 3.5,
      .$Grade == "B"  ~ 3.0,
      .$Grade == "B-" ~ 2.7,
      .$Grade == "C+" ~ 2.3,
      .$Grade == "C"  ~ 2.0,
      .$Grade == "C-" ~ 1.7,
      TRUE            ~ NaN)
  ) %>%
  select(semester, year, semester_code, semester_year, department, course, Name, WesID, `E-mail`, Credit, `Class Year`, Grade, GPA, Majors, `Prereq Met`)

## make enrollment variables have right class
varnames <- names(enrollments)
for (i in c(1:length(varnames))){
  if (str_detect(varnames[i], "Name|E-mail|WesID")){
    enrollments[[i]] <- as.character(enrollments[[i]])
  } else 
    if (str_detect(varnames[i], "Grade")) {
      enrollments[[i]] <- factor(enrollments[[i]], levels = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "CR", "W"))
    } else   
      if (str_detect(varnames[i], "^semester$")) {
        enrollments[[i]] <- factor(enrollments[[i]], levels = c("Spring", "Fall"))
      } else 
        if (str_detect(varnames[i], "^(GPA|year)$")) {
          enrollments[[i]] <- as.numeric(enrollments[[i]])
        } else {
          enrollments[[i]] <- as.factor(enrollments[[i]])
        } 
}

## agregate to course semester level
course_semester_enrollments <-
  enrollments %>% 
  group_by(semester, year, semester_year, department, course) %>%
  summarize(
    enroll_count = n(),
    GPA = mean(GPA, na.rm=T)
  ) %>%
  arrange(year)

##### import evaluation data #####
evaluations <- 
  data_frame(
    document_name = list.files(path = evaluations_data_path, pattern = ".*Response Sheet.*.pdf")
  ) %>%
  mutate(
    evaluation_data = map(document_name, ~ extract_evaluation_data(.))
  ) %>% 
  unnest() %>% 
  mutate(
    responses = map(document_name, ~ extract_responses(.))  
  ) %>% 
  unnest() %>%
  mutate(
    response_data = map(responses, ~ extract_response_data(.))
  ) %>%
  unnest() %>% 
  dcast(semester + year + department + course + response_number ~ questions, value.var = "answers") %>%
  mutate(semester_year = as.numeric(year) + ifelse(semester == "Fall", 0.5, 0)) %>%
  as_tibble()

## make evaluation variables have right class
varnames <- names(evaluations)
for (i in c(1:length(varnames))){
  if(str_detect(varnames[i], "(C|c)omment|advice")){
    evaluations[[i]] <- as.character(evaluations[[i]])
  } else 
    if (str_detect(varnames[i], "expected grade")) {
      evaluations[[i]] <- factor(evaluations[[i]], levels = c("A+ or A", "A- or B+", "B or B-", "C+ or C"))
    } else
      if (str_detect(varnames[i], "percentage of class sessions")) {
        evaluations[[i]] <- factor(evaluations[[i]], levels = c("91 - 100", "81 - 90", "71 - 80", "61 - 70", "51 - 60", "41 - 50", "31 - 40", "21 - 30"))
      } else
        if (str_detect(varnames[i], "^semester$")) {
          evaluations[[i]] <- factor(evaluations[[i]], levels = c("Spring", "Fall"))
        } else
          if (str_detect(varnames[i], "^(department|course)$")) {
            evaluations[[i]] <- as.factor(evaluations[[i]])
          } else {
            evaluations[[i]] <- as.numeric(evaluations[[i]])
          }
}

##### aggregate to course-semester level
course_semester_evaluations <-
  evaluations %>% 
  group_by(semester, year, semester_year, department, course) %>%
  summarize(
    eval_count = n(),
    course_rating = mean(`The Course`, na.rm=T),
    teaching_rating = mean(`The Teaching`, na.rm=T)
  ) %>%
  arrange(year)

course_semesters <-
  course_semester_evaluations %>%
  merge(course_semester_enrollments, by = c("semester", "year", "semester_year", "department", "course"), all = T) %>%
  as_tibble() %>%
  arrange(year)

#### save cleaned data files
save(course_semesters, enrollments, evaluations, file="data/clean-data.rda")
