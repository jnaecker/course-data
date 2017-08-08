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
questions <- c(
  "Rate your level of effort in this course:",
  "What is your expected grade in this course\\?",
  "The Course",
  "The Teaching",
  "I knew what was expected of me in this course\\.",
  "The assignments were a useful part of the course\\.",
  "There was a clear connection between instruction and assessment\\.",
  "Instructor conveyed enthusiasm for the subject\\.",
  "Instructor communicated knowledge effectively\\.",
  "Instructor treated students with respect\\.",
  "Instructor was accessible outside of class\\.",
  "My understanding/ skills grew as a result of this course\\.",
  "Please comment on the strengths and weaknesses of the course\\.",
  "Please comment on the strengths and weaknesses of the teaching\\.",
  "comment on anything else not yet addressed\\.",
  "What percentage of class sessions did you attend/view\\?",
  "On average, how many hours per week did you spend on coursework outside of class\\?",
  "The instructor added value to the course beyond what I could get from simply doing the readings",
  "This course changed the way that I think about the world",
  "My knowledge and understanding of the subject matter have increased as a result of this course",
  "The instructor was successful at facilitating interaction in the classroom",
  "The assignments reinforced my understanding of the course material",
  "Instructor was readily available to answer questions outside of class",
  "Comment on one important thing you learned during this course",
  "Do you have any advice to pass on to students who take this course in the future?"
)

stoppers <- c(
  "What is your expected grade in this course\\?",
  "Please rate the overall quality of:",
  "The Teaching",
  "Please rate the course and the instructor on the following criteria:",
  "The assignments were a useful part of the course\\.",
  "There was a clear connection between instruction and assessment\\.",
  "Instructor conveyed enthusiasm for the subject\\.",
  "Instructor communicated knowledge effectively\\.",
  "Instructor treated students with respect\\.",
  "Instructor was accessible outside of class\\.",
  "My understanding/ skills grew as a result of this course\\.",
  "Please comment on the strengths and weaknesses of the course\\.",
  "Please comment on the strengths and weaknesses of the teaching\\.",
  "Do you have any other comments\\?",
  "What percentage of class sessions did you attend/view\\?",
  "On average, how many hours per week did you spend on coursework outside of class\\?",
  "The instructor added value to the course beyond what I could get from simply doing the readings",
  "This course changed the way that I think about the world",
  "My knowledge and understanding of the subject matter have increased as a result of this course",
  "The instructor was successful at facilitating interaction in the classroom",
  "The assignments reinforced my understanding of the course material",
  "Instructor was readily available to answer questions outside of class",
  "Comment on one important thing you learned during this course",
  "Do you have any advice to pass on to students who take this course in the future?",
  "Copyright Wesleyan University"
)

df <- data.frame(questions, stoppers, answers = NA)

document <- pdf_text(file.path(evaluations_data_path, "Spring 2017 Teaching Evaluation Response Sheet Report for Naecker  Jeffrey (ECON 211) - Faculty View _58f8ed64-b20f-4ae6-ad47-0e2d171ab2b6en-US.pdf"))

response <- 
  str_c(document[[2]], document[[3]]) %>%
  str_replace_all("\n", "") %>%
  str_replace_all("[0-9]{1,2}\\.", "") %>%
  str_replace_all("Naecker, Jeffrey", "") %>%
  str_replace_all("\\(ECON [0-9]{3}\\)", "") %>%
  str_replace_all("[0-9]{1,2}/[0-9]{1,2}", "")

for (q in c(1:dim(df)[1])){
  df$answers[q] <- gsub(paste("(^.*)(", df$questions[q], ")(.*)(", df$stoppers[q], ")(.*$)", sep = ""), "\\3", response)
}

df$answers <- df$answers %>%
  str_replace_all(",$", "") %>%
  str_replace_all("Copyright Wesleyan University", "") %>%
  str_replace_all("\\?", "") %>%
  str_trim() 
    

