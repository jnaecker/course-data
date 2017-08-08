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
document <- pdf_text(file.path(evaluations_data_path, "Spring 2017 Teaching Evaluation Response Sheet Report for Naecker  Jeffrey (ECON 211) - Faculty View _58f8ed64-b20f-4ae6-ad47-0e2d171ab2b6en-US.pdf"))

n <- (length(document) - 1)/2
out <- list()
for (i in c(1:n)) {
  response <- 
    str_c(document[[2*i]], document[[2*i + 1]]) %>%
    str_replace_all("\nCopyright Wesleyan University\\W*[0-9]{1,2}/[0-9]{1,2}\n Naecker, Jeffrey \\(ECON [0-9]{3}\\)", "") # clean up page break
  
  questions <- response %>%
    str_extract_all("(\\W{6,8}[0-9]{1,2}\\..*)|(\\W{16,16}.*\n\\W{18,})") %>%
    unlist() %>%
    str_replace_all("[0-9]{1,2}\\. ", "") %>% # remove qestion numbers
    str_replace_all("^(\"|\\.|!|\\])", "") %>% # remove misc punctuation at beginning of line
    str_replace_all("\\[$", "") %>%           # remove misc punctuation at end of line
    str_trim()
  
  answers <- response %>%
    str_split("(\\W{6,8}[0-9]{1,2}\\..*)|(\\W{16,16}.*\n\\W{18,})") %>%
    unlist() %>%
    str_replace_all("Copyright Wesleyan University\\W*[0-9]{1,2}/[0-9]{1,2}", "") %>% # clean up page break
    str_trim()
  
  out[[i]] <- data.frame(
    id = i,
    questions = questions,
    answers = unlist(answers)[-1]
  )
}

responses <- do.call("rbind", out) %>%
  subset(answers != "")

responses$answers <- gsub("([0-9])-\\w.*$", "\\1", responses$answers)


responses_long <- 
  responses %>% 
  dcast(id ~ questions, value.var = "answers") 

varlist <- c("I knew what was expected of me in this course.","Instructor communicated knowledge effectively.","Instructor conveyed enthusiasm for the subject.","Instructor treated students with respect.","Instructor was accessible outside of class.","Instructor was readily available to answer questions outside of class","My knowledge and understanding of the subject matter have increased as a result of this course","My understanding/ skills grew as a result of this course.","Rate your level of effort in this course:","The assignments reinforced my understanding of the course material","The assignments were a useful part of the course.","The Course","The instructor added value to the course beyond what I could get from simply doing the readings","The instructor was successful at facilitating interaction in the classroom","The Teaching","There was a clear connection between instruction and assessment.","This course changed the way that I think about the world", "On average, how many hours per week did you spend on coursework outside of class?")
for (var in varlist) {
  responses_long[, var] <- as.numeric(responses_long[, var])
}

responses_long$`What is your expected grade in this course?` <- factor(responses_long$`What is your expected grade in this course?`, levels = c("A+ or A", "A- or B+", "B or B-"))


ggplot(aes(x=`The Course`, y=`The Teaching`), data = responses_long) + geom_jitter(width=.1, height=.1) + stat_smooth(method = "lm")

ggplot(aes(x=`The Course`, y=`On average, how many hours per week did you spend on coursework outside of class?`), data = responses_long) + geom_jitter(width=.1, height=.1) + stat_smooth(method = "loess")

ggplot(aes(y=`What is your expected grade in this course?`, x=`On average, how many hours per week did you spend on coursework outside of class?`), 
       data = subset(responses_long, !is.na(`On average, how many hours per week did you spend on coursework outside of class?`))) +
  geom_jitter(width=.1, height=.1)

