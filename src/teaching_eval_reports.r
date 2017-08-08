#### LIBRARIES ####

library(ggplot2)
library(gdata)
library(reshape2)
library(dplyr)

#### SCRIPT ####

## download and clean data

evals <- read.csv("EndofSemester_Assessment.csv", skip=1) %>% 
  select(-c(2:9, 15, 42:45)) %>%
  melt(id.vars = c("course", "semester", "department", "year", "ResponseID", "Finished"))

## make report

ggplot(subset(evals, variable == "How.many.hours.per.week.on.average.did.you.spend.on.this.course..including.class.meetings.."), aes(x=value)) +
  geom_histogram() +
  facet_wrap(~course) +
  theme_minimal()

ggplot(subset(evals, variable == "How.useful.to.you.were.each.of.the.following.course.elements..TA.sessions" & course==301), aes(x=value)) +
  geom_histogram() +
  theme_minimal()



### Wes evals

library(xml2)

report <- read_xml("2015-fall-econ-301.html")
xml_children(xml_children(report)[2])

xml_find_all(xml_children(xml_children(report)[2]), ".//table")

xml_report <- xmlToList(xmlParse("2015-fall-econ-301.html"))
