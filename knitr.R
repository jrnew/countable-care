rm(list = ls())
library(knitr)
setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
file <- "report_mine.Rtex"
knit2pdf(file)
