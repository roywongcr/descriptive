library(shiny)
library(gtsummary)
library(gt)
# make dataset with a few variables to summarize
iris2 <- iris %>% select(Sepal.Length,  Sepal.Width, Species) 

