library(googlesheets4)
library(tidyverse)
papers_apsr <- read_sheet("https://docs.google.com/spreadsheets/d/1O6drZTETvPMyMUV3AbEkmJmUwtdy7s9NmHfV-kaZ0NA/edit?usp=sharing",
                     sheet  = "apsr")


glimpse(papers_apsr)

papers_apsr %>%
  
