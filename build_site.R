rmarkdown::clean_site(preview = FALSE)

## load required libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
#library(flextable)
#library(shiny)

rmarkdown::render_site(encoding = "UTF-8")