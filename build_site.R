rmarkdown::clean_site(preview = FALSE)

## load required libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
#library(flextable)
#library(shiny)

# count how many Rmd and knitred html
to_be_knit_posts <- ((dir(path = "_posts",recursive = TRUE,pattern = "*.Rmd|*.html") %>%
  gsub(pattern = "*.Rmd|*.html",replacement = "") %>%
  table() %>% 
  unlist() %>% 
  as.data.frame() %>% ## Retrieve the Rmd to be knit
  filter(Freq == 1))[,1] %>%
  as.vector())

stop_knit <- "welcome/welcome"

if(length(to_be_knit_posts[!(to_be_knit_posts %in% stop_knit)]) > 0){
  rmd_filenames <- paste0("_posts/",to_be_knit_posts[!(to_be_knit_posts %in% stop_knit)],".Rmd") ## Get the files to be knit except "welcome"
  rmarkdown::render(rmd_filenames, encoding = "UTF-8")
}

rmarkdown::render_site(encoding = "UTF-8")
