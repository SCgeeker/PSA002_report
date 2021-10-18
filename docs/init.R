# Learn more about creating websites with Distill at:
# https://rstudio.github.io/distill/website.html

# Learn more about publishing to GitHub Pages at:
# https://rstudio.github.io/distill/publish_website.html#github-pages

#library(tidyverse)
#library(data.table)
#library(lubridate)
#library(magrittr)
#library(flextable)

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Load lab information
lab_info <- ##dirname(getwd()) %>%
  dir(path = "../EXPDATA/2_seq_web/",full.names = TRUE, recursive = TRUE, include.dirs = TRUE, pattern = "Lab_info.csv")  %>%
  read_csv()

lab_fin <- ##dirname(getwd()) %>%
  dir(path = "../EXPDATA/2_seq_web/",full.names = TRUE, recursive = TRUE, include.dirs = TRUE, pattern = "lab_fin.csv")  %>%
  read_csv()


# Load meta data of in site data: age, female numbers

insite_meta <- dir(path = "../EXPDATA/",full.names = TRUE, recursive = TRUE, include.dirs = TRUE, pattern = "insite_meta.csv") %>%
  read_csv() 

# Load and summary meta data of online data: age, female numbers, language proficiency
osweb_meta <- dir(path = "../EXPDATA/",full.names = TRUE, recursive = TRUE, include.dirs = TRUE, pattern = "jatos_meta.csv") %>%
  read_csv() %>%
  mutate(gender = ifelse(gender==1,"FEMALE",ifelse(gender==2,"MALE","OTHER"))) %>%
  mutate(birth_year = as.numeric(birth_year)) %>%
  mutate(year = ifelse(birth_year > 21 & !is.na(birth_year), 1900 + birth_year, 2000 + birth_year)) %>%
  mutate(age = ifelse(!is.na(year),2021-year,NA)) %>%
  group_by(Batch) %>%
  summarise(N = n(), Female_N = sum(gender=="FEMALE",na.rm = TRUE), Age = mean(age, na.rm=TRUE), Proficiency = mean(lang_prof))

# Load raw data 
## Isolate the data before 2021, in site data
# Filter SP verification responses
SP_V <-  dir(path = "../EXPDATA/1_raw_data/",
             pattern = "all_rawdata_SP_V",   ## include in-site and internet data
             recursive = TRUE, full.names = TRUE) %>% 
  read_csv() %>%
  subset(correct == 1 & Match != "F") %>%  ## Exclude the incorrect responses and filler trials
  inner_join(select(lab_info, PSA_ID, Language), by = "PSA_ID") %>%
  distinct() ## Merge the language aspects


# Load PP verification responses
PP <- dir(path = "../EXPDATA/1_raw_data/",
          pattern = "all_rawdata_PP", 
          recursive = TRUE, full.names = TRUE) %>% 
  read_csv() %>%
  subset(correct == 1 & Identical != "F")  %>%  ## Exclude the incorrect responses and filler trials
  inner_join(select(lab_info, PSA_ID, Language), by = "PSA_ID") %>%
  distinct() ## Merge the language aspects

# Load SP memory responses
SP_M <- dir(path = "../EXPDATA/1_raw_data/",
            pattern = "all_rawdata_SP_M", 
            recursive = TRUE, full.names = TRUE) %>% 
  read_csv()  %>%    
  subset(correct == 1) %>%  ## Exclude the incorrect responses and filler trials
  inner_join(select(lab_info, PSA_ID, Language), by = "PSA_ID") %>%
  distinct() ## Merge the language aspects

## Exclude the participants who had a accuracy lower than the preregistered exclusion criterion (70%)

## Summarize the valid participants' SP verification data
SP_V_subj_site <- SP_V %>%
  filter(opensesame_codename!="osweb") %>%  # exclude jatos data
  group_by(subject_nr) %>%
  mutate(acc = n()/24) %>%
  filter(acc > .7) %>%
  group_by(Language, PSA_ID, subject_nr, Match) %>%
  summarise(V_RT = median(response_time), V_Acc = n()/12) 

## Tidy SP V data for mixed linear model
SP_V_site_tidy <- SP_V %>% 
  filter(opensesame_codename!="osweb") %>%
  inner_join(
    select(SP_V_subj_site, Language, PSA_ID, subject_nr),
    by=c("Language","PSA_ID","subject_nr")
  ) %>% 
  distinct() %>%
  bind_cols(Source = "site") ## mark for separated analysis

## Tidy SP M data for mixed linear model
SP_M_site_tidy <- SP_M %>% 
  filter(opensesame_codename!="osweb") %>%    # exclude jatos data
  inner_join(
    select(SP_V_subj_site, Language, PSA_ID, subject_nr),
    by=c("Language","PSA_ID","subject_nr")
  ) %>% 
  distinct() %>%
  bind_cols(Source = "site") ## mark for separated analysis

## Summarize the valid participants' SP memory data
SP_M_subj_site <- SP_M_site_tidy %>%
  group_by(Language, PSA_ID, subject_nr) %>% 
  summarise(M_Acc = n()/11)

## Tidy PP data for mixed linear model
PP_site_tidy <- PP %>% 
  filter(opensesame_codename!="osweb") %>%   # exclude jatos data
  inner_join(
    (select(SP_V_subj_site, Language, PSA_ID, subject_nr) #%>%
     #  bind_cols(logfile_trans = gsub(SP_V_subj_data$logfile, pattern = "_SP_",replacement = "_PP_"))
    ), ## replace the file name for compatible
    by=c("Language","PSA_ID","subject_nr")
  ) %>% 
  distinct() %>%
  bind_cols(Source = "site") ## mark for separated analysis


## Summarize the valid participants' PP verification data
PP_subj_site <- PP_site_tidy %>%
  #    group_by(subject_nr) %>%
  #    mutate(acc = n()/24) %>%
  #    filter(acc > .7) %>%
  mutate(Match = (Orientation1 == Orientation2)) %>%
  group_by(Language, PSA_ID, subject_nr, Match) %>%
  summarise(P_RT = median(response_time), P_Acc = n()/12) 

SP_V_osweb <-  dir(path = "../EXPDATA/1_raw_data/",
                   pattern = "all_rawdata_SP_V",   ## include in-site and internet data
                   recursive = TRUE, full.names = TRUE) %>% 
  read_csv() %>%
  filter(opensesame_codename=="osweb") %>%   # include jatos data
  subset(correct == 1 & Match != "F") %>%  ## Exclude the incorrect responses and filler trials
  inner_join(select(lab_info, PSA_ID, Language), by = "PSA_ID") %>%
  distinct() %>% ## Merge the language aspects
  filter(!(PSA_ID == "USA_033" & subject_nr == 39)) ## exclude this participant who had not complete PP

## Summarize the valid participants' SP verification data
SP_V_subj_osweb <- SP_V_osweb %>%
  group_by(subject_nr) %>%
  mutate(acc = n()/24) %>%
  filter(acc > .7) %>%
  group_by(Language, PSA_ID, subject_nr, Match) %>%
  summarise(V_RT = median(response_time), V_Acc = n()/12) 

## Tidy SP V data for mixed linear model
SP_V_osweb_tidy <- SP_V_osweb %>% inner_join(
  select(SP_V_subj_osweb, Language, PSA_ID, subject_nr),
  by=c("Language","PSA_ID","subject_nr")
) %>% 
  distinct() %>%
  bind_cols(Source = "site") ## mark for separated analysis

SP_M_osweb <-  dir(path = "../EXPDATA/1_raw_data/",
                   pattern = "all_rawdata_SP_M",   ## include in-site and internet data
                   recursive = TRUE, full.names = TRUE) %>% 
  read_csv() %>%
  filter(opensesame_codename=="osweb") %>%   # include jatos data
  inner_join(select(lab_info, PSA_ID, Language), by = "PSA_ID") %>%
  distinct() %>% ## Merge the language aspects
  filter(!(PSA_ID == "USA_033" & subject_nr == 39)) ## exclude this participant who had not complete PP


## Tidy SP M data for mixed linear model
SP_M_osweb_tidy <- SP_M_osweb %>% inner_join(
  select(SP_V_subj_osweb, Language, PSA_ID, subject_nr),
  by=c("Language","PSA_ID","subject_nr")
) %>% 
  distinct() %>%
  bind_cols(Source = "site") ## mark for separated analysis

## Summarize the valid participants' SP memory data
SP_M_subj_osweb <- SP_M_osweb_tidy %>%
  group_by(Language, PSA_ID, subject_nr) %>% 
  summarise(M_Acc = n()/11)

PP_osweb <-  dir(path = "../EXPDATA/1_raw_data/",
                 pattern = "all_rawdata_PP",   ## include in-site and internet data
                 recursive = TRUE, full.names = TRUE) %>% 
  read_csv() %>%
  filter(opensesame_codename=="osweb") %>%   # include jatos data
  subset(correct == 1 & Identical != "F")  %>%  ## Exclude the incorrect responses and filler trials
  inner_join(select(lab_info, PSA_ID, Language), by = "PSA_ID") %>%
  distinct() %>% ## Merge the language aspects
  filter(!(PSA_ID == "USA_033" & subject_nr == 39)) ## exclude this participant who had not complete PP

## Tidy PP data for mixed linear model
PP_osweb_tidy <- PP_osweb %>% inner_join(
  (select(SP_V_subj_osweb, Language, PSA_ID, subject_nr) ),
  by=c("Language","PSA_ID","subject_nr")
) %>% 
  distinct() %>%
  bind_cols(Source = "site") ## mark for separated analysis

## Summarize the valid participants' PP verification data
PP_subj_osweb <- PP_osweb_tidy %>%
  #    group_by(subject_nr) %>%
  #    mutate(acc = n()/24) %>%
  #    filter(acc > .7) %>%
  mutate(Match = (Orientation1 == Orientation2)) %>%
  group_by(Language, PSA_ID, subject_nr, Match) %>%
  summarise(P_RT = median(response_time), P_Acc = n()/12) 

## Merge tidy data and export to the external directory

bind_rows(SP_V_site_tidy,SP_V_osweb_tidy) %>% write_csv(file = "../EXPDATA/3_analysis/SP_V_tidy.csv")

bind_rows(SP_M_site_tidy,SP_M_osweb_tidy) %>% write_csv(file = "../EXPDATA/3_analysis/SP_M_tidy.csv")

bind_rows(PP_site_tidy,PP_osweb_tidy) %>% write_csv(file = "../EXPDATA/3_analysis/PP_tidy.csv")

