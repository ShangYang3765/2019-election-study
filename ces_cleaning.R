library(haven)
library(tidyverse)

cps_raw <- read_dta("ces_raw/2019 Canadian Election Study - Online Survey v1.0.dta")

cps_reduced <- cps_raw %>%
  select(
    cps19_ResponseId,
    cps19_citizenship,
    cps19_gender,
    cps19_province,
    cps19_education,
    cps19_votechoice
  ) %>%
  filter(cps19_citizenship==4) %>%
  filter(cps19_gender==1|cps19_gender==2) %>%
  filter(cps19_votechoice==1|cps19_votechoice==2|cps19_votechoice==3|
           cps19_votechoice==4|cps19_votechoice==5|cps19_votechoice==6|
           cps19_votechoice==7) %>%
  filter(cps19_education==1|cps19_education==2|cps19_education==3|
           cps19_education==4|cps19_education==5|cps19_education==6|
           cps19_education==7|cps19_education==8|cps19_education==9|
           cps19_education==10|cps19_education==11)

cps_filtered <- na.omit(cps_reduced)
cps_filtered <- labelled::to_factor(cps_filtered)

cps_mapping <- cps_filtered %>%
  mutate(cps19_gender=case_when(cps19_gender=="A man"~"Male", 
                                cps19_gender=="A woman"~"Female")) %>%
  mutate(cps19_education=case_when(cps19_education=="No schooling"|
                                     cps19_education=="Some elementary school"|
                                     cps19_education=="Completed elementary school"|
                                     cps19_education=="Some secondary/ high school" ~
                                     "No certificate, diploma or degree",
                                   cps19_education=="Completed secondary/ high school"|
                                     cps19_education=="Some technical, community college, CEGEP, College Classique"|
                                     cps19_education=="Some university" ~ 
                                     "Secondary (high) school diploma or equivalency certificate",
                                   cps19_education=="Completed technical, community college, CEGEP, College Classique" ~
                                     "College, CEGEP or other non-university certificate or diploma",
                                   cps19_education=="Bachelor's degree" ~ "Bachelor's degree",
                                   cps19_education=="Master's degree" ~ "Master's degree",
                                   cps19_education=="Professional degree or doctorate" ~ 
                                     "Professional degree or doctorate"
                                   )
         )

write_csv(cps_mapping, "ces_data")