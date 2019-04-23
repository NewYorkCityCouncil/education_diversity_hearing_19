library(tidyverse)
library(readxl)
library(janitor)
library(councildown)
library(ggrepel)

school_dems <- read_excel("data/original_data/demographicsnapshot201314to201718public_final.xlsx",
                          sheet = "School") %>%
  clean_names() %>%
  mutate(school_name = case_when(
    school_name == "Queens High School for the Sciences at York Colleg" ~ "Queens High School for the Sciences at York College",
    school_name == "High School for Mathematics, Science and Engineeri" ~ "High School for Mathematics, Science and Engineering",
    school_name == "Brooklyn Latin School, The" ~ "The Brooklyn Latin School",
    TRUE ~ school_name
  ))
