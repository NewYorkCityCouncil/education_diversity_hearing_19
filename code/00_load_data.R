library(tidyverse)
library(readxl)
library(janitor)
library(councildown)
library(ggrepel)

school_dems <- read_excel("data/original_data/demographicsnapshot201314to201718public_final.xlsx",
                          sheet = "School") %>%
  clean_names()



