shs <- c("10X445",
         "13K430",
         "10X696",
         "31R605",
         "02M475",
         "14K449",
         "05M692",
         "28Q687")

shs_demo_plot <- school_dems %>%
  filter(dbn %in% shs, year == "2017-18") %>%
  select(school_name,
         number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white) %>%
  gather("race", "number", -school_name) %>%
  group_by(school_name) %>%
  mutate(prop = number/sum(number),
         pretty_prop = ifelse(race %in% c("number_black", "number_hispanic", "number_multiple_race_categories_not_represented"), scales::percent(prop, accuracy = 1), NA)) %>%
  ungroup() %>%
  mutate(race = str_remove(race, "number_") %>%
           str_replace_all("_", " ") %>%
           str_to_sentence() %>%
           # str_wrap(width = 30) %>%
           reorder(number),
         school_name = str_wrap(school_name, 20)) %>%
  ggplot(aes(school_name, prop, fill = race)) +
  geom_col(position = "stack") +
  # geom_text_repel(aes(y = prop, label = pretty_prop), position = "stack",
  #                 family = "Times New Roman",
  #                 size = 4.5,
  #                 min.segment.length = 0) +
  scale_fill_discrete(reverse = TRUE) +
  # coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Specialized high school diversity (2017-18 school year)",
       subtitle = "The percent of students of each race attending a specialized high school",
       y = "Percent of students",
       x = "",
       caption = "Source: DOE Demographic Snapshot",
       fill =  "Race") +
  theme_nycc(print = TRUE) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



