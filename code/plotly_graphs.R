source("code/00_load_data.R")
library(scales)
library(plotly)

save_plotly <- function(p, name) {

  p$sizingPolicy$padding <- 0

  name <- paste0(name, ".html")
  htmlwidgets::saveWidget(p, name)
  file.copy(name, "docs", overwrite = TRUE)
  file.rename(name, paste0("results/web_plots/", name))
  p
}


# City wide demographics --------------------------------------------------

citywide <- read_excel("data/original_data/demographicsnapshot201314to201718public_final.xlsx",
                       sheet = "Citywide") %>%
  clean_names()

cols <- c("#706AE0", "#16AC9E", "#F59F00", "#CB5871", "#82C91E")

citywide_demos <- citywide %>%
  filter(year == "2017-18") %>%
  select(number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white) %>%
  gather("race", "number") %>%
  mutate(prop = number/sum(number)) %>%
  mutate(race = str_remove(race, "number_") %>%
           str_replace_all("_", " ") %>%
           str_to_sentence() %>%
           str_wrap(width = 20) %>%
           reorder(-number)) %>%
  ggplot(aes(race, prop,
             text = paste(race, percent(prop), sep = "<br>"))) +
  geom_col(aes(fill = race), show.legend = FALSE) +
  # geom_text(aes(label = paste0(number(number,big.mark = ","),
  #                              " (", percent(prop) ,")")),
  #           family = "Times New Roman",
  #           vjust = -.5) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expand_scale(mult = .05, add = c(0, .01))) +
  labs(title = "Demographic breakdown of New York City Public Schools",
       subtitle = "For the 2017-18 school year",
       x = "Race/ethnicity",
       y = "Percent of students",
       caption = "Source: DOE Demographic Snapshot") +
  theme_nycc() +
  theme(panel.border = element_blank(),
        legend.position = "none")

ggplotly(citywide_demos, tooltip = "text") %>%
  config(displayModeBar = FALSE) %>%
  hide_legend() %>%
  layout(margin = list(l = 80)) %>%
  save_plotly("citywide_school_demos")

# Percent in poverty ------------------------------------------------------

more_75_poverty_race <- school_dems %>%
  filter(year == "2017-18", !str_detect(dbn, "^84"), percent_poverty > .75) %>%
  summarize_at(vars(starts_with("number_")), sum) %>%
  select(number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white)

num_race <- school_dems %>%
  filter(year == "2017-18", !str_detect(dbn, "^84")) %>%
  summarize_at(vars(starts_with("number_")), sum) %>%
  select(number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white)



dat <- (more_75_poverty_race/num_race) %>%
  gather("race", "prop") %>%
  mutate(race = str_remove(race, "number_") %>%
           str_replace_all("_", " ") %>%
           str_to_sentence() %>%
           str_wrap(width = 20) %>%
           reorder(-prop))
cols <- c("#706AE0", "#16AC9E", "#F59F00", "#82C91E", "#CB5871")
names(cols) <- levels(dat$race)

pov_plot <- ggplot(dat, aes(race, prop,
                            text = paste(race, percent(prop), sep = "<br>"))) +
  geom_col(aes(fill = race), show.legend = FALSE) +
  scale_fill_manual(values = cols) +
  # geom_text(aes(label = percent(prop)),
  #           family = "Times New Roman",
  #           vjust = -.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expand_scale(mult = .05, add = c(0, .01))) +
  labs(title = "Percentage of students attending a school where\nmore than 75% of students experience poverty",
       x = "Race/ethnicity",
       y = "Percent of students",
       caption = "Source: DOE Demographic Snapshot") +
  theme_nycc() +
  theme(panel.border = element_blank(),
        legend.position = "none")

ggplotly(pov_plot, tooltip = "text") %>%
  config(displayModeBar = FALSE) %>%
  hide_legend() %>%
  layout(margin = list(l = 80, t = 60)) %>%
  save_plotly("students_in_schools_75_pov")

# SHS Demographics --------------------------------------------------------

source("code/shs_diversity.R")

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
  ggplot(aes(school_name, prop, fill = race,
             text = paste(school_name, race, percent(prop), sep = "<br>"))) +
  geom_col(position = "stack") +
  # geom_text_repel(aes(y = prop, label = pretty_prop), position = "stack",
  #                 family = "Times New Roman",
  #                 size = 4.5,
  #                 min.segment.length = 0) +
  scale_fill_discrete(reverse = TRUE) +
  # coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Specialized high school diversity (2017-18 school year)",
       subtitle = "The percent of students of each race and ethnicity attending a specialized high school",
       y = "Percent of students",
       x = "",
       caption = "Source: DOE Demographic Snapshot",
       fill =  "Race/ethnicity") +
  theme_nycc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.border = element_blank())

shs_demo_plot %>%
  ggplotly(tooltip = "text") %>%
  config(displayModeBar = FALSE) %>%
  hide_legend() %>%
  layout(margin = list(l = 80)) %>%
  save_plotly("shs_demos")


# SHS Poverty -------------------------------------------------------------

{
  school_dems %>%
    filter(dbn %in% shs, year == "2017-18") %>%
    select(school_name,
           percent_poverty, economic_need_index) %>%
    mutate(school_name = str_wrap(school_name, width = 20) %>%
             reorder(percent_poverty)) %>%
    ggplot(aes(school_name, percent_poverty, text = paste(school_name, percent(percent_poverty), sep = "<br>"))) +
    geom_col(show.legend = FALSE, fill = "#2F56A6") +
    # ggplot2::scale_fill_gradient(low = "#228AE6", high = "#FFFFFF") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    coord_flip() +
    labs(title = "Students in poverty",
         x = "Specialized high schools",
         y = "Percent of students in poverty") +
    theme_nycc() +
    theme(legend.position = "bottom",
          # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.border = element_blank())
} %>%
  ggplotly(tooltip = "text") %>%
  config(displayModeBar = FALSE) %>%
  hide_legend() %>%
  layout(margin = list(l = 80)) %>%
  save_plotly("shs_poverty")

# Elementary school diversity ---------------------------------------------

source("code/elem_school_maps.R")

{
  rep[mins[,1],]$plots[[1]] +
    theme_nycc() +
    theme(legend.position = "none",
          panel.border = element_blank())
}%>%
  ggplotly(tooltip = "text") %>%
  config(displayModeBar = FALSE) %>%
  hide_legend() %>%
  layout(margin = list(l = 80)) %>%
  save_plotly("star_academy_demos")

{
  non_rep[mins[,2],]$plots[[1]] +
    theme_nycc() +
    theme(legend.position = "none",
          panel.border = element_blank())
  }%>%
  ggplotly(tooltip = "text") %>%
  config(displayModeBar = FALSE) %>%
  hide_legend() %>%
  layout(margin = list(l = 80)) %>%
  save_plotly("neighborhood_school_demos")

