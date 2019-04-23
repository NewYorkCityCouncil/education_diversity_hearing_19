library(sf)
library(leaflet)
library(mapview)
library(cowplot)
library(councildown)

school_locations <- st_read("https://data.cityofnewyork.us/resource/r2nx-nhxe.geojson?$limit=99999",
                            stringsAsFactors = FALSE,
                            quiet = TRUE)

kl_div <- function(p, q) {
  z <- p*log(q/p)
  z[p==0] <- 0
  -sum(z)
}

sj_div <- function(vals, baseline) {
  # vals <- c(...)
  vals[is.na(vals) | is.nan(vals)] <- 0
  m <- .5*(vals + baseline)

  .5*kl_div(vals, m) + .5*kl_div(baseline, m)
}


baseline <- school_dems %>%
  mutate(is_middle = grade_6 == 0 & grade_4 > 0) %>%
  filter(year == "2017-18", is_middle, !str_detect(dbn, "^84")) %>%
  select(school_name,
         year,
         dbn,
         number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white) %>%
  gather("race", "number", -school_name, -dbn, -year) %>%
  group_by(race) %>%
  summarize(number = sum(number)) %>%
  mutate(prop = number/sum(number)) %>%
  pull(prop)

school_divs <- school_dems %>%
  mutate(is_middle = grade_6 == 0 & grade_4 > 0) %>%
  filter(year == "2017-18", is_middle, !str_detect(dbn, "^84")) %>%
  select(school_name,
         year,
         dbn,
         number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white) %>%
  gather("race", "number", -school_name, -dbn, -year) %>%
  group_by(school_name) %>%
  mutate(prop = number/sum(number)) %>%
  ungroup() %>%
  group_by(school_name, dbn) %>%
  summarize(sj = sj_div(prop, baseline = baseline)) %>%
  left_join(school_locations %>%
              select(ats_system_code) %>%
              mutate(ats_system_code = str_trim(ats_system_code)), by = c("dbn" = "ats_system_code")) %>%
  st_as_sf()


plots <- school_dems %>%
  mutate(is_middle = grade_6 == 0 & grade_4 > 0) %>%
  filter(year == "2017-18", is_middle, !str_detect(dbn, "^84")) %>%
  select(school_name,
         year,
         dbn,
         number_asian,
         number_black,
         number_hispanic,
         number_multiple_race_categories_not_represented,
         number_white) %>%
  gather("race", "number", -school_name, -dbn, -year) %>%
  mutate(race = str_remove(race, "number_") %>%
           str_replace_all("_", " ") %>%
           str_to_sentence() %>%
           str_wrap(width = 15)) %>%
  group_by(school_name, dbn) %>%
  nest() %>%
  mutate(data = map(data, ~mutate(.x, prop = number/sum(number))),
         plots = map2(data, school_name, ~ggplot(.x, aes(race, prop)) +
                        geom_col() +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        labs(title = .y,
                             x = "Race",
                             y = "Percent of students") +
                        coord_flip() +
                        theme_nycc(print = TRUE)))

pal <- colorNumeric("Blues", school_divs$sj)

# diversity_map_elem <- school_divs %>%
#   left_join(plots, by = "dbn") %>%
#   leaflet() %>%
#   addCouncilStyle(add_dists = FALSE) %>%
#   addCircleMarkers(radius = 3, stroke = FALSE, fillOpacity = .9,
#                    fillColor = ~pal(sj),
#                    popup = ~mapview::popupGraph(plots)) %>%
#   addLegend(pal = pal, values = ~sj)


tmp <- school_divs %>%
  left_join(plots, by = "dbn")

rep <- tmp %>%
  mutate(is_rep = map_lgl(data, function(x){
    tmp2 <- select(x, -number) %>%
      spread(race, prop) %>%
      mutate(tmp2 = Black + Hispanic) %>%
      pull(tmp2)
    tmp2 > .5 & tmp2 < .9
  })) %>%
  filter(is_rep)

non_rep <- tmp %>%
  mutate(is_rep = map_lgl(data, function(x){
    tmp2 <- select(x, -number) %>%
      spread(race, prop) %>%
      mutate(tmp2 = Black + Hispanic) %>%
      pull(tmp2)
    tmp2 > .5 & tmp2 < .9
  })) %>%
  filter(!is_rep)

dists <- st_distance(rep, non_rep)

mins <- which(dists == min(dists), arr.ind = TRUE)

rep[mins[,1],] %>%
  select(-plots) %>%
  unnest()

non_rep[mins[,2],] %>%
  select(-plots) %>%
  unnest()


plots <- map2(rep[mins[,1],]$plots, non_rep[mins[,2],]$plots, ~plot_grid(.x, .y, align = "hv"))

# map2(plots, 1:length(plots), ~ggsave(paste0("plot_", .y, ".jpg"), .x, , width = 10, height = 5))