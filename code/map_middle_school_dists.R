library(sf)
library(leaflet)
library(mapview)

school_locations <- st_read("https://data.cityofnewyork.us/resource/r2nx-nhxe.geojson?$limit=99999", stringsAsFactors = FALSE)

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
  mutate(is_middle = grade_6 > 0) %>%
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
  mutate(is_middle = grade_6 > 0) %>%
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
  mutate(is_middle = grade_6 > 0) %>%
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
  mutate(plots = map2(data, school_name, ~ggplot(.x, aes(race, number)) + geom_col() + labs(title = .y) + coord_flip())) %>%
  select(dbn, plots)

pal <- colorNumeric("Blues", school_divs$sj)

school_divs %>%
  left_join(plots, by = "dbn") %>%
  leaflet() %>%
  addCouncilStyle(add_dists = FALSE) %>%
  addCircleMarkers(radius = 3, stroke = FALSE, fillOpacity = .9,
                   fillColor = ~pal(sj),
                   popup = ~mapview::popupGraph(plots)) %>%
  addLegend(pal = pal, values = ~sj)
