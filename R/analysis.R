make_acquis_time_sum <- function(data) {
  acquis_time_sum <- data %>%
    group_by(date) %>%
    summarise(sum = sum(across(where(is.numeric))))

  return(acquis_time_sum)
}

make_acquis_time_source <- function(data) {
  acquis_time_source <- data %>%
    # group_by(date) %>
    summarise(across(where(is.numeric), sum)) %>%
    pivot_longer(everything(), names_to = "source", values_to = "total") %>%
    mutate(total_percent = total/sum(.$total)) %>%
    mutate(total_percent = round(total_percent*100)) %>%
    arrange(desc(total))

  return(acquis_time_source)
}

plot_engage_usage_time <- function(data) {
  fig_engag_usage_time <- data %>%
  select(!where(~ is.numeric(.) &&
                  sum(.) <= 200)) %>%
  pivot_longer(cols = -date,) %>%
  rename(session_source = name,
          users = value) %>%
  filter(between(date,
                  as.Date("2022-05-02"),
                  as.Date("2022-05-30"))) %>%
  ggplot(.) +
  aes(x = date,
      y = users,
      fill = session_source) +
  geom_area() +
  # geom_text(
  #   data = media,
  #   aes(
  #     x = published_date_time_AEST, #TODO: maybe this has something to do with the ymd format I got it in
  #     label = platform_name,
  #     y = 0
  #     )
  #   ) +
  scale_fill_viridis(option = "H",
                      discrete = TRUE) +
  ylim(0, 5400) +
  theme_minimal() +
  labs(x = NULL,
        y = "Number of users",
        fill = "Acquisition") +
  annotate(
    "segment",
    x = as.Date("2022-05-03"),
    xend = as.Date("2022-05-03"),
    y = 2500,
    yend = 2700
  ) +
  annotate(
    "text",
    x = as.Date("2022-05-05"),
    y = 2850,
    label = "The Conversation",
    size = 2.5
  ) +
  # annotate("segment", x = as.Date("2022-05-15"), xend = as.Date("2022-05-15"), y = 5150, yend = 5300) +
  annotate(
    "text",
    x = as.Date("2022-05-15"),
    y = 5300,
    label = "ABC Science",
    size = 2.5
  ) +
  annotate(
    "segment",
    x = as.Date("2022-05-24"),
    xend = as.Date("2022-05-24"),
    y = 500,
    yend = 750
  ) +
  annotate(
    "text",
    x = as.Date("2022-05-25"),
    y = 900,
    label = "Behind the News",
    size = 2.5
  ) +
  annotate(
    "segment",
    x = as.Date("2022-05-21"),
    xend = as.Date("2022-05-21"),
    y = 300,
    yend = 1150
  ) +
  annotate(
    "text",
    x = as.Date("2022-05-21"),
    y = 1300,
    label = "Election day",
    size = 2.5
  )

  ggsave(here("output/figures/engag_usage_time.png"), fig_engag_usage_time, units = "cm", width = 12, height = 15, dpi = 300)

  return(fig_engag_usage_time)
}

mk_species_title_spatial <- function(data, data_join) {
  species_title_spatial <- data %>%
    full_join(., data_join) %>%
    group_by(electorate, state_territory, state_territory_abbrev, demographic_class) %>%
    summarise(
      active_users = sum(active_users),
      engaged_sessions = sum(engaged_sessions),
      event_count = sum(event_count)
    ) %>%
    ungroup() %>%
      st_union(by_feature = TRUE)

  return(species_title_spatial)
}

plot_ced_events <- function(data) {
  CED_events <- tm_shape(data,
          # bbox = st_bbox(
          #   c(
          #     xmin = 112.929704,
          #     ymin = -43.627943,
          #     xmax = 155.629864,
          #     ymax = -9.115517
          #   )
          # )
          ) +
    tm_fill(
      "event_count",
      style = "cont",
      # style = "cont",
      title = "Event counts",
      palette = "-inferno"
    ) +
    tm_borders(
      alpha = .6
    ) +
    tm_text(
      "electorate",
      size = "AREA"
    ) +
    tm_compass(
      # position = c(0.32, 0.02),
      position = c("left", "bottom"),
      size = 1.4
    ) +
    tm_scale_bar(
      width = .2,
      position = c("left", "bottom")
    ) +
    tm_layout(
      frame = FALSE,
      # legend.width = .25,
      legend.height = .25,
      # legend.title.size = .8
      # legend.outside = TRUE
      # legend.position = c("left", "BOTTOM")
    )

  tmap_save(CED_events, here("output/figures/CED_events.png"), units = "cm",width = 10, height = 10, dpi = 300)

  return(CED_events)
}

mk_svg_high_engag_species_map <- function(data) {

  aus <- ozmap(x = "country")

  select_species <- data %>%
    filter(vernacular_name_first_clean %in% c("Helmeted Honeyeater", "Olive Python ", "Northern Bettong", "Butler's Dunnart", "Grassland Earless Dragon", "Elegant Frog"))

  species_engag_base_map <- tm_shape(aus,
          bbox = st_bbox(
            c(
              xmin = 112.929704,
              ymin = -43.627943,
              xmax = 155.629864,
              ymax = -9.115517
            )
          )) +
  tm_polygons() +
  tm_layout(frame = FALSE) +
  tm_shape(select_species) +
  tm_polygons("black", border.alpha = .4) +
  tm_compass(
    position = c("left", "bottom"),
    size = 1.2) +
  tm_scale_bar(
    position = c("left", "bottom"),
    width = .2)

  tmap_save(
    species_engag_base_map,
    here("output/figures/engag_case_studies/species_engag_base_map.svg")
  )

}