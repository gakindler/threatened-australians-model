load_json_data <- function() {

  json_paths <- list.files(
    path = here("data/clean_data"), pattern = "*.json", full.names = TRUE
  )

  named_list <- list()

  for (file_path in json_paths) {

    file_name <- tools::file_path_sans_ext(basename(file_path))

    dataframe <- as_tibble(fromJSON(file_path))

    named_list[[file_name]] <- dataframe
  }

  named_list$acquis_time_clean <- named_list$acquis_time_clean |>
    mutate(date = as.Date(date))

  named_list$media_clean <- named_list$media_clean |>
    mutate(
      published_date_time_AEST = ymd_hms(published_date_time_AEST)
    )

  return(named_list)
}

load_spatial_data <- function() {

  gpkg_paths <- list.files(
    path = here("data/clean_data"), pattern = "*.gpkg", full.names = TRUE
  )

  named_list <- list()

  for (file_path in gpkg_paths) {

    file_name <- tools::file_path_sans_ext(basename(file_path))

    dataframe <- st_read(file_path)

    named_list[[file_name]] <- dataframe
  }

  named_list$elect_clean <- named_list$elect_clean %>%
    ms_simplify(.,
      keep = 0.01,
      keep_shape = TRUE
    ) %>%
    st_make_valid()

  return(named_list)
}