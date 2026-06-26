# Code to prepare `DJFMP` dataset

# Main custom cleaning function
transform_DJFMP_data <- function(ls_data) {
  # Explicitly pull out each data frame from the standardized list
  df_stations <- ls_data$stations
  df_data_1976_2001 <- ls_data$data_trawl_1976_2001
  df_data_2002_curr <- ls_data$data_trawl_2002_curr

  # Combine and finish cleaning data
  df_clean <- dplyr::bind_rows(df_data_1976_2001, df_data_2002_curr) |>
    # Remove conductivity data from dates before it was standardized >
    # Methods in metadata say they do not know if their data were corrected for
    # temperature before May 3 or 17 2019 so we will not use conductivity data before
    # June 2019
    dplyr::mutate(
      Conductivity = dplyr::if_else(Date < "2019-06-01", NA_real_, Conductivity)
    ) |>
    # Remove duplicated rows
    dplyr::distinct(Station, Datetime, .keep_all = TRUE) |>
    # Add station coordinates
    dplyr::left_join(df_stations, by = dplyr::join_by(Station)) |>
    dplyr::arrange(Datetime)

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  cli::cli_alert_success("Custom cleaning complete")

  return(df_clean)
}
