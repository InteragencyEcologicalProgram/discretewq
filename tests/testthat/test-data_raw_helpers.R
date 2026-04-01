
# Global checking functions ----------------------------------------------

## Testing data ----------------------------------------------------------

df_test_int <- tibble::tibble(
  col1 = rep(letters, times = 100),
  col2 = as.character(seq_len(length(col1))),
  col3 = paste0(col1, "_dup")
)

# Add file_name and file_path attributes to df_test_int
df_test_int <- structure(
  df_test_int,
  src_file = "test_file_name",
  src_path = "test_file_path/test_file_name"
)

df_test_int_error <- dplyr::mutate(
  df_test_int,
  col1 = dplyr::if_else(col1 == "a", NA_character_, col1)
)

## Tests -----------------------------------------------------------------

test_that("int_check_dataframe() produces expected errors and error messages", {
  expect_no_error(int_check_dataframe(mtcars))
  expect_error(
    int_check_dataframe(c(seq_len(10))),
    "must be a data.frame, not a integer"
  )
  expect_error(
    int_check_dataframe(letters),
    "must be a data.frame, not a character"
  )
  expect_error(
    int_check_dataframe(runif(10)),
    "must be a data.frame, not a numeric"
  )
  expect_error(
    int_check_dataframe(list(a = letters, b = runif(10))),
    "must be a data.frame, not a list"
  )
  expect_error(
    int_check_dataframe(data.frame()),
    "must contain data and not be empty"
  )
})

test_that("int_check_parsing() produces expected errors and messages, but returns NULL invisibly", {
  expect_message(
    res <- int_check_parsing(df_test_int, df_test_int),
    "All columns parsed correctly"
  )
  expect_invisible(expect_null(res))
  expect_no_error(
    suppressMessages(
      int_check_parsing(df_test_int, df_test_int)
    )
  )
  expect_error(
    capture.output(suppressMessages(
      int_check_parsing(df_test_int, df_test_int_error)
    )),
    "Data NOT converted"
  )
  expect_snapshot(
    int_check_parsing(df_test_int, df_test_int_error),
    error = TRUE
  )
})

test_that("int_get_data_dims() produces expected messages", {
  expect_snapshot(int_get_data_dims(df_test_int))
})


# Data download functions ------------------------------------------------

## Testing data ----------------------------------------------------------

test_data_ent <- c("data_ent1", "data_ent2")

## Tests -----------------------------------------------------------------

test_that("subset_data_entity() returns expected data entity string", {
  expect_equal(subset_data_entity(test_data_ent, "data_ent1"), "data_ent1")
  expect_equal(subset_data_entity(test_data_ent, "1$"), "data_ent1")
})

test_that("subset_data_entity() produces expected errors", {
  expect_no_error(subset_data_entity(test_data_ent, "data_ent1"))
  expect_error(subset_data_entity(test_data_ent, "^data"))
  expect_snapshot(subset_data_entity(test_data_ent, "^data"), error = TRUE)
  expect_error(subset_data_entity(test_data_ent, "^foo"))
  expect_snapshot(subset_data_entity(test_data_ent, "^foo"), error = TRUE)
})

test_that("get_update_info() produces expected error with an invalid 'survey' entry", {})
test_that("get_update_info() produces expected error with YBFMP and an invalid 'data_type' entry", {})
test_that("get_update_info() produces expected error when 'data_type' is not NULL with
surveys other than YBFMP", {})

test_that("get_latest_edi_id() produces expected error with an invalid 'last_rev' entry", {})

test_that("get_edi_data() produces expected errors with invalid argument entries", {})
test_that("get_edi_data() successfully downloads files to a temporary directory", {})
test_that("get_edi_data() produces the expected messaging when there are no problems to address", {})
test_that("get_edi_data() returns a named vector", {})

test_that("get_cnra_data_lab() produces expected errors with invalid start and end date entries", {})
test_that("get_cnra_data_lab() successfully downloads files to a temporary directory", {})
test_that("get_cnra_data_lab() produces the expected messaging when there are no problems to address", {})
test_that("get_cnra_data_lab() returns NULL invisibly", {})

test_that("get_cnra_data_field() produces expected errors with invalid start and end date entries", {})
test_that("get_cnra_data_field() successfully downloads files to a temporary directory", {})
test_that("get_cnra_data_field() produces the expected messaging when there are no problems to address", {})
test_that("get_cnra_data_field() returns NULL invisibly", {})

test_that("get_scibase_data() produces expected error with invalid 'entity_regex' entry", {})
test_that("get_scibase_data() successfully downloads files to a temporary directory", {})
test_that("get_scibase_data() produces the expected messaging when there are no problems to address", {})
test_that("get_scibase_data() returns NULL invisibly", {})

test_that("get_usgs_samples_data() successfully downloads files to a temporary directory", {})
test_that("get_usgs_samples_data() produces the expected messaging when there are no problems to address", {})
test_that("get_usgs_samples_data() returns NULL invisibly", {})


# standardize_col_meta() -------------------------------------------------

## Testing data ----------------------------------------------------------

df_test_std_col_raw <- tibble::tibble(
  col1 = letters,
  col2 = as.character(seq_len(length(letters))),
  col3 = paste0(col1, "_dup")
)

# Add file_name and file_path attributes to df_test_std_col_raw
df_test_std_col_raw <- structure(
  df_test_std_col_raw,
  src_file = "test_file_name",
  src_path = "test_file_path/test_file_name"
)

df_test_std_col_raw_error <- dplyr::mutate(
  df_test_std_col_raw,
  col1 = dplyr::if_else(col1 == "a", NA_character_, col1),
  col2 = dplyr::if_else(col2 == "1", "x", col2)
)

df_test_std_col_meta <- tibble::tibble(
  Col_name_exp = names(df_test_std_col_raw),
  Col_name_new = Col_name_exp,
  Col_type = c("character", "numeric", "character")
)

df_test_std_col_meta_error <- dplyr::mutate(
  df_test_std_col_meta,
  Col_name_exp = dplyr::if_else(Col_name_exp == "col1", "col1a", Col_name_exp)
)

df_test_std_col_meta_select <- dplyr::filter(
  df_test_std_col_meta,
  Col_name_exp != "col3"
)

df_test_std_col_meta_rename <- dplyr::mutate(
  df_test_std_col_meta,
  Col_name_new = paste0(Col_name_exp, "_new"),
  Col_type = "character"
)

## Tests -----------------------------------------------------------------

test_that("standardize_col_meta() produces expected column names check", {
  expect_error(
    suppressMessages(
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_error)
    ),
    "The following expected columns are NOT present in the data frame: col1a"
  )
  expect_snapshot(
    standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_error),
    error = TRUE
  )
})

test_that("standardize_col_meta() selects the appropriate columns", {
  expect_equal(
    names(suppressMessages(
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_select)
    )),
    df_test_std_col_meta_select$Col_name_new
  )
})

test_that("standardize_col_meta() converts specified columns to numeric", {
  expect_true(
    is.numeric(
      suppressMessages(
        standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta)
      )$col2
    )
  )
})

test_that("standardize_col_meta() produces expected errors and messages when converting columns to numeric", {
  expect_error(
    capture.output(suppressMessages(suppressWarnings(
      standardize_col_meta(df_test_std_col_raw_error, df_test_std_col_meta)
    ))),
    "Data NOT converted"
  )
  expect_snapshot(
    standardize_col_meta(df_test_std_col_raw_error, df_test_std_col_meta),
    error = TRUE
  )
})

test_that("standardize_col_meta() properly renames columns", {
  expect_equal(
    names(suppressMessages(
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_rename)
    )),
    df_test_std_col_meta_rename$Col_name_new
  )
})

test_that("standardize_col_meta() produces the expected messaging when there are no problems to address", {
  expect_no_error(suppressMessages(
    standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta)
  ))
  expect_snapshot(
    invisible(standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta))
  )
})

test_that("standardize_col_meta() returns a data frame", {
  expect_s3_class(
    suppressMessages(
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta)
    ),
    "data.frame"
  )
})


# Date and Time conversion functions -------------------------------------

## Testing data ----------------------------------------------------------

df_test_dt_raw <-
  tibble::tibble(
    date_raw = lubridate::make_date(day = seq_len(10)),
    datetime_raw = lubridate::make_datetime(
      day = seq_len(10),
      hour = seq_len(10)
    ),
    Date = format(date_raw, "%Y-%m-%d"),
    Time = paste0(as.character(seq_len(10)), ":00"),
    Datetime = format(datetime_raw, "%Y-%m-%d %H:%M:%S")
  )

# Add file_name and file_path attributes to df_test_dt_raw
df_test_dt_raw <- structure(
  df_test_dt_raw,
  src_file = "test_file_name",
  src_path = "test_file_path/test_file_name"
)

df_test_date_time_c <- dplyr::select(df_test_dt_raw, Date, Time)

df_test_date_time_c2 <- dplyr::mutate(
  df_test_date_time_c,
  Date = lubridate::ymd(Date),
  Time = paste0(Time, ":00")
)

df_test_datetime <- dplyr::select(df_test_dt_raw, Datetime)

## Tests -----------------------------------------------------------------

test_that("convert_date() produces expected errors and messages", {
  expect_no_error(
    suppressMessages(convert_date(df_test_date_time_c, "Ymd"))
  )
  expect_snapshot(invisible(convert_date(df_test_date_time_c, "Ymd")))
  expect_error(
    capture.output(suppressMessages(suppressWarnings(
      convert_date(df_test_date_time_c, "mdy")
    ))),
    "Data NOT converted"
  )
  expect_snapshot(convert_date(df_test_date_time_c, "mdy"), error = TRUE)
})

test_that("convert_date() returns a data frame with the Date column converted to date format", {
  date_test <- suppressMessages(convert_date(df_test_date_time_c, "Ymd"))
  expect_s3_class(date_test, "data.frame")
  expect_true(any(names(date_test) == "Date"))
  expect_s3_class(date_test$Date, "Date")
})

test_that("convert_time() produces expected errors and messages", {
  expect_no_error(
    suppressMessages(convert_time(df_test_date_time_c, "HM"))
  )
  expect_snapshot(invisible(convert_time(df_test_date_time_c, "HM")))
  expect_error(
    capture.output(suppressMessages(suppressWarnings(
      convert_time(df_test_date_time_c, "HMS")
    ))),
    "Data NOT converted"
  )
  expect_snapshot(convert_time(df_test_date_time_c, "HMS"), error = TRUE)
})

test_that("convert_time() returns a data frame with the Time column converted to character as HH:MM:SS format", {
  time_test <- suppressMessages(convert_time(df_test_date_time_c, "HM"))
  expect_s3_class(time_test, "data.frame")
  expect_true(any(names(time_test) == "Time"))
  expect_type(time_test$Time, "character")
  expect_match(time_test$Time, "^\\d{2}:\\d{2}:\\d{2}$", all = TRUE)
})

test_that("convert_datetime() produces expected errors and messages", {
  expect_no_error(
    suppressMessages(
      convert_datetime(
        df_test_datetime,
        "Ymd HMS",
        timezone = "America/Los_Angeles"
      )
    )
  )
  expect_snapshot(
    invisible(
      convert_datetime(
        df_test_datetime,
        "Ymd HMS",
        timezone = "America/Los_Angeles"
      )
    )
  )
  expect_error(
    capture.output(suppressMessages(suppressWarnings(
      convert_datetime(
        df_test_datetime,
        "mdy HM",
        timezone = "America/Los_Angeles"
      )
    ))),
    "Data NOT converted"
  )
  expect_snapshot(
    convert_datetime(
      df_test_datetime,
      "mdy HM",
      timezone = "America/Los_Angeles"
    ),
    error = TRUE
  )
})

test_that("convert_datetime() returns a data frame with the Datetime column converted to
POSIXct format in the 'America/Los_Angeles' timezone. Additionally, convert_datetime() creates
a Date column in date format from the Datetime column", {
  datetime_test <- suppressMessages(
    convert_datetime(
      df_test_datetime,
      "Ymd HMS",
      timezone = "Etc/GMT+8"
    )
  )
  expect_s3_class(datetime_test, "data.frame")
  expect_s3_class(datetime_test$Datetime, c("POSIXct", "POSIXt"))
  expect_equal(lubridate::tz(datetime_test$Datetime), "America/Los_Angeles")
  expect_equal(names(datetime_test), c("Date", "Datetime"))
  expect_s3_class(datetime_test$Date, "Date")
  expect_equal(datetime_test$Date, lubridate::date(datetime_test$Datetime))
})

test_that("combine_datetime() returns a data frame with a Datetime column in POSIXct format
created from the Date and Time columns. The Datetime column is in the 'America/Los_Angeles'
timezone. combine_datetime() keeps the Date column but removes the Time column", {
  comb_datetime_test <- combine_datetime(
    df_test_date_time_c2,
    timezone = "Etc/GMT+8"
  )
  expect_s3_class(comb_datetime_test, "data.frame")
  expect_s3_class(comb_datetime_test$Datetime, c("POSIXct", "POSIXt"))
  expect_equal(
    lubridate::tz(comb_datetime_test$Datetime),
    "America/Los_Angeles"
  )
  expect_equal(names(comb_datetime_test), c("Date", "Datetime"))
  expect_all_false(names(comb_datetime_test) == "Time")
})


# Secchi and Depth conversion functions ----------------------------------

## Testing data ----------------------------------------------------------

df_test_depth <- tibble::tibble(
  Depth = runif(10, min = 1, max = 10),
  Depth_m = Depth,
  Depth_ft2m = Depth * 0.3048
)

df_test_secchi <- tibble::tibble(
  Secchi = runif(10, min = 1, max = 10),
  Secchi_cm = Secchi,
  Secchi_m2cm = Secchi * 100
)

## Tests -----------------------------------------------------------------

test_that("convert_depth() produces expected messages", {
  expect_message(
    convert_depth(df_test_depth, "meters"),
    "Depth column is in meters. No conversion necessary."
  )
  expect_message(
    convert_depth(df_test_depth, "feet"),
    "Depth column converted from feet to meters."
  )
})

test_that("convert_depth() returns a data frame with the Depth column in the numeric format", {
  depth_test <- suppressMessages(convert_depth(df_test_depth, "meters"))
  expect_s3_class(depth_test, "data.frame")
  expect_true(any(names(depth_test) == "Depth"))
  expect_true(is.numeric(depth_test$Depth))
})

test_that("convert_depth() properly converts Depth column to meters when 'depth_unit' argument is 'feet'", {
  expect_equal(
    suppressMessages(convert_depth(df_test_depth, "feet"))$Depth,
    df_test_depth$Depth_ft2m
  )
})

test_that("convert_depth() properly keeps Depth column in meters when 'depth_unit' argument is 'meters'", {
  expect_equal(
    suppressMessages(convert_depth(df_test_depth, "meters"))$Depth,
    df_test_depth$Depth_m
  )
})

test_that("convert_secchi() produces expected messages", {
  expect_message(
    convert_secchi(df_test_secchi, "cm"),
    "Secchi column is in centimeters. No conversion necessary."
  )
  expect_message(
    convert_secchi(df_test_secchi, "meters"),
    "Secchi column converted from meters to centimeters."
  )
})

test_that("convert_secchi() returns a data frame with the Secchi column in the numeric format", {
  secchi_test <- suppressMessages(convert_secchi(df_test_secchi, "cm"))
  expect_s3_class(secchi_test, "data.frame")
  expect_true(any(names(secchi_test) == "Secchi"))
  expect_true(is.numeric(secchi_test$Secchi))
})

test_that("convert_secchi() properly converts Secchi column to centimeters when 'depth_unit'
argument is 'meters'", {
  expect_equal(
    suppressMessages(convert_secchi(df_test_secchi, "meters"))$Secchi,
    df_test_secchi$Secchi_m2cm
  )
})

test_that("convert_secchi() properly keeps Secchi column in centimeters when 'depth_unit'
argument is 'cm'", {
  expect_equal(
    suppressMessages(convert_secchi(df_test_secchi, "cm"))$Secchi,
    df_test_secchi$Secchi_cm
  )
})


# standardize_tide_code() ------------------------------------------------

## Testing data ----------------------------------------------------------

df_test_tide <- tibble::tibble(Tide = rep(1L:4L, times = 3))

## Tests -----------------------------------------------------------------

test_that("standardize_tide_code() produces expected error message when values in Tide
column are invalid", {})

test_that("standardize_tide_code() returns a data frame with the Tide column converted
to character format containing one of four categories (Ebb, Flood, High Slack, Low Slack)", {
  tide_test <- standardize_tide_code(df_test_tide)
  tide_cat <- c("Ebb2", "Flood", "High Slack", "Low Slack")
  expect_s3_class(tide_test, "data.frame")
  expect_all_true(names(tide_test) == "Tide")
  expect_type(tide_test$Tide, "character")
  expect_setequal(
    unique(tide_test$Tide),
    c("Ebb", "Flood", "High Slack", "Low Slack")
  )
})


# Latitude and Longitude functions ---------------------------------------

## Testing data ----------------------------------------------------------

df_test_lat_long_dm <- tibble::tibble(
  Lat_Deg = sample(37L:39L, size = 10, replace = TRUE),
  Lat_Min = round(runif(10, min = 0, max = 59), 1),
  Long_Deg = sample(121L:123L, size = 10, replace = TRUE),
  Long_Min = round(runif(10, min = 0, max = 59), 1)
)

df_test_lat_long_dm_c <- df_test_lat_long_dm |>
  dplyr::mutate(
    Latitude = paste(Lat_Deg, Lat_Min),
    Longitude = paste(Long_Deg, Long_Min),
    .keep = "unused"
  )

df_test_lat_long_dms <- tibble::tibble(
  Lat_Deg = sample(37L:39L, size = 10, replace = TRUE),
  Lat_Min = sample(0L:59L, size = 10, replace = TRUE),
  Lat_Sec = round(runif(10, min = 0, max = 59), 1),
  Long_Deg = sample(121L:123L, size = 10, replace = TRUE),
  Long_Min = sample(0L:59L, size = 10, replace = TRUE),
  Long_Sec = round(runif(10, min = 0, max = 59), 1)
)

df_test_lat_long_dms_c <- df_test_lat_long_dms |>
  dplyr::mutate(
    Latitude = paste(Lat_Deg, Lat_Min, Lat_Sec),
    Longitude = paste(Long_Deg, Long_Min, Long_Sec),
    .keep = "unused"
  )

## Tests -----------------------------------------------------------------

test_that("separate_lat_long() returns a data frame with columns for separate coordinate
components (Degrees/Minutes or Degrees/Minutes/Seconds) in the numeric format", {
  lat_long_dm_c_test <- separate_lat_long(
    df_test_lat_long_dm_c,
    delim_chr = " ",
    coord_comp = "DM"
  )
  expect_s3_class(lat_long_dm_c_test, "data.frame")
  expect_equal(
    names(lat_long_dm_c_test),
    c("Lat_Deg", "Lat_Min", "Long_Deg", "Long_Min")
  )
  expect_all_true(
    sapply(lat_long_dm_c_test[names(lat_long_dm_c_test)], is.numeric)
  )

  lat_long_dms_c_test <- separate_lat_long(
    df_test_lat_long_dms_c,
    delim_chr = " ",
    coord_comp = "DMS"
  )
  expect_s3_class(lat_long_dms_c_test, "data.frame")
  expect_equal(
    names(lat_long_dms_c_test),
    c("Lat_Deg", "Lat_Min", "Lat_Sec", "Long_Deg", "Long_Min", "Long_Sec")
  )
  expect_all_true(
    sapply(lat_long_dms_c_test[names(lat_long_dms_c_test)], is.numeric)
  )
})

test_that("separate_lat_long() produces an error when the 'coord_comp' argument is mis-specified", {
  expect_error(
    separate_lat_long(
      df_test_lat_long_dm_c,
      delim_chr = " ",
      coord_comp = "DMS"
    )
  )
  expect_error(
    separate_lat_long(
      df_test_lat_long_dms_c,
      delim_chr = " ",
      coord_comp = "DM"
    )
  )
})

test_that("convert_lat_long() returns a data frame with columns for Latitude and Longitude
derived from their components (Degrees/Minutes or Degrees/Minutes/Seconds). Latitude and Longitude
columns are in decimal degrees (numeric format).", {
  lat_long_dm_test <- convert_lat_long(df_test_lat_long_dm, coord_comp = "DM")
  expect_s3_class(lat_long_dm_test, "data.frame")
  expect_equal(names(lat_long_dm_test), c("Latitude", "Longitude"))
  expect_all_true(
    sapply(lat_long_dm_test[names(lat_long_dm_test)], is.numeric)
  )
  expect_all_true(dplyr::between(lat_long_dm_test$Latitude, 37, 40))
  expect_all_true(dplyr::between(lat_long_dm_test$Longitude, -124, -121))

  lat_long_dms_test <- convert_lat_long(df_test_lat_long_dms, coord_comp = "DMS")
  expect_s3_class(lat_long_dms_test, "data.frame")
  expect_equal(names(lat_long_dms_test), c("Latitude", "Longitude"))
  expect_all_true(
    sapply(lat_long_dms_test[names(lat_long_dms_test)], is.numeric)
  )
  expect_all_true(dplyr::between(lat_long_dms_test$Latitude, 37, 40))
  expect_all_true(dplyr::between(lat_long_dms_test$Longitude, -124, -121))
})


# standardize_param() ----------------------------------------------------

## Testing data ----------------------------------------------------------

## Tests -----------------------------------------------------------------

test_that("standardize_param() produces expected parameter names check", {
  expect_message(
    standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta),
    "All column names are correct"
  )
  expect_error(
    standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_error),
    "The following expected columns are NOT present in the data frame: col1a"
  )
  expect_snapshot(
    standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_error),
    error = TRUE
  )
})

test_that("standardize_param() properly removes unwanted parameters", {
  expect_equal(
    names(standardize_col_meta(
      df_test_std_col_raw,
      df_test_std_col_meta_select
    )),
    dplyr::pull(df_test_std_col_meta_select, Col_name_new)
  )
})

test_that("standardize_param() produces the expected messaging when it removes unwanted parameters", {
  expect_snapshot(standardize_col_meta(
    df_test_std_col_raw,
    df_test_std_col_meta
  ))
})

test_that("standardize_param() properly standardizes parameter names", {
  expect_equal(
    standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_rename),
    df_test_std_col_raw |> dplyr::rename_with(\(x) paste0(x, "_new"))
  )
})

test_that("standardize_col_meta() produces the expected messaging when there are no problems to address and when
there are no unwanted parameters to remove", {
  expect_snapshot(standardize_col_meta(
    df_test_std_col_raw,
    df_test_std_col_meta
  ))
})

# int_get_data_dims() ----------------------------------------------------

## Testing data ----------------------------------------------------------

## Tests -----------------------------------------------------------------

# add_update_info() ------------------------------------------------------

## Testing data ----------------------------------------------------------

## Tests -----------------------------------------------------------------
