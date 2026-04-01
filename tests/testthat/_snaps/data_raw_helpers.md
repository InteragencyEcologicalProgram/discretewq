# int_check_parsing() produces expected errors and messages, but returns NULL invisibly

    Code
      int_check_parsing(df_test_int, df_test_int_error)
    Message
      x The following columns did NOT parse correctly: col1
      i Results of parsing check:
    Output
      # A tibble: 3 x 4
        col_name num_NA_orig num_NA_parsed parse_check
        <chr>          <int>         <int> <lgl>      
      1 col1               0           100 FALSE      
      2 col2               0             0 TRUE       
      3 col3               0             0 TRUE       
    Condition
      Error in `int_check_parsing()`:
      ! x Data NOT converted
      ! Fix problem underlying parsing error before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# int_get_data_dims() produces expected messages

    Code
      int_get_data_dims(df_test_int)
    Message
      i Update data documentation in 'R/data.R':
      * @format a tibble with 2,600 rows and 3 variables

# subset_data_entity() produces expected errors

    Code
      subset_data_entity(test_data_ent, "^data")
    Condition
      Error in `subset_data_entity()`:
      ! x The following data entity regex pattern found more than one data entity: ^data
      i Update data entity regex pattern to find only one data entity before proceeding

---

    Code
      subset_data_entity(test_data_ent, "^foo")
    Condition
      Error in `subset_data_entity()`:
      ! x The following data entity regex pattern did not find a data entity: ^foo
      i Update data entity regex pattern before proceeding

# standardize_col_meta() produces expected column names check

    Code
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_error)
    Message
      i Checking column names and types in test_file_name
    Condition
      Error in `standardize_col_meta()`:
      ! x The following expected columns are NOT present in the data frame: col1a
      ! Update expected column names in Data_column_metadata.csv before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# standardize_col_meta() produces expected errors and messages when converting columns to numeric

    Code
      standardize_col_meta(df_test_std_col_raw_error, df_test_std_col_meta)
    Message
      i Checking column names and types in test_file_name
      v All column names are correct
    Condition
      Warning:
      There was 1 warning in `dplyr::mutate()`.
      i In argument: `dplyr::across(tidyselect::all_of(col_numeric), as.numeric)`.
      Caused by warning:
      ! NAs introduced by coercion
    Message
      i Converting the following columns to numeric: col2
      i Checking for data parsing errors
      x The following columns did NOT parse correctly: col2
      i Results of parsing check:
    Output
      # A tibble: 1 x 4
        col_name num_NA_orig num_NA_parsed parse_check
        <chr>          <int>         <int> <lgl>      
      1 col2               0             1 FALSE      
    Condition
      Error in `fn()`:
      ! x Data NOT converted
      ! Fix problem underlying parsing error before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# standardize_col_meta() produces the expected messaging when there are no problems to address

    Code
      invisible(standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta))
    Message
      i Checking column names and types in test_file_name
      v All column names are correct
      i Converting the following columns to numeric: col2
      i Checking for data parsing errors
      v All columns parsed correctly
      v Column names standardized

# convert_date() produces expected errors and messages

    Code
      invisible(convert_date(df_test_date_time_c, "Ymd"))
    Message
      i Converting Date column from character to date format in test_file_name
      v All columns parsed correctly
    Output
      

---

    Code
      convert_date(df_test_date_time_c, "mdy")
    Condition
      Warning:
      There was 1 warning in `dplyr::mutate()`.
      i In argument: `Date = lubridate::date(lubridate::parse_date_time(.data$Date, date_fmt))`.
      Caused by warning:
      ! All formats failed to parse. No formats found.
    Message
      i Converting Date column from character to date format in test_file_name
      x The following columns did NOT parse correctly: Date
      i Results of parsing check:
    Output
      # A tibble: 1 x 4
        col_name num_NA_orig num_NA_parsed parse_check
        <chr>          <int>         <int> <lgl>      
      1 Date               0            10 FALSE      
    Condition
      Error in `fn()`:
      ! x Data NOT converted
      ! Fix problem underlying parsing error before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# convert_time() produces expected errors and messages

    Code
      invisible(convert_time(df_test_date_time_c, "HM"))
    Message
      i Standardizing Time column in test_file_name
      v All columns parsed correctly
    Output
      

---

    Code
      convert_time(df_test_date_time_c, "HMS")
    Condition
      Warning:
      There was 1 warning in `dplyr::mutate()`.
      i In argument: `Time = format(lubridate::parse_date_time(.data$Time, time_fmt), "%H:%M:%S")`.
      Caused by warning:
      ! All formats failed to parse. No formats found.
    Message
      i Standardizing Time column in test_file_name
      x The following columns did NOT parse correctly: Time
      i Results of parsing check:
    Output
      # A tibble: 1 x 4
        col_name num_NA_orig num_NA_parsed parse_check
        <chr>          <int>         <int> <lgl>      
      1 Time               0            10 FALSE      
    Condition
      Error in `fn()`:
      ! x Data NOT converted
      ! Fix problem underlying parsing error before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# convert_datetime() produces expected errors and messages

    Code
      invisible(convert_datetime(df_test_datetime, "Ymd HMS", timezone = "America/Los_Angeles"))
    Message
      i Converting Datetime column from character to datetime format in test_file_name
      v All columns parsed correctly
      i Creating Date column from parsed Datetime column

---

    Code
      convert_datetime(df_test_datetime, "mdy HM", timezone = "America/Los_Angeles")
    Condition
      Warning:
      There was 1 warning in `dplyr::mutate()`.
      i In argument: `Datetime = lubridate::parse_date_time(.data$Datetime, datetime_fmt, tz = timezone)`.
      Caused by warning:
      ! All formats failed to parse. No formats found.
    Message
      i Converting Datetime column from character to datetime format in test_file_name
      x The following columns did NOT parse correctly: Datetime
      i Results of parsing check:
    Output
      # A tibble: 1 x 4
        col_name num_NA_orig num_NA_parsed parse_check
        <chr>          <int>         <int> <lgl>      
      1 Datetime           0            10 FALSE      
    Condition
      Error in `fn()`:
      ! x Data NOT converted
      ! Fix problem underlying parsing error before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# standardize_param() produces expected parameter names check

    Code
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta_error)
    Message
      i Checking column names and types in test_file_name
    Condition
      Error in `standardize_col_meta()`:
      ! x The following expected columns are NOT present in the data frame: col1a
      ! Update expected column names in Data_column_metadata.csv before proceeding
      i Raw data can be found at the following path:
      test_file_path/test_file_name

# standardize_param() produces the expected messaging when it removes unwanted parameters

    Code
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta)
    Message
      i Checking column names and types in test_file_name
      v All column names are correct
      i Converting the following columns to numeric: col2
      i Checking for data parsing errors
      v All columns parsed correctly
      v Column names standardized
    Output
      # A tibble: 26 x 3
         col1   col2 col3 
         <chr> <dbl> <chr>
       1 a         1 a_dup
       2 b         2 b_dup
       3 c         3 c_dup
       4 d         4 d_dup
       5 e         5 e_dup
       6 f         6 f_dup
       7 g         7 g_dup
       8 h         8 h_dup
       9 i         9 i_dup
      10 j        10 j_dup
      # i 16 more rows

# standardize_col_meta() produces the expected messaging when there are no problems to address and whenthere are no unwanted parameters to remove

    Code
      standardize_col_meta(df_test_std_col_raw, df_test_std_col_meta)
    Message
      i Checking column names and types in test_file_name
      v All column names are correct
      i Converting the following columns to numeric: col2
      i Checking for data parsing errors
      v All columns parsed correctly
      v Column names standardized
    Output
      # A tibble: 26 x 3
         col1   col2 col3 
         <chr> <dbl> <chr>
       1 a         1 a_dup
       2 b         2 b_dup
       3 c         3 c_dup
       4 d         4 d_dup
       5 e         5 e_dup
       6 f         6 f_dup
       7 g         7 g_dup
       8 h         8 h_dup
       9 i         9 i_dup
      10 j        10 j_dup
      # i 16 more rows

