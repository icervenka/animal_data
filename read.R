library(magrittr, include.only = "%>%")

# reading functions -------------------
parse_directory_structure <- function(path,
                                      data_pattern = "_data",
                                      metadata_pattern = "_metadata",
                                      ready_pattern = "_ready",
                                      cohort_pattern = "coh") {
  cohorts <- data.frame(
    folder_full = dir(path, full.names = TRUE, no.. = TRUE),
    folder_name = dir(path, no.. = TRUE), stringsAsFactors = FALSE
  ) %>%
    tidyr::separate(folder_name,
      into = c("driver", "gene", "type", "cohort"),
      remove = FALSE
    ) %>%
    dplyr::mutate(cohort_num = str_replace(cohort, cohort_pattern, "")) %>%
    dplyr::mutate(cohort_num = as.numeric(cohort_num)) %>%
    dplyr::mutate(line = paste(driver, gene, type, sep = "_"))

  metadata_files <- map_chr(cohorts$folder_full, function(x) {
    list.files(x, pattern = metadata_pattern, full.names = TRUE)[1]
  })
  data_files <- map_chr(cohorts$folder_full, function(x) {
    list.files(x, pattern = data_pattern, full.names = TRUE)[1]
  })
  ready <- map_chr(cohorts$folder_full, function(x) {
    list.files(x, pattern = ready_pattern)[1]
  })

  cohorts <- cohorts %>%
    dplyr::mutate(metadata = metadata_files) %>%
    dplyr::mutate(data = data_files) %>%
    dplyr::mutate(ready = case_when(ready == ready_pattern ~ 1, TRUE ~ 0)) %>%
    dplyr::filter(ready == 1) %>%
    tidyr::drop_na()

  return(cohorts)
}

read_data <- function(cohort_files,
                      data_pivot_exclude = c(
                        "module", "exp_type", "exp_sub_1",
                        "exp_sub_2", "replicate", "date"
                      ),
                      group_levels = c("wt", "ko"),
                      result_sheet = "results_long",
                      metadata_column_types = "ccccicc",
                      date_format = "%Y%m%d",
                      time_rounding_func = base::round) {
  metadata <- purrr::map_dfr(cohort_files$metadata, function(x) {
    readr::read_delim(x, delim = "\t", col_types = metadata_column_types)
  }) %>%
    dplyr::mutate(group = factor(group, levels = group_levels))

  data <- purrr::map_dfr(cohort_files$data, function(x) {
    sheets <- readxl::excel_sheets(x)
    if (result_sheet %in% sheets) {
      readxl::read_xlsx(x, sheet = result_sheet) %>%
        tidyr::pivot_longer(-data_pivot_exclude, names_to = "subject")
    } else {
      print(paste0(
        "Result sheet: ", result_sheet,
        " not found in file ", x, " skipping.\n"
      ))
    }
  }) %>%
    dplyr::left_join(metadata, by = c("subject" = "subject")) %>%
    dplyr::mutate(date = as.Date(as.character(date), format = date_format)) %>%
    dplyr::mutate(dob = as.Date(as.character(dob), format = date_format)) %>%
    dplyr::mutate(days = as.numeric(date - dob)) %>%
    dplyr::mutate(months = as.character(time_rounding_func(as.numeric(days) / 30))) %>%
    dplyr::mutate(weeks = as.character(time_rounding_func(as.numeric(days) / 7)))

  return(data)
}
