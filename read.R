# reading functions -------------------
parse_cohort_data <- function(path) {
    cohorts <- data.frame(
        folder_full = dir(path, full.names = T, no.. = T),
        folder_name = dir(path, no.. = T), stringsAsFactors = F
    ) %>%
        separate(folder_name, into = c("driver", "gene", "type", "cohort"), remove = F) %>%
        mutate(cohort_num = str_replace(cohort, "coh", "")) %>%
        mutate(line = paste(driver, gene, type, sep = "_"))

    metadata_files <- map_chr(cohorts$folder_full, function(x) {
        list.files(x, pattern = "_metadata", full.names = T)[1]
    })
    data_files <- map_chr(cohorts$folder_full, function(x) {
        list.files(x, pattern = "_data", full.names = T)[1]
    })
    ready <- map_chr(cohorts$folder_full, function(x) {
        list.files(x, pattern = "_ready")[1]
    })

    cohorts <- cohorts %>%
        mutate(metadata = metadata_files) %>%
        mutate(data = data_files) %>%
        mutate(ready = case_when(ready == "_ready" ~ 1, TRUE ~ 0)) %>%
        dplyr::filter(ready == 1) %>%
        drop_na()

    return(cohorts)
}

read_cohort_data <- function(cohort_df,
                             data_pivot_exclude = c("module", "exp_type", "exp_sub_1", "exp_sub_2", "replicate", "date"),
                             group_levels = c("wt", "ko")) {
    metadata <- map_dfr(cohort_df$metadata, function(x) {
        read_delim(x, delim = "\t", col_types = "cccdcc")
    })
    metadata$group <- factor(metadata$group, levels = group_levels)

    data <- map_dfr(cohort_df$data, function(x) {
        sheets <- excel_sheets(x)
        if ("results_long" %in% sheets) {
            {
                read_xlsx(x, sheet = "results_long")
            } %>% pivot_longer(-data_pivot_exclude, names_to = "subject")
        }
    }) %>% left_join(metadata, by = c("subject" = "subject"))

    return(data)
}
