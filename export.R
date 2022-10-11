load_export_params <- function(jsonfile) {
    param_df <- rjson::fromJSON(file = jsonfile) %>%
        purrr::map_dfr(data.frame)
    return(param_df)
}

ggsave_param <- function(output_dir,
                         params,
                         plot = last_plot(),
                         filename_prefix = "",
                         time_prefix = FALSE) {
    if (time_prefix) {
        tp <- Sys.Date()
    } else {
        tp <- ""
    }
    out_filename <- paste0(output_dir, tp, filename_prefix, params$filename)
    # ggsave(out_filename, plot = plot, params)
    rlang::exec(ggplot2::ggsave, out_filename, plot = plot, !!!params)
}
