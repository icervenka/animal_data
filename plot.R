library(magrittr, include.only = "%>%")

# plot styles ------------------------------------------------------------------
default_boxplot_style <- function() {
  list(
    ggplot2::geom_boxplot(ggplot2::aes(fill = group), outlier.shape = NA),
    ggplot2::geom_jitter(width = 0.15, size = 0.75),
    ggplot2::theme_bw(),
    ggplot2::theme(legend.position = "none"),
    ggplot2::expand_limits(y = 0)
  )
}

# general functions ------------------------------------------------------------
# TODO find a way to filter certain ages
# TODO create a general filtering function

filter <- function(data, experiment_type, cohorts = NULL, subjects = NULL,
                   age = NULL, replicate = NULL) {
  exp_type <- list(
    "bodyweight" = list("exp_type" = "weight", "exp_sub_1" = "body"),
    "mri" = list("exp_type" = "mri", "exp_sub_1" = c("fat", "lean"), "exp_type_2" = "percentage"),
    "gtt" = list("exp_type" = "gtt"),
    "muscle_weights" = list("exp_type" = "weight", "exp_sub_1" = c("SOL", "EDL", "TA", "GA"))
  )
  # deparse(sys.calls())
  # deparse(sys.calls()[[sys.nframe()-1]])
  as.character(match.call()[[1]])
}

filter_experiment_type <- function(data, experiment_type) {

}

filter_subject <- function(data, cohorts = NULL, subjects = NULL) {

}

filter_age <- function(data, age = NULL) {

}

# bodyweight functions ---------------------------------------------------------
filter_bodyweight <- function(data, replicate = NULL) {
  data <- data %>%
    dplyr::filter(exp_type == "weight", exp_sub_1 == "body")

  if (!is.null(replicate)) {
    data <- data %>%
      dplyr::filter(replicate == replicate)
  }
  return(data)
}

summarize_bodyweight <- function(data) {
  data <- data %>%
    dplyr::group_by(batch, subject, group, sex) %>%
    dplyr::summarise(avg = mean(value)) %>%
    tidyr::drop_na()
  return(data)
}

summarize_bodyweight_time <- function(data, frequency = months) {
  data <- data %>%
    dplyr::group_by(batch, subject, group, sex, {{ frequency }}) %>%
    dplyr::summarise(avg = mean(value)) %>%
    tidyr::drop_na()
  return(data)
}

plot_bodyweight <- function(data, facet_sex = TRUE) {
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = group, y = avg)) +
    default_boxplot_style() +
    ggplot2::ylab("body weight [g]")

  ifelse(facet_sex, p + ggplot2::facet_wrap(~sex), p)
  return(p)
}

plot_bodyweight_time <- function(data, type = "boxplot", facet_sex = TRUE) {
  freq_colnames <- c("days", "weeks", "months")

  frequency <- freq_colnames[freq_colnames %in% colnames(data)][1]
  if (is.na(frequency)) {
    print(
      paste0(
        "None of the required frequency variables: ",
        paste(freq_colnames, collapse = " "),
        "are present in the data.\n"
      )
    )
  }

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(frequency), y = avg))

  if (type == "boxplot") {
    p <- p +
      ggplot2::geom_boxplot(ggplot2::aes(fill = group), outlier.shape = NA) +
      ggplot2::geom_point(
        position = ggplot2::position_jitterdodge(),
        size = 0.5
      )
  } else if (type == "scatter") {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = group)) +
      ggplot2::stat_smooth(ggplot2::aes(fill = group),
        method = "lm",
        alpha = 0.2
      )
  } else if (type == "barplot") {
    p <- p +
      ggplot2::geom_bar(ggplot2::aes(fill = group), stat = "identity")
  }

  p <- p +
    # expand_limits(y = 0) +
    ggplot2::theme_bw() +
    # theme(legend.position = "none") +
    ggplot2::ylab("body weight [g]")

  ifelse(facet_sex, p = p + ggplot2::facet_wrap(~sex), p)
  return(p)
}

# grip strength functions ------------------------------------------------------
# plot_grip_strength <- function(data,
#                                lean_norm_data = NULL,
#                                bw_norm_data = NULL) {
#   # TODO from multiple weights use specific one for grip strength normalization
#   p <- data %>%
#     dplyr::filter(exp_type == "grip_strength") %>%
#     dplyr::mutate(raw = value) %>%
#     dplyr::left_join(data %>%
#       dplyr::filter(
#         exp_type == "mri",
#         exp_sub_1 == "lean",
#         exp_sub_2 == "raw"
#       ) %>%
#       dplyr::select(subject, lean_mass = value), by = "subject") %>%
#     dplyr::left_join(data %>%
#       dplyr::filter(exp_type == "weight", exp_sub_1 == "body") %>%
#       dplyr::group_by(subject) %>%
#       dplyr::summarise(bw = mean(value)), by = "subject") %>%
#     dplyr::mutate(norm_lean = raw / lean_mass, norm_bw = raw / bw) %>%
#     dplyr::select(-value) %>%
#     tidyr::pivot_longer(
#       cols = c(raw, norm_lean, norm_bw),
#       names_to = "normalized"
#     ) %>%
#     dplyr::mutate(normalized = fct_relevel(normalized, c("raw", "norm_bw", "norm_lean"))) %>%
#     dplyr::group_by(batch, subject, group, sex, exp_sub_1, exp_sub_2, normalized) %>%
#     dplyr::summarise(avg = mean(value)) %>%
#     ggplot2::ggplot(ggplot2::aes(x = group, y = avg)) +
#     ggplot2::geom_boxplot(ggplot2::aes(fill = group)) +
#     ggplot2::geom_jitter(width = 0.15, size = 0.5) +
#     ggplot2::expand_limits(y = 0) +
#     ggplot2::facet_grid(normalized ~ exp_sub_1, scales = "free") +
#     ggplot2::theme_bw() +
#     ggplot2::theme(legend.position = "none") +
#     ggplot2::ylab("force [N]")
#   return(p)
# }

filter_grip_strength <- function(data) {
  data <- data %>%
    dplyr::filter(exp_type == "grip_strength")
}

summarize_grip_strength <- function(data) {
  data <- data %>%
    dplyr::group_by(batch, subject, group, sex, exp_sub_1, exp_sub_2) %>%
    dplyr::summarise(avg = mean(value)) %>%
    tidyr::drop_na()
  return(data)
}

plot_grip_strength <- function(data, facet_sex = TRUE) {
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = group, y = avg)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = group)) +
    default_boxplot_style() +
    ggplot2::ylab("force [N]")

  if (facet_sex) {
    p <- p + ggplot2::facet_grid(sex ~ exp_sub_1 + exp_sub_2, scales = "free")
  } else {
    p <- p + ggplot2::facet_wrap(~ exp_sub_1 + exp_sub_2, scales = "free")
  }

  return(p)
}

# plot_grip_strength_time <- function(data, filter_module, time_factor = c("pre", "Day1", "Day2", "Day3", "Day14"), lean_norm_data = NULL, bw_norm_data = NULL) {
#   base_data <- data %>%
#     dplyr::filter(module == filter_module) %>%
#     dplyr::filter(exp_type == "grip_strength") %>%
#     dplyr::mutate(exp_sub_2 = fct_relevel(exp_sub_2, time_factor)) %>%
#     dplyr::mutate(raw = value) %>%
#     dplyr::left_join(data %>%
#       dplyr::filter(exp_type == "mri", exp_sub_1 == "lean", exp_sub_2 == "raw") %>%
#       dplyr::select(subject, lean_mass = value), by = "subject") %>%
#     dplyr::left_join(data %>%
#       dplyr::filter(exp_type == "weight", exp_sub_1 == "body", module == filter_module) %>%
#       dplyr::group_by(subject) %>%
#       dplyr::summarise(bw = mean(value, na.rm = T)), by = "subject") %>%
#     dplyr::mutate(norm_lean = raw / lean_mass, norm_bw = raw / bw) %>%
#     dplyr::select(-value) %>%
#     tidyr::pivot_longer(cols = c(raw, norm_lean, norm_bw), names_to = "normalized") %>%
#     dplyr::mutate(normalized = fct_relevel(normalized, c("raw", "norm_bw", "norm_lean"))) %>%
#     dplyr::group_by(subject, group, exp_sub_1, exp_sub_2, normalized)

#   base_data %>%
#     dplyr::group_by(group, exp_sub_1, exp_sub_2, normalized) %>%
#     dplyr::summarise(avg = mean(value, na.rm = T), sd = sd(value, na.rm = T), sem = sd / sqrt(dplyr::n())) %>%
#     ggplot2::ggplot(ggplot2::aes(x = exp_sub_2, y = avg, group = group, color = group)) +
#     ggplot2::geom_line(size = 1.1) +
#     ggplot2::geom_point() +
#     ggplot2::geom_errorbar(ggplot2::aes(x = exp_sub_2, ymin = avg - sem, ymax = avg + sem), width = 0.25, size = 0.35) +
#     ggplot2::geom_jitter(data = base_data %>% dplyr::summarise(avg = mean(value, na.rm = T)), ggplot2::aes(x = exp_sub_2, y = avg, group = group, color = group), size = 0.5, width = 0.1) +
#     ggplot2::expand_limits(y = 0) +
#     ggplot2::facet_grid(normalized ~ exp_sub_1, scales = "free") +
#     ggplot2::theme_bw() +
#     ggplot2::theme(legend.position = "none") +
#     ggplot2::xlab("") +
#     ggplot2::ylab("force [N]")
# }

summarize_grip_strength_time <- function(data) {
  data <- data %>%
    tidyr::separate(exp_sub_3, into = c(NULL, exp_sub_3), sep = "_") %>%
    dplyr::mutate(exp_sub_3 = as.numeric(exp_sub_3)) %>%
    dplyr::group_by(
      batch, subject, group, sex,
      exp_sub_1, exp_sub_2, exp_sub_3
    ) %>%
    dplyr::summarise(avg = mean(value)) %>%
    tidyr::drop_na()
  return(data)
}

plot_grip_strength_time <- function(data, facet_sex = TRUE) {
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = exp_sub_3, y = avg)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(
      x = exp_sub_3,
      ymin = avg - sem,
      ymax = avg + sem
    ),
    width = 0.25,
    size = 0.35
    ) +
    # ggplot2::geom_jitter(data = base_data %>% dplyr::summarise(avg = mean(value, na.rm = T)), ggplot2::aes(x = exp_sub_2, y = avg, group = group, color = group), size = 0.5, width = 0.1) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("force [N]")

  if (facet_sex) {
    p <- p + ggplot2::facet_grid(sex ~ exp_sub_1 + exp_sub_2, scales = "free")
  } else {
    p <- p + ggplot2::facet_wrap(exp_sub_1 ~ exp_sub_2, scales = "free")
  }
}

# mri functions ----------------------------------------------------------------
filter_mri <- function(data, replicate = NULL) {
  data <- data %>%
    dplyr::filter(
      exp_type == "mri",
      exp_sub_1 %in% c("fat", "lean"),
      exp_sub_2 == "percentage"
    )

  if (!is.null(replicate)) {
    data <- data %>%
      dplyr::filter(replicate == replicate)
  }
  return(data)
}


plot_mri <- function(data, facet_sex = TRUE) {
  p <- data %>%
    dplyr::group_by(subject, group, sex, exp_sub_1) %>%
    dplyr::summarise(avg = mean(value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = group, y = avg)) +
    default_boxplot_style() +
    ggplot2::ylab("% body weight")

  if (facet_sex) {
    p <- p + ggplot2::facet_grid(sex ~ exp_sub_1, scales = "free")
  } else {
    p <- p + ggplot2::facet_wrap(~exp_sub_1, scales = "free")
  }

  return(p)
}

# gtt functions ----------------------------------------------------------------
filter_gtt <- function(data,
                       timepoints = c(0, 15, 30, 45, 60, 120),
                       replicate = NULL) {
  data <- data %>%
    dplyr::filter(exp_type == "gtt") %>%
    dplyr::mutate(exp_sub_1 = as.numeric(exp_sub_1)) %>%
    dplyr::filter(exp_sub_1 %in% timepoints)

  if (!is.null(replicate)) {
    data <- data %>%
      dplyr::filter(replicate == replicate)
  }
  return(data)
}

plot_gtt <- function(data) {
  data <- data %>%
    dplyr::group_by(sex, group, exp_sub_1) %>%
    dplyr::summarise(
      avg = mean(value),
      sd = sd(value),
      sem = sd / sqrt(dplyr::n())
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = exp_sub_1,
      y = avg,
      group = group,
      color = group
    )) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_jitter(ggplot2::aes(
      x = exp_sub_1,
      y = value
    ),
    size = 1.15
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(
      x = exp_sub_1,
      ymin = avg - sem,
      ymax = avg + sem
    ),
    width = 4,
    size = 0.75
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_bw() +
    ggplot2::ylab("glucose [mM]") +
    ggplot2::xlab("time [min]")
}

plot_gtt_auc <- function(data, subtract_baseline = TRUE) {
  if (subtract_baseline) {
    data <- data %>%
      dplyr::left_join(data %>%
        dplyr::filter(exp_sub_1 == 0) %>%
        dplyr::select(subject, baseline = value),
      by = "subject"
      ) %>%
      dplyr::mutate(value = value - baseline)
  }

  p <- data %>%
    dplyr::group_by(sex, group, subject) %>%
    dplyr::summarise(auc = auc(exp_sub_1, value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = group, y = auc)) +
    default_boxplot_style() +
    ggplot2::ylab("auc")
  return(p)
}

plot_gtt_timepoint <- function(data, timepoint = 0) {
  data %>%
    dplyr::filter(exp_sub_1 == timepoint) %>%
    dplyr::group_by(sex, group, subject) %>%
    dplyr::summarise(avg = mean(value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = group, y = avg)) +
    default_boxplot_style() +
    ggplot2::ylab("glucose [mM]")
}

# muscle weights functions -----------------------------------------------------
plot_muscle_weights <- function(data,
                                beds = c("SOL", "EDL", "TA", "GA"),
                                facet_sex = TRUE) {
  p <- data %>%
    dplyr::filter(exp_type == "weight", exp_sub_1 %in% beds) %>%
    dplyr::group_by(batch, subject, genotype, sex, exp_sub_1, exp_sub_2) %>%
    dplyr::summarise(avg = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      exp_sub_1 = fct_relevel(exp_sub_1, beds),
      exp_sub_2 = fct_relevel(exp_sub_2, c("raw", "bw", "lean"))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = genotype, y = avg)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = genotype)) +
    ggplot2::geom_jitter(width = 0.15, size = 0.75) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("weight")

  if (facet_sex) {
    p + ggplot2::facet_wrap(exp_sub_2 ~ exp_sub_1 + sex, scales = "free")
  } else {
    p + ggplot2::facet_wrap(exp_sub_2 ~ exp_sub_1, scales = "free")
  }
}

# force measurement functions --------------------------------------------------

filter_force_measurement = function(data,
beds = c("SOL", "EDL", "TA", "GA"),
replicate = NULL) {
  data <- data %>%
    dplyr::filter(exp_type %in% c("force", "fatigue", "recovery")) %>%
    dplyr::mutate(exp_sub_1 %in% beds) %>%
    dplyr::mutate(exp_sub_3 = as.numeric(exp_sub_3))

  if (!is.null(replicate)) {
    data <- data %>%
      dplyr::filter(replicate == replicate)
  }
  return(data)
}

summarize_force_measurement = function(data) {
 data <- data %>%
    dplyr::group_by(batch, group, sex) %>%
    dplyr::summarise(avg = mean(value)) %>%
    tidyr::drop_na()
  return(data)
}

plot_force_measurement = function(data, facet_sex = T) {
  plots = purrr::map(c(data$exp_type), function(x) {
    p <- data %>%
    dplyr::filter(exp_type == x) %>%
    dplyr::summarise(
      avg = mean(value),
      sd = sd(value),
      sem = sd / sqrt(dplyr::n())
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = exp_sub_3,
      y = avg,
      color = group
    )) +
    ggplot2::geom_line(size = 1.1) +
    # ggplot2::geom_jitter(ggplot2::aes(
    #   x = exp_sub_1,
    #   y = value
    # ),
    # size = 1.15
    # ) +
    ggplot2::geom_errorbar(ggplot2::aes(
      x = exp_sub_3,
      ymin = avg - sem,
      ymax = avg + sem
    ),
    width = 4,
    size = 0.75
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_bw() +
    ggplot2::ylab("force") +
    ggplot2::xlab("time [min]")

  if (facet_sex) {
    p + ggplot2::facet_wrap(exp_sub_1 ~ exp_sub_2 + sex, scales = "free")
  } else {
    p + ggplot2::facet_wrap(exp_sub_1 ~ exp_sub_2, scales = "free")
  }
  }) 

  

}