filter_bodyweight = function(data, replicate = NULL) {
  data = data %>%
    dplyr::filter(exp_type == "weight", exp_sub_1 == "body")
  
  if(!is.null(replicate)) {
    data = data %>%
      dplyr::filter(replicate == replicate)
  }
  return(data)
}

summarize_bodyweight = function(data) {
  data = data %>%
    group_by(batch, subject, group, sex) %>%
    summarise(avg = mean(value)) %>%
    drop_na()
  return(data)
}

summarize_bodyweight_frequency(data, frequency = months) {
  data = data %>%
    group_by(batch, subject, group, sex, {{ frequency }}) %>%
    summarise(avg = mean(value)) %>%
    drop_na()
  return(data)
}

plot_bodyweight <- function(data, facet_sex = TRUE) {
    p <- data %>%
        ggplot(aes(x = group, y = avg)) +
        geom_boxplot(aes(fill = group)) +
        geom_jitter(width = 0.15, size = 0.5) +
        expand_limits(y = 0) +
        theme_bw() +
        theme(legend.position = "none") +
        ylab("body weight [g]")

    ifelse(facet_sex, p + facet_wrap(~sex), p)
    return(p)
}

plot_bodyweight_time = function(data, type = "boxplot", facet_sex = TRUE) {
  freq_colnames = c("days", "weeks", "months")
  
  frequency = freq_colnames[freq_colnames %in% colnames(data)][1]
  if(is.na(frequency)) {
    print(
      paste0(
      "None of the required frequency variables: ",
      paste(freq_colnames, collapse = " "),
      "are present in the data.\n"
      )
    )
  }
  
  p = data %>%
    ggplot(aes(x = !!as.symbol(frequency), y = avg))
  
  if(type == "boxplot") {
    p = p +
      geom_boxplot(aes(fill = group), outlier.shape = NA) +
      geom_point(position=position_jitterdodge(), size = 0.5)
  } else if(type == "scatter") {
    p = p + 
      geom_point(aes(color = group)) + 
      stat_smooth(aes(fill = group), method = "lm", alpha = 0.2)
  } else if(type == "barplot") {
    p = p + 
      geom_bar(aes(fill = group), stat = "identity")
  }
    
  p = p +
    # expand_limits(y = 0) +
    theme_bw() + 
    #theme(legend.position = "none") +
    ylab("body weight [g]")
    
  ifelse(facet_sex, p + facet_wrap(~sex), p)
  return(p)
}

plot_bodyweight_time_barplot = function(data, facet_sex = TRUE) {
  p = data %>%
    ggplot(aes(x = !!as.symbol(frequency), y = avg, fill = group)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position=position_jitterdodge(), size = 0.5) +
    expand_limits(y = 0) +
    theme_bw() + 
    theme(legend.position = "none") +
    ylab("body weight [g]")
  
  ifelse(facet_sex, p + facet_wrap(~sex), p)
  return(p)
}

plot_grip_strength <- function(data, lean_norm_data = NULL, bw_norm_data = NULL) {
    # TODO from multiple weights use specific one for grip strength normalization
    p <- data %>%
        dplyr::filter(exp_type == "grip_strength") %>%
        dplyr::mutate(raw = value) %>%
        dplyr::left_join(data %>%
            dplyr::filter(exp_type == "mri", exp_sub_1 == "lean", exp_sub_2 == "raw") %>%
            dplyr::select(subject, lean_mass = value), by = "subject") %>%
        dplyr::left_join(data %>%
            dplyr::filter(exp_type == "weight", exp_sub_1 == "body") %>%
            group_by(subject) %>%
            summarise(bw = mean(value)), by = "subject") %>%
        dplyr::mutate(norm_lean = raw / lean_mass, norm_bw = raw / bw) %>%
        dplyr::select(-value) %>%
        pivot_longer(cols = c(raw, norm_lean, norm_bw), names_to = "normalized") %>%
        dplyr::mutate(normalized = fct_relevel(normalized, c("raw", "norm_bw", "norm_lean"))) %>%
        group_by(batch, subject, group, sex, exp_sub_1, exp_sub_2, normalized) %>%
        summarise(avg = mean(value)) %>%
        ggplot(aes(x = group, y = avg)) +
        geom_boxplot(aes(fill = group)) +
        geom_jitter(width = 0.15, size = 0.5) +
        expand_limits(y = 0) +
        facet_grid(normalized ~ exp_sub_1, scales = "free") +
        theme_bw() +
        theme(legend.position = "none") +
        ylab("force [N]")
    return(p)
}

plot_grip_strength_time <- function(data, filter_module, time_factor = c("pre", "Day1", "Day2", "Day3", "Day14"), lean_norm_data = NULL, bw_norm_data = NULL) {
    base_data <- data %>%
        dplyr::filter(module == filter_module) %>%
        dplyr::filter(exp_type == "grip_strength") %>%
        dplyr::mutate(exp_sub_2 = fct_relevel(exp_sub_2, time_factor)) %>%
        dplyr::mutate(raw = value) %>%
        dplyr::left_join(data %>%
            dplyr::filter(exp_type == "mri", exp_sub_1 == "lean", exp_sub_2 == "raw") %>%
            dplyr::select(subject, lean_mass = value), by = "subject") %>%
        dplyr::left_join(data %>%
            dplyr::filter(exp_type == "weight", exp_sub_1 == "body", module == filter_module) %>%
            group_by(subject) %>%
            summarise(bw = mean(value, na.rm = T)), by = "subject") %>%
        dplyr::mutate(norm_lean = raw / lean_mass, norm_bw = raw / bw) %>%
        dplyr::select(-value) %>%
        pivot_longer(cols = c(raw, norm_lean, norm_bw), names_to = "normalized") %>%
        dplyr::mutate(normalized = fct_relevel(normalized, c("raw", "norm_bw", "norm_lean"))) %>%
        group_by(subject, group, exp_sub_1, exp_sub_2, normalized)

    base_data %>%
        group_by(group, exp_sub_1, exp_sub_2, normalized) %>%
        summarise(avg = mean(value, na.rm = T), sd = sd(value, na.rm = T), sem = sd / sqrt(n())) %>%
        ggplot(aes(x = exp_sub_2, y = avg, group = group, color = group)) +
        geom_line(size = 1.1) +
        geom_point() +
        geom_errorbar(aes(x = exp_sub_2, ymin = avg - sem, ymax = avg + sem), width = 0.25, size = 0.35) +
        geom_jitter(data = base_data %>% summarise(avg = mean(value, na.rm = T)), aes(x = exp_sub_2, y = avg, group = group, color = group), size = 0.5, width = 0.1) +
        expand_limits(y = 0) +
        facet_grid(normalized ~ exp_sub_1, scales = "free") +
        theme_bw() +
        theme(legend.position = "none") +
        xlab("") +
        ylab("force [N]")
}

plot_mri = function(data, bw_norm_data = NULL, facet_sex = F) {
  p = data %>%
    dplyr::filter(exp_type == "mri", exp_sub_2 == "percentage") %>%
    group_by(batch, subject, group, sex, exp_sub_1, exp_sub_2) %>%
    summarise(percentage = mean(value)) %>%
    ggplot(aes(x = group, y = percentage)) +
    geom_boxplot(aes(fill = group)) + 
    geom_jitter(width = 0.15, size = 0.5) + 
    expand_limits(y = 0) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    ylab("% body weight")
  
  if(facet_sex) {
    p + facet_grid(sex ~ exp_sub_1, scales = "free")
  } else {
    p + facet_wrap(~ exp_sub_1, scales = "free")
  }
}

plot_gtt = function(data) {
  data %>%
    dplyr::filter(exp_type == "gtt") %>%
    dplyr::mutate(exp_sub_1 = as.numeric(exp_sub_1)) %>%
    group_by(sex, group, exp_sub_1) %>%
    summarise(avg = mean(value), sd = sd(value), sem=sd/sqrt(n())) %>%
    ggplot(aes(x = exp_sub_1, y = avg, group = group, color = group)) +
    geom_line(size = 1.1) +
    geom_jitter(data = data %>% 
                  dplyr::filter(exp_type == "gtt") %>%
                  dplyr::mutate(exp_sub_1 = as.numeric(exp_sub_1)), 
                aes(x = exp_sub_1, y = value, group = group, color = group), size = 1.15) +
    geom_errorbar(aes(x = exp_sub_1, ymin = avg - sem, ymax = avg + sem), width = 4, size = 0.75) +
    expand_limits(y = 0) + 
    theme_bw() +
    ylab("glucose [mM]") + 
    xlab("time [min]")
}

plot_gtt_auc = function(data) {
  data %>%
    dplyr::filter(exp_type == "gtt") %>%
    dplyr::mutate(exp_sub_1 = as.numeric(exp_sub_1)) %>%
    left_join(data %>%
                dplyr::filter(exp_type == "gtt") %>%
                dplyr::mutate(exp_sub_1 = as.numeric(exp_sub_1)) %>%
                filter(exp_sub_1 == 0) %>%
                select(subject, baseline = value),
              by = "subject") %>%
    mutate(value_norm = value - baseline) %>%
    group_by(sex, group, subject) %>%
    summarise(auc = auc(exp_sub_1, value_norm)) %>%
    ggplot(aes(x = group, y = auc)) +
    geom_boxplot(aes(fill = group), outlier.shape = NA) + 
    geom_jitter(width = 0.15, size = 0.75) + 
    theme_bw()+ 
    theme(legend.position = "none") +
    expand_limits(y = 0) +
    ylab("auc")
}

plot_gtt_timepoint = function(data, timepoint = 0) {
  data %>%
    dplyr::filter(exp_type == "gtt") %>%
    dplyr::mutate(exp_sub_1 = as.numeric(exp_sub_1)) %>%
    filter(exp_sub_1 == timepoint) %>%
    group_by(sex, group, subject) %>%
    summarise(avg = mean(value)) %>%
    ggplot(aes(x = group, y = avg)) +
    geom_boxplot(aes(fill = group), outlier.shape = NA) + 
    geom_jitter(width = 0.15, size = 0.75) + 
    theme_bw()+ 
    theme(legend.position = "none") +
    expand_limits(y = 0) +
    ylab("glucose [mM]")
}

plot_muscle_weights = function(data, beds = c("SOL", "EDL", "TA", "GA"), facet_sex = F) {
  p = data %>%
    dplyr::filter(exp_type == "weight", exp_sub_1 %in% beds) %>%
    group_by(batch, subject, genotype, sex, exp_sub_1, exp_sub_2) %>%
    summarise(avg = mean(value)) %>%
    ungroup() %>%
    dplyr::mutate(exp_sub_1 = fct_relevel(exp_sub_1, beds), 
                  exp_sub_2 = fct_relevel(exp_sub_2, c("raw", "bw", "lean"))) %>%
    ggplot(aes(x = genotype, y = avg)) +
    geom_boxplot(aes(fill = genotype)) + 
    geom_jitter(width = 0.15, size = 0.75) + 
    theme_bw()+ 
    theme(legend.position = "none") +
    ylab("weight")
  
  if(facet_sex) {
    p + facet_wrap(exp_sub_2 ~ exp_sub_1 + sex, scales = "free")
  } else {
    p + facet_wrap(exp_sub_2 ~ exp_sub_1, scales = "free")
  }
}