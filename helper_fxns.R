
# plotting consumption diets of individuals
topuser_plot <- function (data,
                          channel_type,
                          title,
                          ylabel,
                          y_limit = 5e4,
                          figure_size,
                          figure_space) {
  
  nobs <- nrow(data %>% distinct(caseid))
  
  
  if (channel_type == 'Extremist') {
    
    p <- data %>%
      mutate(channel_type = factor(
        channel_type,
        levels = c("extremist", "alternative", "mainstream", "other")
      )) %>%
      ggplot(., aes(x = rank,
                    y = minutes_value,
                    fill = channel_type)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      geom_image(
        data = . %>%
          filter((super_alternative == 1) &
                   channel_type == "other"),
        aes(image = image,
            y = figure_space),
        size = figure_size,
        show.legend = FALSE
      )
    
  } else if (channel_type == 'Alternative') {
    p <-  data %>% 
      ggplot(., aes(x = rank, 
                    y = minutes_value,
                    fill = channel_type)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      geom_image(
        data = . %>%
          filter((super_extremist == 1) &
                   channel_type == "other"),
        aes(image = image,
            y = figure_space),
        size = figure_size,
        show.legend = FALSE
      )
  }
  
  p +
    scale_fill_manual(
      name = "",
      values = c(
        "alternative" = "#FFA500",
        "extremist" = "#CD5C5C",
        "mainstream" = "#015CB9",
        "other" = "#E3E6E6"
      ),
      labels = c(
        "alternative" = "Alternative \nchannels",
        "extremist" = "Extremist \nchannels",
        "mainstream" = "Mainstream \nmedia",
        "other" = "Other \nchannels"
      )
    ) +
    labs(x = "",
         y = ylabel,
         title = paste0(title, " (n = ", nobs, ")")) +
    theme_minimal() +
    theme(
      legend.position = 'bottom',
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_text(hjust = .95)
    ) +
    coord_cartesian(ylim = c(-160, y_limit))
}

# string wrap for factor variables
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# better labels for channel types
recode_channel_type_fxn <- function(old_name) {
  recode(
    old_name,
    alternative = "Alternative \nchannel",
    extremist = "Extremist \nchannel",
    mainstream = "Mainstream media \nchannel",
    other = "Other \nchannel"
  )
}

# labels for predictors
recode_fxn <- function(old_name) {
  recode(
    old_name,
    rr_tercilemed = "RR, medium",
    rr_tercilehigh = "RR, high",
    ft_jew_binnedwarm = "Feeling Jews, warm",
    ft_jew_binnedmedium = "Feeling Jews, medium",
    rr_cts = "Racial resentment",
    rr_cts2 = "Denial of racism",
    `raceNon-white` = "Non-white",
    jw_cts = "Feeling Jews",
    genderMale = "Male",
    fem_cts = "Hostile sexism",
    pid_leanDemocrat = "Democrat",
    pid_leanRepublican = "Republican",
    `educ2Some college` = "Some college",
    `educ2Post-grad` = "Post-grad",
    `educ24-year` = "Bachelors",
    age = "Age"
  )
}

# refactoring order of predictors
refactor_fxn <- function (var_list) {
  if ("Republican" %in% var_list) {
    demo_vars <- c(
      "Post-grad",
      "Bachelors",
      "Some college",
      "Non-white",
      "Male",
      "Age",
      "Republican",
      "Democrat"
    )
  } else {
    demo_vars <- c("Post-grad",
                   "Bachelors",
                   "Some college",
                   "Non-white",
                   "Male",
                   "Age")
  }
  
  if (length(var_list) == 9 * 3 | length(var_list) == 11 * 3) {
    if ("Racial resentment" %in% var_list) {
      factor(
        var_list,
        levels = c(
          demo_vars,
          "Feeling Jews",
          "Racial resentment",
          "Hostile sexism"
        )
      )
    } else
      (factor(var_list,
              levels = c(
                demo_vars,
                "Feeling Jews",
                "Denial of racism",
                "Hostile sexism"
              )))
  } else if (length(var_list) == 7 * 3) {
    if ("Racial resentment" %in% var_list) {
      factor(var_list,
             levels = c(demo_vars, "Racial resentment"))
    } else
      (factor(var_list,
              levels = c(demo_vars, "Female resentment")))
  } else if (length(var_list) == 6 * 3) {
    factor(var_list,
           levels = demo_vars)
  } else {
    var_list
  }
}
# get modal values
mode_fxn <- function(var) {
  factor(which.max(table(with(activity_data, get(
    var
  )))) %>% names())
}

#function to calculate %s as input to waffle plots
recs_prop_table_fxn <- function(channel_type, statistic) {
  filter_var <-
    ifelse(statistic == 'shown', "rec_n_video", "rec_match_")
  results_tab <- recs_data %>%
    select(variable, .data[[channel_type]]) %>%
    filter(str_detect(variable, filter_var)) %>%
    rename(value = .data[[channel_type]]) %>%
    mutate(visit = channel_type,
           prop = value / sum(value))
  if (statistic == "shown") {
    results_tab %>% mutate(
      shown = str_replace(variable, "_all", ""),
      shown = str_match(shown, '(?<=video_).*')[, 1]
    )
  } else if (statistic == "followed") {
    results_tab %>% mutate(
      followed = str_replace(variable, "_match$", ""),
      followed = str_match(followed, '(?<=_match_).*')[, 1]
    )
  }
}

#waffle
waffle_plot_fxn <- function(data,
                            title,
                            statistic = c("shown", "followed"),
                            make_proportional = T) {
  data %>%
    ggplot(aes(fill = factor(.data[[statistic]]),
               values = prop * 100))  +
    geom_waffle(
      n_rows = 10,
      size = .5,
      color = "white",
      make_proportional = make_proportional,
      show.legend = F,
      radius = unit(5, "pt")
    ) +
    scale_fill_manual(
      values = c(
        'alternative' = "#FFA500",
        'extremist' = "#CD5C5C",
        'mainstream' = "#015CB9",
        'other' = '#E3E6E6'
      )
    ) +
    labs(title = title) +
    #theme(plot.title = ggtext::element_markdown(size = 15, hjust = .5))+
    theme(
      legend.position = "none",
      legend.text = element_text(color = "#242829"),
      text = element_text(size = 16),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      plot.title = ggtext::element_markdown(size = 15, hjust = .5),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    )
}

# bar to show relative frequency of visits
scale_plot_fxn <- function(data,
                           multiplicative_factor = 8,
                           text_positions = c(-3, 4, 12, 100),
                           segment_y_lower = 4,
                           segment_y_upper = 8,
                           statistic = c("shown", "followed")) {
  start_points <- c(0,
                    data$cumulative_percent[1],
                    data$cumulative_percent[2],
                    data$cumulative_percent[3])
  end_points <- data$cumulative_percent
  
  bar_recs_shown_table <- data.frame(
    start_points = start_points * multiplicative_factor,
    end_points = end_points * multiplicative_factor,
    text_positions = text_positions * multiplicative_factor,
    color = c("#FFA500", "#CD5C5C", "#015CB9", 'gray'),
    labels = c("alt", "ext", "msm", "other")
  )
  
  out <- bar_recs_shown_table %>%
    ggplot() +
    theme_void() +
    geom_rect(
      aes(
        xmin = start_points,
        xmax = end_points,
        ymin = -2,
        ymax = 4,
        fill = labels
      ),
      show.legend = F
    ) +
    annotate(
      geom = "text",
      x = bar_recs_shown_table$text_positions,
      y = segment_y_upper + 3,
      size = 5,
      label = paste0("(", round(data$overall_percent, 1), "%)")
    ) +
    annotate(
      geom = "text",
      x = bar_recs_shown_table$text_positions,
      y = segment_y_upper + 8,
      size = 4.5,
      label = c("Alternative", "Extremist", "Mainstream\nmedia", "Other")
    ) +
    annotate(
      geom = "segment",
      x = 0,
      xend = bar_recs_shown_table$text_positions[1],
      y = segment_y_lower,
      yend = segment_y_upper
    ) +
    annotate(
      geom = "segment",
      x = 1 * multiplicative_factor,
      xend = bar_recs_shown_table$text_positions[2],
      y = segment_y_lower,
      yend = segment_y_upper
    ) +
    annotate(
      geom = "segment",
      x = 5 * multiplicative_factor,
      xend = bar_recs_shown_table$text_positions[3],
      y = segment_y_lower,
      yend = segment_y_upper
    ) +
    annotate(
      geom = "segment",
      x = 98 * multiplicative_factor,
      xend = bar_recs_shown_table$text_positions[4],
      y = segment_y_lower,
      yend = segment_y_upper
    ) +
    scale_fill_manual(values = c("#FFA500", "#CD5C5C", "#015CB9", '#E3E6E6'),
                      name = "") +
    scale_y_continuous(limits = c(-2, 25)) +
    scale_x_continuous(limits = c(-5, 101) * multiplicative_factor) +
    coord_fixed(ratio = 4)
  return(out)
}

coef_plot <- function(data) {
  
  if (str_detect(deparse(substitute(data)), "time")) {
    facet_labels <- c(
      "alternative" = str_wrap("Minutes/week on alternative channel videos", width = 30),
      "extremist" = str_wrap("Minutes/week on extremist channel videos", width = 30),
      "mainstream" = str_wrap("Minutes/week on mainstream media channel videos", width = 30)
    )
  } else {
    facet_labels <- c(
      "alternative" = str_wrap("Views/week on alternative channel videos", width = 30),
      "extremist" = str_wrap("Views/week on extremist channel videos", width = 30),
      "mainstream" = str_wrap("Views/week on mainstream media channel videos", width = 30)
    )
  }
  
  
  data %>%
    filter(predictor != "(Intercept)") %>%
    mutate(predictor = refactor_fxn(recode_fxn(predictor))) %>%
    ggplot(aes(x = estimate, y = str_wrap_factor(predictor, width = 12))) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_linerange(
      aes(
        xmin = ci_lwr,
        xmax = ci_upr,
        color = channel_type
      ),
      size = 9,
      show.legend = FALSE
    ) +
    geom_point(
      size = 10,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "#FFFFFF"
    ) +
    geom_text(aes(label = round(estimate, 2)),
              size = 3.5, color = "black") +
    geom_text(
      aes(label = stat_sig),
      size = 8,
      nudge_x = .45,
      nudge_y = .25
    ) +
    facet_wrap(~ channel_type,
               labeller = as_labeller(facet_labels)) +
    scale_color_manual(
      values = c(
        "alternative" = "#FFA500",
        "extremist" = "#CD5C5C",
        "mainstream" = "#015CB9"
      )
    ) +
    labs(y = "", x = "Quasipoisson coefficient") +
    theme_bw() +
    theme(
      strip.text = element_text(size = 12, hjust = .5),
      strip.background = element_rect(fill = "grey", color = "grey"),
      axis.text.y = element_text(size = 12)
    )
}