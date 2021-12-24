personality_plot <- function(data = df,
                        test = "HEXACO",
                        domain = "Honesty-Humility") {

  title <- paste0(test, " ", domain, " ", "Scores")

  p <- df %>%
    filter(Test == test,
           Domain == domain) %>%
    ggplot(aes(x = Date_Taken, y = Score, ymin = Scale_Min, ymax = Scale_Max)) +
    geom_ribbon(
      aes(
        ymin = Tenth_Percentile,
        ymax = Ninetieth_Percentile
      ),
      fill = "grey70",
      alpha = .5
    ) +
    geom_line() +
    geom_line(aes(y = Fiftieth_Percentile, x = Date_Taken), color = "grey30", linetype = "dashed") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(~Metric) +
    labs(title = title,
         x = "Year",
         y = "Scale") +
    theme(legend.position = "none")

  return(ggplotly(p))
}

trend_over_time_plot <- function(data = df,
                                 metric = "Grit",
                                 loess = TRUE) {
  if(loess == TRUE) {
    title <- paste0(metric, " Over Time, Using A LOESS Curve")

    p <- data %>%
      filter(Metric == metric) %>%
      ggplot(aes(x=Date_Taken, y=Score, ymin = Scale_Min, ymax = Scale_Max)) +
      geom_smooth(se = F) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = title,
           x = "Year",
           y = "Scale")
   }
  else {
    title <- paste0(metric, " Over Time")

    p <- data %>%
      filter(Metric == metric) %>%
      ggplot(aes(x=Date_Taken, y=Score, ymin = Scale_Min, ymax = Scale_Max)) +
      geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = title,
           x = "Year",
           y = "Scale")
  }

  return(ggplotly(p))
}
