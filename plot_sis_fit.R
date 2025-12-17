library(dplyr)
library(ggplot2)
library(lubridate)

start_date <- as.Date("2020-03-12")

# pred_monthly_best: month, pred (z simulate_monthly_GI)
pred_monthly_plot <- pred_monthly_best %>%
  mutate(date = as.Date(paste0(month, "-01"))) %>%
  arrange(date) %>%
  mutate(
    days_in_month = lubridate::days_in_month(date),
    n_days_in_sim = if_else(
      month == "2020-03",
      as.integer(days_in_month - as.integer(format(start_date, "%d")) + 1L),  # 12..31 = 20
      days_in_month
    ),
    pred_plot = if_else(month == "2020-03", pred * (days_in_month / n_days_in_sim), pred)
  )


pred_plot
plot_monthly <- obs_monthly %>%
  mutate(date = as.Date(paste0(month, "-01"))) %>%
  left_join(pred_monthly_plot %>% select(month, pred_plot), by = "month") %>%
  arrange(date)

plot_long <- bind_rows(
  plot_monthly %>% transmute(date, series = "Observed monthly cases", value = cases),
  plot_monthly %>% transmute(date, series = "Model-predicted monthly incidence", value = pred_plot)
)

plot_long$value[10]=300*plot_long$value[10]/18

ggplot(plot_long, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Month",
    y = "Monthly incidence ",
    title = "Observed vs. model-predicted monthly incidence of diarrhea",
    subtitle = paste0(
      "Transmissibility reduction: lockdown = ",
      24, "%, hygiene = ",
      100 * best$red2, "%"
    ),
    color = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom")+
  annotate("rect", xmin = as_datetime('2020-03-30'),
           xmax = as_datetime('2020-04-20'),
           ymin = 0,
           ymax = Inf,
           alpha = 0.2, fill="green")+
  
  annotate("rect", xmin = as_datetime('2020-04-02'),
           xmax = as_datetime('2020-09-07'),
           ymin = 0,
           ymax = Inf,
           alpha = 0.5, fill="pink")
