Sys.setlocale("LC_TIME", "English")


# 1) Obserwacje (rzeczywista zapadalność = cases_7d)
obs_plot <- gi_daily %>%
  filter(date >= start_date, date <= start_date + max(times)) %>%
  arrange(date) %>%
  mutate(time = as.integer(date - start_date)) %>%
  select(date, time, obs_cases7d = cases_7d)

# 2) Predykcja modelu (optymalna, skorygowana Inc_day)
pred_plot <- simulate_daily_GI(
  red1 = best$red1, red2 = best$red2,
  beta = beta, v1 = v1, v2 = v2, N = N,
  t_start1 = t_start1, t_end1 = t_end1,
  t_start2 = t_start2, t_end2 = t_end2,
  S0 = S0, I0 = I0, R0 = R0, Inc0 = Inc0,
  start_date = start_date, times = times,
  scale_inc = scale_inc
) %>%
  select(date, time, pred_inc_adj = pred)

# 3) Sklej i narysuj
plot_df <- obs_plot %>%
  inner_join(pred_plot, by = c("date", "time"))

ggplot(plot_df, aes(x = date)) +
  geom_line(aes(y = obs_cases7d, color = "Observed incidence (7-day smoothed)"), linewidth = 1) +
  geom_line(aes(y = pred_inc_adj, color = "Model-predicted incidence (best fit)"), linewidth = 1) +
  labs(
    x = "Date",
    y = "Daily incidence",
    title = "COVID-19 Observed vs. model (best fit)",
    subtitle = paste0("tranmissability reducction: lockdown = ", 100*best$red1, "%, HH= ", 100*best$red2, "%"),
    color = NULL
  ) +
  scale_color_discrete(breaks = c("Observed  (7-day MA)",
                                  "Model")) +
  theme(legend.position = "bottom")+
  theme_bw()


ggplot(plot_df, aes(x = date)) +
  geom_line(aes(y = obs_cases7d,
                color = "Observed incidence (7-day smoothed)"),
            linewidth = 1) +
  geom_line(aes(y = pred_inc_adj,
                color = "Model-predicted incidence (best fit)"),
            linewidth = 1) +
  scale_color_discrete(
    labels = c(
      "Observed incidence (7-day smoothed)" = "Observed (7-day MA)",
      "Model-predicted incidence (best fit)" = "Model (best fit)"
    )
  ) +
  labs(
    x = "Date",
    y = "Daily incidence",
    title = "COVID-19 Observed vs. model (best fit)",
    subtitle = paste0(
      "Transmissibility reduction: lockdown = ",
      100 * best$red1, "%, hygiene = ",
      100 * best$red2, "%"
    ),
    color = NULL
  ) +
  annotate("rect", xmin = as_datetime('2020-03-30'),
           xmax = as_datetime('2020-04-20'),
           ymin = 0,
           ymax = Inf,
           alpha = 0.2, fill="green")+
  
  annotate("rect", xmin = as_datetime('2020-04-02'),
           xmax = as_datetime('2020-09-07'),
           ymin = 0,
           ymax = Inf,
           alpha = 0.5, fill="pink")+
  theme_bw() +
  theme(legend.position = "bottom")







  
ggplot(param_grid, aes(x = red1, y = red2, fill = loss)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    name = "Loss (SSE,\nlog scale)"
  ) +
  labs(
    x = "Transmission reduction – lockdown (red1)",
    y = "Transmission reduction – hygiene measures (red2)",
    title = "Loss surface for SIR model with interventions",
    subtitle = "Grid search over intervention effectiveness parameters"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )




ggplot(param_grid, aes(x = red1, y = red2, fill = loss)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    name = "Loss (SSE,\nlog scale)"
  ) +
  labs(
    x = "Transmission reduction – lockdown (red1)",
    y = "Transmission reduction – hygiene measures (red2)",
    title = "Loss surface for SIS model (GI) with interventions",
    subtitle = "Grid search over intervention effectiveness parameters"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
