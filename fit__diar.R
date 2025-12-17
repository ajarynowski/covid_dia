library(deSolve)
library(dplyr)


cases_df <- data.frame(
  date  = as.Date(c("2020-02-01","2020-03-01","2020-04-01","2020-05-01",
                    "2020-06-01","2020-07-01","2020-08-01","2020-09-01")),
  cases = c(117115, 104511, 84680, 87367, 93610, 102593, 93907, 78861)
)
# --- 1) Dane obserwowane: miesiace + cases ---
obs_monthly <- cases_df %>%
  mutate(month = format(date, "%Y-%m")) %>%
  select(month, cases)

sir_intervention_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # bazowa transmisja
    current_beta <- beta
    
    # 1. interwencja: np. 18–37 dzień (tak jak wcześniej)
    if (time >= t_start1 && time <= t_end1) {
      current_beta <- current_beta * (1 - red1)
    }
    
    # 2. interwencja: 21–119 dzień
    if (time >= t_start2 && time <= t_end2) {
      current_beta <- current_beta * (1 - red2)
    }
    
    # równania SIR
    dS <- -current_beta * S * I / N +(v1 + v2) * I
    dI <-  current_beta * S * I / N - (v1 + v2) * I
    dInc=current_beta * S * I / N 
    list(c(dS, dI, dInc))
  })
}

# --- 2) Funkcja: symulacja -> miesieczna incydencja modelu ---
simulate_monthly_GI <- function(red1, red2,
                                beta, v1, v2, N,
                                t_start1, t_end1, t_start2, t_end2,
                                S0, I0, Inc0,
                                start_date, times) {
  
  params <- c(
    beta = beta, v1 = v1, v2 = v2, N = N,
    t_start1 = t_start1, t_end1 = t_end1, red1 = red1,
    t_start2 = t_start2, t_end2 = t_end2, red2 = red2
  )
  
  init <- c(S = S0, I = I0, Inc = Inc0)
  
  out <- ode(
    y = init, times = times, func = sir_intervention_model,
    parms = params, method = "lsoda"
  ) %>% as.data.frame()
  
  out <- out %>%
    arrange(time) %>%
    mutate(
      date = start_date + time,
      Inc_day = c(Inc[1], diff(Inc))
    )
  
  monthly_raw <- out %>%
    group_by(month = format(date, "%Y-%m")) %>%
    summarise(pred = sum(Inc_day, na.rm = TRUE),
              n_days = n(),
              .groups = "drop")
  
  # Korekta dla miesiaca startowego, jeśli symulacja nie obejmuje pełnego miesiaca
  start_month <- format(start_date, "%Y-%m")
  monthly_adj <- monthly_raw %>%
    mutate(
      pred_adj = if_else(month == start_month,
                         pred * (as.integer(format(as.Date(paste0(start_month, "-01")) +
                                                     31, "%d")) / n_days),
                         pred)
    ) %>%
    select(month, pred = pred_adj)
  
  monthly_adj
}
S0 <- 20191800
I0 <- 28700

N  <- S0 + I0 
Inc0=0

beta <- 0.1365
v1   <- 0
v2   <- 0.1

# 1. interwencja (stara)
t_start1 <- 18
t_end1   <- 37
red1     <- 0.3   # 20% redukcji

# 2. interwencja (nowa)
t_start2 <- 21
t_end2   <- 179
red2     <- 0.26   # np. 5% redukcji – możesz zmienić



# --- 3) Funkcja straty (SSE) tylko po redukcji transmisji ---
loss_red2 <- function(red2,
                      fixed_red1 = 0.30,
                      start_date = as.Date("2020-03-12"),
                      times = 0:202) {
  
  pred_monthly <- simulate_monthly_GI(
    red1 = fixed_red1, red2 = red2,
    beta = beta, v1 = v1, v2 = v2, N = N,
    t_start1 = t_start1, t_end1 = t_end1, t_start2 = t_start2, t_end2 = t_end2,
    S0 = S0, I0 = I0, Inc0 = Inc0,
    start_date = start_date, times = times
  )
  
  comp <- obs_monthly %>%
    inner_join(pred_monthly, by = "month") %>%   # tylko miesiace wspólne (ważne przy starcie 2020-03-12)
    mutate(err = pred - cases)
  
  sum(comp$err^2, na.rm = TRUE)
}

# --- 4) Grid search po red2 ---
red2_grid <- seq(0, 0.90, by = 0.01)

fit_grid <- data.frame(red2 = red2_grid, loss = NA_real_)
for (i in seq_len(nrow(fit_grid))) {
  fit_grid$loss[i] <- loss_red2(fit_grid$red2[i])
}

best <- fit_grid[which.min(fit_grid$loss), ]
best



loss_red1_red2 <- function(red1, red2,
                           start_date = as.Date("2020-03-12"),
                           times = 0:202) {
  
  pred_monthly <- simulate_monthly_GI(
    red1 = red1, red2 = red2,
    beta = beta, v1 = v1, v2 = v2, N = N,
    t_start1 = t_start1, t_end1 = t_end1, t_start2 = t_start2, t_end2 = t_end2,
    S0 = S0, I0 = I0, Inc0 = Inc0,
    start_date = start_date, times = times
  )
  
  comp <- obs_monthly %>%
    inner_join(pred_monthly, by = "month") %>%
    mutate(err = pred - cases)
  
  sum(comp$err^2, na.rm = TRUE)
}

red1_grid <- seq(0.00, 0.80, by = 0.02)
red2_grid <- seq(0.00, 0.80, by = 0.02)

param_grid <- expand.grid(red1 = red1_grid, red2 = red2_grid)
param_grid$loss <- NA_real_

for (i in seq_len(nrow(param_grid))) {
  param_grid$loss[i] <- loss_red1_red2(param_grid$red1[i], param_grid$red2[i])
}

best2 <- param_grid[which.min(param_grid$loss), ]
best2