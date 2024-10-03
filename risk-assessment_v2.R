
library(tidyverse)
library(biorisk)

## Initial concentration: https://doi.org/10.3389/fmicb.2018.01263

values <- c(4.3, 0.36, .3, 2.3)

logN0 <- Normal$new("logN0")$
  map_input("mu", Constant$new("mu_logN0", mean(log10(values))))$
  map_input("sigma", Constant$new("sigma_logN0", sd(log10(values))))

prevalence <- 6.36/100

## Inactivation

washing_time <- Triangular$new("washing_time")$
  map_input("a", Constant$new("min_wash_time", 3))$
  map_input("c", Constant$new("max_wash_time", 4))$
  map_input("b", Constant$new("mode_wash_time", 6))

# reductions <- LinealModel$new("reduction")$  # Limonene
#   map_input("x", washing_time)$
#   map_input("a",
#             Normal$new("int_red")$
#               map_input("mu", Constant$new("mean_red", .4))$
#               map_input("sigma", Constant$new("sd_red", .13))
#             )$
#   map_input("b",
#             Normal$new("slope_red")$
#               map_input("mu", Constant$new("mean_slope", .08))$
#               map_input("sigma", Constant$new("sd_slope", .02))
#             )

reductions <- LinealModel$new("reduction")$  # Chlorine
  map_input("x", washing_time)$
  map_input("a",
            Normal$new("int_red")$
              map_input("mu", Constant$new("mean_red", .48))$
              map_input("sigma", Constant$new("sd_red", .13))
            )$
  map_input("b",
            Normal$new("slope_red")$
              map_input("mu", Constant$new("mean_slope", .069))$
              map_input("sigma", Constant$new("sd_slope", .02))
            )

logN_stor <- ElementMinus$new("logN_stor")$
  map_input("a", logN0)$
  map_input("b", reductions)

# plot_model(logN_stor)
# logN_stor$simulate(100)
# logN_stor$simulations

## Growth model

stor_temp <- Triangular$new("refrig_temp")$
  map_input("a", Constant$new("min_temp", 3.9))$
  map_input("b", Constant$new("max_temp", 7.4))$
  map_input("c", Constant$new("most_likely_temp", 5.4))

stor_pH <- Uniform$new("pH")$
  map_input("min", Constant$new("pH_min", 4))$
  map_input("max", Constant$new("pH_max", 5))

gamma_temp <- GammaFullRatkowsky_model$new("gamma_temp")$
  map_input("temperature", stor_temp)$
  map_input("Tmin",
            Normal$new("Tmin")$
              map_input("mu", Constant$new("mu_Tmin", 8.46))$
              map_input("sigma", Constant$new("sigma_Tmin", 2.06))
  )$
  map_input("Tmax",
            Normal$new("Tmax")$
              map_input("mu", Constant$new("mu_Tmax", 55.62))$
              map_input("sigma", Constant$new("sigma_Tmax", 3.36))
  )$
  map_input("c",
            Constant$new("c", 0.1)
  )

gamma_pH <- CardinalParameterModel$new("gamma_pH")$
  map_input("X", stor_pH)$
  map_input("Xmin",
            Normal$new("pHmin")$
              map_input("mu", Constant$new("mu_pHmin", 4.26))$
              map_input("sigma", Constant$new("sigma_pHmin", 0.16))
  )$
  map_input("Xmax",
            Normal$new("pHmax")$
              map_input("mu", Constant$new("mu_pHmax", 7.69))$
              map_input("sigma", Constant$new("sigma_pHmax", 0.16))
  )$
  map_input("Xopt",
            Normal$new("pHopt")$
              map_input("mu", Constant$new("mu_pHopt", 6.96))$
              map_input("sigma", Constant$new("sigma_pHopt", 0.19))
  )$
  map_input("n",
            Constant$new("n_temp", 1)
  )

total_gamma <- ElementTimes$new("gamma_total")$
  map_input("a", gamma_pH)$
  map_input("b", gamma_temp)

mu_stor <- ElementTimes$new("mu_stor")$
  map_input("a", total_gamma)$
  map_input("b", 
            Normal$new("muopt")$
              map_input("mu", Constant$new("mu_muopt", 0.8771))$
              map_input("sigma", Constant$new("sigma_muopt", 0.0821))
  )

mu_stor_filter <- MinFilter$new("mu_stor_filter", 0, rule = 2)$
  map_input("a", mu_stor)


# plot_model(mu_stor_filter)
# mu_stor_filter$simulate(100)  
# mu_stor_filter$density_plot()  

## Growth

stor_time <- Triangular$new("stor_time")$
  map_input("a", Constant$new("min_time", 24))$
  map_input("c", Constant$new("max_time", 24*4))$
  map_input("b", Constant$new("most_likely_time", 24*10))

exposure <- ExponentialGrowthNmax$new("exposure")$
  map_input("logN0", logN_stor)$
  map_input("t", stor_time)$
  map_input("mu", mu_stor_filter)$
  map_input("logNmax", Constant$new("logNmax", 8))

# plot_model(exposure)
# 
# exposure$simulate(1000)
# exposure$simulations
# exposure$density_plot()

## Dose-response: https://qmrawiki.org/experiments/staphylococcus-aureus

dose <- Concentration2Dose$new("dose")$
  map_input("logN", exposure)$
  map_input("size",
            Uniform$new("serving_size")$
              map_input("min", Constant$new("min_serving", 10))$
              map_input("max", Constant$new("max_serving", 60))
  )

Pill <- DoseResponse_Exponential$new("Pill")$
  map_input("dose", dose)$
  map_input("r",
            Constant$new("r_DR", 7.64E-08)
  )


## Number of cases

cases <- Pill2Cases_N$new("cases")$
  map_input("Pill", Pill)$
  map_input("servings", Constant$new("N", 1e6))

##

plot_model(cases)

## Simulations

cases$simulate(1e7, seed = 1242)

quantile_table(cases, chosen = c("exposure", "Pill", "dose"), 
               probs = c(.5, .1, .90)) %>%
  print()

# quantile_table(Pill, chosen = "Pill", probs = c(.5, .1, .90)) %>%
#   select(-node) %>%
#   pivot_longer(everything()) %>%
#   mutate(out = value*prevalence)

quantile_table(cases, chosen = "cases", probs = c(.5, .1, .90)) %>%
  select(-node) %>%
  pivot_longer(everything()) %>%
  mutate(out = value/1e6*prevalence) %>%
  print()



















