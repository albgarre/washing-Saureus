
library(tidyverse)
library(biogrowth)

## data from https://doi.org/10.1111/j.1745-4565.1993.tb00103.x

d1 <- tibble(
  temp = c(rep(45, 6), rep(42, 26), rep(37, 12)),
  nacl = c(rep(8.5, 5), 16.5, rep(0.5, 8), rep(4.5, 6),
           rep(8.5, 6), rep(12.5, 6), rep(0.5, 5),
           rep(4.5, 4), rep(12.5, 3)
           ),
  nano2 = c(100, 0, 100, 100, 200, 100, 0, 0, rep(c(100, 0), 3),
            rep(c(0, 100), 9), rep(0, 5), rep(c(50, 150), 3), 50
            ),
  pH = c(4.8, 6.7, 6.7, 8.4, 6.6, 6.7, 4.5, 5, 5, 6, 6, 7, 7, 7.2, 5, 5, 6,
         6, 7, 7, 5, 5, 6, 6, 7, 7, 5, 5, 6, 6, 7, 7, 4.5, 5, 6, 7, 7.2,
         5.6, 5.7, 7.5, 7.5, 5.7, 5.8, 7.4
         ),
  gt = c(0, 1, 1, 0, .6, 0, 0, 1, 2, .4, 1, .4, .3, .3, 1.1, .9,
         .5, .5, .4, .5, 1.2, 3.7, .6, .8, .6, .8, 1.8, 0, 1.1,
         .9, .7, 1.3, 2.6, 1.2, .5, .3, .3, .5, .3, .8, .8, 2, 1.8, 2)
)

d2 <- tibble(
  temp = c(37, rep(28, 12), rep(19, 22), rep(15, 4), rep(12, 4)),
  nacl = c(12.5, rep(.5, 6), rep(8.5, 4), 16.5, rep(.5, 8), rep(4.5, 6), 8.5, 8.5,
           rep(12.5, 5), 16.5, 16.5, rep(.5, 8)
           ),
  nano2 = c(150, 0, 0, 0, 100, 0, 0, 100, 0, 100, 200, 100, 100, rep(0, 6),
            200, 0, 50, 50, 150, 150, 200, 0, 200, 0, 50, 50, 150, 150, 0, 200, 
            rep(0, 8)),
  pH = c(7.5, 4.5, 5, 6, 6.6, 7, 7.2, 4.8, 6.7, 6.8, 6.8, 8.4, 6.7, 4.5, 5, 6, 7, 7.2, 7.5, 7.5, 7.5, 5.8,
         7.6, 5.8, 7.6, 7.5, 7.5, 7.5, 7.5, 5.9, 7.5, 5.9, 7.5, 7.5, 7.5, 4.5,
         5, 6, 7, 4.5, 5, 6, 7),
  gt = c(2.3, .5, 1.1, .6, 1.2, .6, .6, 0, 3, 2.8, 3, 2.1, 5, 8.5, 3.5, 1.8, 1.7,
         2.1, 2.1, 2.1, 3.2, 5.1, 5.3, 6.9, 4.6, 3.6, 5.7, 6.3, 0, 38.6, 29.9, 0,
         28.2, 0, 0, 0, 0, 5.6, 3.1, 0, 14.5, 12.7, 4.7)
)

d <- bind_rows(d1, d2) %>%
  mutate(mu  = log10(2)/gt) %>%
  mutate(mu = ifelse(is.infinite(mu), 0, mu)) %>%
  select(-gt)

d %>%
  ggplot() +
  geom_point(aes(x = nano2, y = mu)) +
  facet_wrap("temp", scales = "free_y")

## Model

my_start <- list(mu_opt = 1, 
                 pH_xmin = 4, pH_xmax = 8, pH_xopt = 7,
                 temp_xmin = 10, temp_xmax = 48,
                 nacl_xmax = 15, nacl_xopt = 3
                 )

known_pars <- list(pH_n = 1, 
                   nacl_n = 1, nacl_xmin = 0,
                   temp_c = .1
                   )

sec_model_names <- c(pH = "CPM", 
                     temp = "fullRatkowsky",
                     nacl = "CPM"
                     )

m1 <- fit_secondary_growth(d, my_start, known_pars, sec_model_names) 

my_start <- list(mu_opt = 1, 
                 pH_xmin = 4, pH_xmax = 8, pH_xopt = 7,
                 temp_xmin = 10, temp_xmax = 48,
                 nacl_xmax = 15, nacl_xopt = 3
)

known_pars <- list(pH_n = 2, 
                   nacl_n = 1, nacl_xmin = 0,
                   temp_c = .1
)

sec_model_names <- c(pH = "CPM", 
                     temp = "fullRatkowsky",
                     nacl = "CPM"
)

m2 <- fit_secondary_growth(d, my_start, known_pars, sec_model_names) 

compare_secondary_fits(list(`n=1` = m1, `n=2` = m2)) %>% plot()

summary(m1)



