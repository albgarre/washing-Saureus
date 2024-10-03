
library(tidyverse)
library(readxl)
library(ggsci)
library(cowplot)

## Load data

d <- read_excel("./data/Resultados experimento Limoneno Noel Mesozyma guillermondii.xlsx",
           sheet = "exposure_time") %>%
  select(time = `Tiempos (min)`, treatment, starts_with("Log")) %>%
  pivot_longer(-c(treatment, time), names_to = "rep", values_to = "logN") %>%
  mutate(rep = gsub("Log UFC/g ", "", rep))

ggplot(d) +
  geom_point(aes(x = time, y = logN, colour = treatment))

## Calculate number of reductions

d <- d %>%
  group_by(rep) %>%
  mutate(control = mean(ifelse(time == 0, logN, NA), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(red = control - logN)

ggplot(d, aes(x = time, y = red, colour = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

## Figure 2 (A)

p_guille <- d %>%
  group_by(time, treatment) %>%
  summarize(m = mean(red, na.rm = TRUE), s = sd(red, na.rm = TRUE)) %>%
  filter(time > 0) %>%
  ungroup() %>%
  mutate(treatment = ifelse(treatment == "EO", "D-limonene wash", "Commercial wash")) %>%
  ggplot(aes(x = time, y = m, colour = treatment)) +
  geom_smooth(method = "lm", se = FALSE,
              linetype = 2, linewidth = 1) +
  geom_point(aes(shape = treatment), size = 3) +
  geom_errorbar(aes(ymin = m - s/sqrt(5), ymax = m + s/sqrt(5)), 
                width = .2,
                linewidth = 1) +
  theme_classic(base_size = 14) +
  labs(x = "Washing time (min)",
       y = "Microbial reduction (log CFU/ml)") +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_color_aaas() +
  coord_cartesian(xlim = c(0, 15))

p_guille

## Fit linear models (Table 2A)

d %>%
  split(.$treatment) %>%
  map(., ~ lm(red ~ time, data = .)) %>%
  map(summary)

lm(red ~ time + treatment:time, data = d) %>%
  summary()

## Load data aureus

d <- read_excel("./data/Resultados Natanael Staphilococcus aureus.xlsx",
           sheet = "exposure_time") %>%
  pivot_longer(-c(treatment, time), names_to = "rep", values_to = "logN") %>%
  mutate(rep = gsub("LogN ", "", rep)) %>%
  filter(treatment != "agua") %>%
  filter(logN > 2)

ggplot(d) +
  geom_point(aes(x = time, y = logN, colour = treatment))

## Calculate number of reductions

d <- d %>%
  group_by(rep) %>%
  mutate(control = mean(ifelse(time == 0, logN, NA), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(red = control - logN)

ggplot(d, aes(x = time, y = red, colour = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

## Figure 2 (B)

p_aure <- d %>%
  group_by(time, treatment) %>%
  summarize(m = mean(red, na.rm = TRUE), s = sd(red, na.rm = TRUE)) %>%
  filter(time > 0) %>%
  ungroup() %>%
  mutate(treatment = ifelse(treatment == "EO", "D-limonene wash", "Commercial wash")) %>%
  ggplot(aes(x = time, y = m, colour = treatment)) +
  geom_smooth(method = "lm", se = FALSE,
              linetype = 2, linewidth = 1) +
  geom_point(aes(shape = treatment), size = 3) +
  geom_errorbar(aes(ymin = m - s/sqrt(5), ymax = m + s/sqrt(5)), 
                width = .2,
                linewidth = 1) +
  theme_classic(base_size = 14) +
  labs(x = "Washing time (min)",
       y = "Microbial reduction (log CFU/ml)") +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_color_aaas() +
  coord_cartesian(xlim = c(0, 15))

p_aure

## Figure 2

p <- plot_grid(p_guille, p_aure, labels = "AUTO")
ggsave(p, filename = "Figure_2.png", width = 12, height = 6)

## Fit linear models (Table 2-B)

d %>%
  split(.$treatment) %>%
  map(., ~ lm(red ~ time, data = .)) %>%
  map(summary)

lm(red ~ treatment*time, data = d) %>%
  summary()

lm(red ~ time + treatment:time, data = d) %>%
  summary()
























