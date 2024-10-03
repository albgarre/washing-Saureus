
library(tidyverse)
library(readxl)
library(cowplot)

## M. guillermondi data

d <- read_excel("./data/Resultados experimento Limoneno Noel Mesozyma guillermondii.xlsx",
                sheet = "concentration") %>%
  select(treatment = Tratamientos, starts_with("Log")) %>%
  pivot_longer(-treatment, names_to = "rep", values_to = "logN") %>%
  mutate(rep = gsub("Log UFC/g ", "", rep))


d %>%
  ggplot(aes(x = treatment, y = logN)) +
  geom_point()

## Calculate reductions

d <- d %>%
  group_by(rep) %>%
  mutate(control = mean(ifelse(treatment == "Control", logN, NA), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(red = control - logN) %>%
  filter(!is.na(logN), treatment != "Control")

d %>%
  ggplot(aes(x = treatment, y = red)) +
  geom_point()

## 

d %>% 
  group_by(treatment) %>%
  summarize(m = mean(red), s = sd(red))

## Model fitting

d_model <- d %>%
  filter(grepl("L ", treatment)) %>% 
  separate(treatment, into = c("foo", "conc", "foo2"), sep = " ") %>%
  select(-starts_with("foo"), -rep, -logN, -control) %>%
  mutate(conc = as.numeric(conc))

d_model %>%
  group_by(conc) %>%
  summarize(m = mean(red), s = sd(red)) %>%
  ggplot(aes(x = conc, y = m)) +
  geom_point() +
  geom_errorbar(aes(ymin = m - s/sqrt(5), ymax = m + s/sqrt(5))) +
  geom_smooth()

model_conc <- nls(red ~ a + b*(conc/(conc + d)),
    data = d_model,
    start = list(a = 1, b = 1, d = 1))

## Table 1 (A)

model_conc %>%
  summary()

## Figure 1 (A)

p_guille <- d_model %>%
  group_by(conc) %>%
  summarize(m = mean(red), s = sd(red)) %>%
  ggplot(aes(x = conc, y = m)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = m - s/sqrt(5), ymax = m + s/sqrt(5)),
                width = .5, linewidth = 1)

red_cloro <- d %>%
  filter(treatment == "Cloro") %>%
  pull(red) %>%
  mean()

sd_cloro <- d %>%
  filter(treatment == "Cloro") %>%
  pull(red) %>%
  sd()

red_water <- d %>%
  filter(treatment == "Agua Lavado") %>%
  pull(red) %>%
  mean()
  
p_guille <- p_guille +
  geom_line(aes(x = x, y = y),
            linetype = 2, linewidth = 1,
            data = tibble(x = seq(0, 50, length = 1000),
                          y = predict(model_conc, newdata = tibble(conc = seq(0, 50, length = 1000))))
            ) +  
  geom_hline(yintercept = c(red_cloro + sd_cloro, red_cloro - sd_cloro),
                            linetype = 4,
                            linewidth = 1) +
  geom_hline(yintercept = c(red_cloro),
             linetype = 3,
             linewidth = 1) +
  theme_classic(base_size = 14) +
  labs(x = "Concentration of D-limonene (mM)",
       y = "Microbial reduction (log CFU/g)") +
  coord_cartesian(ylim = c(0.8, 2))

p_guille

## S. aureus data

d <- read_excel("./data/Resultados Natanael Staphilococcus aureus.xlsx",
                sheet = "concentration") %>%
  select(treatment = Tratamientos, starts_with("Log")) %>%
  filter(!is.na(treatment)) %>%
  pivot_longer(-treatment, names_to = "rep", values_to = "logN") %>%
  mutate(rep = gsub("Log UFC/g ", "", rep))


d %>%
  ggplot(aes(x = treatment, y = logN)) +
  geom_point()

## Calculate reductions

d <- d %>%
  group_by(rep) %>%
  mutate(control = mean(ifelse(treatment == "C+", logN, NA), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(red = control - logN) %>%
  filter(!is.na(logN), treatment != "C+")

d %>%
  ggplot(aes(x = treatment, y = red)) +
  geom_point()

## 

d %>% 
  group_by(treatment) %>%
  summarize(m = mean(red), s = sd(red))

## Model fitting

d_model <- d %>%
  filter(grepl("^L", treatment)) %>% 
  separate(treatment, into = c("foo", "conc"), sep = 1) %>%
  mutate(conc = gsub(" mM", "", conc),
         conc = as.numeric(conc)) %>%
  select(-starts_with("foo"), -rep, -logN, -control)

d_model %>%
  group_by(conc) %>%
  summarize(m = mean(red), s = sd(red)) %>%
  ggplot(aes(x = conc, y = m)) +
  geom_point() +
  geom_errorbar(aes(ymin = m - s/sqrt(5), ymax = m + s/sqrt(3))) +
  geom_smooth()

model_conc <- nls(red ~ a + b*(conc/(conc + d)),
                  data = d_model,
                  start = list(a = 1, b = 1, d = 1))

# model_conc <- nls(red ~ a*exp(a*c + conc)/(exp(a*c + conc) - 1),
#                   data = d_model,
#                   start = list(a = 2, c = 100))

## Table 1 (B)

model_conc %>%
  summary()

## Figure 1 (B)

p_aure <- d_model %>%
  group_by(conc) %>%
  summarize(m = mean(red), s = sd(red)) %>%
  ggplot(aes(x = conc, y = m)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = m - s/sqrt(5), ymax = m + s/sqrt(5)),
                width = .5, linewidth = 1)

red_cloro <- d %>%
  filter(treatment == "CL") %>%
  pull(red) %>%
  mean()

sd_cloro <- d %>%
  filter(treatment == "CL") %>%
  pull(red) %>%
  sd()

red_water <- d %>%
  filter(treatment == "Agua Lavado") %>%
  pull(red) %>%
  mean()

p_aure <- p_aure +
  geom_line(aes(x = x, y = y),
            linetype = 2, linewidth = 1,
            data = tibble(x = seq(0, 200, length = 1000),
                          y = predict(model_conc, newdata = tibble(conc = seq(0, 200, length = 1000))))
  ) +
  geom_hline(yintercept = c(red_cloro + sd_cloro, red_cloro - sd_cloro),
             linetype = 4,
             linewidth = 1) +
  geom_hline(yintercept = c(red_cloro),
             linetype = 3,
             linewidth = 1) +
  theme_classic(base_size = 14) +
  labs(x = "Concentration of D-limonene (mM)",
       y = "Microbial reduction (log CFU/g)") +
  coord_cartesian(ylim = c(0.8, 2))

p_aure

## Figure 1

p <- plot_grid(p_guille, p_aure, labels = "AUTO")
ggsave(p, filename = "Figure_1.png", width = 12, height = 6)

