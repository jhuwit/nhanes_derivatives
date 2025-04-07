library(dplyr)
library(readr)
library(here)
library(tidyr)
library(ggplot2)

df = readr::read_rds(here::here("data", "df_prescribed_parkinsons.rds"))
df = df %>%
  filter(has_accel & (is_parkinsons_prescribed | is_parkinsons_2021_prescribed)) %>%
  mutate(SEQN = as.character(SEQN))

ac = readr::read_rds(here::here("data", "nhanes_pd_ac.rds"))
flags = readr::read_rds(here::here("data", "nhanes_pd_flags.rds"))
steps = readr::read_rds(here::here("data", "nhanes_pd_scsslsteps.rds"))

data = flags %>%
  full_join(ac) %>%
  full_join(steps)
data = data %>%
  mutate(
    steps = ifelse(flag, NA_real_, steps),
    counts = ifelse(flag, NA_real_, counts)
  )

mean = data %>%
  group_by(SEQN, num_min) %>%
  summarise(
    across(c(steps, counts),
           ~ mean(.x, na.rm = TRUE),
           .names = "mean_{.col}"
    ),
    across(c(steps, counts),
           ~ median(.x, na.rm = TRUE),
           .names = "median_{.col}"
    ),
    n_day_steps = sum(!is.na(steps)),
    n_day_counts = sum(!is.na(counts))
  ) %>%
  ungroup()


mean = mean %>%
  arrange(SEQN, num_min) %>%
  group_by(SEQN) %>%
  mutate(sum_mean_steps = cumsum(mean_steps),
         sum_mean_counts = cumsum(mean_counts)) %>%
  ungroup()
mean = mean %>%
  left_join(df)
readr::write_rds(mean, here::here("data", "nhanes_pd_mean_metrics.rds"), compress = "xz")


waves = mean %>%
  group_by(wave, num_min) %>%
  summarise(
    mean_sum_mean_steps = mean(sum_mean_steps, na.rm = TRUE),
    mean_steps = mean(mean_steps, na.rm = TRUE)
  ) %>%
  ungroup()
overall = mean %>%
  group_by(num_min) %>%
  summarise(
    mean_sum_mean_steps = mean(sum_mean_steps, na.rm = TRUE),
    mean_steps = mean(mean_steps, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(wave = "Combined")
waves = waves %>%
  bind_rows(overall)

readr::write_rds(waves, here::here("data", "nhanes_pd_mean_steps.rds"), compress = "xz")


# SEQN 67306
mean %>%
  arrange(SEQN, num_min) %>%
  group_by(SEQN) %>%
  mutate(mean_steps = cumsum(mean_steps)) %>%
  ungroup() %>%
  mutate(SEQN = as.character(SEQN)) %>%
  ggplot(aes(x = num_min, y = mean_steps, colour = SEQN)) +
  geom_line() +
  guides(colour = "none")


daily = data %>%
  group_by(SEQN, PAXDAYM, PAXDAYWM) %>%
  summarise(
    n_min_steps = sum(!is.na(steps)),
    n_steps = sum(steps, na.rm = TRUE),
    n_min_counts = sum(!is.na(counts)),
    n_counts = sum(counts, na.rm = TRUE)
  ) %>%
  ungroup()


daily = daily %>%
  left_join(df)
readr::write_rds(daily, here::here("data", "nhanes_pd_daily.rds"),
                 compress = "xz")
