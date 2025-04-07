library(dplyr)
library(readr)
library(here)
library(tidyr)

df = readr::read_rds(here::here("data", "df_prescribed_parkinsons.rds"))
df = df %>%
  filter(has_accel & (is_parkinsons_prescribed | is_parkinsons_2021_prescribed))

# Data from https://physionet.org/content/minute-level-step-count-nhanes/1.0.0/
file = here::here("data", "nhanes_1440_AC.csv.xz")
# file = "~/Desktop/test/nhanes_1440_AC.csv.xz"
ac = read_csv(file)

pd_ac = ac %>%
  right_join(df %>% distinct(SEQN))
long = pd_ac %>%
  select(SEQN, PAXDAYM, PAXDAYWM, starts_with("min_")) %>%
  pivot_longer(cols = starts_with("min_"),
               names_to = "num_min",
               values_to = "counts") %>%
  mutate(
    num_min = as.numeric(sub("min_", "", num_min))
  )
stopifnot(!anyNA(long$num_min))
stopifnot(all(long$num_min %in% 1:1440))
long = long %>%
  mutate(counts = round(counts))
long = long %>%
  mutate(
    SEQN = as.character(SEQN),
    PAXDAYM = as.character(PAXDAYM),
    counts = as.integer(counts),
    num_min = as.integer(num_min)
  )
readr::write_rds(long, here::here("data", "nhanes_pd_ac.rds"),
                 compress = "xz")

# file = "~/Desktop/test/nhanes_1440_PAXFLGSM.csv.xz"
file = here::here("data", "nhanes_1440_PAXFLGSM.csv.xz")
flags = read_csv(file)
pd_flags = flags %>%
  right_join(df %>% distinct(SEQN))
long = pd_flags %>%
  select(SEQN, PAXDAYM, PAXDAYWM, starts_with("min_")) %>%
  pivot_longer(cols = starts_with("min_"),
               names_to = "num_min",
               values_to = "flag") %>%
  mutate(
    num_min = as.numeric(sub("min_", "", num_min))
  )
long = long %>%
  mutate(
    SEQN = as.character(SEQN),
    PAXDAYM = as.character(PAXDAYM),
    num_min = as.integer(num_min)
  )
stopifnot(!anyNA(long$num_min))
stopifnot(all(long$num_min %in% 1:1440))
readr::write_rds(long, here::here("data", "nhanes_pd_flags.rds"), compress = "xz")


file = here::here("data", "nhanes_1440_scsslsteps.csv.xz")
# file = "~/Desktop/test/nhanes_1440_scsslsteps.csv.xz"
steps = read_csv(file)

pd_steps = steps %>%
  right_join(df %>% distinct(SEQN))
long = pd_steps %>%
  select(SEQN, PAXDAYM, PAXDAYWM, starts_with("min_")) %>%
  pivot_longer(cols = starts_with("min_"),
               names_to = "num_min",
               values_to = "steps") %>%
  mutate(
    num_min = as.numeric(sub("min_", "", num_min))
  )
stopifnot(!anyNA(long$num_min))
stopifnot(all(long$num_min %in% 1:1440))
long = long %>%
  mutate(
    SEQN = as.character(SEQN),
    PAXDAYM = as.character(PAXDAYM),
    steps = as.integer(steps),
    num_min = as.integer(num_min)
  )
readr::write_rds(long, here::here("data", "nhanes_pd_scsslsteps.rds"), compress = "xz")
long = long %>%
  group_by(SEQN, PAXDAYM, PAXDAYWM) %>%
  summarise(
    n_min = sum(!is.na(steps)),
    n_steps = sum(steps, na.rm = TRUE)
  )

