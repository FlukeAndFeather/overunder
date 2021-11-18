library(tidyverse)

# For one site
phenocamr::download_phenocam(site = "NEON.D01.BART.DP1.00042",
                             veg_type = "UN",
                             roi_id = "1000",
                             frequency = "3",
                             smooth = TRUE)
understory <- phenocamr::read_phenocam(dir(tempdir(),
                                           "NEON.D01.BART.DP1.00042",
                                           full.names = TRUE))
understory_data <- understory$data %>%
  mutate(date = as.Date(date))
ggplot(understory_data, aes(date, smooth_gcc_mean)) +
  geom_line() +
  theme_minimal()

understory_phases <- phenocamr::phenophases(understory)
understory_rising <- understory_phases$rising %>%
  mutate(across(starts_with("transition"),
                ~ as.Date(.x, origin = "1970-01-01")))
understory_falling <- understory_phases$falling %>%
  mutate(across(starts_with("transition"),
                ~ as.Date(.x, origin = "1970-01-01")))

rising_dates <- understory_rising$transition_50[understory_rising$gcc_value == "gcc_mean"]
falling_dates <- understory_falling$transition_50[understory_falling$gcc_value == "gcc_mean"]
ggplot(understory_data, aes(date, smooth_gcc_mean)) +
  geom_line() +
  geom_vline(xintercept = rising_dates, linetype = 2, color = "green") +
  geom_vline(xintercept = falling_dates, linetype = 2, color = "brown") +
  theme_minimal()

rising_doy <- lubridate::yday(rising_dates)
falling_doy <- lubridate::yday(falling_dates)
green_days <- falling_doy - rising_doy
dateyear <- function(d) lubridate::year(lubridate::ymd(d))
phase_data <- tibble(rising_doy, falling_doy, year = dateyear(rising_dates)) %>%
  pivot_longer(c(rising_doy, falling_doy),
               names_to = "phenology",
               values_to = "date")

ggplot(phase_data, aes(year, date, color = phenology)) +
  geom_line() +
  scale_color_manual(values = c("brown", "green")) +
  theme_minimal() +
  theme(legend.position = "none")

phase_data %>%
  group_by(year) %>%
  summarize(duration = max(date) - min(date)) %>%
  ggplot(aes(year, duration)) +
  geom_line() +
  theme_minimal()
