library(readr); 
library(dplyr); 
library(stringr)
library(lubridate); 
library(janitor); 
library(forcats);
library(ggplot2);
library(broom)

top_n_types <- 10
max_days    <- 60

# Reading and picking cols
raw <- read_csv("raw_sf311_project2.csv", show_col_types = FALSE) %>% clean_names()

open_col  <- names(raw)[stringr::str_detect(names(raw), "(^|_)open|create|start")][1]
close_col <- names(raw)[stringr::str_detect(names(raw), "(^|_)clos")][1]
type_col  <- if ("request_details" %in% names(raw)) "request_details" else "request_type"

# Parse durations
parse_dt <- function(x) parse_date_time(
  x,
  orders = c("m/d/Y I:M:S p","m/d/y I:M:S p","m/d/Y I:M p","m/d/y I:M p"),
  tz = "America/Los_Angeles"
)

dat1 <- raw %>%
  mutate(opened_dt = parse_dt(.data[[open_col]]),
         closed_dt = parse_dt(.data[[close_col]]),
         duration_hours = as.numeric(difftime(closed_dt, opened_dt, units = "hours")))

# Filter durations
dat2 <- dat1 %>%
  filter(!is.na(opened_dt), !is.na(closed_dt), !is.na(duration_hours)) %>%
  filter(duration_hours >= 0, duration_hours <= 24 * max_days)

# Convert time to 24 hour
raw_open_kept <- raw[[open_col]][as.integer(rownames(dat2))]
m   <- stringr::str_match(raw_open_kept,"\\b(\\d{1,2}):(\\d{2})(?::\\d{2})?\\s*([AaPp][Mm])\\b")
h   <- suppressWarnings(as.integer(m[, 2]))
tag <- toupper(m[, 4])
hour_final <- h
hour_final[tag == "PM" & h != 12] <- h[tag == "PM" & h != 12] + 12L
hour_final[tag == "AM" & h == 12] <- 0L

# Create cleaned dataframe
dat3 <- dat2 %>%
  mutate(
    request_type = factor(raw[[type_col]][as.integer(rownames(dat2))]),
    request_type = fct_lump_n(request_type, n = top_n_types, other_level = "Other"),
    weekday      = wday(opened_dt, label = TRUE, abbr = TRUE),
    tod_bin      = case_when(
      hour_final >=  0 & hour_final <  6 ~ "Night (0–5)",
      hour_final >=  6 & hour_final < 12 ~ "Morning (6–11)",
      hour_final >= 12 & hour_final < 18 ~ "Afternoon (12–17)",
      hour_final >= 18 & hour_final <= 23 ~ "Evening (18–23)"
    )
  ) %>%
  mutate(tod_bin = factor(tod_bin, levels = c("Night (0–5)", "Morning (6–11)", "Afternoon (12–17)", "Evening (18–23)"))) %>%
  select(duration_hours, request_type, weekday, tod_bin) %>%
  tidyr::drop_na() %>%
  droplevels()

dat3 <- dat3 %>%
  mutate(
    weekday  = factor(weekday, ordered = FALSE),
    tod_bin  = factor(tod_bin, ordered = FALSE),
    request_type = factor(request_type)
  )
options(contrasts = c("contr.treatment", "contr.treatment"))

# Fit model
lm_ext <- lm(duration_hours ~ request_type + weekday + tod_bin, data = dat3)
summary(lm_ext)

# Make coef into plot labels
coef_df <- broom::tidy(lm_ext, conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(group = dplyr::case_when(stringr::str_starts(term, "request_type") ~ "Request type",
                                         stringr::str_starts(term, "weekday") ~ "Weekday",
                                         stringr::str_starts(term, "tod_bin") ~ "Time of day"),
                level = term %>%
                  stringr::str_remove("^request_type") %>%
                  stringr::str_remove("^weekday") %>%
                  stringr::str_remove("^tod_bin"))

# Request type only
coef_req <- coef_df %>% 
  dplyr::filter(group == "Request type") %>%
  dplyr::mutate(level = forcats::fct_reorder(level, estimate))

ggplot(coef_req, aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 2) +
  labs(x = "Difference in mean closure time (hours) vs reference",
       y = NULL) +
  theme_minimal(base_size = 12)

# Time of day only
coef_tod <- coef_df %>% dplyr::filter(group == "Time of day") %>%
  dplyr::mutate(level = forcats::fct_reorder(level, estimate))

ggplot(coef_tod, aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 2) +
  labs(x = "Difference in mean closure time (hours) vs reference",
       y = NULL) +
  theme_minimal(base_size = 12)

# Weekday only
coef_wday <- coef_df %>% dplyr::filter(group == "Weekday") %>%
  dplyr::mutate(level = forcats::fct_reorder(level, estimate))

ggplot(coef_wday, aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 2) +
  labs(x = "Difference in mean closure time (hours) vs reference",
       y = NULL) +
  theme_minimal(base_size = 12)

# Diagnostics
op <- par(mfrow = c(1, 2))

# Resid
plot(lm_ext, which = 1)

# Q-Q
plot(lm_ext, which = 2)

# Summary stats
dat3 %>%
  summarise(
    n_requests = n(),
    dur_min    = min(duration_hours, na.rm = TRUE),
    dur_q1     = quantile(duration_hours, 0.25, na.rm = TRUE),
    dur_median = median(duration_hours, na.rm = TRUE),
    dur_q3     = quantile(duration_hours, 0.75, na.rm = TRUE),
    dur_max    = max(duration_hours, na.rm = TRUE)) %>%
  kable(caption = "Sample size and closure-time summary (hours) after cleaning.")

kable(count(cleaned, request_type, sort = TRUE),
      caption = "Requests by request type (top 10 types kept, remaining lumped to 'Other').")

kable(count(cleaned, weekday),
      caption = "Requests by weekday (day of opening).")

kable(count(cleaned, tod_bin),
      caption = "Requests by time of day (based on opening hour).")