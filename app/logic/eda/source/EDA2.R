# =========================================================
# 00. Load datasets ---------------------------------------------------
# =========================================================
car_cost_2026_08 <- readRDS("processed/car_cost_2026_08.rds")
car_cost_archive <- readRDS("processed/car_cost_archive.rds")
coe_results <- readRDS("processed/coe_results.rds")
dereg_lta <- readRDS("processed/dereg_lta.rds")
omv_by_category <- readRDS("processed/omv_by_category.rds")
pop_by_age <- readRDS("processed/pop_by_age.rds")
pqp <- readRDS("processed/pqp.rds")
renewal_rate <- readRDS("processed/renewal_rate.rds")
revalidation <- readRDS("processed/revalidation.rds")
singstat_coe <- readRDS("processed/singstat_coe.rds")
vehicle_flows <- readRDS("processed/vehicle_flows.rds")

# =========================================================
# RQ1: Monthly COE supply — Categories A and B
# Chart 1: COE supply over time
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Select categories and check duplicates ---------------------------

coe_ab <- coe_results %>%
  filter(category %in% c("A", "B"))

stopifnot(
  !anyDuplicated(
    coe_ab[c("month", "bidding_no", "category")]
  )
)

# 2. Calculate monthly quota ------------------------------------------
# Only sum months with both bidding exercises available.
# Incomplete months remain NA and appear as gaps.

coe_monthly <- coe_ab %>%
  group_by(category, month) %>%
  summarise(
    quota = if (
      n_distinct(bidding_no) == 2L &&
      !anyNA(quota)
    ) {
      sum(quota)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  complete(
    category,
    month = seq(min(month), max(month), by = "month")
  ) %>%
  arrange(category, month) %>%
  mutate(
    category = factor(
      category,
      levels = c("A", "B")
    )
  )

# 3. Set a common y-axis range ----------------------------------------

y_max <- ceiling(
  max(coe_monthly$quota, na.rm = TRUE) / 1000
) * 1000

first_year <- as.integer(
  format(min(coe_monthly$month), "%Y")
)

last_year <- as.integer(
  format(max(coe_monthly$month), "%Y")
)

year_breaks <- as.Date(
  paste0(seq(first_year, last_year, by = 2), "-01-01")
)

# 4. Plot monthly quota as a solid line -------------------------------

p_rq1 <- ggplot(
  coe_monthly,
  aes(x = month, y = quota)
) +

  geom_line(
    colour = "#176B87",
    linewidth = 0.85,
    linetype = "solid",
    na.rm = TRUE
  ) +

  facet_wrap(
    ~ category,
    ncol = 1,
    scales = "fixed",
    axes = "all_x",
    axis.labels = "all_x",
    labeller = labeller(
      category = c(
        A = "Category A",
        B = "Category B",
      )
    )
  ) +

  scale_y_continuous(
    limits = c(0, y_max),
    breaks = seq(0, y_max, by = 1000),
    labels = scales::label_comma(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))
  ) +

  scale_x_date(
    breaks = year_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +

  labs(
    title = "COE supply over time",
    subtitle = "Monthly auction quotas for Categories A and B",
    x = NULL,
    y = "Number of COEs",
    caption = paste(
      "Source: supplied coe_results dataset.",
      "All panels share the same y-axis range. Incomplete months appear as gaps.",
      sep = "\n"
    )
  ) +

  theme_minimal(base_size = 12, base_family = "sans") +

  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "#E5EAF0",
      linewidth = 0.3
    ),

    plot.title = element_text(
      size = 22,
      face = "bold",
      colour = "#172B42"
    ),

    plot.subtitle = element_text(
      colour = "#52657A",
      margin = margin(b = 16)
    ),

    plot.title.position = "plot",

    strip.background = element_rect(
      fill = "#F1F5F9",
      colour = NA
    ),

    strip.text = element_text(
      size = 12,
      face = "bold",
      colour = "#172B42",
      hjust = 0
    ),

    axis.text = element_text(
      size = 10,
      colour = "#52657A"
    ),

    axis.title.y = element_text(
      colour = "#52657A",
      margin = margin(r = 12)
    ),

    panel.spacing = grid::unit(1.3, "lines"),

    plot.caption.position = "plot",

    plot.caption = element_text(
      hjust = 0,
      size = 9,
      colour = "#52657A",
      margin = margin(t = 12)
    ),

    plot.margin = margin(20, 24, 18, 20)
  )

print(p_rq1)

# 5. Save chart and monthly data --------------------------------------

dir.create(
  "outputs/rq1",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = "outputs/rq1/monthly_quota_AB.png",
  plot = p_rq1,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

write.csv(
  coe_monthly,
  "outputs/rq1/monthly_quota_AB.csv",
  row.names = FALSE
)

# =========================================================
# Chart 2: COE supply volatility over time
# Run after Chart 1, which creates coe_monthly
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Calculate trailing 12-month standard deviation -------------------

coe_volatility <- coe_monthly %>%
  filter(category %in% c("A", "B")) %>%
  select(category, month, quota) %>%

  # Ensure each observation represents one calendar month
  complete(
    category,
    month = seq(min(month), max(month), by = "month")
  ) %>%

  arrange(category, month) %>%
  group_by(category) %>%

  mutate(
    rolling_sd = vapply(
      seq_along(quota),

      function(i) {
        # At least 12 months are required
        if (i < 12L) {
          return(NA_real_)
        }

        window <- quota[(i - 11L):i]

        # Do not calculate across incomplete windows
        if (anyNA(window)) {
          return(NA_real_)
        }

        sd(window)
      },

      numeric(1)
    )
  ) %>%

  ungroup() %>%

  mutate(
    category = factor(
      category,
      levels = c("A", "B")
    )
  )

# 2. Define a shared y-axis range -------------------------------------
# Round the upper limit up to the next 100 COEs.
# Shared A/B scale.

valid_sd <- coe_volatility$rolling_sd[
  is.finite(coe_volatility$rolling_sd)
]

stopifnot(length(valid_sd) > 0)

sd_y_max <- max(
  100,
  ceiling(max(valid_sd) / 100) * 100
)

first_year <- as.integer(
  format(min(coe_volatility$month), "%Y")
)

last_year <- as.integer(
  format(max(coe_volatility$month), "%Y")
)

year_breaks <- as.Date(
  paste0(seq(first_year, last_year, by = 2), "-01-01")
)

# 3. Plot volatility --------------------------------------------------

p_volatility <- ggplot(
  coe_volatility,
  aes(x = month, y = rolling_sd)
) +

  geom_line(
    colour = "#176B87",
    linewidth = 0.85,
    linetype = "solid",
    na.rm = TRUE
  ) +

  facet_wrap(
    ~ category,
    ncol = 1,
    scales = "fixed",
    axes = "all_x",
    axis.labels = "all_x",

    labeller = labeller(
      category = c(
        A = "Category A",
        B = "Category B",
      )
    )
  ) +

  scale_y_continuous(
    limits = c(0, sd_y_max),
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_comma(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))
  ) +

  scale_x_date(
    breaks = year_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +

  labs(
    title = "COE supply volatility over time",

    subtitle = paste(
      "Trailing 12-month standard deviation",
      "for Categories A and B"
    ),

    x = NULL,
    y = "Standard deviation of monthly quota (COEs)",

    caption = paste(
      "Source: supplied coe_results dataset. All panels share the same y-axis range.",
      "Each value requires 12 complete months. Incomplete windows appear as gaps.",
      "Historical variability is not a forecast prediction interval.",
      sep = "\n"
    )
  ) +

  theme_minimal(
    base_size = 12,
    base_family = "sans"
  ) +

  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "#E5EAF0",
      linewidth = 0.3
    ),

    plot.title = element_text(
      size = 22,
      face = "bold",
      colour = "#172B42"
    ),

    plot.subtitle = element_text(
      size = 12,
      colour = "#52657A",
      margin = margin(b = 16)
    ),

    plot.title.position = "plot",

    strip.background = element_rect(
      fill = "#F1F5F9",
      colour = NA
    ),

    strip.text = element_text(
      size = 12,
      face = "bold",
      colour = "#172B42",
      hjust = 0
    ),

    axis.text = element_text(
      size = 10,
      colour = "#52657A"
    ),

    axis.title.y = element_text(
      colour = "#52657A",
      margin = margin(r = 12)
    ),

    panel.spacing = grid::unit(1.3, "lines"),

    plot.caption.position = "plot",

    plot.caption = element_text(
      hjust = 0,
      size = 9,
      colour = "#52657A",
      margin = margin(t = 12)
    ),

    plot.margin = margin(20, 24, 18, 20)
  )

print(p_volatility)

# 4. Save the chart and calculated values -----------------------------

dir.create(
  "outputs/chart2",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = "outputs/chart2/coe_volatility_AB.png",
  plot = p_volatility,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

write.csv(
  coe_volatility,
  "outputs/chart2/coe_volatility_AB.csv",
  row.names = FALSE
)

# =========================================================
# RQ2: How do historical deregistrations relate to subsequent COE supply?
# =========================================================
# Uses existing coe_results and dereg_lta objects; no data import required.
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

stopifnot(
  !anyDuplicated(coe_results[c("month", "bidding_no", "category")]),
  !anyDuplicated(dereg_lta[c("month", "category")]),
  inherits(coe_results$month, "Date"),
  inherits(dereg_lta$month, "Date")
)

# 1. Construct monthly quotas for A and B --------------------------
rq2_monthly <- coe_results %>%
  filter(category %in% c("A", "B")) %>%
  group_by(category, month) %>%
  summarise(
    quota = if (setequal(bidding_no, c(1, 2)) && !anyNA(quota))
      sum(quota) else NA_real_,
    .groups = "drop"
  ) %>%
  complete(category, month = seq(min(month), max(month), by = "month")) %>%
  mutate(
    # Quota periods begin in February, May, August and November.
    quota_start = floor_date(month %m-% months(1), "quarter") %m+% months(1)
  )

# 2. Require all three months of each quota period --------------------
rq2_quota <- rq2_monthly %>%
  group_by(category, quota_start) %>%
  summarise(
    complete_months = sum(!is.na(quota)),
    quarterly_quota = if (n() == 3L && !anyNA(quota))
      sum(quota) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(quota_start >= as.Date("2023-02-01")) %>%
  mutate(window_end = quota_start %m-% months(2))

# 3. Prepare monthly deregistration drivers ---------------------------
# A -> A deregistrations; B -> B deregistrations.
dereg_wide <- dereg_lta %>%
  filter(category %in% c("A", "B")) %>%
  select(month, category, deregistrations_lta) %>%
  complete(category, month = seq(min(month), max(month), by = "month")) %>%
  pivot_wider(names_from = category, values_from = deregistrations_lta)

stopifnot(all(c("A", "B") %in% names(dereg_wide)))

rq2_drivers <- dereg_wide %>%
  transmute(month, A, B) %>%
  pivot_longer(c(A, B), names_to = "category", values_to = "dereg") %>%
  arrange(category, month) %>%
  group_by(category) %>%
  mutate(
    # Rolling SUM, not rolling mean. Missing windows remain NA.
    deregistrations_12m = as.numeric(stats::filter(dereg, rep(1, 12), sides = 1))
  ) %>%
  ungroup() %>%
  transmute(category, window_end = month, deregistrations_12m)

# 4. Match historical information to the subsequent quota period ------
# Example: Feb-Apr 2023 quota uses Jan-Dec 2022 deregistrations.
rq2_all <- rq2_quota %>%
  left_join(rq2_drivers, by = c("category", "window_end")) %>%
  mutate(window_start = window_end %m-% months(11))

rq2_excluded <- rq2_all %>%
  filter(is.na(quarterly_quota) | is.na(deregistrations_12m))

rq2_data <- rq2_all %>%
  filter(!is.na(quarterly_quota), !is.na(deregistrations_12m)) %>%
  mutate(category = factor(category, levels = c("A", "B")))

print(rq2_excluded)
print(rq2_data)

# 5. Prepare and render aligned charts --------------------------------
rq2_plot_data <- rq2_data %>%
  mutate(category = factor(category, levels = c("A", "B"))) %>%
  arrange(category, quota_start)

# Each row is a matched historical window and subsequent quota period.
rq2_long <- rq2_plot_data %>%
  select(category, quota_start, deregistrations_12m, quarterly_quota) %>%
  pivot_longer(c(deregistrations_12m, quarterly_quota),
    names_to = "metric", values_to = "count") %>%
  mutate(
    metric = factor(metric, levels = c("deregistrations_12m", "quarterly_quota")),
    panel = paste(category, metric, sep = "_")
  )

panel_levels <- c(
  "A_deregistrations_12m", "A_quarterly_quota",
  "B_deregistrations_12m", "B_quarterly_quota",
)
panel_labels <- c(
  "A_deregistrations_12m" = "A | Historical deregistrations (12 months)",
  "A_quarterly_quota" = "A | Subsequent COE quota (3 months)",
  "B_deregistrations_12m" = "B | Historical deregistrations (12 months)",
  "B_quarterly_quota" = "B | Subsequent COE quota (3 months)",
)
rq2_long <- rq2_long %>%
  mutate(panel = factor(panel, levels = panel_levels))

# Display one date label every two quota periods on all four panels.
periods <- sort(unique(rq2_plot_data$quota_start))
date_breaks <- periods[seq(1, length(periods), by = 2)]

# Descriptive correlations annotate the right-hand panels.
rq2_correlations <- rq2_plot_data %>%
  group_by(category) %>%
  summarise(
    n = n(),
    r = if (n() >= 3 && sd(deregistrations_12m) > 0 && sd(quarterly_quota) > 0)
      cor(deregistrations_12m, quarterly_quota) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    panel = factor(paste0(category, "_quarterly_quota"), levels = panel_levels),
    label = sprintf("Matched-period r = %.2f | n = %d", r, n)
  )

p_rq2_aligned <- ggplot(rq2_long, aes(quota_start, count, colour = metric)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2) +
  geom_text(data = rq2_correlations,
    aes(x = min(rq2_plot_data$quota_start),
      y = Inf,
      label = label),
    inherit.aes = FALSE,
    hjust = -0.03, vjust = 1.4, size = 3.1, colour = "#52657A") +
  facet_wrap(~panel, ncol = 2, scales = "free_y",
    axes = "all", axis.labels = "all",
    labeller = labeller(panel = panel_labels)) +
  scale_colour_manual(values = c(
      "deregistrations_12m" = "#D97706", "quarterly_quota" = "#176B87"),
    guide = "none") +
  scale_x_date(breaks = date_breaks, date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(limits = c(0, NA), labels = scales::label_comma(accuracy = 1),
    breaks = scales::breaks_pretty(n = 4),
    expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Historical deregistrations and subsequent COE supply",
    subtitle = "Preceding 12-month deregistration counts and subsequent 3-month quotas, by category",
    x = "Start of subsequent 3-month quota period", y = "Number of vehicles / COEs",
    caption = paste(
      "Orange: actual counts in the relevant historical 12-month window. Blue: actual quota in the subsequent 3 months.",
      "Panel scales differ to reveal movements; compare direction and timing, not line heights or apparent slopes.",
      "For example, February 2023 pairs January-December 2022 deregistrations with February-April 2023 quota.",
      "Correlations are descriptive; overlapping windows and common trends matter.",
      "Source: supplied coe_results and dereg_lta datasets. Complete matched periods only.",
      sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5EAF0", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 22, colour = "#172B42"),
    plot.subtitle = element_text(size = 11, colour = "#52657A", margin = margin(b = 18)),
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, size = 9, colour = "#52657A", lineheight = 1.2,
      margin = margin(t = 14)),
    strip.background = element_rect(fill = "#F1F5F9", colour = NA),
    strip.text = element_text(face = "bold", size = 10, colour = "#172B42", hjust = 0),
    axis.text = element_text(colour = "#52657A", size = 9),
    axis.title = element_text(colour = "#52657A"),
    panel.spacing = grid::unit(1.6, "lines"),
    plot.margin = margin(20, 24, 18, 20))

print(p_rq2_aligned)
dir.create("outputs/rq2", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/rq2/deregistrations_and_future_supply_aligned.png", p_rq2_aligned,
  width = 14, height = 11, dpi = 300, bg = "white")
write.csv(rq2_correlations, "outputs/rq2/aligned_chart_correlations.csv", row.names = FALSE)

write.csv(rq2_data, "outputs/rq2/analysis_data.csv", row.names = FALSE)
write.csv(rq2_excluded, "outputs/rq2/excluded_periods.csv", row.names = FALSE)
capture.output(sessionInfo(), file = "outputs/rq2/sessionInfo.txt")

# RQ3: Are there seasonal or cyclical patterns in COE supply?
# =========================================================
# Chart 1: Monthly seasonality in COE supply
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Prepare monthly quotas -------------------------------------------
season_monthly <- coe_results %>%
  filter(category %in% c("A", "B"))

stopifnot(
  !anyDuplicated(
    season_monthly[c("month", "bidding_no", "category")]
  )
)

season_monthly <- season_monthly %>%
  group_by(category, month) %>%
  summarise(
    quota = if (
      setequal(bidding_no, c(1, 2)) && !anyNA(quota)
    ) sum(quota) else NA_real_,
    .groups = "drop"
  ) %>%
  complete(
    category,
    month = seq(min(month), max(month), by = "month")
  ) %>%
  mutate(
    year = as.integer(format(month, "%Y")),
    calendar_month = factor(
      as.integer(format(month, "%m")),
      levels = 1:12,
      labels = month.abb
    ),
    category = factor(category, levels = c("A", "B"))
  )

# 2. Keep years complete across A and B ------------------
season_data <- season_monthly %>%
  group_by(year) %>%
  filter(n() == 24L, !anyNA(quota)) %>%
  ungroup() %>%
  group_by(category, year) %>%
  mutate(
    annual_monthly_mean = mean(quota),
    deviation = quota - annual_monthly_mean
  ) %>%
  ungroup()

# 3. Create the boxplots ----------------------------------------------
p_seasonality <- ggplot(
  season_data,
  aes(x = calendar_month, y = deviation)
) +
  geom_hline(
    yintercept = 0,
    colour = "#52657A",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_boxplot(
    fill = "#DCECF0",
    colour = "#176B87",
    width = 0.6,
    linewidth = 0.5,
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(
      width = 0.12,
      height = 0,
      seed = 42
    ),
    colour = "#176B87",
    alpha = 0.4,
    size = 1.2
  ) +
  facet_wrap(
    ~ category,
    ncol = 1,
    scales = "free_y",
    axes = "all_x",
    axis.labels = "all_x",
    labeller = labeller(
      category = c(
        A = "Category A",
        B = "Category B",
      )
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Monthly patterns in COE supply",
    subtitle = paste(
      "Monthly quotas relative to each year's average;",
      "complete years only"
    ),
    x = NULL,
    y = "COEs above / below the year's monthly average",
    caption = paste(
      "Each dot represents one year. Boxes show the middle 50%; the middle line is the median.",
      "Panel scales differ. Annual centring removes year-level differences, but not within-year trends.",
      "Source: supplied coe_results dataset. Incomplete years excluded.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "#E5EAF0", linewidth = 0.3
    ),
    plot.title = element_text(
      face = "bold", size = 21, colour = "#172B42"
    ),
    plot.subtitle = element_text(
      colour = "#52657A", margin = margin(b = 15)
    ),
    strip.background = element_rect(
      fill = "#F1F5F9", colour = NA
    ),
    strip.text = element_text(
      face = "bold", hjust = 0, colour = "#172B42"
    ),
    axis.text = element_text(colour = "#52657A"),
    panel.spacing = grid::unit(1.3, "lines"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
      hjust = 0, size = 9, colour = "#52657A",
      margin = margin(t = 12)
    ),
    plot.margin = margin(20, 24, 18, 20)
  )

print(p_seasonality)

# 4. Save -------------------------------------------------------------
dir.create(
  "outputs/rq3",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "outputs/rq3/seasonality_AB.png",
  plot = p_seasonality,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)
# =========================================================
# Chart 2: Longer-term movements in COE supply
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Prepare monthly quotas -------------------------------------------
cycle_monthly <- coe_results %>%
  filter(category %in% c("A", "B"))

stopifnot(
  !anyDuplicated(
    cycle_monthly[c("month", "bidding_no", "category")]
  )
)

cycle_monthly <- cycle_monthly %>%
  group_by(category, month) %>%
  summarise(
    quota = if (
      setequal(bidding_no, c(1, 2)) && !anyNA(quota)
    ) sum(quota) else NA_real_,
    .groups = "drop"
  ) %>%
  complete(
    category,
    month = seq(min(month), max(month), by = "month")
  ) %>%
  mutate(
    year = as.integer(format(month, "%Y")),
    category = factor(category, levels = c("A", "B"))
  )

# 2. Identify years complete across A and B --------------
complete_years <- cycle_monthly %>%
  group_by(year) %>%
  summarise(
    usable = n() == 24L && !anyNA(quota),
    .groups = "drop"
  ) %>%
  filter(usable) %>%
  pull(year)

# Keep incomplete years as NA so the lines show gaps
annual_supply <- cycle_monthly %>%
  group_by(category, year) %>%
  summarise(
    average_monthly_quota = if (
      first(year) %in% complete_years
    ) mean(quota) else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(category, year)

# 3. Set a shared numerical y-axis ------------------------------------
y_upper <- ceiling(
  max(annual_supply$average_monthly_quota, na.rm = TRUE) / 1000
) * 1000

year_breaks <- seq(
  min(annual_supply$year),
  max(annual_supply$year),
  by = 2
)

# 4. Create the chart -------------------------------------------------
p_long_term <- ggplot(
  annual_supply,
  aes(x = year, y = average_monthly_quota)
) +
  geom_line(
    colour = "#176B87",
    linewidth = 0.85,
    linetype = "solid",
    na.rm = TRUE
  ) +
  geom_point(
    colour = "#176B87",
    size = 2,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ category,
    ncol = 1,
    scales = "fixed",
    axes = "all_x",
    axis.labels = "all_x",
    labeller = labeller(
      category = c(
        A = "Category A",
        B = "Category B",
      )
    )
  ) +
  scale_x_continuous(
    breaks = year_breaks
  ) +
  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = seq(0, y_upper, by = 1000),
    labels = scales::label_comma()
  ) +
  labs(
    title = "Longer-term movements in COE supply",
    subtitle = "Average monthly quota within each complete calendar year",
    x = NULL,
    y = "Average number of COEs per month",
    caption = paste(
      "All categories share one scale. Incomplete years appear as gaps; no interpolation is used.",
      "Broad rises and falls do not establish a regular or predictable cycle.",
      "Source: supplied coe_results dataset. Category definitions change over time.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "#E5EAF0", linewidth = 0.3
    ),
    plot.title = element_text(
      face = "bold", size = 21, colour = "#172B42"
    ),
    plot.subtitle = element_text(
      colour = "#52657A", margin = margin(b = 15)
    ),
    strip.background = element_rect(
      fill = "#F1F5F9", colour = NA
    ),
    strip.text = element_text(
      face = "bold", hjust = 0, colour = "#172B42"
    ),
    axis.text = element_text(colour = "#52657A"),
    panel.spacing = grid::unit(1.3, "lines"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
      hjust = 0, size = 9, colour = "#52657A",
      margin = margin(t = 12)
    ),
    plot.margin = margin(20, 24, 18, 20)
  )

print(p_long_term)

# 5. Save -------------------------------------------------------------
dir.create(
  "outputs/rq3",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "outputs/rq3/long_term_AB.png",
  plot = p_long_term,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

write.csv(
  annual_supply,
  "outputs/rq3/annual_supply_AB.csv",
  row.names = FALSE
)
# =========================================================
# RQ4: COE renewal activity over time
# =========================================================
# Uses existing renewal_rate object. No import or file-selection prompt.
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Select A and B and check data ------------------------------------
renewal_ab <- renewal_rate %>% filter(category %in% c("A", "B"))
stopifnot(inherits(renewal_ab$month, "Date"),
  !anyNA(renewal_ab[c("month", "category")]),
  !anyDuplicated(renewal_ab[c("month", "category")]),
  all(renewal_ab$revalidations >= 0 | is.na(renewal_ab$revalidations)),
  all(renewal_ab$deregistrations_lta >= 0 | is.na(renewal_ab$deregistrations_lta)))

# 2. Calculate the transparent proxy from its actual components -------
# This is not a measured probability of renewal among expiring vehicles.
renewal_plot_data <- renewal_ab %>%
  transmute(month, category, revalidations, deregistrations_lta,
    denominator = revalidations + deregistrations_lta,
    renewal_share = if_else(denominator > 0,
      revalidations / denominator, NA_real_)) %>%
  complete(category, month = seq(min(month), max(month), by = "month")) %>%
  mutate(category = factor(category, levels = c("A", "B"))) %>%
  arrange(category, month)

# 3. Descriptive summaries (pooled share uses counts, not average percentages) ---
renewal_summary <- renewal_plot_data %>%
  group_by(category) %>%
  summarise(
    observed_months = sum(!is.na(renewal_share)),
    missing_months = sum(is.na(renewal_share)),
    pooled_renewal_share = sum(revalidations[!is.na(renewal_share)]) /
    sum(denominator[!is.na(renewal_share)]),
    minimum_monthly_share = min(renewal_share, na.rm = TRUE),
    maximum_monthly_share = max(renewal_share, na.rm = TRUE),
    .groups = "drop")
print(renewal_summary)

# 4. Same percentage scale for both categories ------------------------
y_upper <- min(1, ceiling(max(renewal_plot_data$renewal_share, na.rm = TRUE) * 10) / 10)
first_year <- as.integer(format(min(renewal_plot_data$month), "%Y"))
last_year <- as.integer(format(max(renewal_plot_data$month), "%Y"))
year_breaks <- as.Date(paste0(seq(first_year, last_year, by = 2), "-01-01"))

p_renewal <- ggplot(renewal_plot_data, aes(month, renewal_share)) +
  geom_line(colour = "#176B87", linewidth = 0.8, linetype = "solid", na.rm = TRUE) +
  facet_wrap(~category, ncol = 1, scales = "fixed", axes = "all_x", axis.labels = "all_x",
    labeller = labeller(category = c(A = "Category A", B = "Category B"))) +
  scale_x_date(breaks = year_breaks, date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, y_upper), breaks = seq(0, y_upper, by = 0.1),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))) +
  labs(title = "COE renewal activity over time",
    subtitle = "Monthly renewal share proxy for Categories A and B",
    x = NULL, y = "Renewal share proxy (%)",
    caption = paste(
      "Proxy = revalidations / (revalidations + deregistrations). Both categories share the same scale.",
      "This is not a verified renewal rate among vehicles reaching COE expiry, or a forecast of future deregistrations.",
      "Source: supplied renewal_rate dataset; January 2015-July 2026.",
      sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5EAF0", linewidth = 0.3),
    plot.title = element_text(size = 22, face = "bold", colour = "#172B42"),
    plot.subtitle = element_text(colour = "#52657A", margin = margin(b = 15)),
    plot.title.position = "plot", plot.caption.position = "plot",
    strip.background = element_rect(fill = "#F1F5F9", colour = NA),
    strip.text = element_text(face = "bold", hjust = 0, colour = "#172B42"),
    axis.text = element_text(colour = "#52657A"),
    panel.spacing = grid::unit(1.3, "lines"),
    plot.caption = element_text(hjust = 0,
      size = 9,
      colour = "#52657A",
      margin = margin(t = 12)),
    plot.margin = margin(20, 24, 18, 20))
print(p_renewal)

# 5. Save chart and tables --------------------------------------------
dir.create("outputs/rq4", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/rq4/renewal_share_AB.png",
  p_renewal,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white")
write.csv(renewal_plot_data, "outputs/rq4/renewal_share_data.csv", row.names = FALSE)
write.csv(renewal_summary, "outputs/rq4/renewal_summary.csv", row.names = FALSE)

# =========================================================
# RQ5: How does COE supply relate to premiums and business exposure?
# =========================================================
# Chart 1: COE supply versus premium. Uses existing coe_results.
library(dplyr)
library(tidyr)
library(ggplot2)

market_raw <- coe_results %>% filter(category %in% c("A", "B"))
stopifnot(!anyDuplicated(market_raw[c("month", "bidding_no", "category")]))
market_monthly <- market_raw %>%
  group_by(category, month) %>%
  summarise(
    quota = if (setequal(bidding_no, c(1, 2)) && !anyNA(quota)) sum(quota) else NA_real_,
    premium = if (setequal(bidding_no,
        c(1,
          2)) && !anyNA(premium)) mean(premium) else NA_real_,
    .groups = "drop") %>%
  complete(category, month = seq(min(month), max(month), by = "month")) %>%
  arrange(category, month)

market_long <- market_monthly %>%
  pivot_longer(c(quota, premium), names_to = "metric", values_to = "value") %>%
  mutate(panel = factor(paste(category, metric, sep = "_"), levels = c(
        "A_quota", "A_premium", "B_quota", "B_premium", )))
panel_labels <- c(
  A_quota = "Category A | Monthly quota (COEs)",
  A_premium = "Category A | Average premium (S$)",
  B_quota = "Category B | Monthly quota (COEs)",
  B_premium = "Category B | Average premium (S$)",)
years <- as.integer(format(range(market_monthly$month), "%Y"))

p_supply_premium <- ggplot(market_long, aes(month, value, colour = metric)) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  facet_wrap(~panel, ncol = 2, scales = "free_y", axes = "all", axis.labels = "all",
    labeller = labeller(panel = panel_labels)) +
  scale_colour_manual(values = c(quota = "#176B87", premium = "#D97706"), guide = "none") +
  scale_x_date(breaks = as.Date(paste0(seq(years[1],
          years[2],
          by = 4),
        "-01-01")),
    date_labels = "%Y") +
  scale_y_continuous(limits = c(0,
      NA),
    labels = scales::label_comma(),
    breaks = scales::breaks_pretty(n = 4)) +
  labs(title = "COE supply and premiums over time",
    subtitle = "Monthly quotas and average auction premiums for Categories A and B",
    x = NULL, y = NULL,
    caption = paste(
      "Quota: sum of the two exercises. Premium: simple average of their clearing premiums (not PQP or a transaction-weighted price).",
      "Panels have separate scales; compare dates and direction, not heights. Incomplete months remain gaps.",
      "Co-movement does not establish causality. Demand, policy and category-definition changes also matter.",
      "Source: supplied coe_results dataset.", sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5EAF0", linewidth = 0.3),
    plot.title = element_text(size = 22, face = "bold", colour = "#172B42"),
    plot.subtitle = element_text(colour = "#52657A", margin = margin(b = 15)),
    strip.background = element_rect(fill = "#F1F5F9", colour = NA),
    strip.text = element_text(face = "bold", hjust = 0, colour = "#172B42", size = 10),
    axis.text = element_text(colour = "#52657A"),
    panel.spacing = grid::unit(1.5,
      "lines"),
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0,
      size = 9,
      colour = "#52657A",
      margin = margin(t = 12)),
    plot.margin = margin(20, 24, 18, 20))
print(p_supply_premium)

# Transparent illustrative exposure arithmetic, not estimated company losses.
# Replace these scenario inputs with the firm's uncovered COE requirements.
exposure_scenarios <- expand_grid(uncovered_vehicles = c(10, 50, 100),
  premium_increase_sgd = c(1000, 5000, 10000)) %>%
  mutate(additional_procurement_cost_sgd = uncovered_vehicles * premium_increase_sgd)

dir.create("outputs/rq5", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/rq5/supply_vs_premium_AB.png", p_supply_premium,
  width = 14, height = 10, dpi = 300, bg = "white")
write.csv(market_monthly, "outputs/rq5/market_monthly.csv", row.names = FALSE)
write.csv(exposure_scenarios,
  "outputs/rq5/illustrative_exposure_scenarios.csv",
  row.names = FALSE)
# Chart 2: Vehicle age distribution. Uses existing pop_by_age.
library(dplyr)
library(ggplot2)

car_age <- pop_by_age %>% filter(vehicle_type == "Cars")
stopifnot(!anyDuplicated(car_age[c("month", "vehicle_type", "age_lower")]))
# Select the latest snapshot with all 21 supplied age bins observed.
complete_snapshots <- car_age %>%
  group_by(month) %>%
  summarise(complete = n() == 21L && setequal(age_lower,
      0:20) && !anyNA(population),
    .groups = "drop") %>%
  filter(complete)
stopifnot(nrow(complete_snapshots) > 0)
snapshot_date <- max(complete_snapshots$month)
car_age_latest <- car_age %>%
  filter(month == snapshot_date) %>% arrange(age_lower) %>%
  mutate(age_label = if_else(age_lower == 20,
      "20+",
      paste0(age_lower,
        "-<",
        age_lower + 1)),
    age_label = factor(age_label, levels = age_label),
    cohort = if_else(age_lower %in% c(8, 9), "8 to under 10 years", "Other ages"))

age_summary <- car_age_latest %>% summarise(
  snapshot = snapshot_date, total_cars = sum(population),
  cars_8_to_under_10 = sum(population[age_lower %in% c(8, 9)]),
  share_8_to_under_10 = cars_8_to_under_10 / total_cars)
print(age_summary)

p_vehicle_age <- ggplot(car_age_latest, aes(age_label, population, fill = cohort)) +
  geom_col(width = 0.75) +
  geom_text(data = filter(car_age_latest, age_lower %in% c(8, 9)),
    aes(label = scales::comma(population)),
    vjust = -0.5,
    colour = "#9A4E00",
    size = 3.5) +
  scale_fill_manual(name = NULL, breaks = c("Other ages", "8 to under 10 years"),
    values = c("Other ages" = "#176B87",
      "8 to under 10 years" = "#D97706")) +
  scale_y_continuous(labels = scales::label_comma(),
    expand = expansion(mult = c(0,
        0.12))) +
  labs(title = "Car population by vehicle age",
    subtitle = paste("Latest complete snapshot:", format(snapshot_date, "%B %Y")),
    x = "Vehicle age (years)", y = "Number of cars",
    caption = paste(
      "The supplied data groups cars as A+B; separate A/B age profiles are unavailable.",
      "Highlighted cohorts are ages 8 to under 10. They are not guaranteed future deregistrations or COE supply.",
      "Vehicle age is not remaining COE validity. Renewals and early deregistration affect timing. The 20+ bin is open-ended.",
      "Source: supplied pop_by_age dataset. One snapshot is used; monthly populations are not summed.",
      sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5EAF0", linewidth = 0.3),
    plot.title = element_text(size = 22, face = "bold", colour = "#172B42"),
    plot.subtitle = element_text(colour = "#52657A", margin = margin(b = 15)),
    axis.text = element_text(colour = "#52657A"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top", legend.justification = "left",
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0,
      size = 9,
      colour = "#52657A",
      margin = margin(t = 12)),
    plot.margin = margin(20, 24, 18, 20))
print(p_vehicle_age)
dir.create("outputs/rq5", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/rq5/vehicle_age_cars.png", p_vehicle_age,
  width = 13, height = 8, dpi = 300, bg = "white")
write.csv(car_age_latest, "outputs/rq5/car_age_latest.csv", row.names = FALSE)
write.csv(age_summary, "outputs/rq5/age_summary.csv", row.names = FALSE)
