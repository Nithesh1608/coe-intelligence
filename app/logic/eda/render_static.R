# Regenerate Cat A/B static EDA PNGs into app/static/eda/.
# Source notebooks: app/logic/eda/source/EDA1.Rmd and EDA2.R
# Run from repo root: source("app/logic/eda/render_static.R")

root <- if (file.exists("rhino.yml")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../../../rhino.yml")) {
  normalizePath("../../..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run from the repo root (where rhino.yml lives) or from app/logic/eda/.")
}
setwd(root)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate); library(readr)
  library(ggplot2); library(scales)
})

out_dir <- file.path(root, "app/static/eda")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
data_dir <- file.path(root, "data", "processed")
rds <- function(name) read_rds(file.path(data_dir, name))

coe      <- rds("coe_results.rds")
singstat <- rds("singstat_coe.rds")
dereg    <- rds("dereg_lta.rds")
reval    <- rds("revalidation.rds")
age      <- rds("pop_by_age.rds")
pqp      <- rds("pqp.rds")

cat_colours <- c(A = "#1f77b4", B = "#d6404e")
cat_labels  <- c(A = "Cat A (smaller cars)", B = "Cat B (bigger cars)")
theme_coe <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_text(colour = "grey40", hjust = 0),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )
n0 <- function(x) comma(round(x))
pc <- function(x) percent(x, accuracy = 1)
dl <- function(x) dollar(round(x), prefix = "S$")
quota_quarter <- function(m) floor_date(m %m-% months(1), "quarter") %m+% months(1)
regimes <- tibble::tibble(
  start  = as.Date(c("2002-01-01", "2014-02-01", "2022-05-01")),
  end    = as.Date(c("2014-01-31", "2022-04-30", "2026-12-31")),
  regime = c("R1: Cat A = up to 1,600cc", "R2: plus a 97 kW power cap", "R3: EVs sorted by a 110 kW cap")
)
policy <- tibble::tibble(
  date  = as.Date(c("2013-03-01", "2018-02-01", "2020-04-01", "2023-05-01", "2025-02-01")),
  label = c("Loan curbs", "0% car growth", "COVID pause", "Cut and fill", "Extra COEs")
)
save_p <- function(p, name, w = 11, h = 6) {
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 140, bg = "white")
  cat("wrote", name, "\n")
}

# 1 premiums
premiums <- bind_rows(
  singstat |> filter(category %in% c("A", "B"), month < as.Date("2010-01-01")) |>
    select(month, bidding_no, category, premium),
  coe |> filter(category %in% c("A", "B")) |> select(month, bidding_no, category, premium)
) |>
  filter(!is.na(premium)) |>
  mutate(round_date = month + days(if_else(bidding_no == 1, 0L, 14L)))
lo <- premiums |> filter(category == "A") |> slice_min(premium, n = 1, with_ties = FALSE)
hi <- premiums |> filter(category == "B") |> slice_max(premium, n = 1, with_ties = FALSE)
p_prem <- ggplot(premiums, aes(round_date, premium)) +
  geom_rect(data = regimes, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = regime),
            inherit.aes = FALSE, alpha = 0.22) +
  geom_vline(data = policy, aes(xintercept = date), linetype = "dotted", colour = "grey40") +
  geom_text(data = policy, aes(x = date, y = Inf, label = label), inherit.aes = FALSE,
            angle = 90, hjust = 1.1, vjust = -0.4, size = 3.3, colour = "grey30") +
  geom_line(aes(colour = category), linewidth = 0.6) +
  scale_colour_manual(values = cat_colours, labels = cat_labels) +
  scale_fill_manual(values = c("grey60", "#9ecae1", "#fcbba1")) +
  scale_y_continuous(labels = dollar_format(prefix = "S$")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = paste0("Premiums have ranged from ", dl(lo$premium), " (Cat A, ", year(lo$month), ") to ",
                   dl(hi$premium), " (Cat B, ", year(hi$month), ")"),
    subtitle = "Every bidding round. Shading shows how Cat A was defined; dotted lines are policy changes.",
    x = NULL, y = "Quota premium", colour = NULL, fill = "Category definition"
  ) + theme_coe
save_p(p_prem, "e1_01_premiums.png", 11, 6.5)

# 2 age
aug <- age |>
  filter(vehicle_type == "Cars", month(month) == 8) |>
  transmute(year = year(month), a = pmin(age_lower, 20), population) |>
  group_by(year, a) |>
  summarise(pop = sum(population), .groups = "drop")
leave <- aug |>
  mutate(year = year + 1, a_next = pmin(a + 1, 20)) |>
  group_by(year, a_next) |>
  summarise(pop_start = sum(pop), .groups = "drop") |>
  inner_join(aug |> rename(a_next = a, pop_end = pop), by = c("year", "a_next")) |>
  mutate(exits = pop_start - pop_end, q = exits / pop_start)
scrap10 <- leave |> filter(a_next == 10)
scrap10_last <- scrap10$q[scrap10$year == max(scrap10$year)]
latest_month <- max(age$month)
ly <- year(latest_month)
cars_age <- age |>
  filter(vehicle_type == "Cars", month == latest_month) |>
  arrange(age_lower) |>
  mutate(
    reg = ifelse(age_lower >= 20, paste0("before\n", ly - 20),
                 paste0(ly - age_lower - 1, "/", substr(ly - age_lower, 3, 4))),
    short = ifelse(age_lower >= 20, "20+", as.character(age_lower)),
    label = factor(paste0(short, "\n", reg), levels = paste0(short, "\n", reg)),
    group = case_when(
      age_lower %in% 7:9 ~ "Reach year 10 by 2029",
      age_lower %in% 2:4 ~ "Bought in the 2021 to 2024 shortage",
      TRUE ~ "Other ages"
    ),
    group = factor(group, levels = c("Reach year 10 by 2029",
                                     "Bought in the 2021 to 2024 shortage", "Other ages"))
  )
aged_7_10 <- sum(cars_age$population[cars_age$age_lower %in% 7:9])
aged_2_5  <- sum(cars_age$population[cars_age$age_lower %in% 2:4])
cliff_y   <- max(cars_age$population[cars_age$age_lower %in% 9:10])
p_age <- ggplot(cars_age, aes(label, population, fill = group)) +
  geom_col() +
  geom_text(data = filter(cars_age, group != "Other ages"),
            aes(label = comma(population)), vjust = -0.4, size = 3.2) +
  annotate("segment", x = 10.6, xend = 10.6, y = cliff_y * 0.95, yend = cliff_y * 0.35,
           arrow = arrow(length = unit(0.25, "cm")), colour = "grey20") +
  annotate("text", x = 10.75, y = cliff_y * 0.7, hjust = 0, size = 3.6,
           label = paste0("About ", pc(scrap10_last), " of cars\nleave the road\nat year 10")) +
  scale_fill_manual(values = c(
    "Reach year 10 by 2029" = "#d6404e",
    "Bought in the 2021 to 2024 shortage" = "#1f77b4",
    "Other ages" = "grey75"
  )) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = paste0(n0(aged_7_10), " cars reach their 10-year COE expiry by 2029, but only ",
                   n0(aged_2_5), " follow behind them"),
    subtitle = paste("Cars on the road by age,", format(latest_month, "%B %Y")),
    x = "Age of car in years", y = "Number of cars", fill = NULL
  ) + theme_coe + theme(axis.text.x = element_text(size = 8.5))
save_p(p_age, "e1_02_age_profile.png", 11, 6)

# 3 quota rule
actual <- coe |>
  filter(category %in% c("A", "B"), !suspended) |>
  mutate(q_start = quota_quarter(month)) |>
  group_by(q_start, category) |>
  summarise(actual = sum(quota, na.rm = TRUE), exercises = sum(!is.na(quota)), .groups = "drop") |>
  filter(exercises == 6)
last12 <- dereg |>
  filter(category %in% c("A", "B")) |>
  arrange(category, month) |>
  group_by(category) |>
  mutate(last12 = cumsum(deregistrations_lta) - lag(cumsum(deregistrations_lta), 12)) |>
  ungroup() |>
  mutate(q_start = month %m+% months(2)) |>
  select(q_start, category, last12)
# A/B formula share of the replacement base (open-category reserve kept implicit).
ab_formula_share <- 0.90
since2023 <- actual |>
  inner_join(last12, by = c("q_start", "category")) |>
  filter(q_start >= as.Date("2023-02-01")) |>
  mutate(
    rule = ab_formula_share * 0.25 * last12,
    extra = actual - rule,
    q_label = ifelse(month(q_start) == 11,
                     paste0("Nov ", year(q_start), "-\nJan ", year(q_start) + 1),
                     paste0(format(q_start, "%b"), "-", format(q_start %m+% months(2), "%b"), "\n", year(q_start))),
    q_label = factor(q_label, levels = unique(q_label[order(q_start)])),
    x = as.integer(q_label)
  )
bars <- bind_rows(
  since2023 |> transmute(category, x, part = "COEs the 25% rule gives", ymin = 0, ymax = pmin(actual, rule)),
  since2023 |> filter(extra > 0) |> transmute(category, x, part = "Extra COEs LTA added", ymin = rule, ymax = actual),
  since2023 |> filter(extra < 0) |> transmute(category, x, part = "COEs LTA held back", ymin = actual, ymax = rule)
) |>
  mutate(part = factor(part, levels = c("COEs the 25% rule gives", "Extra COEs LTA added", "COEs LTA held back")))
labels_tbl <- since2023 |>
  mutate(
    txt = ifelse(extra >= 0, paste0("+", comma(round(extra))), comma(round(extra))),
    y = pmax(actual, rule), sign = ifelse(extra >= 0, "add", "cut")
  )
q_pos <- function(d) since2023$x[match(as.Date(d), since2023$q_start)]
events <- tibble::tibble(
  x = c(q_pos("2023-02-01"), q_pos("2023-05-01"), q_pos("2023-11-01"), q_pos("2025-02-01")),
  kind = c("ev_rule", "ev_add", "ev_add", "ev_add"),
  text = c("New\n25% rule\nstarts", "6,000\nCOEs\nbrought\nforward",
           "3,509\nmore\nCOEs\nadded", "20,000\nextra\nCOEs\nbegin"),
  category = "A"
) |> filter(!is.na(x))
extra_ab_share <- sum(since2023$extra) / sum(since2023$actual)
p_rule <- ggplot() +
  geom_rect(data = events, aes(xmin = x - 0.46, xmax = x + 0.46, ymin = -Inf, ymax = Inf, fill = kind),
            show.legend = FALSE, alpha = 0.6) +
  geom_text(data = events, aes(x = x, y = Inf, label = text, colour = kind), vjust = 1.1, size = 3.1,
            lineheight = 0.9, fontface = "bold", show.legend = FALSE) +
  geom_rect(data = bars, aes(xmin = x - 0.36, xmax = x + 0.36, ymin = ymin, ymax = ymax, fill = part)) +
  geom_text(data = labels_tbl, aes(x = x, y = y, label = txt, colour = sign),
            vjust = -0.5, size = 3.3, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ category, ncol = 1, scales = "free_y",
             labeller = labeller(category = c(A = "Cat A (smaller cars)", B = "Cat B (bigger cars)"))) +
  scale_fill_manual(
    values = c(
      "COEs the 25% rule gives" = "#c9ced6", "Extra COEs LTA added" = "#2b8cbe",
      "COEs LTA held back" = "#e34a33", ev_rule = "#ece7f7", ev_add = "#fff3c4"
    ),
    breaks = c("COEs the 25% rule gives", "Extra COEs LTA added", "COEs LTA held back")
  ) +
  scale_colour_manual(values = c(add = "#08589e", cut = "#b30000", ev_add = "#7a5c00", ev_rule = "#54278f")) +
  scale_x_continuous(breaks = seq_along(levels(since2023$q_label)), labels = levels(since2023$q_label),
                     expand = expansion(add = 0.3)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.45))) +
  labs(
    title = paste0("Since 2023, ", pc(extra_ab_share), " of Cat A and B COEs came from LTA's decisions, not its formula"),
    subtitle = "Grey = 25% rule; blue = extra LTA added; red = held back.",
    x = NULL, y = "COEs in the quarter", fill = NULL
  ) + theme_coe +
  theme(strip.text = element_text(face = "bold", size = 13, hjust = 0),
        panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 9))
save_p(p_rule, "e1_03_quota_rule.png", 12, 8.5)

# 4 locked-in
locked_in <- reval |>
  filter(category %in% c("A", "B"), tenure_years == 5) |>
  mutate(renew_year = year(month)) |>
  group_by(renew_year, category) |>
  summarise(renewals = sum(revalidations), months = n_distinct(month), .groups = "drop") |>
  filter(months == 12) |>
  mutate(exit_year = renew_year + 5) |>
  filter(exit_year >= 2020)
locked_year <- locked_in |> group_by(exit_year) |> summarise(cars = sum(renewals), .groups = "drop")
avg_21_24 <- mean(locked_year$cars[locked_year$exit_year %in% 2021:2024])
avg_26_28 <- mean(locked_year$cars[locked_year$exit_year %in% 2026:2028])
this_year <- year(max(reval$month))
x_today <- which(sort(unique(locked_in$exit_year)) == this_year) - 0.5
p_locked <- ggplot(locked_in, aes(factor(exit_year), renewals, fill = category)) +
  annotate("rect", xmin = x_today, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey93") +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_vline(xintercept = x_today, linetype = "dashed", colour = "grey30") +
  annotate("text", x = x_today + 0.1, y = Inf, vjust = 1.5, hjust = 0, size = 3.6,
           label = "Future, but already fixed", colour = "grey30") +
  scale_fill_manual(values = cat_colours, labels = cat_labels) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = paste0("Locked-in deregistrations average ", n0(avg_21_24), " a year in 2021 to 2024 and ",
                   n0(avg_26_28), " in 2026 to 2028"),
    subtitle = "Cars that must be scrapped because they took a 5-year COE renewal",
    x = "Year the car must leave the road", y = "Number of cars", fill = NULL
  ) + theme_coe
save_p(p_locked, "e1_04_locked_exits.png", 11, 6)

# 5 renewal scatter
renew_year <- reval |>
  filter(category %in% c("A", "B")) |>
  mutate(year = year(month)) |>
  group_by(year) |>
  summarise(
    renew_5yr = sum(revalidations[tenure_years == 5]),
    renew_10yr = sum(revalidations[tenure_years == 10]),
    months = n_distinct(month), .groups = "drop"
  ) |>
  filter(months == 12) |>
  mutate(renew_total = renew_5yr + renew_10yr)
turning <- function(a, name) {
  age |>
    filter(vehicle_type == "Cars", age_lower == a, month(month) == 12) |>
    transmute(year = year(month) + 1, population) |>
    rename(!!name := population)
}
pqp_year <- pqp |>
  filter(category %in% c("A", "B"), !is.na(pqp)) |>
  group_by(year = year(month)) |>
  summarise(avg_pqp = mean(pqp), .groups = "drop")
renewal <- renew_year |>
  inner_join(turning(9, "cars_turning_10"), by = "year") |>
  left_join(turning(19, "cars_turning_20"), by = "year") |>
  inner_join(pqp_year, by = "year") |>
  mutate(
    rate_high = renew_total / cars_turning_10,
    rate_low = renew_total / (cars_turning_10 + coalesce(cars_turning_20, 0)),
    rate_mid = (rate_high + rate_low) / 2
  ) |>
  arrange(year)
ct <- cor.test(renewal$rate_mid, renewal$avg_pqp)
p_ren <- ggplot(renewal, aes(avg_pqp / 1000, rate_mid)) +
  geom_linerange(aes(ymin = rate_low, ymax = rate_high), colour = "grey75", linewidth = 3) +
  geom_path(colour = "grey55", arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  geom_point(aes(size = cars_turning_10 / 1000), colour = "#1f77b4", alpha = 0.85) +
  geom_text(aes(label = year), nudge_y = 0.04, size = 3.6) +
  scale_y_continuous(labels = percent) +
  scale_size_continuous(name = "Cars turning 10\n(thousands)", range = c(2, 9)) +
  labs(
    title = paste(
      "Renewal rates have been", ifelse(ct$estimate < 0, "lower", "higher"),
      "in years when renewing cost more"
    ),
    subtitle = paste0(
      "Each dot is one year, Cat A + B. r = ", round(ct$estimate, 2),
      ", 95% CI ", round(ct$conf.int[1], 2), " to ", round(ct$conf.int[2], 2)
    ),
    x = "Average PQP (S$ thousands)", y = "Share of owners who renewed"
  ) + theme_coe + theme(legend.position = "right")
save_p(p_ren, "e1_05_renewal_pqp.png", 11, 6)

# 6 supply vs price
qd <- coe |>
  filter(category %in% c("A", "B"), !is.na(premium)) |>
  mutate(q_start = quota_quarter(month)) |>
  group_by(category, q_start) |>
  summarise(quota = sum(quota, na.rm = TRUE), premium = mean(premium, na.rm = TRUE), .groups = "drop") |>
  group_by(category) |>
  mutate(
    quota_change = quota - lag(quota, 4),
    premium_change = premium - lag(premium, 4),
    regime = case_when(
      q_start < as.Date("2014-02-01") ~ "R1",
      q_start < as.Date("2022-05-01") ~ "R2",
      TRUE ~ "R3"
    )
  ) |>
  ungroup() |>
  filter(is.finite(quota_change), is.finite(premium_change))
fit <- function(df) {
  m <- lm(premium_change ~ quota_change, data = df)
  tibble(r2 = summary(m)$r.squared, n = nrow(df))
}
by_regime <- qd |> group_by(category, regime) |> group_modify(~ fit(.x)) |> ungroup()
rg <- function(cat, reg, col) {
  v <- by_regime[[col]][by_regime$category == cat & by_regime$regime == reg]
  if (!length(v) || is.na(v[[1]])) NA_real_ else v[[1]]
}
strip_lab <- c(
  R1 = paste0("R1  (A R²=", percent(rg("A", "R1", "r2"), 1), ", B R²=", percent(rg("B", "R1", "r2"), 1), ")"),
  R2 = paste0("R2  (A R²=", percent(rg("A", "R2", "r2"), 1), ", B R²=", percent(rg("B", "R2", "r2"), 1), ")"),
  R3 = paste0("R3  (A R²=", percent(rg("A", "R3", "r2"), 1), ", B R²=", percent(rg("B", "R3", "r2"), 1), ")")
)
p_supply <- ggplot(qd, aes(quota_change, premium_change, colour = category)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15) +
  facet_wrap(~ regime, scales = "free_x", labeller = labeller(regime = strip_lab)) +
  scale_colour_manual(values = cat_colours, labels = cat_labels) +
  labs(
    title = paste0(
      "Since May 2022, quota changes explain ", pc(rg("A", "R3", "r2")),
      " of Cat A premium changes and ", pc(rg("B", "R3", "r2")), " of Cat B"
    ),
    subtitle = "Each dot is one quota quarter vs the same quarter a year earlier",
    x = "Change in quarterly quota", y = "Change in average premium", colour = NULL
  ) + theme_coe
save_p(p_supply, "e1_06_supply_price.png", 11, 6.5)

cat("EDA1 PNGs done\n")

# ---- EDA2 story panels (Cat A/B only) → numbered app assets ----------------

theme_eda2 <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5EAF0", linewidth = 0.3),
    plot.title = element_text(size = 20, face = "bold", colour = "#172B42"),
    plot.subtitle = element_text(colour = "#52657A", margin = margin(b = 14)),
    plot.title.position = "plot",
    strip.background = element_rect(fill = "#F1F5F9", colour = NA),
    strip.text = element_text(size = 11, face = "bold", colour = "#172B42", hjust = 0),
    axis.text = element_text(size = 10, colour = "#52657A"),
    axis.title = element_text(colour = "#52657A"),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.caption = element_text(hjust = 0, size = 9, colour = "#52657A", margin = margin(t = 10)),
    plot.margin = margin(16, 20, 14, 16)
  )
lab_ab <- c(A = "Category A", B = "Category B")

coe_monthly <- coe |>
  filter(category %in% c("A", "B")) |>
  group_by(category, month) |>
  summarise(
    quota = if (n_distinct(bidding_no) == 2L && !anyNA(quota)) sum(quota) else NA_real_,
    premium = if (n_distinct(bidding_no) == 2L && !anyNA(premium)) mean(premium) else NA_real_,
    .groups = "drop"
  ) |>
  complete(category, month = seq(min(month), max(month), by = "month")) |>
  arrange(category, month) |>
  mutate(category = factor(category, levels = c("A", "B")))

y_max <- ceiling(max(coe_monthly$quota, na.rm = TRUE) / 1000) * 1000
year_breaks <- as.Date(paste0(
  seq(as.integer(format(min(coe_monthly$month), "%Y")),
      as.integer(format(max(coe_monthly$month), "%Y")), by = 2),
  "-01-01"
))

p_supply_time <- ggplot(coe_monthly, aes(month, quota)) +
  geom_line(colour = "#176B87", linewidth = 0.85, na.rm = TRUE) +
  facet_wrap(~ category, ncol = 1, scales = "fixed", labeller = labeller(category = lab_ab)) +
  scale_y_continuous(limits = c(0, y_max), labels = label_comma(), expand = expansion(mult = c(0, 0.03))) +
  scale_x_date(breaks = year_breaks, date_labels = "%Y") +
  labs(
    title = "COE supply over time",
    subtitle = "Monthly auction quotas for Categories A and B",
    x = NULL, y = "Number of COEs"
  ) + theme_eda2
save_p(p_supply_time, "01_COE_supply_over_time.png", 12, 8)

coe_vol <- coe_monthly |>
  group_by(category) |>
  mutate(
    rolling_sd = vapply(seq_along(quota), function(i) {
      if (i < 12L) return(NA_real_)
      window <- quota[(i - 11L):i]
      if (anyNA(window)) return(NA_real_)
      stats::sd(window)
    }, numeric(1))
  ) |>
  ungroup()
sd_y_max <- max(100, ceiling(max(coe_vol$rolling_sd, na.rm = TRUE) / 100) * 100)
p_vol <- ggplot(coe_vol, aes(month, rolling_sd)) +
  geom_line(colour = "#176B87", linewidth = 0.85, na.rm = TRUE) +
  facet_wrap(~ category, ncol = 1, scales = "fixed", labeller = labeller(category = lab_ab)) +
  scale_y_continuous(limits = c(0, sd_y_max), labels = label_comma()) +
  scale_x_date(breaks = year_breaks, date_labels = "%Y") +
  labs(
    title = "COE supply volatility over time",
    subtitle = "Trailing 12-month standard deviation for Categories A and B",
    x = NULL, y = "SD of monthly quota (COEs)"
  ) + theme_eda2
save_p(p_vol, "02_COE_supply_volatility.png", 12, 8)

# Deregistrations → subsequent A/B quota
rq2_monthly <- coe |>
  filter(category %in% c("A", "B")) |>
  group_by(category, month) |>
  summarise(
    quota = if (setequal(bidding_no, c(1, 2)) && !anyNA(quota)) sum(quota) else NA_real_,
    .groups = "drop"
  ) |>
  complete(category, month = seq(min(month), max(month), by = "month")) |>
  mutate(quota_start = floor_date(month %m-% months(1), "quarter") %m+% months(1))
rq2_quota <- rq2_monthly |>
  group_by(category, quota_start) |>
  summarise(
    quarterly_quota = if (n() == 3L && !anyNA(quota)) sum(quota) else NA_real_,
    .groups = "drop"
  ) |>
  filter(quota_start >= as.Date("2023-02-01")) |>
  mutate(window_end = quota_start %m-% months(2))
rq2_drivers <- dereg |>
  filter(category %in% c("A", "B")) |>
  arrange(category, month) |>
  group_by(category) |>
  mutate(deregistrations_12m = as.numeric(stats::filter(deregistrations_lta, rep(1, 12), sides = 1))) |>
  ungroup() |>
  transmute(category, window_end = month, deregistrations_12m)
rq2_data <- rq2_quota |>
  inner_join(rq2_drivers, by = c("category", "window_end")) |>
  filter(!is.na(quarterly_quota), !is.na(deregistrations_12m)) |>
  mutate(category = factor(category, levels = c("A", "B")))
rq2_long <- rq2_data |>
  select(category, quota_start, deregistrations_12m, quarterly_quota) |>
  pivot_longer(c(deregistrations_12m, quarterly_quota), names_to = "metric", values_to = "count") |>
  mutate(
    metric = factor(metric, levels = c("deregistrations_12m", "quarterly_quota")),
    panel = factor(
      paste(category, metric, sep = "_"),
      levels = c(
        "A_deregistrations_12m", "A_quarterly_quota",
        "B_deregistrations_12m", "B_quarterly_quota"
      )
    )
  )
panel_lab <- c(
  A_deregistrations_12m = "A | Historical deregistrations (12 months)",
  A_quarterly_quota = "A | Subsequent COE quota (3 months)",
  B_deregistrations_12m = "B | Historical deregistrations (12 months)",
  B_quarterly_quota = "B | Subsequent COE quota (3 months)"
)
p_rq2 <- ggplot(rq2_long, aes(quota_start, count, colour = metric)) +
  geom_line(linewidth = 0.85) + geom_point(size = 2) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y", labeller = labeller(panel = panel_lab)) +
  scale_colour_manual(values = c(deregistrations_12m = "#D97706", quarterly_quota = "#176B87"), guide = "none") +
  scale_y_continuous(limits = c(0, NA), labels = label_comma()) +
  labs(
    title = "Historical deregistrations and subsequent COE supply",
    subtitle = "Preceding 12-month deregistrations and subsequent 3-month quotas — Categories A and B",
    x = "Start of subsequent quota period", y = "Vehicles / COEs"
  ) + theme_eda2
save_p(p_rq2, "03_Deregistrations_and_supply.png", 13, 9)

season_monthly <- coe_monthly |>
  mutate(
    year = as.integer(format(month, "%Y")),
    calendar_month = factor(as.integer(format(month, "%m")), levels = 1:12, labels = month.abb)
  )
season_data <- season_monthly |>
  group_by(year) |>
  filter(n() == 24L, !anyNA(quota)) |>
  ungroup() |>
  group_by(category, year) |>
  mutate(deviation = quota - mean(quota)) |>
  ungroup()
p_season <- ggplot(season_data, aes(calendar_month, deviation)) +
  geom_hline(yintercept = 0, colour = "#52657A", linetype = "dashed", linewidth = 0.4) +
  geom_boxplot(fill = "#DCECF0", colour = "#176B87", width = 0.6, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 42),
             colour = "#176B87", alpha = 0.4, size = 1.2) +
  facet_wrap(~ category, ncol = 1, scales = "free_y", labeller = labeller(category = lab_ab)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Monthly patterns in COE supply",
    subtitle = "Monthly quotas relative to each year's average; Categories A and B",
    x = NULL, y = "COEs above / below the year's monthly average"
  ) + theme_eda2
save_p(p_season, "04_Monthly_patterns.png", 12, 8)

complete_years <- season_monthly |>
  group_by(year) |>
  summarise(usable = n() == 24L && !anyNA(quota), .groups = "drop") |>
  filter(usable) |>
  pull(year)
annual_supply <- season_monthly |>
  group_by(category, year) |>
  summarise(
    average_monthly_quota = if (first(year) %in% complete_years) mean(quota) else NA_real_,
    .groups = "drop"
  )
y_upper <- ceiling(max(annual_supply$average_monthly_quota, na.rm = TRUE) / 1000) * 1000
p_long <- ggplot(annual_supply, aes(year, average_monthly_quota)) +
  geom_line(colour = "#176B87", linewidth = 0.85, na.rm = TRUE) +
  geom_point(colour = "#176B87", size = 2, na.rm = TRUE) +
  facet_wrap(~ category, ncol = 1, scales = "fixed", labeller = labeller(category = lab_ab)) +
  scale_y_continuous(limits = c(0, y_upper), labels = label_comma()) +
  labs(
    title = "Longer-term movements in COE supply",
    subtitle = "Average monthly quota within each complete calendar year — A and B",
    x = NULL, y = "Average COEs per month"
  ) + theme_eda2
save_p(p_long, "05_Long_term_movements.png", 12, 8)

renewal_rate <- rds("renewal_rate.rds")
renewal_plot <- renewal_rate |>
  filter(category %in% c("A", "B")) |>
  transmute(
    month, category, revalidations, deregistrations_lta,
    denominator = revalidations + deregistrations_lta,
    renewal_share = if_else(denominator > 0, revalidations / denominator, NA_real_)
  ) |>
  complete(category, month = seq(min(month), max(month), by = "month")) |>
  mutate(category = factor(category, levels = c("A", "B")))
y_ren <- min(1, ceiling(max(renewal_plot$renewal_share, na.rm = TRUE) * 10) / 10)
p_ren_act <- ggplot(renewal_plot, aes(month, renewal_share)) +
  geom_line(colour = "#176B87", linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ category, ncol = 1, scales = "fixed", labeller = labeller(category = lab_ab)) +
  scale_y_continuous(limits = c(0, y_ren), labels = label_percent(accuracy = 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "COE renewal activity over time",
    subtitle = "Monthly renewal share proxy for Categories A and B",
    x = NULL, y = "Renewal share proxy"
  ) + theme_eda2
save_p(p_ren_act, "06_Renewal_activity.png", 12, 7)

market_long <- coe_monthly |>
  pivot_longer(c(quota, premium), names_to = "metric", values_to = "value") |>
  mutate(panel = factor(
    paste(category, metric, sep = "_"),
    levels = c("A_quota", "A_premium", "B_quota", "B_premium")
  ))
m_lab <- c(
  A_quota = "Category A | Monthly quota (COEs)",
  A_premium = "Category A | Average premium (S$)",
  B_quota = "Category B | Monthly quota (COEs)",
  B_premium = "Category B | Average premium (S$)"
)
p_mkt <- ggplot(market_long, aes(month, value, colour = metric)) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y", labeller = labeller(panel = m_lab)) +
  scale_colour_manual(values = c(quota = "#176B87", premium = "#D97706"), guide = "none") +
  scale_y_continuous(limits = c(0, NA), labels = label_comma()) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  labs(
    title = "COE supply and premiums over time",
    subtitle = "Monthly quotas and average auction premiums for Categories A and B",
    x = NULL, y = NULL
  ) + theme_eda2
save_p(p_mkt, "07_Supply_and_premiums.png", 13, 8)

car_age <- age |> filter(vehicle_type == "Cars")
snap <- car_age |>
  group_by(month) |>
  summarise(complete = n() == 21L && setequal(age_lower, 0:20) && !anyNA(population), .groups = "drop") |>
  filter(complete)
snapshot_date <- max(snap$month)
car_age_latest <- car_age |>
  filter(month == snapshot_date) |>
  arrange(age_lower) |>
  mutate(
    age_label = if_else(age_lower == 20, "20+", paste0(age_lower, "-<", age_lower + 1)),
    age_label = factor(age_label, levels = age_label),
    cohort = if_else(age_lower %in% c(8, 9), "8 to under 10 years", "Other ages")
  )
p_age2 <- ggplot(car_age_latest, aes(age_label, population, fill = cohort)) +
  geom_col(width = 0.75) +
  scale_fill_manual(
    name = NULL,
    values = c("Other ages" = "#176B87", "8 to under 10 years" = "#D97706")
  ) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Car population by vehicle age",
    subtitle = paste("Latest complete snapshot:", format(snapshot_date, "%B %Y"), "— A+B cars"),
    x = "Vehicle age (years)", y = "Number of cars"
  ) + theme_eda2 + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
save_p(p_age2, "08_Vehicle_age_distribution.png", 12, 7)

cat("All static EDA PNGs written to", out_dir, "\n")
