# Render the improved 6-chart EDA gallery into app/static/eda/.
# Source notebook: app/logic/eda/source/coe_eda_gallery.Rmd
# Run from repo root:
#   source("app/logic/eda/render_gallery.R")

root <- if (file.exists("rhino.yml")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../../../rhino.yml")) {
  normalizePath("../../..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run from the repo root (where rhino.yml lives).")
}
setwd(root)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate); library(readr)
  library(ggplot2); library(scales); library(patchwork); library(ggridges)
})

out_dir <- file.path(root, "app/static/eda")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
data_dir <- file.path(root, "data", "processed")
rds <- function(name) read_rds(file.path(data_dir, name))

coe   <- rds("coe_results.rds")
dereg <- rds("dereg_lta.rds")
reval <- rds("revalidation.rds")
age   <- rds("pop_by_age.rds")
pqp   <- rds("pqp.rds")

cat_colours <- c(A = "#1f77b4", B = "#d6404e")
cat_labels  <- c(A = "Cat A (smaller cars)", B = "Cat B (bigger cars)")
theme_coe <- theme_minimal(base_size = 12.5) +
  theme(
    legend.position = "top", legend.justification = "left",
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "grey30"),
    plot.caption = element_text(colour = "grey40", hjust = 0, size = 10),
    plot.caption.position = "plot",
    strip.text = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )
src <- function(...) paste0("Source: ", ..., ". Analysis: DBA3702 Group 6.")
n0 <- function(x) comma(round(x))
pc <- function(x) percent(x, accuracy = 1)
dl <- function(x) dollar(round(x), prefix = "S$")
dk <- function(x) ifelse(round(abs(x) / 1000) == 0, "S$0",
                         paste0(ifelse(x < 0, "-", "+"), "S$", round(abs(x) / 1000), "k"))
quota_quarter <- function(m) floor_date(m %m-% months(1), "quarter") %m+% months(1)
regime_of <- function(m) case_when(
  m < as.Date("2014-02-01") ~ "R1 (to Jan 2014)",
  m < as.Date("2022-05-01") ~ "R2 (Feb 2014 to Apr 2022)",
  TRUE ~ "R3 (May 2022 on)"
)
r3_start <- as.Date("2022-05-01")

save_p <- function(p, name, w = 11, h = 5.6) {
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 140, bg = "white")
  cat("wrote", name, "\n")
}

quarters <- coe |>
  filter(category %in% c("A", "B"), !is.na(premium)) |>
  mutate(q_start = quota_quarter(month)) |>
  group_by(category, q_start) |>
  summarise(quota = sum(quota), bids = sum(bids_received), premium = mean(premium),
            rounds = n(), .groups = "drop") |>
  filter(rounds == 6) |>
  mutate(bids_per_coe = bids / quota)

last12 <- dereg |>
  filter(category %in% c("A", "B")) |>
  arrange(category, month) |>
  group_by(category) |>
  mutate(last12 = cumsum(deregistrations_lta) - lag(cumsum(deregistrations_lta), 12)) |>
  ungroup() |>
  mutate(q_start = month %m+% months(2)) |>
  select(q_start, category, last12)

rule <- quarters |>
  inner_join(last12, by = c("q_start", "category")) |>
  filter(q_start >= as.Date("2023-02-01")) |>
  mutate(rule = 0.9 * 0.25 * last12)

rule_q <- rule |>
  group_by(q_start) |>
  summarise(actual = sum(quota), rule = sum(rule), .groups = "drop") |>
  mutate(extra_share = (actual - rule) / actual)
peak_extra <- rule_q |> slice_max(extra_share, n = 1)
last_extra <- rule_q |> slice_max(q_start, n = 1)

latest_month <- max(age$month)
ly <- year(latest_month)
cars_age <- age |>
  filter(vehicle_type == "Cars", month == latest_month) |>
  arrange(age_lower)
aged_7_10 <- sum(cars_age$population[cars_age$age_lower %in% 7:9])
aged_2_5  <- sum(cars_age$population[cars_age$age_lower %in% 2:4])

aug <- age |>
  filter(vehicle_type == "Cars", month(month) == 8) |>
  transmute(year = year(month), a = pmin(age_lower, 20), population) |>
  group_by(year, a) |> summarise(pop = sum(population), .groups = "drop")

renew_type <- reval |>
  filter(category %in% c("A", "B")) |>
  group_by(year = year(month), tenure_years) |>
  summarise(n = sum(revalidations), months = n_distinct(month), .groups = "drop") |>
  filter(months == 12)
r5 <- renew_type |> filter(tenure_years == 5)

last_share <- rule |>
  group_by(category) |>
  slice_max(q_start, n = 1) |>
  mutate(share = (quota - rule) / quota) |>
  ungroup()

# ---- 1 stacked area: formula vs LTA top-up ----
area <- rule |>
  transmute(category, q_start,
            `From the 25% rule` = pmin(quota, rule),
            `LTA top-up`        = pmax(quota - rule, 0)) |>
  pivot_longer(-c(category, q_start), names_to = "part", values_to = "coes") |>
  mutate(part = factor(part, levels = c("LTA top-up", "From the 25% rule")))
layer_lab <- rule |>
  filter(q_start == as.Date("2024-08-01")) |>
  transmute(category, x = q_start, grey_y = rule / 2, blue_y = rule + (quota - rule) / 2)
p1 <- ggplot(area, aes(q_start, coes, fill = part)) +
  geom_area(stat = "identity", position = "stack", colour = NA) +
  geom_text(data = layer_lab, aes(x, grey_y, label = "From the\n25% rule"), inherit.aes = FALSE,
            size = 3.6, colour = "grey15", lineheight = 0.9) +
  geom_text(data = layer_lab, aes(x, blue_y, label = "LTA top-up"), inherit.aes = FALSE,
            size = 3.6, colour = "white", fontface = "bold") +
  geom_text(data = last_share, aes(q_start, quota, label = paste0("Top-up now\n", pc(pmax(share, 0)))),
            inherit.aes = FALSE, hjust = 1, vjust = -0.4, size = 3.3, lineheight = 0.9, colour = "grey20") +
  geom_vline(xintercept = as.Date(c("2023-05-01", "2025-02-01")), linetype = "dotted", colour = "grey30") +
  geom_text(data = tibble::tibble(x = as.Date(c("2023-05-01", "2025-02-01")), lab = c("Cut and fill", "Extra COEs")),
            aes(x = x, y = Inf, label = lab), inherit.aes = FALSE, vjust = 1.4, hjust = -0.05, size = 3.2, colour = "grey25") +
  facet_wrap(~ category, labeller = labeller(category = cat_labels)) +
  scale_fill_manual(values = c("From the 25% rule" = "grey75", "LTA top-up" = "#2b8cbe"), guide = "none") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = paste0("LTA top-ups peaked at ", pc(peak_extra$extra_share), " of Cat A + B supply and are now ",
                   pc(last_extra$extra_share)),
    subtitle = "COEs per quota quarter, split into what LTA's 25% rule gives and what LTA added on top",
    x = NULL, y = "COEs per quarter",
    caption = src("LTA COE results and deregistrations. Rule = 25% of last 12 months' deregistrations, less ~10% to Cat E")
  ) + theme_coe
save_p(p1, "g1_quota_rule_topup.png", 11, 5.6)

# ---- 2 age heatmap ----
heat <- age |> filter(vehicle_type == "Cars", age_lower <= 15)
lab_big   <- paste0("Bought 2017-19: ", n0(aged_7_10), " cars")
lab_small <- paste0("Bought 2022-24: ", n0(aged_2_5), " cars")
p2 <- ggplot(heat, aes(month, age_lower + 0.5, fill = population / 1000)) +
  geom_tile(width = 33, height = 1) +
  geom_hline(yintercept = 10, colour = "white", linewidth = 1.1) +
  annotate("text", x = as.Date("2016-03-01"), y = 10.55, label = "Year 10: COE expires",
           colour = "white", hjust = 0, fontface = "bold", size = 3.5) +
  annotate("segment", x = as.Date("2018-01-01"), xend = as.Date("2026-04-01"), y = 0.6, yend = 8.6,
           colour = "white", linewidth = 0.9, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2021-09-01"), y = 5.7, label = lab_big, angle = 21, colour = "white",
           fontface = "bold", size = 3.5) +
  annotate("segment", x = as.Date("2023-01-01"), xend = as.Date("2026-04-01"), y = 0.6, yend = 3.6,
           colour = "white", linewidth = 0.9, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2024-02-01"), y = 3.3, label = lab_small, angle = 21, colour = "white",
           fontface = "bold", size = 3.2) +
  scale_fill_viridis_c(option = "magma", name = "Cars\n(thousands)") +
  scale_y_continuous(breaks = seq(0, 15, 1), expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  labs(
    title = paste0(n0(aged_7_10), " cars bought in 2017-19 reach year 10 by 2029; half as many follow"),
    subtitle = "Cars on the road by age, every month 2016-2026, ages 0 to 15",
    x = NULL, y = "Age of car (years)", caption = src("LTA, cars on the road by age (Cat A + B combined)")
  ) + theme_coe + theme(legend.position = "right", panel.grid = element_blank())
save_p(p2, "g2_age_heatmap.png", 11, 5.8)

# ---- 3 guaranteed exits lollipop ----
exits <- r5 |>
  transmute(due = year + 5, cars = n) |>
  mutate(when = ifelse(due <= year(latest_month) - 1, "Already happened", "Still to come"))
due_peak   <- exits |> filter(due %in% 2021:2024) |> summarise(m = mean(cars)) |> pull(m)
due_trough <- exits |> filter(due %in% 2026:2028) |> summarise(m = mean(cars)) |> pull(m)
cut_x <- which(sort(exits$due) == year(latest_month)) - 0.5
p3 <- ggplot(exits, aes(factor(due), cars, colour = when)) +
  annotate("rect", xmin = cut_x, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fff1e6") +
  geom_segment(aes(xend = factor(due), y = 0, yend = cars), linewidth = 1.3) +
  geom_point(size = 5) +
  geom_text(aes(label = comma(round(cars, -2))), vjust = -1.2, size = 3.3, colour = "grey20") +
  geom_vline(xintercept = cut_x, linetype = "dashed", colour = "grey40") +
  annotate("text", x = cut_x - 0.15, y = max(exits$cars) * 1.17, label = "Already happened",
           hjust = 1, size = 3.8, colour = "grey45", fontface = "bold") +
  annotate("text", x = cut_x + 0.15, y = max(exits$cars) * 1.17, label = "Still to come, but already fixed",
           hjust = 0, size = 3.8, colour = "#a63603", fontface = "bold") +
  scale_colour_manual(values = c("Already happened" = "grey65", "Still to come" = "#e6550d"), guide = "none") +
  scale_y_continuous(labels = comma, limits = c(0, max(exits$cars) * 1.22)) +
  labs(
    title = paste0("Guaranteed exits fall from ~", n0(round(due_peak, -2)), " a year to ~",
                   n0(round(due_trough, -2)), " in 2026-28"),
    subtitle = "Cars on a 5-year COE renewal, by the year their COE runs out (renewal year + 5), Cat A + B",
    x = "Year the 5-year renewal runs out", y = "Cars",
    caption = src("LTA COE revalidations")
  ) + theme_coe + theme(panel.grid.major.x = element_blank())
save_p(p3, "g3_locked_exits_lollipop.png", 11, 5.2)

# ---- 4 owner reaction scatter ----
prem_sa <- coe |>
  filter(category %in% c("A", "B"), !is.na(premium)) |>
  mutate(year = year(month %m+% months(4))) |>
  group_by(year) |> summarise(premium = mean(premium), quota = sum(quota), .groups = "drop")
pqp_sa <- pqp |>
  filter(category %in% c("A", "B"), !is.na(pqp)) |>
  mutate(year = year(month %m+% months(4))) |>
  group_by(year) |> summarise(pqp = mean(pqp), .groups = "drop")
drivers <- aug |>
  mutate(year = year + 1, a_next = pmin(a + 1, 20)) |>
  group_by(year, a_next) |> summarise(start = sum(pop), .groups = "drop") |>
  inner_join(aug |> rename(a_next = a, end = pop), by = c("year", "a_next")) |>
  group_by(year) |>
  summarise(
    early = sum(start[a_next %in% 1:9] - end[a_next %in% 1:9]) / sum(start[a_next %in% 1:9]),
    y10 = 1 - end[a_next == 10] / start[a_next == 10],
    .groups = "drop"
  ) |>
  inner_join(prem_sa, by = "year") |>
  inner_join(pqp_sa, by = "year")
ct_early <- cor.test(drivers$early, drivers$premium)
ct_y10   <- cor.test(drivers$y10, drivers$pqp)
fmt_r <- function(ct) paste0("r = ", sprintf("%.2f", ct$estimate), ", p = ", signif(ct$p.value, 2))
p4a <- ggplot(drivers, aes(premium / 1000, early)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "grey30", fill = "grey88") +
  geom_point(size = 3, colour = "#54278f") +
  geom_text(aes(label = year), vjust = -1, size = 3.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Young cars scrapped vs COE premium", subtitle = fmt_r(ct_early),
       x = "Average COE premium (S$ thousands)", y = "Share of cars aged 1-9 scrapped") +
  theme_coe + theme(plot.title = element_text(size = 12))
p4b <- ggplot(drivers, aes(pqp / 1000, y10)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "grey30", fill = "grey88") +
  geom_point(size = 3, colour = "#54278f") +
  geom_text(aes(label = year), vjust = -1, size = 3.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Cars scrapped at year 10 vs renewal price", subtitle = fmt_r(ct_y10),
       x = "Average PQP, the price to renew (S$ thousands)", y = "Share scrapped at year 10") +
  theme_coe + theme(plot.title = element_text(size = 12))
p4 <- (p4a | p4b) + plot_annotation(
  title = "When COEs are dear, owners keep young cars; at year 10, a dear renewal pushes them to scrap",
  caption = src("LTA cars on the road by age and COE results; SingStat PQP. Years to August 2017-2026"),
  theme = theme_coe
)
save_p(p4, "g4_owner_price_response.png", 11, 5.4)

# ---- 5 bids per COE violin ----
bands <- quarters |>
  mutate(band = cut(premium, c(0, 50000, 100000, Inf),
                    labels = c("Under S$50k", "S$50k to 100k", "Over S$100k")))
band_med <- bands |> group_by(band) |> summarise(med = median(bids_per_coe), n = n(), .groups = "drop")
spikes <- bands |> filter(bids_per_coe > 2.2)
p5 <- ggplot(bands, aes(band, bids_per_coe)) +
  geom_hline(yintercept = 1.5, linetype = "dashed", colour = "grey45") +
  geom_violin(fill = "grey92", colour = "grey65", width = 0.8) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.6, size = 1.8, colour = "grey35") +
  geom_point(data = band_med, aes(band, med), shape = 95, size = 16, colour = "black") +
  geom_text(data = band_med, aes(band, med, label = paste0("median ", sprintf("%.2f", med), "\n(", n, " quarters)")),
            nudge_x = 0.42, size = 3.2, lineheight = 0.9) +
  annotate("text", x = 2.12, y = max(spikes$bids_per_coe), hjust = 0, size = 3.1, colour = "grey25",
           label = "Early 2013: rush before\nthe car-loan curbs") +
  labs(
    title = paste0("Bids per COE sits near ", round(median(quarters$bids_per_coe), 1),
                   " whether a COE costs S$30k or S$130k"),
    subtitle = "Bids received per COE in each quota quarter, Cat A and B, grouped by the quarter's average premium, 2010-2026",
    x = "Average premium in the quarter", y = "Bids per COE",
    caption = src("LTA COE results")
  ) + theme_coe + theme(panel.grid.major.x = element_blank())
save_p(p5, "g5_bids_per_coe_violin.png", 11, 5.2)

# ---- 6 dealer risk ridges ----
all_rounds <- coe |>
  filter(category %in% c("A", "B"), !is.na(premium)) |>
  arrange(category, month, bidding_no)
now_prem <- all_rounds |> group_by(category) |> slice_tail(n = 1) |> select(category, now = premium)
moves_all <- lapply(c(2, 6, 12), function(h) {
  all_rounds |>
    group_by(category) |>
    mutate(future = lead(premium, h), future_month = lead(month, h)) |>
    ungroup() |>
    filter(!is.na(future), interval(month, future_month) %/% months(1) <= h / 2 + 1) |>
    transmute(category, month, horizon = ifelse(h == 2, "1 month", paste(h / 2, "months")),
              pct = future / premium - 1)
}) |>
  bind_rows() |>
  filter(month >= r3_start) |>
  left_join(now_prem, by = "category") |>
  mutate(change = pct * now,
         horizon = factor(horizon, levels = c("6 months", "3 months", "1 month")))
ridge_q <- moves_all |>
  group_by(category, horizon) |>
  summarise(p50 = median(change), p90 = quantile(change, 0.9),
            lo = quantile(change, 0.01), n = n(), .groups = "drop")
rq <- function(cat, hz, v) ridge_q[[v]][ridge_q$category == cat & ridge_q$horizon == hz]
moves_plot <- moves_all |>
  left_join(ridge_q |> select(category, horizon, lo), by = c("category", "horizon")) |>
  filter(change >= lo)
p6 <- ggplot(moves_plot, aes(change, horizon, fill = category)) +
  geom_density_ridges(scale = 1.2, rel_min_height = 0.01, colour = "grey30", alpha = 0.55,
                      quantile_lines = TRUE, quantiles = c(0.5, 0.9)) +
  geom_vline(xintercept = 0, colour = "grey20") +
  geom_label(data = ridge_q, aes(x = p90, y = horizon, label = paste0("1 in 10: ", dk(p90), "\ntypical: ", dk(p50))),
             inherit.aes = FALSE, vjust = -0.25, hjust = -0.08, size = 3, lineheight = 0.9,
             label.size = 0, fill = "white", alpha = 0.85) +
  facet_wrap(~ category, labeller = labeller(category = cat_labels)) +
  scale_fill_manual(values = cat_colours, guide = "none") +
  scale_x_continuous(labels = dk, expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title = paste0("The longer the wait, the bigger the bad case: ", dk(rq("A", "6 months", "p90")),
                   " per Cat A car after 6 months"),
    subtitle = "Change in premium per car since May 2022, at today's premium.\nLeft line in each hill: typical change. Right line: 1-in-10 bad case.",
    x = "Change per car (right of zero: dealer pays more)", y = "Order to COE",
    caption = src("LTA COE results. Percentage changes between rounds, applied to the latest premium; lowest 1% trimmed")
  ) + theme_coe + theme(panel.grid.major.y = element_blank())
save_p(p6, "g6_dealer_risk_ridges.png", 11, 5.8)

cat("Gallery PNGs written to", out_dir, "\n")
