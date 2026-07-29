# Sensitivity analysis: how do M12 model results change with temperature threshold scenario?
#
# Scenarios from wnv_transmission_season_gridMET_sensitivity.csv:
#
#   State-wide (uniform threshold across all counties): ### add mean (16.7) for data viz and comparison purposes
#     state_low  = 14.9°C
#     state_high = 16.7°C
#     state_high = 17.8°C
#
#
#   Population-specific (threshold varies by subtropical classification):
#     population_sp_low  = 15.6°C subtropical / 15.0°C non-subtropical
#     population_sp_mean = 17.8°C subtropical / 16.7°C non-subtropical
#     population_sp_high = 20.2°C subtropical / 17.9°C non-subtropical
#
# For each threshold_id × outcome, refits M12 (quadratic year × elevation + ENSO RW2).
#
# Collects for each scenario × outcome:
#   - Fixed-effect posteriors: year_c, I(year_c^2)
#   - State-average total change 1999→2024 (days, posterior mean + 95% CrI)
#
# Produces two figures:
#   1. Fixed-effect coefficients vs. scenario
#   2. Total change 1999→2024 vs. scenario

library(tidyverse)
library(INLA)
library(spdep)
library(sf)
library(tigris)
library(rsoi)

# Run from the project root (paths below are relative to it)

###----- Load sensitivity data and get scenario IDs

sens_raw <- read.csv("data/wnv_transmission_season_gridMET_sensitivity.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", county)))

# Add state_mean (16.7°C uniform threshold) from the main 1980–2024 dataset
state_mean_raw <- read.csv("data/wnv_transmission_season_gridMET_1980_2024.csv") %>%
  mutate(
    county       = tolower(gsub("[^a-zA-Z]", "", county)),
    threshold_id = "state_mean"
  ) %>%
  select(threshold_id, county, year, FirstValid, LastValid, SeasonLength)

sens_raw <- bind_rows(sens_raw, state_mean_raw)

# Ordered for plotting: state-wide low/mean/high first, then population-specific low→high
scenario_order <- c("state_low", "state_mean", "state_high",
                    "population_sp_low", "population_sp_mean", "population_sp_high")

threshold_ids <- intersect(scenario_order, unique(sens_raw$threshold_id))
cat("Scenarios:", paste(threshold_ids, collapse = ", "), "\n")

# x-axis labels for figures
scenario_labels <- c(
  state_low          = "State\nlow\n(14.9°C)",
  state_mean         = "State\nmean\n(16.7°C)",
  state_high         = "State\nhigh\n(17.8°C)",
  population_sp_low  = "Pop.\nlow\n(15.0/15.6°C)",
  population_sp_mean = "Pop.\nmean\n(16.7/17.8°C)",
  population_sp_high = "Pop.\nhigh\n(17.9/20.2°C)"
)

###----- ONI data

oni <- rsoi::download_oni(use_cache = TRUE)

enso_mayjul <- oni %>%
  filter(month(Date) >= 5, month(Date) <= 10) %>%
  group_by(Year) %>%
  summarise(enso_seasonal = mean(ONI, na.rm = TRUE)) %>%
  rename(year = Year)

enso_aprjul <- oni %>%
  filter(month(Date) >= 4, month(Date) <= 7) %>%
  group_by(Year) %>%
  summarise(enso_seasonal = mean(ONI, na.rm = TRUE)) %>%
  rename(year = Year)

###----- Spatial setup (shared across all fits)

ny_counties <- tigris::counties(state = "NY", cb = TRUE, class = "sf") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", NAME))) %>%
  arrange(county)

county_centroids_all <- read.csv("data/NY_county_mean_elevation.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", county_name)),
         elev   = mean_elev_m,
         elev_c = as.numeric(scale(mean_elev_m))) %>%
  select(county, elev, elev_c)

###----- INLA priors

bym2      <- list(phi  = list(prior = "pc",      param = c(0.5, 0.5)),
                  prec = list(prior = "pc.prec",  param = c(1, 0.01)))
rw2_prior <- list(prec = list(prior = "pc.prec",  param = c(0.1, 0.01)))

###----- Helpers

build_adj <- function(county_lvls) {
  ny_sub <- ny_counties %>%
    filter(county %in% county_lvls) %>%
    arrange(county)
  proj <- sf::st_transform(ny_sub, crs = 32618)
  nb   <- spdep::poly2nb(proj, queen = TRUE, snap = 2000)
  tmp  <- tempfile(fileext = ".graph")
  spdep::nb2INLA(tmp, nb)
  INLA::inla.read.graph(tmp)
}

prep_data <- function(df, outcome_col, enso_df) {
  df <- df %>%
    filter(!is.na(.data[[outcome_col]])) %>%
    mutate(year_c = year - mean(year))

  county_levels <- sort(unique(df$county))
  df <- df %>%
    mutate(county_idx = as.integer(factor(county, levels = county_levels))) %>%
    left_join(enso_df, by = "year") %>%
    mutate(enso_scaled = as.numeric(scale(enso_seasonal)))

  enso_breaks <- quantile(df$enso_scaled, probs = seq(0, 1, length.out = 21), na.rm = TRUE)
  df <- df %>%
    mutate(enso_bin = as.integer(cut(enso_scaled, breaks = enso_breaks,
                                      include.lowest = TRUE, labels = FALSE)),
           el_nino  = as.integer(enso_seasonal >= 0.5))

  list(
    data          = df,
    county_levels = county_levels,
    outcome_mean  = mean(df[[outcome_col]], na.rm = TRUE),
    outcome_sd    = sd(df[[outcome_col]],   na.rm = TRUE),
    yr_mean       = mean(df$year)
  )
}

fit_m12 <- function(prep, outcome_col) {
  adj  <- build_adj(prep$county_levels)
  data <- prep$data %>%
    left_join(county_centroids_all %>% select(county, elev_c), by = "county") %>%
    mutate(outcome_scaled = (.data[[outcome_col]] - prep$outcome_mean) / prep$outcome_sd)

  formula_m12 <- outcome_scaled ~ year_c + I(year_c^2) + elev_c +
    year_c:elev_c + I(year_c^2):elev_c +
    f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
    f(enso_bin,   model = "rw2",  hyper = rw2_prior)

  inla(formula_m12, family = "gaussian", data = data,
       control.compute   = list(dic = TRUE, waic = TRUE, config = TRUE),
       control.predictor = list(compute = TRUE),
       control.inla      = list(strategy    = "simplified.laplace",
                                int.strategy = "eb"))
}

# Draw posterior samples once per model (shared across extract_* calls)
draw_samples <- function(model, n_samp = 1000) {
  inla.posterior.sample(n = n_samp, result = model)
}

# Combined year_c + I(year_c^2) trajectory in days, anchored to change = 0 at ref_year.
# Returns one row per year with posterior mean + 95% CrI.
extract_trajectory <- function(samps, prep, years = 1980:2024, ref_year = 1999) {
  sd      <- prep$outcome_sd
  yr_mean <- prep$yr_mean
  t_ref   <- ref_year - yr_mean

  b1_vec <- sapply(samps, function(s) s$latent["year_c:1", 1])
  b2_vec <- sapply(samps, function(s) s$latent["I(year_c^2):1", 1])

  map_dfr(years, function(yr) {
    t            <- yr - yr_mean
    delta_t      <- t - t_ref
    delta_t2     <- t^2 - t_ref^2
    change_samps <- (b1_vec * delta_t + b2_vec * delta_t2) * sd
    tibble(
      year = yr,
      mean = mean(change_samps),
      lo   = quantile(change_samps, 0.025),
      hi   = quantile(change_samps, 0.975)
    )
  })
}

# State-average change between two years (at mean elevation, elev_c = 0).
# Accepts pre-drawn samples so inla.posterior.sample is only called once per model.
extract_period_change <- function(samps, prep, year_start, year_end) {
  sd      <- prep$outcome_sd
  yr_mean <- prep$yr_mean
  t1 <- year_start - yr_mean
  t2 <- year_end   - yr_mean

  b1_vec <- sapply(samps, function(s) s$latent["year_c:1", 1])
  b2_vec <- sapply(samps, function(s) s$latent["I(year_c^2):1", 1])

  change_samps <- (b1_vec * (t2 - t1) + b2_vec * (t2^2 - t1^2)) * sd

  tibble(
    mean = mean(change_samps),
    lo   = quantile(change_samps, 0.025),
    hi   = quantile(change_samps, 0.975)
  )
}

###----- Main loop: fit M12 for every threshold_id × outcome

outcomes <- list(
  list(col = "FirstValid",   enso = enso_aprjul, label = "First day"),
  list(col = "SeasonLength", enso = enso_mayjul, label = "Season length"),
  list(col = "LastValid",    enso = enso_mayjul, label = "Last day")
)

trajectory_results      <- list()
trajectory_1980_results <- list()
change_results_early    <- list()
change_results_late     <- list()

for (thresh_id in threshold_ids) {
  cat("\n===== Scenario:", thresh_id, "=====\n")

  thresh_df <- sens_raw %>% filter(threshold_id == thresh_id)

  for (out in outcomes) {
    cat("  Outcome:", out$col, "\n")

    prep <- prep_data(thresh_df, out$col, out$enso)

    if (nrow(prep$data) < 50) {
      cat("  Skipping — too few observations\n")
      next
    }

    key <- paste(thresh_id, out$col)

    fit_ok <- tryCatch({
      model <- fit_m12(prep, out$col)
      samps <- draw_samples(model)            # draw once, reuse below

      traj <- extract_trajectory(samps, prep, ref_year = 1999) %>%
        mutate(threshold_id = thresh_id, outcome = out$label)
      trajectory_results[[key]] <- traj

      traj_1980 <- extract_trajectory(samps, prep, ref_year = 1980) %>%
        mutate(threshold_id = thresh_id, outcome = out$label)
      trajectory_1980_results[[key]] <- traj_1980

      ch_early <- extract_period_change(samps, prep, 1980, 1998) %>%
        mutate(threshold_id = thresh_id, outcome = out$label)
      change_results_early[[key]] <- ch_early

      ch_late <- extract_period_change(samps, prep, 1999, 2024) %>%
        mutate(threshold_id = thresh_id, outcome = out$label)
      change_results_late[[key]] <- ch_late

      TRUE
    }, error = function(e) {
      cat("  WARNING: model failed for", key, "—", conditionMessage(e), "\n")
      cat("  Skipping this combination.\n")
      FALSE
    })
  }
}

add_groups <- function(df) {
  df %>% mutate(
    threshold_id = factor(threshold_id, levels = scenario_order),
    sens_group   = factor(
      ifelse(threshold_id %in% c("state_low", "state_mean", "state_high"),
             "State-wide", "Population-specific"),
      levels = c("State-wide", "Population-specific")
    )
  )
}

trajectory_df      <- bind_rows(trajectory_results)      %>% add_groups()
trajectory_1980_df <- bind_rows(trajectory_1980_results) %>% add_groups()
change_df_early <- bind_rows(change_results_early) %>%
  mutate(threshold_id = factor(threshold_id, levels = scenario_order))
change_df_late  <- bind_rows(change_results_late) %>%
  mutate(threshold_id = factor(threshold_id, levels = scenario_order))

write.csv(trajectory_df,   "data/sensitivity_trajectory.csv",             row.names = FALSE)
write.csv(change_df_early, "data/sensitivity_period_change_1980_1998.csv", row.names = FALSE)
write.csv(change_df_late,  "data/sensitivity_period_change_1999_2024.csv", row.names = FALSE)

###----- Figure 1: combined temporal trajectory (change in days from 1980)
# Faceted by outcome; one line per threshold scenario; ribbons = 95% CrI

scenario_colors <- c(
  state_low          = "#4575b4",
  state_mean         = "#555555",
  state_high         = "#d73027",
  population_sp_low  = "#74add1",
  population_sp_mean = "#f46d43",
  population_sp_high = "#a50026"
)

scenario_linetypes <- c(
  state_low          = "dashed",
  state_mean         = "solid",
  state_high         = "dashed",
  population_sp_low  = "dashed",
  population_sp_mean = "solid",
  population_sp_high = "dashed"
)

p_traj <- ggplot(trajectory_df,
                 aes(x = year, y = mean,
                     color = threshold_id, fill = threshold_id,
                     linetype = threshold_id)) +
  annotate("rect", xmin = 1980, xmax = 1999, ymin = -Inf, ymax = Inf,
           fill = "grey95", color = NA) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = 1999, linetype = "dotted", color = "grey50", linewidth = 0.5) +
  facet_grid(outcome ~ sens_group, scales = "free_y") +
  scale_color_manual(values = scenario_colors,  labels = scenario_labels) +
  scale_fill_manual(values  = scenario_colors,  labels = scenario_labels) +
  scale_linetype_manual(values = scenario_linetypes, labels = scenario_labels) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 10)) +
  theme_classic(base_size = 11) +
  theme(legend.position  = "bottom",
        legend.key.width  = unit(1.5, "cm"),
        strip.text        = element_text(face = "bold"),
        strip.background  = element_rect(fill = "grey92", colour = NA),
        panel.spacing     = unit(0.8, "lines")) +
  guides(color    = guide_legend(nrow = 2),
         fill     = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  labs(
    title    = "Combined temporal trend: change in days from 1999 baseline",
    subtitle = "Posterior mean ± 95% CrI; at mean elevation (elev_c = 0); state average; shaded = pre-emergence (1980–1998)",
    x        = "Year",
    y        = "Change in days (from 1999)",
    color    = NULL, fill = NULL, linetype = NULL
  )

###----- Figure 1b: same trajectory plot, anchored to 1980

p_traj_1980 <- ggplot(trajectory_1980_df,
                      aes(x = year, y = mean,
                          color = threshold_id, fill = threshold_id,
                          linetype = threshold_id)) +
  annotate("rect", xmin = 1980, xmax = 1999, ymin = -Inf, ymax = Inf,
           fill = "grey95", color = NA) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = 1999, linetype = "dotted", color = "grey50", linewidth = 0.5) +
  facet_grid(outcome ~ sens_group, scales = "free_y") +
  scale_color_manual(values = scenario_colors,  labels = scenario_labels) +
  scale_fill_manual(values  = scenario_colors,  labels = scenario_labels) +
  scale_linetype_manual(values = scenario_linetypes, labels = scenario_labels) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 10)) +
  theme_classic(base_size = 11) +
  theme(legend.position  = "bottom",
        legend.key.width  = unit(1.5, "cm"),
        strip.text        = element_text(face = "bold"),
        strip.background  = element_rect(fill = "grey92", colour = NA),
        panel.spacing     = unit(0.8, "lines")) +
  guides(color    = guide_legend(nrow = 2),
         fill     = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  labs(
    title    = "Combined temporal trend: change in days from 1980 baseline",
    subtitle = "Posterior mean ± 95% CrI; at mean elevation (elev_c = 0); state average; shaded = pre-emergence (1980–1998)",
    x        = "Year",
    y        = "Change in days (from 1980)",
    color    = NULL, fill = NULL, linetype = NULL
  )

change_plot_base <- function(df, title, y_label) {
  ggplot(df, aes(x = threshold_id, y = mean, color = outcome, fill = outcome)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, group = outcome), alpha = 0.15, color = NA) +
    geom_line(aes(group = outcome), linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_x_discrete(labels = scenario_labels) +
    scale_color_manual(values = c("First day"     = "#1b7837",
                                   "Season length" = "#762a83",
                                   "Last day"      = "#e08214")) +
    scale_fill_manual(values  = c("First day"     = "#1b7837",
                                   "Season length" = "#762a83",
                                   "Last day"      = "#e08214")) +
    theme_classic(base_size = 11) +
    theme(axis.text.x     = element_text(hjust = 0.5),
          legend.position = "bottom") +
    labs(
      title    = title,
      subtitle = "State-average change (days); posterior mean ± 95% CrI; at mean elevation",
      x        = "Threshold scenario",
      y        = y_label,
      color    = NULL, fill = NULL
    )
}

###----- Figure 2: total change 1980→1998 vs. scenario

p_change_early <- change_plot_base(
  change_df_early,
  title   = "Sensitivity of total change 1980→1998 to temperature threshold scenario",
  y_label = "Change in days (1980 → 1998)"
)

###----- Figure 3: total change 1999→2024 vs. scenario

p_change_late <- change_plot_base(
  change_df_late,
  title   = "Sensitivity of total change 1999→2024 to temperature threshold scenario",
  y_label = "Change in days (1999 → 2024)"
)

###----- Save figures

ggsave("results/figures/sensitivity_trajectory_ref1999.png",      p_traj,         width = 12, height = 8,  dpi = 300)
ggsave("data/sensitivity_trajectory_ref1980.png",      p_traj_1980,    width = 12, height = 8,  dpi = 300)
ggsave("data/sensitivity_period_change_1980_1998.png", p_change_early, width = 7,  height = 5,  dpi = 300)
ggsave("data/sensitivity_period_change_1999_2024.png", p_change_late,  width = 7,  height = 5,  dpi = 300)

cat("\nDone. Results written to:",
    "\n  data/sensitivity_trajectory.csv",
    "\n  data/sensitivity_period_change_1980_1998.csv",
    "\n  data/sensitivity_period_change_1999_2024.csv",
    "\n  results/figures/sensitivity_trajectory_ref1999.png",
    "\n  data/sensitivity_trajectory_ref1980.png",
    "\n  data/sensitivity_period_change_1980_1998.png",
    "\n  data/sensitivity_period_change_1999_2024.png\n")
