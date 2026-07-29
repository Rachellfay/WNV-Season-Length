# M12: Quadratic year × elevation + ENSO RW2
# Figure: map 1980→2024 | map 1999→2024 | trend (ref: 1999)
# + statewide season length change estimates

library(tidyverse)
library(INLA)
library(spdep)
library(sf)
library(tigris)
library(rsoi)
library(patchwork)

# Run from the project root (paths below are relative to it)

###----- Load raw data

raw <- read.csv("data/wnv_transmission_season_gridMET_1980_2024.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", county)))

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

###----- Prepare datasets

prep_data <- function(raw, outcome_col, enso_df) {
  df <- raw %>%
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

first_day_prep  <- prep_data(raw, "FirstValid",   enso_aprjul)
season_len_prep <- prep_data(raw, "SeasonLength", enso_mayjul)
last_day_prep   <- prep_data(raw, "LastValid",    enso_mayjul)

###----- Spatial setup

ny_counties <- tigris::counties(state = "NY", cb = TRUE, class = "sf") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", NAME))) %>%
  arrange(county)

mean_elev <- read.csv("data/NY_county_mean_elevation.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", county_name))) %>%
  select(county, mean_elev_m)

centroid_sf <- ny_counties %>% sf::st_centroid()

county_centroids_all <- centroid_sf %>%
  mutate(lat = sf::st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry() %>%
  select(county, lat) %>%
  left_join(mean_elev, by = "county") %>%
  rename(elev = mean_elev_m) %>%
  mutate(lat_c = as.numeric(scale(lat)), elev_c = as.numeric(scale(elev)))

elev_cutoff_m <- median(county_centroids_all$elev)
cat(sprintf("Elevation median split cutoff: %.0f m\n", elev_cutoff_m))
cat(sprintf("  Low elev mean:  %.0f m\n", mean(county_centroids_all$elev[county_centroids_all$elev <  elev_cutoff_m])))
cat(sprintf("  High elev mean: %.0f m\n", mean(county_centroids_all$elev[county_centroids_all$elev >= elev_cutoff_m])))

###----- Fit M12 models

bym2      <- list(phi  = list(prior = "pc",     param = c(0.5, 0.5)),
                  prec = list(prior = "pc.prec", param = c(1, 0.01)))
rw2_prior <- list(prec = list(prior = "pc.prec", param = c(0.1, 0.01)))

build_adj <- function(county_lvls) {
  ny_sub <- ny_counties %>% filter(county %in% county_lvls) %>% arrange(county)
  proj   <- sf::st_transform(ny_sub, crs = 32618)
  nb     <- spdep::poly2nb(proj, queen = TRUE, snap = 2000)
  tmp    <- tempfile(fileext = ".graph")
  spdep::nb2INLA(tmp, nb)
  INLA::inla.read.graph(tmp)
}

refit_m12 <- function(prep, outcome_col, rds_path) {
  adj  <- build_adj(prep$county_levels)
  data <- prep$data %>%
    left_join(county_centroids_all %>% select(county, elev_c), by = "county") %>%
    mutate(outcome_scaled = (.data[[outcome_col]] - prep$outcome_mean) / prep$outcome_sd)

  formula_m12 <- outcome_scaled ~ year_c + I(year_c^2) + elev_c +
    year_c:elev_c + I(year_c^2):elev_c +
    f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
    f(enso_bin,   model = "rw2",  hyper = rw2_prior)

  m <- inla(formula_m12, family = "gaussian", data = data,
            control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
            control.predictor = list(compute = TRUE))
  saveRDS(m, rds_path)
  m
}

load_or_fit <- function(prep, outcome_col, rds_path) {
  if (file.exists(rds_path)) {
    message("Loading cached model: ", rds_path)
    readRDS(rds_path)
  } else {
    message("Fitting model (this may take several minutes): ", rds_path)
    refit_m12(prep, outcome_col, rds_path)
  }
}

m12_first  <- load_or_fit(first_day_prep,  "FirstValid",   "data/gridMET_first_day_m12_elev_quad_enso_rw2.rds")
m12_season <- load_or_fit(season_len_prep, "SeasonLength", "data/gridMET_m12_elev_quad_enso_rw2.rds")
m12_last   <- load_or_fit(last_day_prep,   "LastValid",    "data/gridMET_last_day_m12_elev_quad_enso_rw2.rds")
message("Done.")

###----- Helper: trend panel (ref: 1999)

make_trend_panel_m12 <- function(model, prep, raw_col, y_label, title, n_samp = 2000) {
  data          <- prep$data
  outcome_sd    <- prep$outcome_sd
  outcome_mean  <- prep$outcome_mean
  yr_mean       <- prep$yr_mean
  county_levels <- prep$county_levels

  ref_c      <- 1999 - yr_mean
  years      <- sort(unique(data$year))
  year_c_vec <- years - yr_mean

  county_elev <- county_centroids_all %>% filter(county %in% county_levels)
  elev_med    <- median(county_elev$elev_c)
  elev_lo     <- mean(county_elev$elev_c[county_elev$elev_c <  elev_med])
  elev_hi     <- mean(county_elev$elev_c[county_elev$elev_c >= elev_med])

  samps  <- inla.posterior.sample(n = n_samp, result = model)
  b1_vec <- sapply(samps, function(s) s$latent["year_c:1",               1])
  b2_vec <- sapply(samps, function(s) s$latent["I(year_c^2):1",          1])
  b3_vec <- sapply(samps, function(s) s$latent["year_c:elev_c:1",        1])
  b4_vec <- sapply(samps, function(s) s$latent["I(year_c^2):elev_c:1",   1])

  make_elev_trend <- function(elev_c_val, group_label) {
    eff_b1       <- b1_vec + b3_vec * elev_c_val
    eff_b2       <- b2_vec + b4_vec * elev_c_val
    trend_mat    <- (outer(year_c_vec, eff_b1) + outer(year_c_vec^2, eff_b2)) * outcome_sd
    pred_ref_vec <- (eff_b1 * ref_c + eff_b2 * ref_c^2) * outcome_sd
    trend_rel    <- sweep(trend_mat, 2, pred_ref_vec, "-")
    data.frame(
      year      = years,
      mean_days = rowMeans(trend_rel),
      lo_days   = apply(trend_rel, 1, quantile, 0.025),
      hi_days   = apply(trend_rel, 1, quantile, 0.975),
      group     = group_label
    )
  }

  trend_df <- bind_rows(
    make_elev_trend(elev_lo, "Low elevation"),
    make_elev_trend(elev_hi, "High elevation")
  ) %>% mutate(group = factor(group, levels = c("Low elevation", "High elevation")))

  elev_colors    <- c("Low elevation" = "#4393c3", "High elevation" = "#d6604d")
  pred_ref_mean  <- mean((b1_vec * ref_c + b2_vec * ref_c^2) * outcome_sd)

  sp_re <- model$summary.random$county_idx %>%
    mutate(county_idx = ID, sp_eff = mean * outcome_sd) %>%
    select(county_idx, sp_eff)

  enso_re <- model$summary.random$enso_bin %>%
    mutate(enso_bin = ID, enso_eff = mean * outcome_sd) %>%
    select(enso_bin, enso_eff)

  raw_year <- data %>%
    left_join(sp_re,   by = "county_idx") %>%
    left_join(enso_re, by = "enso_bin") %>%
    mutate(partial = .data[[raw_col]] - outcome_mean - sp_eff - enso_eff - pred_ref_mean) %>%
    group_by(year) %>%
    summarise(mean_partial = mean(partial, na.rm = TRUE))

  ggplot() +
    annotate("rect", xmin = 1980, xmax = 1999, ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.6) +
    geom_ribbon(data = trend_df,
                aes(x = year, ymin = lo_days, ymax = hi_days, fill = group), alpha = 0.18) +
    geom_line(data = trend_df,
              aes(x = year, y = mean_days, color = group), linewidth = 1.0) +
    geom_point(data = raw_year, aes(x = year, y = mean_partial),
               color = "grey35", size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed",  color = "black",    linewidth = 0.5) +
    geom_vline(xintercept = 1999, linetype = "dotted", color = "firebrick", linewidth = 0.7) +
    scale_color_manual(values = elev_colors, name = NULL) +
    scale_fill_manual( values = elev_colors, name = NULL) +
    theme_classic(base_size = 11) +
    theme(legend.position = "top",
          legend.key.size = unit(0.5, "cm"),
          legend.text     = element_text(size = 10)) +
    labs(title = title, x = "Year", y = y_label)
}

###----- Helper: compute county total_change values (for shared scale limits)

county_change_vals <- function(model, prep, county_centroids_all, year_start, year_end) {
  outcome_sd    <- prep$outcome_sd
  yr_mean       <- prep$yr_mean
  county_levels <- prep$county_levels
  cf            <- model$summary.fixed
  t1 <- year_start - yr_mean
  t2 <- year_end   - yr_mean
  county_centroids_all %>%
    filter(county %in% county_levels) %>%
    mutate(
      b1 = (cf["year_c",      "mean"] + cf["year_c:elev_c",      "mean"] * elev_c) * outcome_sd,
      b2 = (cf["I(year_c^2)", "mean"] + cf["I(year_c^2):elev_c", "mean"] * elev_c) * outcome_sd,
      total_change = b1 * (t2 - t1) + b2 * (t2^2 - t1^2)
    ) %>%
    pull(total_change)
}

###----- Helper: county-level change map (shared color scale per metric via fixed_limits)

make_map_panel_period <- function(model, prep, ny_counties, county_centroids_all,
                                   year_start, year_end, title, n_samp = 2000,
                                   fixed_limits = NULL) {
  outcome_sd    <- prep$outcome_sd
  yr_mean       <- prep$yr_mean
  county_levels <- prep$county_levels
  cf            <- model$summary.fixed
  t1 <- year_start - yr_mean
  t2 <- year_end   - yr_mean

  samps  <- inla.posterior.sample(n = n_samp, result = model)
  b1_vec <- sapply(samps, function(s) s$latent["year_c:1",               1])
  b2_vec <- sapply(samps, function(s) s$latent["I(year_c^2):1",          1])
  b3_vec <- sapply(samps, function(s) s$latent["year_c:elev_c:1",        1])
  b4_vec <- sapply(samps, function(s) s$latent["I(year_c^2):elev_c:1",   1])

  group_change <- function(elev_c_val) {
    eff_b1  <- b1_vec + b3_vec * elev_c_val
    eff_b2  <- b2_vec + b4_vec * elev_c_val
    samp_ch <- (eff_b1 * (t2 - t1) + eff_b2 * (t2^2 - t1^2)) * outcome_sd
    list(mean = mean(samp_ch), lo = quantile(samp_ch, 0.025), hi = quantile(samp_ch, 0.975))
  }

  county_elev <- county_centroids_all %>% filter(county %in% county_levels)
  elev_med    <- median(county_elev$elev_c)
  ch_lo <- group_change(mean(county_elev$elev_c[county_elev$elev_c <  elev_med]))
  ch_hi <- group_change(mean(county_elev$elev_c[county_elev$elev_c >= elev_med]))

  subtitle_text <- paste0(
    "Low elev: ",   round(ch_lo$mean, 1), " days (", round(ch_lo$lo, 1), ", ", round(ch_lo$hi, 1), ")\n",
    "High elev: ", round(ch_hi$mean, 1), " days (", round(ch_hi$lo, 1), ", ", round(ch_hi$hi, 1), ")"
  )

  centroids <- county_centroids_all %>%
    filter(county %in% county_levels) %>%
    mutate(
      b1 = (cf["year_c",      "mean"] + cf["year_c:elev_c",      "mean"] * elev_c) * outcome_sd,
      b2 = (cf["I(year_c^2)", "mean"] + cf["I(year_c^2):elev_c", "mean"] * elev_c) * outcome_sd,
      total_change = b1 * (t2 - t1) + b2 * (t2^2 - t1^2)
    )

  county_sf <- ny_counties %>%
    filter(county %in% county_levels) %>%
    left_join(centroids %>% select(county, total_change), by = "county")

  if (is.null(fixed_limits)) {
    lo  <- min(county_sf$total_change, na.rm = TRUE)
    hi  <- max(county_sf$total_change, na.rm = TRUE)
    mid <- (lo + hi) / 2
  } else {
    lo  <- fixed_limits$lo
    hi  <- fixed_limits$hi
    mid <- (lo + hi) / 2
  }

  ggplot(county_sf) +
    geom_sf(aes(fill = total_change), color = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "steelblue", mid = "lightyellow", high = "firebrick",
      midpoint = mid, limits = c(lo, hi), oob = scales::squish, name = "Days"
    ) +
    theme_void(base_size = 11) +
    theme(plot.subtitle = element_text(size = 8.5, lineheight = 1.3)) +
    labs(title = title, subtitle = subtitle_text)
}

###----- Shared color limits per metric (so 1980→2024 and 1999→2024 maps are comparable)

make_limits <- function(model, prep) {
  vals <- c(county_change_vals(model, prep, county_centroids_all, 1980, 2024),
            county_change_vals(model, prep, county_centroids_all, 1999, 2024))
  list(lo = min(vals, na.rm = TRUE), hi = max(vals, na.rm = TRUE), mid = 0)
}

lim_first  <- make_limits(m12_first,  first_day_prep)
lim_season <- make_limits(m12_season, season_len_prep)
lim_last   <- make_limits(m12_last,   last_day_prep)

###----- Build panels

p_trend_first_1999  <- make_trend_panel_m12(m12_first,  first_day_prep,  "FirstValid",
  y_label = "Change in first day (days vs. 1999)",    title = "First day (ref: 1999)")
p_trend_season_1999 <- make_trend_panel_m12(m12_season, season_len_prep, "SeasonLength",
  y_label = "Change in season length (days vs. 1999)", title = "Season length (ref: 1999)")
p_trend_last_1999   <- make_trend_panel_m12(m12_last,   last_day_prep,   "LastValid",
  y_label = "Change in last day (days vs. 1999)",     title = "Last day (ref: 1999)")

p_trend_season_1999 <- p_trend_season_1999 + theme(legend.position = "none")
p_trend_last_1999   <- p_trend_last_1999   + theme(legend.position = "none")

p_map_full_first  <- make_map_panel_period(m12_first,  first_day_prep,  ny_counties,
  county_centroids_all, 1980, 2024, "First day: change 1980→2024")
p_map_full_season <- make_map_panel_period(m12_season, season_len_prep, ny_counties,
  county_centroids_all, 1980, 2024, "Season length: change 1980→2024")
p_map_full_last   <- make_map_panel_period(m12_last,   last_day_prep,   ny_counties,
  county_centroids_all, 1980, 2024, "Last day: change 1980→2024")

p_map_post_first  <- make_map_panel_period(m12_first,  first_day_prep,  ny_counties,
  county_centroids_all, 1999, 2024, "First day: change 1999→2024")
p_map_post_season <- make_map_panel_period(m12_season, season_len_prep, ny_counties,
  county_centroids_all, 1999, 2024, "Season length: change 1999→2024")
p_map_post_last   <- make_map_panel_period(m12_last,   last_day_prep,   ny_counties,
  county_centroids_all, 1999, 2024, "Last day: change 1999→2024")

###----- Assemble figure: map 1980→2024 | map 1999→2024 | trend (ref: 1999)

col_widths <- c(1.5, 1.5, 1)

fig_m12 <-
  ((p_trend_first_1999  | p_map_full_first  | p_map_post_first)  + plot_layout(widths = col_widths)) /
  ((p_trend_season_1999 | p_map_full_season | p_map_post_season) + plot_layout(widths = col_widths)) /
  ((p_trend_last_1999   | p_map_full_last   | p_map_post_last)   + plot_layout(widths = col_widths)) 

fig_m12
ggsave("results/figures/gridMET_m12_emergence_threepanel.png", fig_m12, width = 20, height = 13, dpi = 600)

###----- Statewide average season length change (elev_c = 0 = state mean elevation)

statewide_season_change <- function(model, prep, year_start, year_end, n_samp = 2000) {
  outcome_sd <- prep$outcome_sd
  yr_mean    <- prep$yr_mean
  t1 <- year_start - yr_mean
  t2 <- year_end   - yr_mean
  samps  <- inla.posterior.sample(n = n_samp, result = model)
  b1_vec <- sapply(samps, function(s) s$latent["year_c:1",       1])
  b2_vec <- sapply(samps, function(s) s$latent["I(year_c^2):1",  1])
  samp_ch <- (b1_vec * (t2 - t1) + b2_vec * (t2^2 - t1^2)) * outcome_sd
  list(mean = mean(samp_ch), lo = quantile(samp_ch, 0.025), hi = quantile(samp_ch, 0.975))
}

sw_1980 <- statewide_season_change(m12_season, season_len_prep, 1980, 2024)
sw_1999 <- statewide_season_change(m12_season, season_len_prep, 1999, 2024)

cat("\nStatewide average change in season length:\n")
cat(sprintf("  1980-2024: %.1f days (95%% CrI: %.1f, %.1f)\n", sw_1980$mean, sw_1980$lo, sw_1980$hi))
cat(sprintf("  1999-2024: %.1f days (95%% CrI: %.1f, %.1f)\n", sw_1999$mean, sw_1999$lo, sw_1999$hi))
