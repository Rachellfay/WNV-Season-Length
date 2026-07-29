library(tidyverse)
library(INLA)
library(mgcv)
library(ggplot2)
library(patchwork)
library(sf)
library(tigris)
library(readxl)
library(spdep)

# Run from the project root (paths below are relative to it)

###----- Load raw season data and county elevation

raw <- read.csv("data/wnv_transmission_season_gridMET_1980_2024.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", county)))

county_elev_raw <- read.csv("data/NY_county_pop_wt_elevation.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", NAME)),
         elev   = pmax(as.numeric(pop_wt_elev_m), 0, na.rm = TRUE)) %>%
  select(county, elev) %>%
  mutate(elev_c = as.numeric(scale(elev)))

###----- Extract per-county season changes from M12 INLA models
#
# M12: outcome_scaled ~ year_c + I(year_c^2) + elev_c
#       + year_c:elev_c + I(year_c^2):elev_c + f(county_idx, bym2) + f(enso_bin, rw2)
#
# County change from t1 to t2:
#   b1(elev) * (t2 - t1) + b2(elev) * (t2^2 - t1^2), back-transformed by outcome_sd

compute_county_changes <- function(model_path, outcome_col, year_starts, year_ends) {

  model <- readRDS(model_path)

  df_filtered <- raw[!is.na(raw[[outcome_col]]), ]
  yr_mean     <- mean(df_filtered$year)
  outcome_sd  <- sd(df_filtered[[outcome_col]], na.rm = TRUE)

  cf            <- model$summary.fixed
  county_levels <- sort(unique(df_filtered$county))
  county_df     <- county_elev_raw[county_elev_raw$county %in% county_levels, ]
  elev_c_vec    <- county_df$elev_c

  period_list <- purrr::map2(year_starts, year_ends, function(ys, ye) {
    t1 <- ys - yr_mean
    t2 <- ye - yr_mean

    b1    <- (cf["year_c",      "mean"]       + cf["year_c:elev_c",      "mean"]       * elev_c_vec) * outcome_sd
    b2    <- (cf["I(year_c^2)", "mean"]       + cf["I(year_c^2):elev_c", "mean"]       * elev_c_vec) * outcome_sd
    b1_lo <- (cf["year_c",      "0.025quant"] + cf["year_c:elev_c",      "0.025quant"] * elev_c_vec) * outcome_sd
    b2_lo <- (cf["I(year_c^2)", "0.025quant"] + cf["I(year_c^2):elev_c", "0.025quant"] * elev_c_vec) * outcome_sd
    b1_hi <- (cf["year_c",      "0.975quant"] + cf["year_c:elev_c",      "0.975quant"] * elev_c_vec) * outcome_sd
    b2_hi <- (cf["I(year_c^2)", "0.975quant"] + cf["I(year_c^2):elev_c", "0.975quant"] * elev_c_vec) * outcome_sd

    data.frame(
      change    = b1    * (t2 - t1) + b2    * (t2^2 - t1^2),
      change_lo = b1_lo * (t2 - t1) + b2_lo * (t2^2 - t1^2),
      change_hi = b1_hi * (t2 - t1) + b2_hi * (t2^2 - t1^2)
    )
  })

  period_df        <- dplyr::bind_cols(period_list)
  names(period_df) <- paste0(rep(names(year_starts), each = 3), c("_change", "_lo95", "_hi95"))

  dplyr::bind_cols(county_df[, c("county", "elev")], period_df)
}

periods     <- c("p1980_1999" = 1980, "p1999_2024" = 1999, "p1980_2024" = 1980)
period_ends <- c("p1980_1999" = 1999, "p1999_2024" = 2024, "p1980_2024" = 2024)

add_prefix <- function(df, prefix) {
  period_cols <- setdiff(names(df), c("county", "elev"))
  names(df)[names(df) %in% period_cols] <- paste0(prefix, period_cols)
  df
}

message("Loading season length model (M12)...")
season_len_changes <- add_prefix(
  compute_county_changes("data/gridMET_m12_elev_quad_enso_rw2.rds",
                         "SeasonLength", periods, period_ends),
  "season_length_"
)

message("Loading first day model (M12)...")
first_day_changes <- add_prefix(
  compute_county_changes("data/gridMET_first_day_m12_elev_quad_enso_rw2.rds",
                         "FirstValid", periods, period_ends),
  "first_day_"
)

message("Loading last day model (M12)...")
last_day_changes <- add_prefix(
  compute_county_changes("data/gridMET_last_day_m12_elev_quad_enso_rw2.rds",
                         "LastValid", periods, period_ends),
  "last_day_"
)

county_changes <- dplyr::left_join(season_len_changes,
                                   first_day_changes[, names(first_day_changes) != "elev"],
                                   by = "county") |>
  dplyr::left_join(last_day_changes[, names(last_day_changes) != "elev"],
                   by = "county")

###----- WNV cases and incidence per county (1999-2024)

cases_raw        <- read.csv("data/cases_by_county_year.csv")
cases_raw$county <- tolower(gsub("[^a-zA-Z]", "", cases_raw$county))
cases_long       <- cases_raw[cases_raw$year >= 1999 & cases_raw$year <= 2024, ]

pop_raw        <- read.csv("data/ny_counties_ghsl_population_annual_1999_2024.csv")
pop_raw$county <- tolower(gsub("[^a-zA-Z]", "", pop_raw$county_name))
pop_long       <- pop_raw[, c("county", "year", "population")]

cases_pop <- merge(cases_long, pop_long, by = c("county", "year"), all.x = TRUE)

total_cases_county <- dplyr::summarise(
  dplyr::group_by(cases_pop, county),
  total_cases_1999_2024 = sum(total_cases, na.rm = TRUE),
  mean_pop_1999_2024    = mean(population,  na.rm = TRUE),
  incidence_per_10k     = sum(total_cases, na.rm = TRUE) / mean(population, na.rm = TRUE) * 1e4,
  log_incidence_per_10k = log1p(sum(total_cases, na.rm = TRUE) / mean(population, na.rm = TRUE) * 1e4),
  .groups = "drop"
)

county_changes <- dplyr::left_join(county_changes, total_cases_county, by = "county")

write.csv(county_changes,
          "data/county_season_changes_by_pop_wt_elevation.csv",
          row.names = FALSE)
message("Saved: data/county_season_changes_by_pop_wt_elevation.csv")

###----- NY spatial data (for inset maps)

elev_range <- range(county_changes$elev, na.rm = TRUE)

ny_sf <- tigris::counties(state = "NY", cb = TRUE, class = "sf") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", NAME))) %>%
  left_join(county_changes[, c("county", "incidence_per_10k", "elev")],
            by = "county")

###----- WNV prevalence (mosquito pools) per county

prev_raw <- read_excel(
  "data/WNV prevalence master spreadsheet.xlsx",
  sheet     = "prevalence calculations",
  col_names = FALSE,
  skip      = 3
)
names(prev_raw) <- c("row_label", "N", "P", "grand_total")

prev_county <- prev_raw[
  !is.na(prev_raw$row_label) &
  prev_raw$row_label != "Grand Total" &
  !grepl("^[0-9]+$", trimws(prev_raw$row_label)), ]

prev_county$county <- tolower(gsub("[^a-zA-Z]", "", prev_county$row_label))
prev_county$N      <- as.numeric(prev_county$N)
prev_county$P      <- as.numeric(prev_county$P)

prev_summary <- dplyr::summarise(
  dplyr::group_by(prev_county, county),
  avg_prevalence = weighted.mean(P, N, na.rm = TRUE),
  total_N        = sum(N, na.rm = TRUE),
  .groups        = "drop"
)

message("Prevalence counties found: ", nrow(prev_summary))

###----- Average summer (Jun-Aug) SPEI90d per county, 1999-2024

spei_wide       <- read.csv("data/county_gridmet_data/NY_county_gridMET_spei90d.csv",
                             check.names = FALSE)
spei_wide$date  <- as.Date(spei_wide$time)
spei_wide$year  <- as.integer(format(spei_wide$date, "%Y"))
spei_wide$month <- as.integer(format(spei_wide$date, "%m"))

spei_summer <- spei_wide[spei_wide$year >= 1999 & spei_wide$year <= 2024 &
                           spei_wide$month %in% 6:8, ]

spei_long <- tidyr::pivot_longer(
  spei_summer,
  cols      = -c(time, date, year, month),
  names_to  = "county_raw",
  values_to = "spei30d"
) %>%
  mutate(county  = tolower(gsub("[^a-zA-Z]", "", county_raw)),
         spei30d = as.numeric(spei30d))

avg_summer_spei <- dplyr::summarise(
  dplyr::group_by(spei_long, county),
  avg_summer_spei = mean(spei30d, na.rm = TRUE),
  .groups = "drop"
)

###----- GAM data prep

gam_dat <- county_changes %>%
  dplyr::left_join(avg_summer_spei, by = "county") %>%
  dplyr::left_join(prev_summary[, c("county", "avg_prevalence")], by = "county")

gam_inc_dat  <- gam_dat[!is.na(gam_dat$season_length_p1999_2024_change) &
                           !is.na(gam_dat$total_cases_1999_2024) &
                           !is.na(gam_dat$mean_pop_1999_2024) &
                           gam_dat$mean_pop_1999_2024 > 0 &
                           !is.na(gam_dat$avg_summer_spei), ]
gam_prev_dat <- gam_dat[!is.na(gam_dat$season_length_p1999_2024_change) &
                           !is.na(gam_dat$avg_prevalence) &
                           !is.na(gam_dat$avg_summer_spei), ]

message("GAM incidence n = ", nrow(gam_inc_dat),
        " | GAM prevalence n = ", nrow(gam_prev_dat))

###----- GAM models

# nb() with log-population offset: models log(incidence rate), handles zeros
gam_inc <- mgcv::gam(
  total_cases_1999_2024 ~ s(season_length_p1999_2024_change, k = 3) +
                           avg_summer_spei +
                           offset(log(mean_pop_1999_2024)),
  data   = gam_inc_dat,
  method = "REML",
  family = nb()
)
cat("\n===== GAM: WNV incidence ~ season length change + avg summer SPEI =====\n")
print(summary(gam_inc))

# quasibinomial handles proportions with exact zeros
gam_prev <- mgcv::gam(
  avg_prevalence ~ s(season_length_p1999_2024_change, k = 3) +
                       avg_summer_spei,
  data   = gam_prev_dat,
  method = "REML",
  family = quasibinomial(link = "logit")
)
cat("\n===== GAM: WNV prevalence ~ season length change + avg summer SPEI =====\n")
print(summary(gam_prev))

###----- GAM diagnostics

cat("\n===== k-check: incidence =====\n")
print(mgcv::k.check(gam_inc))
cat("\n===== k-check: prevalence =====\n")
print(mgcv::k.check(gam_prev))

cat("\n===== Concurvity: incidence =====\n")
print(mgcv::concurvity(gam_inc, full = FALSE))
cat("\n===== Concurvity: prevalence =====\n")
print(mgcv::concurvity(gam_prev, full = FALSE))

cat("\n===== Dispersion: prevalence (quasibinomial) =====\n")
cat("Estimated dispersion:", summary(gam_prev)$dispersion, "\n")

png("data/gam_check_incidence.png", width = 1200, height = 1200, res = 150)
  mgcv::gam.check(gam_inc)
dev.off()
message("Saved: data/gam_check_incidence.png")

png("data/gam_check_prevalence.png", width = 1200, height = 1200, res = 150)
  mgcv::gam.check(gam_prev)
dev.off()
message("Saved: data/gam_check_prevalence.png")

# nb() uses Chi.sq; quasibinomial uses F — grab whichever is present
gam_summary_to_df <- function(gam_obj, label) {
  s         <- summary(gam_obj)
  smth      <- as.data.frame(s$s.table)
  smth$term          <- rownames(smth)
  smth$model         <- label
  smth$dev_explained <- paste0(round(s$dev.expl * 100, 1), "%")
  stat_col  <- intersect(c("F", "Chi.sq"), colnames(smth))[1]
  smth$stat <- smth[[stat_col]]
  smth[, c("model", "term", "edf", "Ref.df", "stat", "p-value", "dev_explained")]
}

gam_summary <- dplyr::bind_rows(
  gam_summary_to_df(gam_inc,  "total_cases_nb_offset"),
  gam_summary_to_df(gam_prev, "avg_prevalence")
)
print(gam_summary)
write.csv(gam_summary, "data/gam_season_spei_summary.csv", row.names = FALSE)
message("Saved: data/gam_season_spei_summary.csv")

###----- Partial residuals and GAM smooth

# rate_scale converts from rate-per-person to desired display scale
# pop: divide count residuals by population before scaling (nb offset model)
add_partial_resid <- function(gam_obj, dat, rate_scale = 1, pop = NULL) {
  terms_mat         <- predict(gam_obj, type = "terms")
  inv               <- gam_obj$family$linkinv
  intercept         <- coef(gam_obj)["(Intercept)"]
  season_contrib    <- terms_mat[, "s(season_length_p1999_2024_change)"]
  mean_spei_contrib <- mean(terms_mat[, "avg_summer_spei"], na.rm = TRUE)
  smooth_pred       <- inv(intercept + season_contrib + mean_spei_contrib) * rate_scale
  resid_raw         <- residuals(gam_obj, type = "response")
  if (!is.null(pop)) resid_raw <- resid_raw / pop * rate_scale
  dat$partial_resid <- smooth_pred + resid_raw
  dat
}

gam_inc_dat  <- add_partial_resid(gam_inc,  gam_inc_dat,
                                   rate_scale = 1e4,
                                   pop = gam_inc_dat$mean_pop_1999_2024)
gam_prev_dat <- add_partial_resid(gam_prev, gam_prev_dat)

# mean_pop_1999_2024 = 1 → offset(log(1)) = 0, giving rate per person; multiply by rate_scale
gam_smooth_df <- function(gam_obj, dat, n = 200, rate_scale = 1) {
  x_seq  <- seq(min(dat$season_length_p1999_2024_change, na.rm = TRUE),
                max(dat$season_length_p1999_2024_change, na.rm = TRUE),
                length.out = n)
  newdat <- data.frame(
    season_length_p1999_2024_change = x_seq,
    avg_summer_spei                 = mean(dat$avg_summer_spei, na.rm = TRUE),
    mean_pop_1999_2024              = 1
  )
  pred <- predict(gam_obj, newdata = newdat, type = "link", se.fit = TRUE)
  inv  <- gam_obj$family$linkinv
  data.frame(
    x   = x_seq,
    fit = inv(as.numeric(pred$fit)) * rate_scale,
    lo  = inv(as.numeric(pred$fit) - 1.96 * as.numeric(pred$se.fit)) * rate_scale,
    hi  = inv(as.numeric(pred$fit) + 1.96 * as.numeric(pred$se.fit)) * rate_scale
  )
}

cat("\n===== Incidence: counties ranked by partial residual (descending) =====\n")
print(gam_inc_dat[order(-gam_inc_dat$partial_resid),
                  c("county", "season_length_p1999_2024_change", "partial_resid")],
      digits = 3)

cat("\n===== Prevalence: counties ranked by partial residual (descending) =====\n")
print(gam_prev_dat[order(-gam_prev_dat$partial_resid),
                   c("county", "season_length_p1999_2024_change", "partial_resid")],
      digits = 3)

label_counties_inc  <- c("greene", "yates")
label_counties_prev <- c("rockland", "erie")

smooth_inc  <- gam_smooth_df(gam_inc,  gam_inc_dat,  rate_scale = 1e4)
smooth_prev <- gam_smooth_df(gam_prev, gam_prev_dat)

gam_lbl <- function(gam_obj) {
  s <- summary(gam_obj)
  paste0("Dev. expl. = ", round(s$dev.expl * 100, 1), "%")
}
inc_lbl  <- gam_lbl(gam_inc)
prev_lbl <- gam_lbl(gam_prev)

###----- Figure: GAM scatter panels with inset maps

ny_sf_gam <- ny_sf %>%
  dplyr::left_join(prev_summary[, c("county", "avg_prevalence")], by = "county")

inset_theme_sm <- theme(
  legend.position      = c(1.15, 0.99),
  legend.justification = c("right", "top"),
  legend.direction     = "vertical",
  legend.text          = element_text(size = 9),
  legend.title         = element_text(size = 9, face = "bold"),
  legend.key.height    = unit(0.45, "cm"),
  legend.key.width     = unit(0.22, "cm"),
  legend.background    = element_rect(fill = alpha("white", 0.7), color = NA),
  plot.background      = element_blank(),
  plot.title           = element_blank()
)

p_inc_map <- ggplot(ny_sf_gam) +
  geom_sf(aes(fill = incidence_per_10k), color = "white", linewidth = 0.25) +
  scale_fill_gradient(name = "Incidence\nper 10k",
                      low = "#e8def1", high = "#300163", na.value = "grey80") +
  theme_void(base_size = 9) + inset_theme_sm

p_prev_map <- ggplot(ny_sf_gam) +
  geom_sf(aes(fill = avg_prevalence), color = "white", linewidth = 0.25) +
  scale_fill_gradient(name = "Avg. WNV\nprevalence",
                      low = "#fde8d0", high = "#b5320a", na.value = "grey80") +
  theme_void(base_size = 9) + inset_theme_sm

p_inc_gam <- ggplot() +
  geom_ribbon(data = smooth_inc,
              aes(x = x, ymin = pmax(lo, 0), ymax = hi),
              fill = "darkorchid4", alpha = 0.15) +
  geom_line(data  = smooth_inc, aes(x = x, y = fit),
            color = "darkorchid4", linewidth = 0.9) +
  geom_point(data = gam_inc_dat,
             aes(x = season_length_p1999_2024_change,
                 y = partial_resid, fill = elev),
             size = 3.5, alpha = 0.6, shape = 21, color = "white", stroke = 0.3) +
  ggrepel::geom_text_repel(
    data = gam_inc_dat[gam_inc_dat$county %in% setdiff(label_counties_inc, "greene"), ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4, max.overlaps = 20,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  ggrepel::geom_text_repel(
    data = gam_inc_dat[gam_inc_dat$county == "greene", ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4,
    nudge_x = 2, nudge_y = -0.25,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  scale_fill_gradient(name = "Pop.-wtd.\nelev. (m)",
                      low = "#96f188", high = "#4a1c0a",
                      limits = elev_range, na.value = "grey80") +
  annotate("text", x = Inf, y = Inf, label = inc_lbl,
           hjust = 1.05, vjust = 1.4, size = 5.5,
           color = "grey30", fontface = "bold.italic") +
  theme_classic(base_size = 16) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Change in season length (days, 1999-2024)",
       y = "WNV incidence per 10k")

p_prev_gam <- ggplot() +
  geom_ribbon(data = smooth_prev,
              aes(x = x, ymin = pmax(lo, 0), ymax = hi),
              fill = "#b5320a", alpha = 0.15) +
  geom_line(data  = smooth_prev, aes(x = x, y = fit),
            color = "#b5320a", linewidth = 0.9) +
  geom_point(data = gam_prev_dat,
             aes(x = season_length_p1999_2024_change,
                 y = partial_resid, fill = elev),
             size = 3.5, alpha = 0.6, shape = 21, color = "white", stroke = 0.3) +
  ggrepel::geom_text_repel(
    data = gam_prev_dat[gam_prev_dat$county == "erie", ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  ggrepel::geom_text_repel(
    data = gam_prev_dat[gam_prev_dat$county == "rockland", ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4,
    nudge_x = 2.5, nudge_y = -0.025,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  scale_fill_gradient(name = "Pop.-wtd.\nelev. (m)",
                      low = "#96f188", high = "#4a1c0a",
                      limits = elev_range, na.value = "grey80") +
  annotate("text", x = Inf, y = Inf, label = prev_lbl,
           hjust = 1.05, vjust = 1.4, size = 5.5,
           color = "grey30", fontface = "bold.italic") +
  theme_classic(base_size = 16) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Change in season length (days, 1999-2024)",
       y = "Avg. WNV prevalence (mosquito pools)")

p_inc_panel <- (p_inc_gam + guides(fill = "none") + labs(tag = "A")) +
  inset_element(p_inc_map,
                left = 0, bottom = 0.45, right = 0.55, top = 1.0,
                align_to = "panel")

p_prev_panel <- (p_prev_gam + guides(fill = "none") + labs(tag = "B")) +
  inset_element(p_prev_map,
                left = 0, bottom = 0.45, right = 0.55, top = 1.0,
                align_to = "panel")

elev_bar_dat <- data.frame(elev = seq(elev_range[1], elev_range[2], length.out = 500),
                           y    = 0)
p_elev_bar <- ggplot(elev_bar_dat, aes(x = elev, y = y, fill = elev)) +
  geom_raster() +
  scale_fill_gradient(low = "#96f188", high = "#4a1c0a",
                      limits = elev_range, guide = "none") +
  scale_x_continuous(name   = "Pop.-wtd. elevation (m)",
                     breaks = pretty(elev_range, n = 4),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 16) +
  theme(axis.line.y   = element_blank(),
        axis.text.y   = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_text(size = 16, face = "italic"),
        plot.margin   = margin(8, 80, 5, 80))

fig_gam_panel <- (p_inc_panel | p_prev_panel) / p_elev_bar +
  plot_layout(ncol = 1, heights = c(10, 0.4)) &
  theme(text     = element_text(size = 16),
        plot.tag = element_text(size = 18, face = "bold"))

ggsave("results/figures/gam_season_length_panel.png",
       fig_gam_panel, width = 16, height = 6, dpi = 600)
message("Saved: results/figures/gam_season_length_panel.png")

###----- Moran's I on GAM residuals

centroids <- sf::st_centroid(ny_sf) %>%
  mutate(lon = sf::st_coordinates(.)[, 1],
         lat = sf::st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry() %>%
  select(county, lon, lat)

run_morans <- function(gam_obj, dat, label) {
  dat$resid <- residuals(gam_obj)
  dat <- dplyr::left_join(dat, centroids, by = "county")
  dat <- dat[!is.na(dat$lon), ]

  coords  <- as.matrix(dat[, c("lon", "lat")])
  nb      <- spdep::knn2nb(spdep::knearneigh(coords, k = 5))
  listw   <- spdep::nb2listw(nb, style = "W")
  mt      <- spdep::moran.test(dat$resid, listw)

  cat("\n===== Moran's I on", label, "GAM residuals =====\n")
  print(mt)

  tibble(
    model       = label,
    morans_I    = round(mt$estimate["Moran I statistic"], 4),
    expectation = round(mt$estimate["Expectation"], 4),
    p_value     = round(mt$p.value, 4)
  )
}

morans_results <- dplyr::bind_rows(
  run_morans(gam_inc,  gam_inc_dat,  "incidence_per_10k"),
  run_morans(gam_prev, gam_prev_dat, "avg_prevalence")
)

print(morans_results)
write.csv(morans_results, "data/gam_residuals_morans_i.csv", row.names = FALSE)
message("Saved: data/gam_residuals_morans_i.csv")

###----- Leave-one-out sensitivity: incidence GAM

# Fixed x grid and mean SPEI used for all LOO predictions
loo_x_seq <- seq(min(gam_inc_dat$season_length_p1999_2024_change, na.rm = TRUE),
                 max(gam_inc_dat$season_length_p1999_2024_change, na.rm = TRUE),
                 length.out = 200)
loo_newdat <- data.frame(
  season_length_p1999_2024_change = loo_x_seq,
  avg_summer_spei                 = mean(gam_inc_dat$avg_summer_spei, na.rm = TRUE),
  mean_pop_1999_2024              = 1
)

# Full-model smooth on same grid (per 10k)
full_fit <- exp(as.numeric(predict(gam_inc, newdata = loo_newdat, type = "link"))) * 1e4

# loo_fits stores both the smooth predictions and per-model stats
loo_fits <- purrr::map(gam_inc_dat$county, function(co) {
  dat_loo <- gam_inc_dat[gam_inc_dat$county != co, ]
  fit_loo <- tryCatch(
    mgcv::gam(
      total_cases_1999_2024 ~ s(season_length_p1999_2024_change, k = 3) +
                               avg_summer_spei +
                               offset(log(mean_pop_1999_2024)),
      data = dat_loo, method = "REML", family = nb()
    ),
    error = function(e) NULL
  )
  if (is.null(fit_loo)) return(NULL)
  s <- summary(fit_loo)
  list(
    smooth = data.frame(
      excluded = co,
      x        = loo_x_seq,
      fit      = exp(as.numeric(predict(fit_loo, newdata = loo_newdat, type = "link"))) * 1e4
    ),
    stats = data.frame(
      excluded    = co,
      edf         = round(s$s.table[1, "edf"], 3),
      smooth_p    = s$s.table[1, "p-value"],
      dev_expl    = round(s$dev.expl * 100, 1)
    )
  )
})

loo_smooths <- purrr::map_dfr(loo_fits, "smooth")
loo_stats   <- purrr::map_dfr(loo_fits, "stats")

# Max absolute deviation from full-model smooth per excluded county
loo_influence <- loo_smooths %>%
  dplyr::left_join(data.frame(x = loo_x_seq, full = full_fit), by = "x") %>%
  dplyr::group_by(excluded) %>%
  dplyr::summarise(max_abs_dev  = max(abs(fit - full)),
                   mean_abs_dev = mean(abs(fit - full)),
                   .groups = "drop") %>%
  dplyr::left_join(loo_stats, by = "excluded") %>%
  dplyr::arrange(dplyr::desc(max_abs_dev))

cat("\n===== LOO influence: top 10 most influential counties (incidence GAM) =====\n")
print(head(loo_influence, 10), digits = 3)
write.csv(loo_influence, "data/gam_loo_influence.csv", row.names = FALSE)
message("Saved: data/gam_loo_influence.csv")

# Spaghetti plot: all LOO smooths (grey) vs full model (purple)
p_loo <- ggplot() +
  geom_line(data = loo_smooths,
            aes(x = x, y = fit, group = excluded),
            color = "grey60", linewidth = 0.35, alpha = 0.5) +
  geom_line(data = data.frame(x = loo_x_seq, fit = full_fit),
            aes(x = x, y = fit),
            color = "darkorchid4", linewidth = 1.2) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_classic(base_size = 13) +
  labs(x    = "Change in season length (days, 1999-2024)",
       y    = "WNV incidence per 10k (ref. pop.)")
 #      title = "Leave-one-out sensitivity: incidence GAM",
 #      subtitle = "Grey = each county excluded; purple = full model")

p_loo_dev_inc <- ggplot(
    loo_influence %>%
      mutate(county_lbl = tools::toTitleCase(excluded),
             county_lbl = forcats::fct_reorder(county_lbl, dev_expl)),
    aes(x = dev_expl, y = county_lbl)
  ) +
  geom_vline(xintercept = summary(gam_inc)$dev.expl * 100,
             color = "darkorchid4", linewidth = 0.7, linetype = "dashed") +
  geom_point(size = 2, color = "grey40") +
  theme_classic(base_size = 11) +
  labs(x = "Deviance explained (%)",
       y = NULL,
  #     title = "LOO deviance explained",
       subtitle = "Dashed = full model (46.4%)") +
  theme(axis.text.y = element_text(size = 7))

fig_loo_inc <- p_loo + p_loo_dev_inc +
  plot_layout(widths = c(1.6, 1)) &
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))

ggsave("data/gam_loo_sensitivity.png",
       fig_loo_inc, width = 14, height = 6.5, dpi = 300)
message("Saved: data/gam_loo_sensitivity.png")

###----- Leave-one-out sensitivity: prevalence GAM

loo_x_seq_prev <- seq(min(gam_prev_dat$season_length_p1999_2024_change, na.rm = TRUE),
                      max(gam_prev_dat$season_length_p1999_2024_change, na.rm = TRUE),
                      length.out = 200)
loo_newdat_prev <- data.frame(
  season_length_p1999_2024_change = loo_x_seq_prev,
  avg_summer_spei                 = mean(gam_prev_dat$avg_summer_spei, na.rm = TRUE)
)

full_fit_prev <- plogis(as.numeric(predict(gam_prev, newdata = loo_newdat_prev, type = "link")))

loo_fits_prev <- purrr::map(gam_prev_dat$county, function(co) {
  dat_loo <- gam_prev_dat[gam_prev_dat$county != co, ]
  fit_loo <- tryCatch(
    mgcv::gam(
      avg_prevalence ~ s(season_length_p1999_2024_change, k = 3) +
                           avg_summer_spei,
      data = dat_loo, method = "REML", family = quasibinomial(link = "logit")
    ),
    error = function(e) NULL
  )
  if (is.null(fit_loo)) return(NULL)
  s <- summary(fit_loo)
  list(
    smooth = data.frame(
      excluded = co,
      x        = loo_x_seq_prev,
      fit      = plogis(as.numeric(predict(fit_loo, newdata = loo_newdat_prev, type = "link")))
    ),
    stats = data.frame(
      excluded = co,
      edf      = round(s$s.table[1, "edf"], 3),
      smooth_p = s$s.table[1, "p-value"],
      dev_expl = round(s$dev.expl * 100, 1)
    )
  )
})

loo_smooths_prev <- purrr::map_dfr(loo_fits_prev, "smooth")
loo_stats_prev   <- purrr::map_dfr(loo_fits_prev, "stats")

loo_influence_prev <- loo_smooths_prev %>%
  dplyr::left_join(data.frame(x = loo_x_seq_prev, full = full_fit_prev), by = "x") %>%
  dplyr::group_by(excluded) %>%
  dplyr::summarise(max_abs_dev  = max(abs(fit - full)),
                   mean_abs_dev = mean(abs(fit - full)),
                   .groups = "drop") %>%
  dplyr::left_join(loo_stats_prev, by = "excluded") %>%
  dplyr::arrange(dplyr::desc(max_abs_dev))

cat("\n===== LOO influence: top 10 most influential counties (prevalence GAM) =====\n")
print(head(loo_influence_prev, 10), digits = 3)
write.csv(loo_influence_prev, "data/gam_loo_influence_prevalence.csv", row.names = FALSE)
message("Saved: data/gam_loo_influence_prevalence.csv")

p_loo_prev <- ggplot() +
  geom_line(data = loo_smooths_prev,
            aes(x = x, y = fit, group = excluded),
            color = "grey60", linewidth = 0.35, alpha = 0.5) +
  geom_line(data = data.frame(x = loo_x_seq_prev, fit = full_fit_prev),
            aes(x = x, y = fit),
            color = "#b5320a", linewidth = 1.2) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_classic(base_size = 13) +
  labs(x        = "Change in season length (days, 1999-2024)",
       y        = "Avg. WNV prevalence (mosquito pools)")
 #      title    = "Leave-one-out sensitivity: prevalence GAM",
 #      subtitle = "Grey = each county excluded; orange = full model")

p_loo_dev_prev <- ggplot(
    loo_influence_prev %>%
      mutate(county_lbl = tools::toTitleCase(excluded),
             county_lbl = forcats::fct_reorder(county_lbl, dev_expl)),
    aes(x = dev_expl, y = county_lbl)
  ) +
  geom_vline(xintercept = summary(gam_prev)$dev.expl * 100,
             color = "#b5320a", linewidth = 0.7, linetype = "dashed") +
  geom_point(size = 2, color = "grey40") +
  theme_classic(base_size = 11) +
  labs(x = "Deviance explained (%)",
       y = NULL,
       subtitle = "Dashed = full model (29.7%)") +
  theme(axis.text.y = element_text(size = 7))

fig_loo_prev <- p_loo_prev + p_loo_dev_prev +
  plot_layout(widths = c(1.6, 1)) &
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))

ggsave("data/gam_loo_sensitivity_prevalence.png",
       fig_loo_prev, width = 14, height = 6.5, dpi = 300)
message("Saved: data/gam_loo_sensitivity_prevalence.png")

###----- Four-panel LOO figure (incidence + prevalence)

fig_loo_fourpanel <- (p_loo | p_loo_dev_inc) / (p_loo_prev | p_loo_dev_prev) +
  plot_layout(widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 13, face = "bold"),
        plot.title    = element_text(size = 12),
        plot.subtitle = element_text(size = 9))

ggsave("results/figures/gam_loo_fourpanel.png",
       fig_loo_fourpanel, width = 14, height = 12, dpi = 300)
message("Saved: results/figures/gam_loo_fourpanel.png")

###----- Export plot data for standalone figure script

dir.create("data/plot_inputs", showWarnings = FALSE)

write.csv(
  gam_inc_dat[, c("county", "season_length_p1999_2024_change", "elev", "partial_resid")],
  "data/plot_inputs/plot_data_inc.csv", row.names = FALSE)

write.csv(
  gam_prev_dat[, c("county", "season_length_p1999_2024_change", "elev", "partial_resid")],
  "data/plot_inputs/plot_data_prev.csv", row.names = FALSE)

write.csv(smooth_inc,  "data/plot_inputs/plot_smooth_inc.csv",  row.names = FALSE)
write.csv(smooth_prev, "data/plot_inputs/plot_smooth_prev.csv", row.names = FALSE)

write.csv(
  dplyr::left_join(
    county_changes[, c("county", "incidence_per_10k", "elev")],
    prev_summary[,  c("county", "avg_prevalence")],
    by = "county"),
  "data/plot_inputs/plot_map_counties.csv", row.names = FALSE)

write.csv(loo_smooths, "data/plot_inputs/plot_loo_smooths_inc.csv", row.names = FALSE)
write.csv(data.frame(x = loo_x_seq, fit = full_fit),
          "data/plot_inputs/plot_loo_full_inc.csv", row.names = FALSE)
write.csv(loo_influence[, c("excluded", "dev_expl")],
          "data/plot_inputs/plot_loo_deviance_inc.csv", row.names = FALSE)

write.csv(loo_smooths_prev, "data/plot_inputs/plot_loo_smooths_prev.csv", row.names = FALSE)
write.csv(data.frame(x = loo_x_seq_prev, fit = full_fit_prev),
          "data/plot_inputs/plot_loo_full_prev.csv", row.names = FALSE)
write.csv(loo_influence_prev[, c("excluded", "dev_expl")],
          "data/plot_inputs/plot_loo_deviance_prev.csv", row.names = FALSE)

write.csv(
  data.frame(
    key   = c("inc_lbl", "prev_lbl", "inc_dev_expl", "prev_dev_expl"),
    value = c(inc_lbl, prev_lbl,
              round(summary(gam_inc)$dev.expl  * 100, 1),
              round(summary(gam_prev)$dev.expl * 100, 1))),
  "data/plot_inputs/plot_meta.csv", row.names = FALSE)

message("Saved: data/plot_inputs/ (plot input CSVs for standalone figure script)")
