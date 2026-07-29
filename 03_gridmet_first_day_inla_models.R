# INLA model of WNV season start (first day of transmission) analysis — gridMET 1980-2024

# --- install packages if needed (run once) ---
# install.packages(c("spdep", "sf", "tigris", "rsoi", "elevatr", "patchwork"))
# install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
# ---------------------------------------------

library(tidyverse); library(data.table); library(dplyr)
library(INLA)
library(spdep)
library(sf)
library(tigris)
library(rsoi)
library(patchwork)

# Run from the project root (paths below are relative to it)

wnv_data <- read.csv("data/wnv_transmission_season_gridMET_1980_2024.csv")

###----- Prepare data

wnv_data <- wnv_data %>%
  mutate(
    county = tolower(gsub("[^a-zA-Z]", "", county)),
    year_c = year - mean(year)
  ) %>%
  filter(!is.na(FirstValid))

county_levels <- sort(unique(wnv_data$county))
wnv_data <- wnv_data %>%
  mutate(
    county_idx  = as.integer(factor(county, levels = county_levels)),
    county_idx2 = as.integer(factor(county, levels = county_levels)),
    year_idx    = as.integer(factor(year))
  )

n_counties <- length(county_levels)

first_mean <- mean(wnv_data$FirstValid)
first_sd   <- sd(wnv_data$FirstValid)
wnv_data   <- wnv_data %>%
  mutate(outcome_scaled = as.numeric(scale(FirstValid)))

# ONI: Apr-Jul seasonal average (pre-season conditions affecting season start)
oni <- rsoi::download_oni(use_cache = TRUE)

enso_seasonal <- oni %>%
  filter(month(Date) >= 4, month(Date) <= 7) %>%
  group_by(Year) %>%
  summarise(enso_seasonal = mean(ONI, na.rm = TRUE)) %>%
  rename(year = Year)

wnv_data <- wnv_data %>%
  left_join(enso_seasonal, by = "year") %>%
  mutate(enso_scaled = as.numeric(scale(enso_seasonal)))

enso_breaks <- quantile(wnv_data$enso_scaled, probs = seq(0, 1, length.out = 21), na.rm = TRUE)
wnv_data <- wnv_data %>%
  mutate(enso_bin  = as.integer(cut(enso_scaled, breaks = enso_breaks,
                                     include.lowest = TRUE, labels = FALSE)),
         is_elnino = as.integer(enso_seasonal >= 0.5))

year_span_report <- 2024 - 1999   # 25 years — WNV surveillance era

###----- Build spatial adjacency matrix for NY counties

ny_counties <- tigris::counties(state = "NY", cb = TRUE, class = "sf") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", NAME))) %>%
  filter(county %in% county_levels) %>%
  arrange(county)

stopifnot(all(county_levels %in% ny_counties$county))

mean_elev <- read.csv("data/NY_county_mean_elevation.csv") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", county_name))) %>%
  select(county, mean_elev_m)

centroid_sf <- ny_counties %>% sf::st_centroid()

county_centroids <- centroid_sf %>%
  mutate(lat = sf::st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry() %>%
  select(county, lat) %>%
  left_join(mean_elev, by = "county") %>%
  rename(elev = mean_elev_m) %>%
  mutate(lat_c = as.numeric(scale(lat)), elev_c = as.numeric(scale(elev)))

wnv_data <- wnv_data %>%
  left_join(county_centroids %>% select(county, lat_c, elev_c), by = "county")

ny_counties_proj <- sf::st_transform(ny_counties, crs = 32618)
nb  <- spdep::poly2nb(ny_counties_proj, queen = TRUE, snap = 2000)
spdep::nb2INLA("data/ny_counties_adj.graph", nb)
adj <- INLA::inla.read.graph("data/ny_counties_adj.graph")

bym2 <- list(
  phi  = list(prior = "pc", param = c(0.5, 0.5)),
  prec = list(prior = "pc.prec", param = c(1, 0.01))
)

###----- Fit models

fit_model <- function(formula) {
  inla(formula, family = "gaussian", data = wnv_data,
       control.compute    = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
       control.predictor  = list(compute = TRUE))
}

rw2_prior <- list(prec = list(prior = "pc.prec", param = c(0.1, 0.01)))

##--- Baseline models
formula_m1 <- outcome_scaled ~ year_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m2 <- outcome_scaled ~ year_c + I(year_c^2) +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m3 <- outcome_scaled ~ year_c + enso_scaled +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m4 <- outcome_scaled ~ year_c + I(year_c^2) +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

##--- County-specific slope models
formula_m5 <- outcome_scaled ~ year_c +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties)

formula_m6 <- outcome_scaled ~ year_c + I(year_c^2) +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties)

formula_m7 <- outcome_scaled ~ year_c + enso_scaled +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties)

formula_m8 <- outcome_scaled ~ year_c + I(year_c^2) +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

##--- Elevation x year models
formula_m9  <- outcome_scaled ~ year_c * elev_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m10 <- outcome_scaled ~ year_c + I(year_c^2) + elev_c +
  year_c:elev_c + I(year_c^2):elev_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m11 <- outcome_scaled ~ year_c * elev_c + enso_scaled +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m12 <- outcome_scaled ~ year_c + I(year_c^2) + elev_c +
  year_c:elev_c + I(year_c^2):elev_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

##--- Latitude x year models
formula_m13 <- outcome_scaled ~ year_c * lat_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m14 <- outcome_scaled ~ year_c + I(year_c^2) + lat_c +
  year_c:lat_c + I(year_c^2):lat_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m15 <- outcome_scaled ~ year_c * lat_c + enso_scaled +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m16 <- outcome_scaled ~ year_c + I(year_c^2) + lat_c +
  year_c:lat_c + I(year_c^2):lat_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

##--- Linear year + ENSO RW2 variants (completing each group's linear/ENSO-RW2 cell)
formula_m17 <- outcome_scaled ~ year_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

formula_m18 <- outcome_scaled ~ year_c +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

formula_m19 <- outcome_scaled ~ year_c * elev_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

formula_m20 <- outcome_scaled ~ year_c * lat_c +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2) +
  f(enso_bin, model = "rw2", hyper = rw2_prior)

##--- El Nino binary models (is_elnino: 1 if Apr-Jul mean ONI >= 0.5)
# Baseline
formula_m21 <- outcome_scaled ~ year_c + is_elnino +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m22 <- outcome_scaled ~ year_c + I(year_c^2) + is_elnino +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

# County-specific slope
formula_m23 <- outcome_scaled ~ year_c + is_elnino +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties)

formula_m24 <- outcome_scaled ~ year_c + I(year_c^2) + is_elnino +
  f(county_idx,  model = "bym2", graph = adj, hyper = bym2) +
  f(county_idx2, year_c, model = "iid", values = 1:n_counties)

# Elevation x year
formula_m25 <- outcome_scaled ~ year_c * elev_c + is_elnino +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m26 <- outcome_scaled ~ year_c + I(year_c^2) + elev_c +
  year_c:elev_c + I(year_c^2):elev_c + is_elnino +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

# Latitude x year
formula_m27 <- outcome_scaled ~ year_c * lat_c + is_elnino +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

formula_m28 <- outcome_scaled ~ year_c + I(year_c^2) + lat_c +
  year_c:lat_c + I(year_c^2):lat_c + is_elnino +
  f(county_idx, model = "bym2", graph = adj, hyper = bym2)

model_m1  <- fit_model(formula_m1)
model_m2  <- fit_model(formula_m2)
model_m3  <- fit_model(formula_m3)
model_m4  <- fit_model(formula_m4)
model_m5  <- fit_model(formula_m5)
model_m6  <- fit_model(formula_m6)
model_m7  <- fit_model(formula_m7)
model_m8  <- fit_model(formula_m8)
model_m9  <- fit_model(formula_m9)
model_m10 <- fit_model(formula_m10)
model_m11 <- fit_model(formula_m11)
model_m12 <- fit_model(formula_m12)
model_m13 <- fit_model(formula_m13)
model_m14 <- fit_model(formula_m14)
model_m15 <- fit_model(formula_m15)
model_m16 <- fit_model(formula_m16)
model_m17 <- fit_model(formula_m17)
model_m18 <- fit_model(formula_m18)
model_m19 <- fit_model(formula_m19)
model_m20 <- fit_model(formula_m20)
model_m21 <- fit_model(formula_m21)
model_m22 <- fit_model(formula_m22)
model_m23 <- fit_model(formula_m23)
model_m24 <- fit_model(formula_m24)
model_m25 <- fit_model(formula_m25)
model_m26 <- fit_model(formula_m26)
model_m27 <- fit_model(formula_m27)
model_m28 <- fit_model(formula_m28)

###----- Utility functions

lcpo <- function(m) sum(log(m$cpo$cpo), na.rm = TRUE)

yr_mean <- mean(wnv_data$year)
t1 <- 1999 - yr_mean
t2 <- 2024 - yr_mean

total_change <- function(model, quant = "mean") {
  cf <- model$summary.fixed
  b1 <- cf["year_c",      quant] * first_sd
  b2 <- if ("I(year_c^2)" %in% rownames(cf)) cf["I(year_c^2)", quant] * first_sd else 0
  b1 * (t2 - t1) + b2 * (t2^2 - t1^2)
}

bt <- function(model, label) {
  (model$summary.fixed[, c("mean", "0.025quant", "0.975quant")] * first_sd) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    mutate(model = label)
}

###----- Model comparison table

model_list <- list(
  # baseline
  "M1: linear year"                              = model_m1,
  "M2: quadratic year"                           = model_m2,
  "M3: linear year + ENSO linear"                = model_m3,
  "M17: linear year + ENSO RW2"                  = model_m17,
  "M4: quadratic year + ENSO RW2"                = model_m4,
  # county slope
  "M5: linear year x county slope"               = model_m5,
  "M6: quadratic year + county slope"            = model_m6,
  "M7: linear year x county + ENSO linear"       = model_m7,
  "M18: linear year x county + ENSO RW2"         = model_m18,
  "M8: quadratic year + county + ENSO RW2"       = model_m8,
  # elevation
  "M9: linear year x elevation"                  = model_m9,
  "M10: quadratic year x elevation"              = model_m10,
  "M11: linear year x elevation + ENSO linear"   = model_m11,
  "M19: linear year x elevation + ENSO RW2"      = model_m19,
  "M12: quadratic year x elev + ENSO RW2"        = model_m12,
  # latitude
  "M13: linear year x latitude"                  = model_m13,
  "M14: quadratic year x latitude"               = model_m14,
  "M15: linear year x latitude + ENSO linear"    = model_m15,
  "M20: linear year x latitude + ENSO RW2"       = model_m20,
  "M16: quadratic year x lat + ENSO RW2"         = model_m16,
  # el nino binary
  "M21: linear year + El Nino binary"            = model_m21,
  "M22: quadratic year + El Nino binary"         = model_m22,
  "M23: linear year x county + El Nino binary"   = model_m23,
  "M24: quadratic year + county + El Nino binary"= model_m24,
  "M25: linear year x elev + El Nino binary"     = model_m25,
  "M26: quadratic year x elev + El Nino binary"  = model_m26,
  "M27: linear year x lat + El Nino binary"      = model_m27,
  "M28: quadratic year x lat + El Nino binary"   = model_m28
)

model_comparison <- data.frame(
  model     = names(model_list),
  DIC       = sapply(model_list, function(m) m$dic$dic),
  pD        = sapply(model_list, function(m) m$dic$p.eff),
  WAIC      = sapply(model_list, function(m) m$waic$waic),
  LCPO      = sapply(model_list, lcpo),
  total_est = sapply(model_list, function(m) total_change(m)),
  total_lo  = sapply(model_list, function(m) total_change(m, "0.025quant")),
  total_hi  = sapply(model_list, function(m) total_change(m, "0.975quant"))
) %>%
  mutate(
    dDIC  = DIC  - min(DIC),
    dWAIC = WAIC - min(WAIC),
    dLCPO = LCPO - max(LCPO)
  ) %>%
  arrange(DIC)

model_comparison
write.csv(model_comparison, "results/tables/gridMET_first_day_model_comparison.csv", row.names = FALSE)

###----- Fixed effects (back-transformed to days)

fixed_effects <- bind_rows(lapply(names(model_list), function(nm) bt(model_list[[nm]], nm)))
fixed_effects
write.csv(fixed_effects, "data/gridMET_first_day_fixed_effects.csv", row.names = FALSE)


###----- Save models

saveRDS(model_m1,  "data/gridMET_first_day_m1_linear.rds")
saveRDS(model_m2,  "data/gridMET_first_day_m2_quad.rds")
saveRDS(model_m3,  "data/gridMET_first_day_m3_linear_enso.rds")
saveRDS(model_m4,  "data/gridMET_first_day_m4_quad_enso_rw2.rds")
saveRDS(model_m5,  "data/gridMET_first_day_m5_slope.rds")
saveRDS(model_m6,  "data/gridMET_first_day_m6_quad_slope.rds")
saveRDS(model_m7,  "data/gridMET_first_day_m7_slope_enso.rds")
saveRDS(model_m8,  "data/gridMET_first_day_m8_quad_slope_enso_rw2.rds")
saveRDS(model_m9,  "data/gridMET_first_day_m9_elev_linear.rds")
saveRDS(model_m10, "data/gridMET_first_day_m10_elev_quad.rds")
saveRDS(model_m11, "data/gridMET_first_day_m11_elev_linear_enso.rds")
saveRDS(model_m12, "data/gridMET_first_day_m12_elev_quad_enso_rw2.rds")
saveRDS(model_m13, "data/gridMET_first_day_m13_lat_linear.rds")
saveRDS(model_m14, "data/gridMET_first_day_m14_lat_quad.rds")
saveRDS(model_m15, "data/gridMET_first_day_m15_lat_linear_enso.rds")
saveRDS(model_m16, "data/gridMET_first_day_m16_lat_quad_enso_rw2.rds")
saveRDS(model_m17, "data/gridMET_first_day_m17_linear_enso_rw2.rds")
saveRDS(model_m18, "data/gridMET_first_day_m18_slope_linear_enso_rw2.rds")
saveRDS(model_m19, "data/gridMET_first_day_m19_elev_linear_enso_rw2.rds")
saveRDS(model_m20, "data/gridMET_first_day_m20_lat_linear_enso_rw2.rds")
saveRDS(model_m21, "data/gridMET_first_day_m21_linear_elnino_binary.rds")
saveRDS(model_m22, "data/gridMET_first_day_m22_quad_elnino_binary.rds")
saveRDS(model_m23, "data/gridMET_first_day_m23_slope_linear_elnino_binary.rds")
saveRDS(model_m24, "data/gridMET_first_day_m24_slope_quad_elnino_binary.rds")
saveRDS(model_m25, "data/gridMET_first_day_m25_elev_linear_elnino_binary.rds")
saveRDS(model_m26, "data/gridMET_first_day_m26_elev_quad_elnino_binary.rds")
saveRDS(model_m27, "data/gridMET_first_day_m27_lat_linear_elnino_binary.rds")
saveRDS(model_m28, "data/gridMET_first_day_m28_lat_quad_elnino_binary.rds")
