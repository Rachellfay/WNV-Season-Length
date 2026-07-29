library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(sf)
library(tigris)

# Run from the project root (paths below are relative to it)

# ---- Load pre-computed plot data ----

dat_inc  <- read.csv("data/plot_inputs/plot_data_inc.csv")
dat_prev <- read.csv("data/plot_inputs/plot_data_prev.csv")
smo_inc  <- read.csv("data/plot_inputs/plot_smooth_inc.csv")
smo_prev <- read.csv("data/plot_inputs/plot_smooth_prev.csv")
map_cty  <- read.csv("data/plot_inputs/plot_map_counties.csv")

loo_smo_inc   <- read.csv("data/plot_inputs/plot_loo_smooths_inc.csv")
loo_full_inc  <- read.csv("data/plot_inputs/plot_loo_full_inc.csv")
loo_dev_inc   <- read.csv("data/plot_inputs/plot_loo_deviance_inc.csv")
loo_smo_prev  <- read.csv("data/plot_inputs/plot_loo_smooths_prev.csv")
loo_full_prev <- read.csv("data/plot_inputs/plot_loo_full_prev.csv")
loo_dev_prev  <- read.csv("data/plot_inputs/plot_loo_deviance_prev.csv")

meta <- setNames(
  as.list(read.csv("data/plot_inputs/plot_meta.csv")$value),
  read.csv("data/plot_inputs/plot_meta.csv")$key)

inc_lbl      <- meta$inc_lbl
prev_lbl     <- meta$prev_lbl
inc_dev_expl <- as.numeric(meta$inc_dev_expl)
prev_dev_expl <- as.numeric(meta$prev_dev_expl)

elev_range <- range(map_cty$elev, na.rm = TRUE)

# ---- NY spatial data (public download — no private files needed) ----

ny_sf <- tigris::counties(state = "NY", cb = TRUE, class = "sf") %>%
  mutate(county = tolower(gsub("[^a-zA-Z]", "", NAME))) %>%
  left_join(map_cty, by = "county")

# ============================================================
# COLORS — edit these to change the figure aesthetics
# ============================================================

# GAM scatter panels
col_inc_line   <- "darkorchid4"   # incidence smooth line + ribbon
col_prev_line  <- "#b5320a"       # prevalence smooth line + ribbon

# Elevation point fill gradient
col_elev_lo    <- "#96f188"
col_elev_hi    <- "#4a1c0a"

# Inset maps
col_inc_map_lo  <- "#e8def1"
col_inc_map_hi  <- "#300163"
col_prev_map_lo <- "#fde8d0"
col_prev_map_hi <- "#b5320a"

# LOO spaghetti
col_loo_grey   <- "grey60"
col_loo_vline_inc  <- "darkorchid4"
col_loo_vline_prev <- "#b5320a"
# ============================================================

label_counties_inc  <- c("greene", "yates")
label_counties_prev <- c("rockland", "erie")

# ---- Inset map theme ----

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

# ---- Inset maps ----

p_inc_map <- ggplot(ny_sf) +
  geom_sf(aes(fill = incidence_per_10k), color = "white", linewidth = 0.25) +
  scale_fill_gradient(name = "Incidence\nper 10k",
                      low = col_inc_map_lo, high = col_inc_map_hi,
                      na.value = "grey80") +
  theme_void(base_size = 9) + inset_theme_sm

p_prev_map <- ggplot(ny_sf) +
  geom_sf(aes(fill = avg_prevalence), color = "white", linewidth = 0.25) +
  scale_fill_gradient(name = "Avg. WNV\nprevalence",
                      low = col_prev_map_lo, high = col_prev_map_hi,
                      na.value = "grey80") +
  theme_void(base_size = 9) + inset_theme_sm

# ---- GAM scatter: incidence ----

p_inc_gam <- ggplot() +
  geom_ribbon(data = smo_inc,
              aes(x = x, ymin = pmax(lo, 0), ymax = hi),
              fill = col_inc_line, alpha = 0.15) +
  geom_line(data  = smo_inc, aes(x = x, y = fit),
            color = col_inc_line, linewidth = 0.9) +
  geom_point(data = dat_inc,
             aes(x = season_length_p1999_2024_change,
                 y = partial_resid, fill = elev),
             size = 3.5, alpha = 0.6, shape = 21, color = "white", stroke = 0.3) +
  ggrepel::geom_text_repel(
    data = dat_inc[dat_inc$county %in% setdiff(label_counties_inc, "greene"), ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4, max.overlaps = 20,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  ggrepel::geom_text_repel(
    data = dat_inc[dat_inc$county == "greene", ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4,
    nudge_x = 2, nudge_y = -0.25,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  scale_fill_gradient(name = "Pop.-wtd.\nelev. (m)",
                      low = col_elev_lo, high = col_elev_hi,
                      limits = elev_range, na.value = "grey80") +
  annotate("text", x = Inf, y = Inf, label = inc_lbl,
           hjust = 1.05, vjust = 1.4, size = 5.5,
           color = "grey30", fontface = "bold.italic") +
  theme_classic(base_size = 16) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Change in season length (days, 1999-2024)",
       y = "WNV incidence per 10k")

# ---- GAM scatter: prevalence ----

p_prev_gam <- ggplot() +
  geom_ribbon(data = smo_prev,
              aes(x = x, ymin = pmax(lo, 0), ymax = hi),
              fill = col_prev_line, alpha = 0.15) +
  geom_line(data  = smo_prev, aes(x = x, y = fit),
            color = col_prev_line, linewidth = 0.9) +
  geom_point(data = dat_prev,
             aes(x = season_length_p1999_2024_change,
                 y = partial_resid, fill = elev),
             size = 3.5, alpha = 0.6, shape = 21, color = "white", stroke = 0.3) +
  ggrepel::geom_text_repel(
    data = dat_prev[dat_prev$county == "erie", ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  ggrepel::geom_text_repel(
    data = dat_prev[dat_prev$county == "rockland", ],
    aes(x = season_length_p1999_2024_change, y = partial_resid,
        label = tools::toTitleCase(county)),
    size = 4, color = "grey20", box.padding = 0.4,
    nudge_x = 2.5, nudge_y = -0.025,
    min.segment.length = 0, segment.color = "grey40", segment.size = 0.4) +
  scale_fill_gradient(name = "Pop.-wtd.\nelev. (m)",
                      low = col_elev_lo, high = col_elev_hi,
                      limits = elev_range, na.value = "grey80") +
  annotate("text", x = Inf, y = Inf, label = prev_lbl,
           hjust = 1.05, vjust = 1.4, size = 5.5,
           color = "grey30", fontface = "bold.italic") +
  theme_classic(base_size = 16) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Change in season length (days, 1999-2024)",
       y = "Avg. WNV prevalence (mosquito pools)")

# ---- Elevation color bar ----

elev_bar_dat <- data.frame(elev = seq(elev_range[1], elev_range[2], length.out = 500),
                           y    = 0)
p_elev_bar <- ggplot(elev_bar_dat, aes(x = elev, y = y, fill = elev)) +
  geom_raster() +
  scale_fill_gradient(low = col_elev_lo, high = col_elev_hi,
                      limits = elev_range, guide = "none") +
  scale_x_continuous(name   = "Pop.-wtd. elevation (m)",
                     breaks = pretty(elev_range, n = 4),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 16) +
  theme(axis.line.y  = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16, face = "italic"),
        plot.margin  = margin(8, 80, 5, 80))

# ---- Figure 1: main GAM panel ----

p_inc_panel <- (p_inc_gam + guides(fill = "none") + labs(tag = "A")) +
  inset_element(p_inc_map,
                left = 0, bottom = 0.45, right = 0.55, top = 1.0,
                align_to = "panel")

p_prev_panel <- (p_prev_gam + guides(fill = "none") + labs(tag = "B")) +
  inset_element(p_prev_map,
                left = 0, bottom = 0.45, right = 0.55, top = 1.0,
                align_to = "panel")

fig_gam_panel <- (p_inc_panel | p_prev_panel) / p_elev_bar +
  plot_layout(ncol = 1, heights = c(10, 0.4)) &
  theme(text     = element_text(size = 16),
        plot.tag = element_text(size = 18, face = "bold"))

ggsave("results/figures/gam_season_length_panel.png",
       fig_gam_panel, width = 16, height = 6, dpi = 600)
message("Saved: results/figures/gam_season_length_panel.png")

# ---- Figure 2: LOO sensitivity — incidence ----

p_loo <- ggplot() +
  geom_line(data = loo_smo_inc,
            aes(x = x, y = fit, group = excluded),
            color = col_loo_grey, linewidth = 0.35, alpha = 0.5) +
  geom_line(data = loo_full_inc,
            aes(x = x, y = fit),
            color = col_inc_line, linewidth = 1.2) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_classic(base_size = 13) +
  labs(x = "Change in season length (days, 1999-2024)",
       y = "WNV incidence per 10k (ref. pop.)")

p_loo_dev_inc <- ggplot(
    loo_dev_inc %>%
      mutate(county_lbl = tools::toTitleCase(excluded),
             county_lbl = forcats::fct_reorder(county_lbl, dev_expl)),
    aes(x = dev_expl, y = county_lbl)
  ) +
  geom_vline(xintercept = inc_dev_expl,
             color = col_loo_vline_inc, linewidth = 0.7, linetype = "dashed") +
  geom_point(size = 2, color = "grey40") +
  theme_classic(base_size = 11) +
  labs(x        = "Deviance explained (%)",
       y        = NULL,
       subtitle = paste0("Dashed = full model (", inc_dev_expl, "%)")) +
  theme(axis.text.y = element_text(size = 7))

fig_loo_inc <- p_loo + p_loo_dev_inc +
  plot_layout(widths = c(1.6, 1)) &
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))

ggsave("data/gam_loo_sensitivity.png",
       fig_loo_inc, width = 14, height = 6.5, dpi = 300)
message("Saved: data/gam_loo_sensitivity.png")

# ---- Figure 3: LOO sensitivity — prevalence ----

p_loo_prev <- ggplot() +
  geom_line(data = loo_smo_prev,
            aes(x = x, y = fit, group = excluded),
            color = col_loo_grey, linewidth = 0.35, alpha = 0.5) +
  geom_line(data = loo_full_prev,
            aes(x = x, y = fit),
            color = col_prev_line, linewidth = 1.2) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_classic(base_size = 13) +
  labs(x = "Change in season length (days, 1999-2024)",
       y = "Avg. WNV prevalence (mosquito pools)")

p_loo_dev_prev <- ggplot(
    loo_dev_prev %>%
      mutate(county_lbl = tools::toTitleCase(excluded),
             county_lbl = forcats::fct_reorder(county_lbl, dev_expl)),
    aes(x = dev_expl, y = county_lbl)
  ) +
  geom_vline(xintercept = prev_dev_expl,
             color = col_loo_vline_prev, linewidth = 0.7, linetype = "dashed") +
  geom_point(size = 2, color = "grey40") +
  theme_classic(base_size = 11) +
  labs(x        = "Deviance explained (%)",
       y        = NULL,
       subtitle = paste0("Dashed = full model (", prev_dev_expl, "%)")) +
  theme(axis.text.y = element_text(size = 7))

fig_loo_prev <- p_loo_prev + p_loo_dev_prev +
  plot_layout(widths = c(1.6, 1)) &
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))

ggsave("data/gam_loo_sensitivity_prevalence.png",
       fig_loo_prev, width = 14, height = 6.5, dpi = 300)
message("Saved: data/gam_loo_sensitivity_prevalence.png")

# ---- Figure 4: four-panel LOO ----

fig_loo_fourpanel <- (p_loo | p_loo_dev_inc) / (p_loo_prev | p_loo_dev_prev) +
  plot_layout(widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag      = element_text(size = 13, face = "bold"),
        plot.title    = element_text(size = 12),
        plot.subtitle = element_text(size = 9))

ggsave("results/figures/gam_loo_fourpanel.png",
       fig_loo_fourpanel, width = 14, height = 12, dpi = 300)
message("Saved: results/figures/gam_loo_fourpanel.png")
