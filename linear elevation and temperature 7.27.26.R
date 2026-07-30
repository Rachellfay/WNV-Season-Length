# ========================================================
# SETUP
# ========================================================

setwd("~/desktop/rachelWNV/data")

library(tidyverse)
library(broom)
library(ggrepel)
library(showtext)
library(ragg)
library(sysfonts)
library(patchwork)

# ========================================================
# FONT SETTINGS (MATCH GAM FIGURE)
# ========================================================

dir.create("~/fonts", showWarnings = FALSE)

download.file(
  "https://raw.githubusercontent.com/senotrusov/dejavu-fonts-ttf/master/ttf/DejaVuSans.ttf",
  destfile = path.expand("~/fonts/DejaVuSans.ttf"),
  mode = "wb"
)
download.file(
  "https://raw.githubusercontent.com/senotrusov/dejavu-fonts-ttf/master/ttf/DejaVuSans-Bold.ttf",
  destfile = path.expand("~/fonts/DejaVuSans-Bold.ttf"),
  mode = "wb"
)

# Sanity check — should each be several hundred KB, not a few bytes
file.info(path.expand("~/fonts/DejaVuSans.ttf"))$size
file.info(path.expand("~/fonts/DejaVuSans-Bold.ttf"))$size

font_add(
  family  = "DejaVu Sans",
  regular = path.expand("~/fonts/DejaVuSans.ttf"),
  bold    = path.expand("~/fonts/DejaVuSans-Bold.ttf")
)
showtext_auto()

FONT_FAMILY <- "DejaVu Sans"

BASE_SIZE <- 18
AXIS_TITLE_SIZE <- 16
AXIS_TEXT_SIZE <- 12

ANNOTATE_SIZE <- 4.5
REPEL_SIZE <- 4


base_theme <- theme_classic(
  base_size = BASE_SIZE,
  base_family = FONT_FAMILY
) +
  theme(
    text = element_text(
      family = FONT_FAMILY,
      size = BASE_SIZE
    ),
    
    axis.title = element_text(
      family = FONT_FAMILY,
      size = AXIS_TITLE_SIZE,
      face = "bold"
    ),
    
    axis.text = element_text(
      family = FONT_FAMILY,
      size = AXIS_TEXT_SIZE
    ),
    
    plot.margin = margin(15,20,15,15)
  )


showtext_opts(dpi = 600)


# ========================================================
# LOAD SEASON LENGTH DATA
# ========================================================

season <- read.csv(
  "county_season_length_change_1999_2024.csv"
)

season$county <- str_to_title(season$county)


season <- season %>%
  mutate(
    elev = as.numeric(elev),
    season_length_change_1999_2024 =
      as.numeric(season_length_change_1999_2024)
  )



# ========================================================
# LOAD TEMPERATURE DATA
# ========================================================

temp <- read.csv(
  "mean temp and elevation.csv"
)

temp$county <- str_to_title(temp$county)


temp <- temp %>%
  mutate(
    mean_elev_m = as.numeric(mean_elev_m),
    Avg_Temperature_1999_2024 =
      as.numeric(Avg_Temperature_1999_2024)
  )



# ========================================================
# MERGE TEMPERATURE INTO SEASON DATA
# ========================================================

dat <- season %>%
  left_join(
    temp %>%
      select(
        county,
        Avg_Temperature_1999_2024
      ),
    by = "county"
  ) %>%
  drop_na()



# ========================================================
# 1. ELEVATION VS SEASON LENGTH
# ========================================================

mod_elev <- lm(
  season_length_change_1999_2024 ~ elev,
  data = dat
)


r2 <- summary(mod_elev)$r.squared

coef_mod <- coef(mod_elev)

label_text <- paste0(
  "R² = ",
  round(r2,3),
  "\nP = ",
  format.pval(
    tidy(mod_elev)$p.value[2],
    digits=2
  )
)



dat$resid <- resid(mod_elev)


label_df <- dat %>%
  mutate(abs_resid = abs(resid)) %>%
  arrange(desc(abs_resid)) %>%
  slice(1:10)



p_elev_season <- ggplot(
  dat,
  aes(
    elev,
    season_length_change_1999_2024
  )
)+
  
  geom_point(
    size=3,
    alpha=0.8
  )+
  
  geom_smooth(
    method="lm",
    se=TRUE,
    linewidth=1.1
  )+
  
  geom_text_repel(
    data=label_df,
    aes(label=county),
    size=REPEL_SIZE,
    family=FONT_FAMILY,
    box.padding=.6,
    point.padding=.4,
    segment.color="grey50",
    max.overlaps=Inf
  )+
  
  annotate(
    "text",
    x=Inf,
    y=Inf,
    label=label_text,
    hjust=1.1,
    vjust=1.2,
    size=ANNOTATE_SIZE,
    family=FONT_FAMILY
  )+
  
  base_theme+
  
  labs(
    x="Mean county elevation (m)",
    y="Season length change (days, 1999–2024)"
  )


ggsave(
  "season_length_change_vs_elevation.png",
  p_elev_season,
  width=7,
  height=5.5,
  dpi=600,
  device=ragg::agg_png
)



# ========================================================
# TEMPERATURE VS ELEVATION
# ========================================================

mod_temp_elev <- lm(
  Avg_Temperature_1999_2024 ~ elev,
  data = dat
)

r2 <- summary(mod_temp_elev)$r.squared

coef_mod <- coef(mod_temp_elev)

intercept <- coef_mod[1]
slope <- coef_mod[2]

p_value <- tidy(mod_temp_elev) %>%
  filter(term == "elev") %>%
  pull(p.value)


label_text <- paste0(
  "Temp = ",
  round(intercept,2),
  " + ",
  round(slope,4),
  " × Elev\n",
  "R² = ",
  round(r2,3),
  "\nP = ",
  format.pval(p_value, digits=2)
)


p_temp_elev <- ggplot(
  dat,
  aes(
    x = elev,
    y = Avg_Temperature_1999_2024
  )
)+
  
  geom_point(
    size=3,
    alpha=0.8
  )+
  
  geom_smooth(
    method="lm",
    se=TRUE,
    linewidth=1.1
  )+
  
  annotate(
    "text",
    x=Inf,
    y=Inf,
    label=label_text,
    hjust=1.05,
    vjust=1.2,
    size=ANNOTATE_SIZE,
    family=FONT_FAMILY
  )+
  
  base_theme+
  
  labs(
    x="Mean county elevation (m)",
    y="Mean temperature (°C, 1999–2024)"
  )


ggsave(
  "temperature_vs_elevation.png",
  p_temp_elev,
  width=7,
  height=5.5,
  dpi=600,
  device=ragg::agg_png
)


# ========================================================
# TEMPERATURE VS SEASON LENGTH CHANGE
# ========================================================


mod_temp_season <- lm(
  season_length_change_1999_2024 ~ Avg_Temperature_1999_2024,
  data=dat
)


r2 <- summary(mod_temp_season)$r.squared


coef_mod <- coef(mod_temp_season)

intercept <- coef_mod[1]
slope <- coef_mod[2]


p_value <- tidy(mod_temp_season) %>%
  filter(term=="Avg_Temperature_1999_2024") %>%
  pull(p.value)


label_text <- paste0(
  "Season length = ",
  round(intercept,2),
  " + ",
  round(slope,2),
  " × Temp\n",
  "R² = ",
  round(r2,3),
  "\nP = ",
  format.pval(p_value,digits=2)
)



p_temp_season <- ggplot(
  dat,
  aes(
    x=Avg_Temperature_1999_2024,
    y=season_length_change_1999_2024
  )
)+
  
  geom_point(
    size=3,
    alpha=0.8
  )+
  
  geom_smooth(
    method="lm",
    se=TRUE,
    linewidth=1.1
  )+
  
  annotate(
    "text",
    x = min(dat$Avg_Temperature_1999_2024),
    y = max(dat$season_length_change_1999_2024) + 5,
    label = label_text,
    hjust = 0,
    vjust = 1,
    size = ANNOTATE_SIZE,
    family = FONT_FAMILY
  )+
  
  base_theme+
  
  labs(
    x="Mean county temperature (°C, 1999–2024)",
    y="Season length change (days, 1999–2024)"
  )



ggsave(
  "season_length_change_vs_temperature.png",
  p_temp_season,
  width=7,
  height=5.5,
  dpi=600,
  device=ragg::agg_png
)

# ========================================================
# MODEL OUTPUTS
# ========================================================

summary(mod_elev)
summary(mod_temp_elev)
summary(mod_temp_season)


# ========================================================
# COMBINED TWO-PANEL FIGURE
# A = elevation vs temperature
# B = temperature vs season length
# ========================================================

p_combined <- p_temp_elev + p_temp_season +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(
      family = FONT_FAMILY,
      size = BASE_SIZE,
      face = "bold",
      margin = margin(r = 15)
    ),
    plot.tag.position = c(0, 1)
  )


ggsave(
  "season_length_combined_panels.png",
  p_combined,
  width=13,
  height=5.5,
  dpi=600,
  device=ragg::agg_png
)