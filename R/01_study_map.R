# =============================================================================
# Study Location Map — Soybean Harvest Index Meta-Analysis
# Figure 2: World map of included studies
# =============================================================================
# Author: Elvis F. Elli
# Created: 2025-11-04
# =============================================================================

# --- Set working directory to project root -----------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  root     <- dirname(dirname(normalizePath(sub("--file=", "", file_arg[1]))))
}
setwd(root)
cat("Project root:", getwd(), "\n")

# --- Packages ----------------------------------------------------------------
library(tidyverse)
library(maps)
library(ggrepel)

# --- Publication theme -------------------------------------------------------
pub_theme <- theme(
  axis.ticks.length    = unit(0.2, "cm"),
  axis.text            = element_text(size = 10, colour = "black"),
  axis.title           = element_text(size = 10, colour = "black"),
  plot.background      = element_blank(),
  panel.background     = element_rect(fill = "grey90"),
  panel.grid.major     = element_line(colour = "white"),
  panel.grid.minor     = element_blank(),
  panel.border         = element_rect(colour = "black", fill = NA, linewidth = 0.5),
  plot.margin          = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  legend.position      = "right",
  legend.background    = element_rect(fill = NA),
  legend.text          = element_text(size = 10),
  legend.key           = element_rect(fill = "transparent"),
  strip.background     = element_rect(colour = "black", linewidth = 0.5),
  strip.text           = element_text(size = 10)
)

# --- Data --------------------------------------------------------------------
soybean_hi_data <- read.csv("data/studies_metadata.csv")

world_map <- map_data("world")

# --- Plot --------------------------------------------------------------------
p <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "black", linewidth = 0.3) +
  geom_point(data = soybean_hi_data,
             aes(x = Long, y = Lat, fill = Country),
             size = 4, colour = "black", shape = 21, alpha = 0.8) +
  scale_fill_viridis_d(name = "Country") +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  pub_theme +
  theme(legend.position = "none")

# --- Save --------------------------------------------------------------------
ggsave("figures/fig2_study_map.tiff",
       plot = p, width = 25, height = 15, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")

cat("Saved: figures/fig2_study_map.tiff\n")

# --- Summary statistics ------------------------------------------------------
country_summary <- soybean_hi_data %>%
  group_by(Country) %>%
  summarise(
    n_studies  = n(),
    year_range = paste(min(Paper_Year), "-", max(Paper_Year)),
    .groups    = "drop"
  ) %>%
  arrange(desc(n_studies))

cat("\nStudies by country:\n")
print(country_summary)
