# =============================================================================
# Aboveground Biomass vs. Grain Yield with HI Isolines
# Figure 3
# =============================================================================
# Author: Elvis F. Elli
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
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(scales)

# --- Data --------------------------------------------------------------------
df <- read.csv("data/biomass_yield.csv")
colnames(df) <- c("Biomass", "Yield")
df$Biomass <- suppressWarnings(as.numeric(df$Biomass))
df$Yield   <- suppressWarnings(as.numeric(df$Yield))
df <- df[complete.cases(df), ]

# --- HI isoline parameters ---------------------------------------------------
hi_values <- c(0.20, 0.30, 0.40, 0.50, 0.60)
hi_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")

x_max <- 20000
y_max <- 10000

hi_labels_df <- data.frame(HI = hi_values, Color = hi_colors) |>
  mutate(
    y_raw = HI * (x_max * 0.95),
    y_lbl = ifelse(y_raw > y_max * 0.95, y_max * 0.95, y_raw),
    x_lbl = ifelse(y_raw > y_max * 0.95, y_lbl / HI,   x_max * 0.95),
    hjust = ifelse(y_raw > y_max * 0.95, 0, 1),
    x_lbl = ifelse(HI == 0.6, 14000, x_lbl),
    label = paste0("HI = ", sprintf("%.2f", HI))
  )

# --- Plot --------------------------------------------------------------------
p <- ggplot(df, aes(x = Biomass, y = Yield)) +

  mapply(function(hi, col) {
    geom_segment(x = 0, y = 0,
                 xend = x_max, yend = hi * x_max,
                 color = col, linewidth = 0.65,
                 linetype = "longdash", alpha = 0.85,
                 inherit.aes = FALSE)
  }, hi_values, hi_colors, SIMPLIFY = FALSE) +

  geom_point(alpha = 0.55, size = 1.8, shape = 21,
             fill = "grey30", color = "white", stroke = 0.25) +

  geom_smooth(method = "lm", se = FALSE,
              color = "black", linewidth = 0.9, linetype = "solid") +

  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label), sep = "~~~")),
    formula = y ~ x,
    parse   = TRUE,
    size    = 3.5,
    label.x = 0.05,
    label.y = 0.97,
    color   = "black"
  ) +

  geom_text(
    data = hi_labels_df,
    aes(x = x_lbl, y = y_lbl, label = label, color = Color, hjust = hjust),
    vjust = -0.3, size = 3.0,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_color_identity() +

  scale_x_continuous(
    name   = expression(paste("Aboveground Biomass (kg ha"^{-1}, ")")),
    limits = c(0, x_max),
    breaks = seq(0, x_max, 5000),
    labels = label_comma(),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name   = expression(paste("Grain Yield (kg ha"^{-1}, ")")),
    limits = c(0, y_max),
    breaks = seq(0, y_max, 2000),
    labels = label_comma(),
    expand = c(0, 0)
  ) +

  theme_classic(base_size = 12, base_family = "sans") +
  theme(
    axis.title       = element_text(size = 11, face = "plain", color = "black"),
    axis.text        = element_text(size = 10, color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.line        = element_line(color = "black", linewidth = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin      = margin(10, 15, 10, 10, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- Save --------------------------------------------------------------------
ggsave("figures/fig3_biomass_yield.tiff",
       plot = p, width = 6.5, height = 5.5, dpi = 400, bg = "white")
ggsave("figures/fig3_biomass_yield.pdf",
       plot = p, width = 6.5, height = 5.5, device = cairo_pdf)

cat("Saved: figures/fig3_biomass_yield.tiff & .pdf\n")
