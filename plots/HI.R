# =============================================================================
# Harvest Index vs. Seed Yield with Biomass Isolines
# =============================================================================

# --- Set working directory to script location --------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  }
}
cat("Working directory:", getwd(), "\n")

# --- Packages -----------------------------------------------------------------
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(scales)

# --- Data ---------------------------------------------------------------------
df <- read.csv("biomass_yield.csv")
colnames(df) <- c("Biomass", "Yield")
df$Biomass <- suppressWarnings(as.numeric(df$Biomass))
df$Yield   <- suppressWarnings(as.numeric(df$Yield))
df <- df[complete.cases(df), ]

# Compute HI and filter implausible values
df$HI <- df$Yield / df$Biomass
df    <- df[df$HI <= 0.70, ]

# --- Biomass isoline parameters ----------------------------------------------
bm_values <- c(2000, 4000, 6000, 8000, 12000, 16000)
bm_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928")

x_max <- 0.70
y_max <- 9000

# Build segment endpoints and label positions (clipped to plot area)
bm_df <- data.frame(BM = bm_values, Color = bm_colors) |>
  mutate(
    x_start = 0,
    y_start = 0,
    # Where does line exit? Right edge or top edge?
    x_end   = ifelse(BM * x_max > y_max, y_max / BM, x_max),
    y_end   = ifelse(BM * x_max > y_max, y_max,       BM * x_max),
    # Label position
    x_lbl_raw = x_end * 0.97,
    y_lbl_raw = BM * x_lbl_raw,
    exits_top = y_lbl_raw > y_max * 0.96,
    y_lbl  = ifelse(exits_top, y_max * 0.975, y_lbl_raw),
    x_lbl  = ifelse(exits_top, y_lbl / BM,    x_lbl_raw),
    y_lbl = ifelse(BM=="16000",8500,y_lbl),
    hjust  = ifelse(exits_top, 0, 1),
    label  = paste0(BM / 1000, " t ha\u207b\u00b9")
  )

# --- Plot --------------------------------------------------------------------
p <- ggplot(df, aes(x = HI, y = Yield)) +
  
  # Biomass isolines
  mapply(function(xs, ys, xe, ye, col) {
    geom_segment(x = xs, y = ys, xend = xe, yend = ye,
                 color = col, linewidth = 0.65,
                 linetype = "longdash", alpha = 0.85,
                 inherit.aes = FALSE)
  }, bm_df$x_start, bm_df$y_start, bm_df$x_end, bm_df$y_end, bm_df$Color,
  SIMPLIFY = FALSE) +
  
  # Scatter points
  geom_point(alpha = 0.55, size = 1.8, shape = 21,
             fill = "grey30", color = "white", stroke = 0.25) +
  
  # Regression line — no SE
  geom_smooth(method = "lm", se = FALSE,
              color = "black", linewidth = 0.9, linetype = "solid") +
  
  # Equation + R²
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
  
  # Isoline labels
  geom_text(
    data = bm_df,
    aes(x = x_lbl, y = y_lbl, label = label, color = Color, hjust = hjust),
    vjust = -0.3, size = 2.8,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_color_identity() +
  
  # Scales
  scale_x_continuous(
    name   = "Harvest Index (HI)",
    limits = c(0, x_max),
    breaks = seq(0, x_max, 0.10),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name   = expression(paste("Seed Yield (kg ha"^{-1}, ")")),
    limits = c(0, y_max),
    breaks = seq(0, y_max, 2000),
    labels = label_comma(),
    expand = c(0, 0)
  ) +
  
  # Theme — plain, no bold
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
ggsave("HI_yield.tiff",
       plot = p, width = 6.5, height = 5.5, dpi = 400, bg = "white")
ggsave("HI_yield.pdf",
       plot = p, width = 6.5, height = 5.5, device = cairo_pdf)

cat("Saved: HI_yield.png & HI_yield.pdf\n")

