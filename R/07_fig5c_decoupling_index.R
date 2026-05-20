# Figure 5c: decoupling and reallocation indices
# Calculates the decoupling index and tests its association with biomass reallocation.

suppressPackageStartupMessages({
# Reproducible project paths and packages
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

repo_dir <- normalizePath(file.path(get_script_dir(), ".."), winslash = "/", mustWork = FALSE)
path_from_env <- function(env_var, default_path) {
  normalizePath(Sys.getenv(env_var, unset = default_path), winslash = "/", mustWork = FALSE)
}

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      "\nInstall them with: install.packages(c('", paste(missing, collapse = "', '"), "'))",
      call. = FALSE
    )
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

  pkgs <- c("readr", "dplyr", "ggplot2", "patchwork", "multcompView")
  ensure_packages(pkgs)
})

# Settings
base_dir <- path_from_env("FIG5C_DATA_DIR", file.path(repo_dir, "data", "fig5", "panel_c"))
out_dir  <- path_from_env("FIG5C_OUT_DIR",  file.path(repo_dir, "outputs", "fig5", "panel_c"))
if (!dir.exists(base_dir)) stop("Input directory does not exist: ", base_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pt_file <- file.path(base_dir, "plant trait.csv")
fg_file <- file.path(base_dir, "fig3.csv")

if (!file.exists(pt_file)) stop("File not found: ", pt_file)
if (!file.exists(fg_file)) stop("File not found: ", fg_file)

# Output names
out_panel_pdf  <- file.path(out_dir, "fig5c_decoupling_panel.pdf")
out_panel_png  <- file.path(out_dir, "fig5c_decoupling_panel.png")
out_left_png   <- file.path(out_dir, "fig5c_decoupling_by_treatment.png")
out_right_png  <- file.path(out_dir, "fig5c_reallocation_scatter.png")
out_data_csv   <- file.path(out_dir, "fig5c_derived_data.csv")
out_model_csv  <- file.path(out_dir, "fig5c_model_summary.csv")
out_legend_txt <- file.path(out_dir, "fig5c_legend_text.txt")

# Read and merge
pt <- read_csv(pt_file, show_col_types = FALSE) %>%
  group_by(Treatment) %>%
  mutate(Rep = row_number()) %>%
  ungroup()

fg <- read_csv(fg_file, show_col_types = FALSE) %>%
  group_by(Treatment) %>%
  mutate(Rep = row_number()) %>%
  ungroup()

dat <- pt %>%
  left_join(fg %>% select(Treatment, Rep, SACd), by = c("Treatment", "Rep")) %>%
  mutate(
    TotalAboveCdExtraction = BECd + SECd,
    DecouplingIndex = log(TotalAboveCdExtraction / SACd)
  )

# Reallocation index
# Higher values indicate stronger reallocation away from harvestable shoots
x <- dat %>%
  transmute(
    berry_alloc = `Berry/Total aboveground`,
    root_alloc  = `Root / Total Aboveground`,
    neg_BTF     = -BTF,
    neg_STF     = -STF
  )

pca <- prcomp(x, center = TRUE, scale. = TRUE)
dat$ReallocationIndex <- pca$x[, 1]

if (cor(dat$ReallocationIndex, dat$`Root / Total Aboveground`) < 0) {
  dat$ReallocationIndex <- -dat$ReallocationIndex
}

# Factor order and colors
ord <- c("CK", "PE0.5", "PVC5")
dat$Treatment <- factor(dat$Treatment, levels = ord)

pal <- c(
  "CK"    = "#4D4D4D",
  "PE0.5" = "#2E8B57",
  "PVC5"  = "#4F81BD"
)

fill_pal <- c(
  "CK"    = "#D9D9D9",
  "PE0.5" = "#BFDDBF",
  "PVC5"  = "#C7D8EE"
)

shape_pal <- c(
  "CK"    = 16,
  "PE0.5" = 17,
  "PVC5"  = 15
)

# Models
fit_treat <- lm(DecouplingIndex ~ Treatment, data = dat)
fit_raw   <- lm(DecouplingIndex ~ ReallocationIndex, data = dat)
fit_adj   <- lm(DecouplingIndex ~ ReallocationIndex + SDW + Treatment, data = dat)

coef_raw <- coef(summary(fit_raw))["ReallocationIndex", ]
coef_adj_realloc <- coef(summary(fit_adj))["ReallocationIndex", ]
coef_adj_sdw <- coef(summary(fit_adj))["SDW", ]

# Tukey letters
aov_left <- aov(DecouplingIndex ~ Treatment, data = dat)
tk <- TukeyHSD(aov_left)
ltr <- multcompLetters4(aov_left, tk)

letter_df <- data.frame(
  Treatment = names(ltr$Treatment$Letters),
  letter = ltr$Treatment$Letters,
  stringsAsFactors = FALSE
) %>%
  mutate(Treatment = factor(Treatment, levels = ord))

means <- dat %>%
  group_by(Treatment) %>%
  summarise(y = max(DecouplingIndex, na.rm = TRUE) + 0.045, .groups = "drop") %>%
  left_join(letter_df, by = "Treatment")

# Formatting helpers
fmt_p <- function(p) {
  if (is.na(p)) return("P = NA")
  if (p < 0.001) return("P < 0.001")
  sprintf("P = %.3f", p)
}

fmt_r2 <- function(x) sprintf("R² = %.3f", x)
fmt_b <- function(x) sprintf("β = %.3f", x)

treat_lab <- paste(fmt_r2(summary(fit_treat)$r.squared), fmt_p(anova(fit_treat)$`Pr(>F)`[1]), sep = ", ")

unadj_lab <- paste(
  "Unadjusted model (solid line)",
  paste(fmt_b(coef_raw["Estimate"]), fmt_p(coef_raw["Pr(>|t|)"]), sep = ", "),
  sep = "\n"
)

adj_lab <- paste(
  "Adjusted model",
  paste("βrealloc =", sprintf("%.3f", coef_adj_realloc["Estimate"]), ",", fmt_p(coef_adj_realloc["Pr(>|t|)"])),
  paste("βshoot =", sprintf("%.3f", coef_adj_sdw["Estimate"]), ",", fmt_p(coef_adj_sdw["Pr(>|t|)"])),
  sep = "\n"
)

# Save derived data
write_csv(dat, out_data_csv)

model_tbl <- tibble(
  model = c("Treatment model", "Unadjusted model: realloc only", "Adjusted model: realloc", "Adjusted model: SDW"),
  estimate = c(
    NA_real_,
    coef_raw["Estimate"],
    coef_adj_realloc["Estimate"],
    coef_adj_sdw["Estimate"]
  ),
  p = c(
    anova(fit_treat)$`Pr(>F)`[1],
    coef_raw["Pr(>|t|)"],
    coef_adj_realloc["Pr(>|t|)"],
    coef_adj_sdw["Pr(>|t|)"]
  ),
  r2 = c(
    summary(fit_treat)$r.squared,
    summary(fit_raw)$r.squared,
    summary(fit_adj)$r.squared,
    summary(fit_adj)$r.squared
  ),
  pca_variance = c(
    NA_real_,
    NA_real_,
    summary(pca)$importance[2, 1],
    summary(pca)$importance[2, 1]
  )
)

write_csv(model_tbl, out_model_csv)

# Left panel
ymin_left <- min(dat$DecouplingIndex, na.rm = TRUE) - 0.035
ymax_left <- max(dat$DecouplingIndex, na.rm = TRUE) + 0.12

p_left <- ggplot(dat, aes(Treatment, DecouplingIndex)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.50, alpha = 0.68, outlier.shape = NA,
               colour = "black", linewidth = 0.45) +
  geom_jitter(aes(colour = Treatment),
              width = 0.065, height = 0, size = 2.15, alpha = 0.95) +
  geom_text(data = means, aes(Treatment, y, label = letter),
            inherit.aes = FALSE, size = 4.4) +
  annotate("text",
           x = 1.05,
           y = ymax_left - 0.015,
           label = treat_lab,
           hjust = 0, vjust = 1, size = 3.45) +
  scale_fill_manual(values = fill_pal) +
  scale_colour_manual(values = pal) +
  coord_cartesian(ylim = c(ymin_left, ymax_left), clip = "off") +
  labs(x = NULL, y = "Decoupling index") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 11.5),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(linewidth = 0.45),
    axis.ticks = element_line(linewidth = 0.45),
    plot.margin = margin(8, 8, 8, 8)
  )

# Right panel
xrange <- range(dat$ReallocationIndex, na.rm = TRUE)
yrange <- range(dat$DecouplingIndex, na.rm = TRUE)

anno_x <- xrange[1] + 0.08 * diff(xrange)
anno_y_unadj <- yrange[1] + 0.22 * diff(yrange)
anno_y_adj   <- yrange[1] + 0.04 * diff(yrange)

p_right <- ggplot(dat, aes(ReallocationIndex, DecouplingIndex)) +
  geom_point(aes(colour = Treatment, shape = Treatment), size = 3.0, alpha = 0.95) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.72) +
  annotate(
    "label",
    x = anno_x,
    y = anno_y_unadj,
    label = unadj_lab,
    hjust = 0, vjust = 0,
    size = 3.20, label.size = 0.25,
    fill = "white", alpha = 0.96
  ) +
  annotate(
    "label",
    x = anno_x,
    y = anno_y_adj,
    label = adj_lab,
    hjust = 0, vjust = 0,
    size = 3.20, label.size = 0.25,
    fill = "white", alpha = 0.96
  ) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = shape_pal) +
  labs(x = "Reallocation index", y = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = c(0.84, 0.82),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(linewidth = 0.45),
    axis.ticks = element_line(linewidth = 0.45),
    plot.margin = margin(8, 8, 8, 2)
  )

# Combine and save
panel_d <- p_left + p_right + plot_layout(widths = c(1.0, 1.18))

ggsave(out_panel_pdf, panel_d, width = 10.2, height = 4.45, bg = "white")
ggsave(out_panel_png, panel_d, width = 10.2, height = 4.45, dpi = 300, bg = "white")
ggsave(out_left_png, p_left, width = 4.55, height = 4.25, dpi = 300, bg = "white")
ggsave(out_right_png, p_right, width = 5.25, height = 4.25, dpi = 300, bg = "white")

# Legend text
legend_text <- paste0(
  "Figure 5c. Biomass reallocation tracks the decoupling between soil available Cd and harvestable Cd extraction. ",
  "Left, decoupling index, defined as ln(total aboveground Cd extraction / soil available Cd), across CK, PE0.5 and PVC5 treatments. ",
  "Lower values indicate weaker conversion of soil available Cd into harvestable aboveground Cd extraction. Different letters indicate significant differences among treatments based on Tukey's HSD test. ",
  "Right, relationship between the decoupling index and the reallocation index derived from berry allocation, root allocation, and the inverse of berry/shoot and shoot/root translocation factors. ",
  "Higher reallocation index values indicate stronger redistribution away from harvestable shoots. ",
  "The solid line shows the unadjusted regression, whereas the lower annotation box reports the adjusted model controlling for shoot dry weight and treatment."
)
writeLines(legend_text, out_legend_txt)

message("Done. Files written to: ", out_dir)
