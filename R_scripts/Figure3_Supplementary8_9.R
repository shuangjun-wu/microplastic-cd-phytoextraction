# Figure 3 and supplementary soil-trait scripts
# Generates the Fig. 3 soil-variable heatmap and supplementary grouped dot plots.

required_pkgs <- c("tidyverse", "car", "rstatix", "multcompView", "ggh4x")

installed <- rownames(installed.packages())
for (pkg in required_pkgs) {
  if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
}

library(tidyverse)
library(car)
library(rstatix)
library(multcompView)
library(ggh4x)
library(grid)

# -----------------------------
# 1. Paths
# -----------------------------
root_dir <- getwd()
file_in <- file.path(root_dir, "Source_Data_Fig3.csv")
if (!file.exists(file_in)) file_in <- file.path(root_dir, "fig3.csv")

# Create the output folder if it does not already exist.
out_dir <- file.path(root_dir, "Fig3_final_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Output files
heatmap_pdf   <- file.path(out_dir, "Fig3a_main_heatmap_7vars.pdf")
heatmap_tiff  <- file.path(out_dir, "Fig3a_main_heatmap_7vars.tiff")
supp_pdf      <- file.path(out_dir, "Supplementary_soil_traits_remaining_soil_traits.pdf")
supp_tiff     <- file.path(out_dir, "Supplementary_soil_traits_remaining_soil_traits.tiff")

heatmap_stat    <- file.path(out_dir, "Fig3a_heatmap_pairwise_vs_CK_stats.csv")
heatmap_mat     <- file.path(out_dir, "Fig3a_heatmap_plot_matrix.csv")
supp_sum_csv    <- file.path(out_dir, "Supplementary_soil_traits_summary_letters.csv")
supp_post_csv   <- file.path(out_dir, "Supplementary_soil_traits_posthoc_details.csv")
supp_method_csv <- file.path(out_dir, "Supplementary_soil_traits_test_methods.csv")

# -----------------------------
# 2. Font
# -----------------------------
if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}

# -----------------------------
# 3. Data
# -----------------------------
dat <- read_csv(file_in, show_col_types = FALSE) %>%
  mutate(Treatment = factor(Treatment, levels = c("CK", "PE0.5", "PVC5")))

# Main figure variables: internal name -> original column
main_var_map <- c(
  "Available Cd"                  = "SACd",
  "pH"                            = "pH",
  "Dissolved organic carbon"      = "SDOC",
  "Total carbon"                  = "STC",
  "Urease"                        = "S-UE",
  "Alkaline hydrolyzable nitrogen"= "SAHN",
  "Acid phosphatase"              = "S-ACP"
)

# Main figure display labels
main_display_map <- c(
  "Available Cd"                   = "Available Cd",
  "pH"                             = "pH",
  "Dissolved organic carbon"       = "Dissolved organic\ncarbon",
  "Total carbon"                   = "Total carbon",
  "Urease"                         = "Urease",
  "Alkaline hydrolyzable nitrogen" = "Alkaline hydrolyzable\nnitrogen",
  "Acid phosphatase"               = "Acid phosphatase"
)

# Supplementary figure: internal name -> original column
supp_var_map <- c(
  "Beta-glucosidase"     = "S-β-GC",
  "Available potassium"  = "SAK",
  "Available phosphorus" = "SAP",
  "Total potassium"      = "STK",
  "Total nitrogen"       = "STN",
  "Total phosphorus"     = "STP",
  "Total Cd"             = "SCd"
)

# Supplementary facet labels include variable names and units.
supp_display_map <- c(
  "Beta-glucosidase"     = "Beta-glucosidase\n(μgl/h/g)",
  "Available potassium"  = "Available potassium\n(mg/kg)",
  "Available phosphorus" = "Available phosphorus\n(mg/kg)",
  "Total potassium"      = "Total potassium\n(g/kg)",
  "Total nitrogen"       = "Total nitrogen\n(g/kg)",
  "Total phosphorus"     = "Total phosphorus\n(mg/kg)",
  "Total Cd"             = "Total Cd\n(mg/kg)"
)

# -----------------------------
# 4. Helper functions
# -----------------------------
hedges_g <- function(x_treat, x_ck) {
  x_treat <- stats::na.omit(x_treat)
  x_ck    <- stats::na.omit(x_ck)

  n1 <- length(x_treat)
  n0 <- length(x_ck)

  s1 <- stats::var(x_treat)
  s0 <- stats::var(x_ck)

  s_pooled <- sqrt(((n1 - 1) * s1 + (n0 - 1) * s0) / (n1 + n0 - 2))
  d <- (mean(x_treat) - mean(x_ck)) / s_pooled

  J <- 1 - 3 / (4 * (n1 + n0) - 9)
  g <- J * d
  return(g)
}

pairwise_vs_ck_test <- function(data, response, group = "Treatment") {
  levs <- levels(data[[group]])
  ck_dat <- data %>% filter(.data[[group]] == "CK") %>% pull(.data[[response]])

  results <- map_dfr(setdiff(levs, "CK"), function(trt) {
    trt_dat <- data %>% filter(.data[[group]] == trt) %>% pull(.data[[response]])

    x <- c(ck_dat, trt_dat)
    g <- factor(c(rep("CK", length(ck_dat)), rep(trt, length(trt_dat))), levels = c("CK", trt))
    tmp <- tibble(value = x, group = g)

    shapiro_ck  <- if (length(ck_dat) >= 3) shapiro.test(ck_dat)$p.value else NA_real_
    shapiro_trt <- if (length(trt_dat) >= 3) shapiro.test(trt_dat)$p.value else NA_real_
    normal_ok   <- !is.na(shapiro_ck) && !is.na(shapiro_trt) &&
      shapiro_ck >= 0.05 && shapiro_trt >= 0.05

    levene_p <- tryCatch({
      as.numeric(car::leveneTest(value ~ group, data = tmp)[1, "Pr(>F)"])
    }, error = function(e) NA_real_)

    if (normal_ok && !is.na(levene_p) && levene_p >= 0.05) {
      p_value <- t.test(value ~ group, data = tmp, var.equal = TRUE)$p.value
      method  <- "Student_t_test"
    } else if (normal_ok) {
      p_value <- t.test(value ~ group, data = tmp, var.equal = FALSE)$p.value
      method  <- "Welch_t_test"
    } else {
      p_value <- wilcox.test(value ~ group, data = tmp, exact = FALSE)$p.value
      method  <- "Wilcoxon_rank_sum"
    }

    tibble(
      treatment = trt,
      method = method,
      shapiro_ck = shapiro_ck,
      shapiro_trt = shapiro_trt,
      levene_p = levene_p,
      p_value = p_value
    )
  })

  results
}

make_letters_from_p <- function(p_df, group_levels = c("CK", "PE0.5", "PVC5")) {
  if (is.null(p_df) || nrow(p_df) == 0) {
    return(tibble(
      Treatment = factor(group_levels, levels = group_levels),
      letters = rep("a", length(group_levels))
    ))
  }

  comps <- paste(p_df$group1, p_df$group2, sep = "-")
  pvals <- p_df$p_adj
  names(pvals) <- comps

  letters <- multcompView::multcompLetters(pvals, threshold = 0.05)$Letters

  tibble(
    Treatment = factor(names(letters), levels = group_levels),
    letters = unname(letters)
  ) %>%
    right_join(tibble(Treatment = factor(group_levels, levels = group_levels)), by = "Treatment") %>%
    mutate(letters = ifelse(is.na(letters), "a", letters))
}

run_group_comparison <- function(data, response, group = "Treatment") {
  df <- data %>%
    select(all_of(group), all_of(response)) %>%
    rename(group_col = all_of(group), value = all_of(response)) %>%
    filter(!is.na(value)) %>%
    mutate(group_col = factor(group_col, levels = c("CK", "PE0.5", "PVC5")))

  lm_fit <- lm(value ~ group_col, data = df)
  shapiro_p <- tryCatch(shapiro.test(residuals(lm_fit))$p.value, error = function(e) NA_real_)
  levene_p  <- tryCatch(as.numeric(car::leveneTest(value ~ group_col, data = df)[1, "Pr(>F)"]), error = function(e) NA_real_)

  if (!is.na(shapiro_p) && shapiro_p >= 0.05 && !is.na(levene_p) && levene_p >= 0.05) {
    method <- "One-way ANOVA + Tukey HSD"
    fit <- aov(value ~ group_col, data = df)
    overall_p <- summary(fit)[[1]][["Pr(>F)"]][1]

    if (overall_p < 0.05) {
      tk <- TukeyHSD(fit)$group_col
      posthoc <- tibble(
        group1 = sub("-.*", "", rownames(tk)),
        group2 = sub(".*-", "", rownames(tk)),
        p_adj  = tk[, "p adj"]
      )
      letter_df <- make_letters_from_p(posthoc)
    } else {
      posthoc <- tibble(group1 = character(), group2 = character(), p_adj = numeric())
      letter_df <- tibble(
        Treatment = factor(c("CK", "PE0.5", "PVC5"), levels = c("CK", "PE0.5", "PVC5")),
        letters = c("a", "a", "a")
      )
    }

  } else if (!is.na(shapiro_p) && shapiro_p >= 0.05) {
    method <- "Welch ANOVA + Games-Howell"
    overall_p <- oneway.test(value ~ group_col, data = df, var.equal = FALSE)$p.value

    if (overall_p < 0.05) {
      gh <- rstatix::games_howell_test(df, value ~ group_col)
      posthoc <- gh %>% transmute(group1, group2, p_adj = p.adj)
      letter_df <- make_letters_from_p(posthoc)
    } else {
      posthoc <- tibble(group1 = character(), group2 = character(), p_adj = numeric())
      letter_df <- tibble(
        Treatment = factor(c("CK", "PE0.5", "PVC5"), levels = c("CK", "PE0.5", "PVC5")),
        letters = c("a", "a", "a")
      )
    }

  } else {
    method <- "Kruskal-Wallis + Dunn BH"
    overall_p <- kruskal.test(value ~ group_col, data = df)$p.value

    if (overall_p < 0.05) {
      dn <- rstatix::dunn_test(df, value ~ group_col, p.adjust.method = "BH")
      posthoc <- dn %>% transmute(group1, group2, p_adj = p.adj)
      letter_df <- make_letters_from_p(posthoc)
    } else {
      posthoc <- tibble(group1 = character(), group2 = character(), p_adj = numeric())
      letter_df <- tibble(
        Treatment = factor(c("CK", "PE0.5", "PVC5"), levels = c("CK", "PE0.5", "PVC5")),
        letters = c("a", "a", "a")
      )
    }
  }

  summary_df <- df %>%
    group_by(group_col) %>%
    summarise(
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      sem = sd / sqrt(n),
      .groups = "drop"
    ) %>%
    rename(Treatment = group_col) %>%
    left_join(letter_df, by = "Treatment")

  y_range <- max(summary_df$mean + summary_df$sem, na.rm = TRUE) -
    min(summary_df$mean - summary_df$sem, na.rm = TRUE)
  if (is.na(y_range) || y_range == 0) y_range <- max(summary_df$mean, na.rm = TRUE) * 0.1
  if (is.na(y_range) || y_range == 0) y_range <- 1

  summary_df <- summary_df %>%
    mutate(label_y = mean + sem + 0.10 * y_range)

  method_df <- tibble(
    method = method,
    overall_p = overall_p,
    residual_shapiro_p = shapiro_p,
    levene_p = levene_p
  )

  list(
    summary = summary_df,
    posthoc = posthoc,
    method = method_df
  )
}

# -----------------------------
# 5. Main heatmap statistics
# -----------------------------
heatmap_stats <- map_dfr(names(main_var_map), function(vlab) {
  vcol <- unname(main_var_map[vlab])
  ck <- dat %>% filter(Treatment == "CK") %>% pull(all_of(vcol))
  pair_stats <- pairwise_vs_ck_test(dat, vcol)

  pair_stats %>%
    mutate(
      variable = vlab,
      variable_display = unname(main_display_map[vlab]),
      mean_ck = mean(ck, na.rm = TRUE),
      sd_ck   = sd(ck, na.rm = TRUE),
      mean_trt = map_dbl(treatment, ~ dat %>% filter(Treatment == .x) %>% pull(all_of(vcol)) %>% mean(na.rm = TRUE)),
      sd_trt   = map_dbl(treatment, ~ dat %>% filter(Treatment == .x) %>% pull(all_of(vcol)) %>% sd(na.rm = TRUE)),
      hedges_g = map_dbl(treatment, ~ hedges_g(
        dat %>% filter(Treatment == .x) %>% pull(all_of(vcol)),
        ck
      ))
    ) %>%
    select(variable, variable_display, treatment, mean_ck, sd_ck, mean_trt, sd_trt,
           hedges_g, method, shapiro_ck, shapiro_trt, levene_p, p_value)
}) %>%
  mutate(
    p_fdr = p.adjust(p_value, method = "BH"),
    sig = case_when(
      p_fdr < 0.001 ~ "***",
      p_fdr < 0.01  ~ "**",
      p_fdr < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

write_csv(heatmap_stats, heatmap_stat)

row_order_main <- c(
  "Available Cd",
  "pH",
  "Dissolved organic carbon",
  "Total carbon",
  "Urease",
  "Alkaline hydrolyzable nitrogen",
  "Acid phosphatase"
)
col_order_main <- c("PE0.5", "PVC5")

heatmap_plot_df <- heatmap_stats %>%
  mutate(
    variable = factor(variable, levels = rev(row_order_main)),
    variable_display = factor(variable_display, levels = rev(unname(main_display_map[row_order_main]))),
    treatment = factor(treatment, levels = col_order_main)
  ) %>%
  arrange(variable, treatment)

write_csv(heatmap_plot_df, heatmap_mat)

max_abs <- max(abs(heatmap_plot_df$hedges_g), na.rm = TRUE)
max_abs <- ceiling(max_abs * 2) / 2

heatmap_plot <- ggplot(heatmap_plot_df, aes(x = treatment, y = variable_display, fill = hedges_g)) +
  geom_tile(width = 0.92, height = 0.92, colour = "white", linewidth = 0.8) +
  geom_text(
    aes(label = sig),
    size = 3.1,
    family = "Arial",
    colour = "black",
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#3B66B0",
    mid = "#F7F7F7",
    high = "#B24745",
    midpoint = 0,
    limits = c(-max_abs, max_abs),
    breaks = pretty(c(-max_abs, max_abs), n = 5),
    name = "Hedges' g\n(vs CK)"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(34, "mm"),
      barwidth  = unit(4, "mm")
    )
  ) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 0.42, clip = "off") +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 7, colour = "black", face = "bold", family = "Arial"),
    axis.text.y = element_text(size = 6.5, colour = "black", family = "Arial", lineheight = 0.95),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    axis.ticks.length = unit(1.2, "mm"),
    legend.title = element_text(size = 7, colour = "black", family = "Arial"),
    legend.text = element_text(size = 6, colour = "black", family = "Arial"),
    legend.position = "right",
    plot.margin = margin(2, 3, 2, 2, unit = "mm")
  )

# Save heatmap
if (capabilities("cairo")) {
  ggsave(
    filename = heatmap_pdf,
    plot = heatmap_plot,
    width = 89, height = 92, units = "mm",
    device = cairo_pdf, bg = "white"
  )
} else {
  ggsave(
    filename = heatmap_pdf,
    plot = heatmap_plot,
    width = 89, height = 92, units = "mm",
    device = "pdf", bg = "white"
  )
}

ggsave(
  filename = heatmap_tiff,
  plot = heatmap_plot,
  width = 89, height = 92, units = "mm",
  dpi = 600,
  compression = "lzw",
  device = "tiff",
  bg = "white"
)

# -----------------------------
# 6. Supplementary grouped dot statistics
# -----------------------------
supp_results <- map(names(supp_var_map), function(vlab) {
  vcol <- unname(supp_var_map[vlab])
  res <- run_group_comparison(dat, vcol)

  list(
    summary = res$summary %>% mutate(variable = vlab, variable_display = unname(supp_display_map[vlab]), .before = 1),
    posthoc = res$posthoc %>% mutate(variable = vlab, .before = 1),
    method  = res$method %>% mutate(variable = vlab, .before = 1)
  )
})

supp_summary <- bind_rows(map(supp_results, "summary")) %>%
  mutate(
    variable = factor(variable, levels = names(supp_var_map)),
    variable_display = factor(variable_display, levels = unname(supp_display_map[names(supp_var_map)])),
    Treatment = factor(Treatment, levels = c("CK", "PE0.5", "PVC5"))
  )

supp_posthoc <- bind_rows(map(supp_results, "posthoc"))
supp_method  <- bind_rows(map(supp_results, "method"))

panel_letter_df <- tibble(
  variable = factor(names(supp_var_map), levels = names(supp_var_map)),
  variable_display = factor(unname(supp_display_map[names(supp_var_map)]),
                            levels = unname(supp_display_map[names(supp_var_map)])),
  panel = c("a", "b", "c", "d", "e", "f", "g")
)

write_csv(supp_summary, supp_sum_csv)
write_csv(supp_posthoc, supp_post_csv)
write_csv(supp_method, supp_method_csv)

raw_supp_df <- dat %>%
  pivot_longer(cols = all_of(unname(supp_var_map)), names_to = "orig_var", values_to = "value") %>%
  mutate(
    variable = names(supp_var_map)[match(orig_var, unname(supp_var_map))],
    variable_display = unname(supp_display_map[variable]),
    variable = factor(variable, levels = names(supp_var_map)),
    variable_display = factor(variable_display, levels = unname(supp_display_map[names(supp_var_map)])),
    Treatment = factor(Treatment, levels = c("CK", "PE0.5", "PVC5"))
  )

trt_cols <- c("CK" = "#4D4D4D", "PE0.5" = "#9E9E9E", "PVC5" = "#B24745")

supp_plot <- ggplot(supp_summary, aes(x = Treatment, y = mean, colour = Treatment)) +
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.12,
    linewidth = 0.45
  ) +
  geom_point(
    data = raw_supp_df,
    aes(x = Treatment, y = value, colour = Treatment),
    inherit.aes = FALSE,
    position = position_jitter(width = 0.09, height = 0, seed = 1),
    size = 1.3,
    alpha = 0.75,
    stroke = 0
  ) +
  geom_point(size = 2.6, stroke = 0) +
  geom_text(
    aes(y = label_y, label = letters),
    family = "Arial",
    fontface = "bold",
    size = 3.0,
    colour = "black"
  ) +
  geom_text(
    data = panel_letter_df,
    aes(x = 0.58, y = Inf, label = panel),
    inherit.aes = FALSE,
    family = "Arial",
    fontface = "bold",
    size = 4.6,
    colour = "black",
    hjust = 0,
    vjust = 1.15
  ) +
  ggh4x::facet_wrap2(
    ~ variable_display,
    scales = "free_y",
    ncol = 4,
    axes = "x",
    remove_labels = "none"
  ) +
  scale_colour_manual(values = trt_cols) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 7, face = "bold", colour = "black", family = "Arial", lineheight = 0.95,
                              margin = margin(t = 1, b = 2)),
    axis.text.x = element_text(size = 7, colour = "black", face = "bold", family = "Arial"),
    axis.text.y = element_text(size = 6.5, colour = "black", family = "Arial"),
    axis.title.y = element_blank(),
    axis.line = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks.length = unit(1.1, "mm"),
    legend.position = "none",
    panel.spacing = unit(4.5, "mm"),
    plot.margin = margin(3, 3, 2, 2, unit = "mm")
  )

if (capabilities("cairo")) {
  ggsave(
    filename = supp_pdf,
    plot = supp_plot,
    width = 183, height = 135, units = "mm",
    device = cairo_pdf, bg = "white"
  )
} else {
  ggsave(
    filename = supp_pdf,
    plot = supp_plot,
    width = 183, height = 135, units = "mm",
    device = "pdf", bg = "white"
  )
}

ggsave(
  filename = supp_tiff,
  plot = supp_plot,
  width = 183, height = 135, units = "mm",
  dpi = 600,
  compression = "lzw",
  device = "tiff",
  bg = "white"
)

# -----------------------------
# 7. Console message
# -----------------------------
message("Done.")
message("Input file: ", file_in)
message("Output folder: ", out_dir)
message("Saved main heatmap: ", heatmap_pdf)
message("Saved main heatmap: ", heatmap_tiff)
message("Saved supplementary grouped dot: ", supp_pdf)
message("Saved supplementary grouped dot: ", supp_tiff)
message("Saved heatmap stats: ", heatmap_stat)
message("Saved heatmap plot matrix: ", heatmap_mat)
message("Saved supplementary summary/letters: ", supp_sum_csv)
message("Saved supplementary posthoc details: ", supp_post_csv)
message("Saved supplementary method table: ", supp_method_csv)
