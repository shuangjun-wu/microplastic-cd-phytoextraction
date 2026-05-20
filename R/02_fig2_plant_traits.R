# Figure 2: plant traits and Cd partitioning
# Generates phenotype, biomass, allocation, Cd concentration, translocation and extraction panels.

# Packages and paths
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

pkg_needed <- c(
  "tidyverse",
  "ggplot2",
  "patchwork",
  "magick",
  "car",
  "multcompView",
  "rstatix",
  "ragg",
  "grid"
)
ensure_packages(pkg_needed)

# ---------- font ----------
base_family <- "Arial"
if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}

data_dir <- path_from_env("FIG2_DATA_DIR", file.path(repo_dir, "data", "fig2"))
out_dir  <- path_from_env("FIG2_OUT_DIR",  file.path(repo_dir, "outputs", "fig2"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
csv_file <- file.path(data_dir, "fig2.csv")

photo_candidates <- c(
  file.path(data_dir, "phenotype.tif"),
  file.path(data_dir, "phenotype.tiff"),
  file.path(data_dir, "Fig2_photo.tif"),
  file.path(data_dir, "Fig2_photo.tiff")
)

photo_exists <- file.exists(photo_candidates)
photo_path <- if (any(photo_exists)) photo_candidates[which(photo_exists)[1]] else NA_character_

if (!file.exists(csv_file)) {
  stop("Cannot find fig2.csv at: ", csv_file)
}

has_photo <- !is.na(photo_path)

# ---------- colors ----------
col_map <- c("CK" = "#6E6E6E", "PE0.5" = "#4C78A8", "PVC5" = "#F28E2B")

# ---------- axis labels (use plotmath expressions for reliable superscripts) ----------
ylab_dryweight  <- expression("Dry weight (" * g ~ plant^{-1} * ")")
ylab_allocation <- "Allocation fraction (%)"
ylab_cdconc     <- expression("Cd concentration (" * mg ~ kg^{-1} * ")")
ylab_tf         <- "Translocation factor"
ylab_cdextract  <- expression("Cd extraction (" * mu * g ~ pot^{-1} * ")")

# ---------- helpers ----------
scale_to_percent <- function(x) {
  if (max(x, na.rm = TRUE) <= 1.2) x * 100 else x
}

theme_pub <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family, color = "black"),
      axis.title.x = element_text(face = "bold", size = base_size + 1, color = "black"),
      axis.title.y = element_text(face = "bold", size = base_size + 1, color = "black"),
      axis.text.x  = element_text(face = "bold", size = base_size + 1, color = "black"),
      axis.text.y  = element_text(size = base_size, color = "black"),
      axis.line.x.bottom = element_line(color = "black", linewidth = 0.55),
      axis.line.y.left   = element_line(color = "black", linewidth = 0.55),
      axis.line.x.top    = element_blank(),
      axis.line.y.right  = element_blank(),
      axis.ticks.x.bottom = element_line(color = "black", linewidth = 0.50),
      axis.ticks.y.left   = element_line(color = "black", linewidth = 0.50),
      axis.ticks.length   = unit(2.2, "pt"),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = base_size, margin = margin(t = 0.5, b = 0.5)),
      legend.position = "none",
      panel.spacing = unit(0.55, "lines"),
      plot.margin = margin(4, 4, 4, 3)
    )
}

pairwise_to_letters <- function(pair_df, mean_df, all_levels, alpha = 0.05) {
  pvec <- pair_df$p.adj
  names(pvec) <- paste(pair_df$group1, pair_df$group2, sep = "-")
  raw_letters <- multcompView::multcompLetters(pvec, threshold = alpha)$Letters

  out <- tibble(
    trt = factor(names(raw_letters), levels = all_levels),
    letters = unname(raw_letters)
  ) %>%
    arrange(trt) %>%
    left_join(
      mean_df %>% transmute(trt = factor(trt, levels = all_levels), mean),
      by = "trt"
    )

  old_letter_order <- out %>%
    arrange(desc(mean)) %>%
    pull(letters) %>%
    paste(collapse = "") %>%
    strsplit("", fixed = TRUE) %>%
    .[[1]]

  old_letter_order <- unique(old_letter_order[old_letter_order != ""])

  if (length(old_letter_order) == 0) {
    return(out %>% select(trt, letters))
  }

  map_tbl <- tibble(old = old_letter_order, new = letters[seq_along(old_letter_order)])

  remap_letters <- function(x) {
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    chars <- chars[chars != ""]
    new_chars <- map_tbl$new[match(chars, map_tbl$old)]
    paste(sort(unique(new_chars)), collapse = "")
  }

  out %>%
    mutate(letters = vapply(letters, remap_letters, character(1))) %>%
    select(trt, letters)
}

# ---------- statistical methods ----------
method_map <- c(
  "BDW" = "ANOVA_Tukey",
  "SDW" = "Welch_GamesHowell",
  "RDW" = "ANOVA_Tukey",
  "Berry/Total aboveground" = "ANOVA_Tukey",
  "Root / Total Aboveground" = "ANOVA_Tukey",
  "BCd" = "ANOVA_Tukey",
  "SCd" = "ANOVA_Tukey",
  "RCd" = "ANOVA_Tukey",
  "BTF" = "Welch_GamesHowell",
  "STF" = "ANOVA_Tukey",
  "BECd" = "ANOVA_Tukey",
  "SECd" = "ANOVA_Tukey",
  "AboveExtract" = "Kruskal_Dunn_BH"
)

analyse_trait <- function(data, response, trt = "Treatment", alpha = 0.05) {
  df <- data %>% select(all_of(trt), all_of(response)) %>% drop_na()
  names(df) <- c("trt", "y")
  df$trt <- factor(df$trt, levels = levels(data[[trt]]))

  desc <- df %>%
    group_by(trt) %>%
    summarise(
      mean = mean(y, na.rm = TRUE),
      se = sd(y, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )

  chosen_method <- method_map[[response]]
  if (is.null(chosen_method)) stop("No test method specified for trait: ", response)

  diag <- tibble(trait = response, method = chosen_method)

  if (chosen_method == "ANOVA_Tukey") {
    fit_aov <- aov(y ~ trt, data = df)
    tukey_df <- as.data.frame(TukeyHSD(fit_aov)$trt) %>%
      rownames_to_column("comparison") %>%
      separate(comparison, into = c("group1", "group2"), sep = "-") %>%
      transmute(group1, group2, p.adj = `p adj`)
    letters_df <- pairwise_to_letters(tukey_df, desc, levels(df$trt), alpha = alpha)
    out <- desc %>% left_join(letters_df, by = "trt") %>% mutate(method = chosen_method)
    return(list(summary = out, diag = diag))
  }

  if (chosen_method == "Welch_GamesHowell") {
    gh_df <- rstatix::games_howell_test(df, y ~ trt) %>%
      transmute(group1, group2, p.adj)
    letters_df <- pairwise_to_letters(gh_df, desc, levels(df$trt), alpha = alpha)
    out <- desc %>% left_join(letters_df, by = "trt") %>% mutate(method = chosen_method)
    return(list(summary = out, diag = diag))
  }

  if (chosen_method == "Kruskal_Dunn_BH") {
    dunn_df <- rstatix::dunn_test(df, y ~ trt, p.adjust.method = "BH") %>%
      transmute(group1, group2, p.adj)
    letters_df <- pairwise_to_letters(dunn_df, desc, levels(df$trt), alpha = alpha)
    out <- desc %>% left_join(letters_df, by = "trt") %>% mutate(method = chosen_method)
    return(list(summary = out, diag = diag))
  }

  stop("Unsupported method for trait: ", response)
}

get_letters_df <- function(data, response, facet_label) {
  analyse_trait(data, response)$summary %>%
    transmute(trt, mean, se, letters, method, trait = facet_label)
}

build_long_df <- function(data, responses, facet_labels) {
  data %>%
    select(Treatment, all_of(responses)) %>%
    pivot_longer(cols = all_of(responses), names_to = "trait_raw", values_to = "value") %>%
    mutate(
      Treatment = factor(Treatment, levels = c("CK", "PE0.5", "PVC5")),
      trait_raw = factor(trait_raw, levels = responses),
      trait = factor(as.character(trait_raw), levels = responses, labels = facet_labels)
    )
}

build_letter_df <- function(data, responses, facet_labels, long_df, offset_mult = 0.08) {
  letter_df <- purrr::map2_dfr(responses, facet_labels, ~ get_letters_df(data, .x, .y)) %>%
    mutate(trait = factor(trait, levels = facet_labels))

  local_y <- long_df %>%
    group_by(trait, Treatment) %>%
    summarise(local_max = max(value, na.rm = TRUE), .groups = "drop")

  facet_range <- long_df %>%
    group_by(trait) %>%
    summarise(
      facet_min = min(value, na.rm = TRUE),
      facet_max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(facet_range = ifelse(facet_max - facet_min == 0, abs(facet_max) * 0.08 + 1, facet_max - facet_min))

  letter_df %>%
    left_join(local_y, by = c("trait", "trt" = "Treatment")) %>%
    left_join(facet_range %>% select(trait, facet_range), by = "trait") %>%
    mutate(y = local_max + offset_mult * facet_range)
}

finish_facet_plot <- function(p, y_lab, free_y = TRUE, show_x_labels = TRUE, x_text_angle = 45, top_expand = 0.14) {
  p <- p +
    facet_wrap(~ trait, nrow = 1, scales = if (free_y) "free_y" else "fixed") +
    labs(x = NULL, y = y_lab) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.03, top_expand))) +
    theme_pub()

  if (!show_x_labels) {
    p <- p + theme(axis.text.x = element_blank())   # keep x-axis tick marks
  } else {
    p <- p + theme(
      axis.text.x = element_text(
        angle = x_text_angle, hjust = 1, vjust = 1,
        face = "bold", size = 10, color = "black"
      )
    )
  }
  p
}

plot_point_summary <- function(data, responses, facet_labels, y_lab, free_y = TRUE, show_x_labels = TRUE) {
  long_df <- build_long_df(data, responses, facet_labels)
  letter_df <- build_letter_df(data, responses, facet_labels, long_df, offset_mult = 0.08)

  p <- ggplot(long_df, aes(x = Treatment, y = value, color = Treatment)) +
    geom_jitter(width = 0.08, size = 2.1, alpha = 0.9) +
    stat_summary(fun = mean, geom = "point", color = "black", shape = 18, size = 3.1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = 0.12, linewidth = 0.45) +
    geom_text(
      data = letter_df,
      aes(x = trt, y = y, label = letters),
      inherit.aes = FALSE,
      color = "black",
      family = base_family,
      size = 3.2,
      vjust = 0
    ) +
    scale_color_manual(values = col_map)

  finish_facet_plot(p, y_lab = y_lab, free_y = free_y, show_x_labels = show_x_labels, top_expand = 0.14)
}

plot_box_jitter_sem <- function(data, responses, facet_labels, y_lab, free_y = TRUE, show_x_labels = TRUE) {
  long_df <- build_long_df(data, responses, facet_labels)
  letter_df <- build_letter_df(data, responses, facet_labels, long_df, offset_mult = 0.10)

  p <- ggplot(long_df, aes(x = Treatment, y = value, fill = Treatment, color = Treatment)) +
    geom_boxplot(width = 0.58, outlier.shape = NA, alpha = 0.25, linewidth = 0.45) +
    geom_jitter(width = 0.08, size = 1.9, alpha = 0.82) +
    stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = 0.12, linewidth = 0.50) +
    stat_summary(fun = mean, geom = "point", color = "black", shape = 18, size = 2.9) +
    geom_text(
      data = letter_df,
      aes(x = trt, y = y, label = letters),
      inherit.aes = FALSE,
      color = "black",
      family = base_family,
      size = 3.2,
      vjust = 0
    ) +
    scale_fill_manual(values = col_map) +
    scale_color_manual(values = col_map)

  finish_facet_plot(p, y_lab = y_lab, free_y = free_y, show_x_labels = show_x_labels, top_expand = 0.17)
}

plot_bar_jitter <- function(data, responses, facet_labels, y_lab, free_y = TRUE, show_x_labels = TRUE) {
  long_df <- build_long_df(data, responses, facet_labels)

  summary_df <- long_df %>%
    group_by(trait, Treatment) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
      .groups = "drop"
    )

  letter_df <- build_letter_df(data, responses, facet_labels, long_df, offset_mult = 0.08)

  p <- ggplot() +
    geom_col(
      data = summary_df,
      aes(x = Treatment, y = mean, fill = Treatment),
      width = 0.62,
      alpha = 0.72,
      color = "black",
      linewidth = 0.35
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = Treatment, ymin = mean - se, ymax = mean + se),
      width = 0.12,
      linewidth = 0.50,
      color = "black"
    ) +
    geom_jitter(
      data = long_df,
      aes(x = Treatment, y = value, color = Treatment),
      width = 0.08,
      size = 2.0,
      alpha = 0.85
    ) +
    geom_text(
      data = letter_df,
      aes(x = trt, y = y, label = letters),
      inherit.aes = FALSE,
      color = "black",
      family = base_family,
      size = 3.2,
      vjust = 0
    ) +
    scale_fill_manual(values = col_map) +
    scale_color_manual(values = col_map)

  finish_facet_plot(p, y_lab = y_lab, free_y = free_y, show_x_labels = show_x_labels, top_expand = 0.14)
}

make_photo_panel <- function(photo_path) {
  img_a <- magick::image_read(photo_path)
  img_a <- magick::image_trim(img_a)
  info <- magick::image_info(img_a)
  aspect <- info$height / info$width
  img_grob <- grid::rasterGrob(as.raster(img_a), interpolate = TRUE)

  ggplot() +
    annotation_custom(
      img_grob,
      xmin = 0, xmax = 1,
      ymin = 0, ymax = aspect
    ) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, aspect), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(4, 4, 4, 2))
}

save_plot_pair <- function(plot_obj, filename_stub, width_mm, height_mm, dpi = 600) {
  ggsave(
    filename = file.path(out_dir, paste0(filename_stub, ".tiff")),
    plot = plot_obj,
    device = ragg::agg_tiff,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi,
    compression = "lzw"
  )

  ggsave(
    filename = file.path(out_dir, paste0(filename_stub, ".pdf")),
    plot = plot_obj,
    width = width_mm,
    height = height_mm,
    units = "mm",
    device = cairo_pdf,
    family = base_family
  )
}

# ---------- data ----------
dat <- readr::read_csv(csv_file, show_col_types = FALSE) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("CK", "PE0.5", "PVC5")),
    `Berry/Total aboveground` = scale_to_percent(`Berry/Total aboveground`),
    `Root / Total Aboveground` = scale_to_percent(`Root / Total Aboveground`),
    AboveExtract = BECd + SECd
  )

traits_all <- c(
  "RDW", "SDW", "BDW",
  "Berry/Total aboveground", "Root / Total Aboveground",
  "RCd", "SCd", "BCd",
  "SECd", "BECd", "AboveExtract",
  "STF", "BTF"
)

stat_list <- setNames(lapply(traits_all, function(x) analyse_trait(dat, x)), traits_all)

readr::write_csv(
  bind_rows(lapply(stat_list, `[[`, "diag")),
  file.path(out_dir, "Fig2_stat_diagnostics.csv")
)

readr::write_csv(
  bind_rows(lapply(names(stat_list), function(x) stat_list[[x]]$summary %>% mutate(trait = x))),
  file.path(out_dir, "Fig2_group_summary_and_letters.csv")
)

# ---------- panels ----------
if (has_photo) {
  p_a <- make_photo_panel(photo_path)
}

p_b <- plot_point_summary(
  data = dat,
  responses = c("BDW", "SDW", "RDW"),
  facet_labels = c("Berry", "Shoot", "Root"),
  y_lab = ylab_dryweight,
  free_y = TRUE,
  show_x_labels = FALSE
)

p_c <- plot_point_summary(
  data = dat,
  responses = c("Berry/Total aboveground", "Root / Total Aboveground"),
  facet_labels = c("Berry / aboveground", "Root / aboveground"),
  y_lab = ylab_allocation,
  free_y = FALSE,
  show_x_labels = TRUE
)

p_d <- plot_box_jitter_sem(
  data = dat,
  responses = c("BCd", "SCd", "RCd"),
  facet_labels = c("Berry", "Shoot", "Root"),
  y_lab = ylab_cdconc,
  free_y = TRUE,
  show_x_labels = FALSE
)

p_e <- plot_box_jitter_sem(
  data = dat,
  responses = c("BTF", "STF"),
  facet_labels = c("Berry / shoot", "Shoot / root"),
  y_lab = ylab_tf,
  free_y = TRUE,
  show_x_labels = TRUE
)

p_f <- plot_bar_jitter(
  data = dat,
  responses = c("BECd", "SECd", "AboveExtract"),
  facet_labels = c("Berry", "Shoot", "Aboveground"),
  y_lab = ylab_cdextract,
  free_y = TRUE,
  show_x_labels = TRUE
)

# ---------- export single panels ----------
if (has_photo) save_plot_pair(p_a, "panel_a", width_mm = 245, height_mm = 52)
save_plot_pair(p_b, "panel_b", width_mm = 180, height_mm = 62)
save_plot_pair(p_c, "panel_c", width_mm = 135, height_mm = 62)
save_plot_pair(p_d, "panel_d", width_mm = 180, height_mm = 62)
save_plot_pair(p_e, "panel_e", width_mm = 135, height_mm = 62)
save_plot_pair(p_f, "panel_f", width_mm = 180, height_mm = 66)

# ---------- combined figure ----------
if (has_photo) {
  middle_row <- (p_b / p_c) | (p_d / p_e)
  combined_plot <- p_a / middle_row / p_f +
    plot_layout(heights = c(0.68, 1.56, 0.78), widths = c(1, 1))
} else {
  middle_row <- (p_b / p_c) | (p_d / p_e)
  combined_plot <- middle_row / p_f +
    plot_layout(heights = c(1.56, 0.78), widths = c(1, 1))
}

save_plot_pair(combined_plot, "fig2_combined", width_mm = 245, height_mm = 210)

print(combined_plot)
message("Done. Output saved in: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
