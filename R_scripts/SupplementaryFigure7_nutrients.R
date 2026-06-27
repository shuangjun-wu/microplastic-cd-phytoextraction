# Supplementary Figure 7 source-data script
# Generates nutrient-concentration plots and statistical summaries.

pkg_needed <- c(
  "tidyverse",
  "rstatix",
  "car",
  "multcompView",
  "ggh4x",
  "ggplot2"
)

pkg_to_install <- pkg_needed[!pkg_needed %in% installed.packages()[, "Package"]]
if (length(pkg_to_install) > 0) {
  install.packages(pkg_to_install, dependencies = TRUE)
}

library(tidyverse)
library(rstatix)
library(car)
library(multcompView)
library(ggh4x)
library(ggplot2)

## ---------- 1. Paths ----------
out_dir <- getwd()
input_file <- file.path(out_dir, "Source_Data_FigS7.csv")
if (!file.exists(input_file)) input_file <- file.path(out_dir, "figS7.csv")

## ---------- 2. Font ----------
base_family <- "Arial"
if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}

## ---------- 3. Read data ----------
df_wide <- read.csv(input_file, check.names = FALSE)
df_wide$Treatment <- factor(df_wide$Treatment, levels = c("CK", "PE0.5", "PVC5"))

df_wide <- df_wide %>%
  dplyr::select(-any_of("RCd (mg/kg)"))

## ---------- 4. Reshape data ----------
df_long <- df_wide %>%
  tidyr::pivot_longer(
    cols = -Treatment,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  tidyr::extract(
    col = Variable,
    into = c("OrganCode", "Element", "Unit"),
    regex = "^([BSR])([A-Za-z]+) \\((.+)\\)$",
    remove = FALSE
  ) %>%
  dplyr::mutate(
    Organ = dplyr::case_when(
      OrganCode == "B" ~ "Berry",
      OrganCode == "S" ~ "Shoot",
      OrganCode == "R" ~ "Root",
      TRUE ~ NA_character_
    ),
    Element = factor(Element, levels = c("P", "Fe", "Mg", "K")),
    Organ = factor(Organ, levels = c("Berry", "Shoot", "Root")),
    ElementLabel = dplyr::case_when(
      Element == "P"  ~ "Phosphorus concentration\n(g kg\u207B\u00B9)",
      Element == "Fe" ~ "Iron concentration\n(mg kg\u207B\u00B9)",
      Element == "Mg" ~ "Magnesium concentration\n(g kg\u207B\u00B9)",
      Element == "K"  ~ "Potassium concentration\n(g kg\u207B\u00B9)",
      TRUE ~ NA_character_
    ),
    ElementLabel = factor(
      ElementLabel,
      levels = c(
        "Phosphorus concentration\n(g kg\u207B\u00B9)",
        "Iron concentration\n(mg kg\u207B\u00B9)",
        "Magnesium concentration\n(g kg\u207B\u00B9)",
        "Potassium concentration\n(g kg\u207B\u00B9)"
      )
    )
  )

bad_vars <- df_long %>%
  dplyr::filter(is.na(Organ) | is.na(Element) | is.na(Unit)) %>%
  dplyr::distinct(Variable)

if (nrow(bad_vars) > 0) {
  warning(
    "These columns did not match the expected pattern and were removed: ",
    paste(bad_vars$Variable, collapse = ", ")
  )
  df_long <- df_long %>% dplyr::filter(!is.na(Organ), !is.na(Element), !is.na(Unit))
}

## ---------- 5. Panel order and tags ----------
panel_info <- df_long %>%
  dplyr::distinct(Organ, Element, Unit, ElementLabel) %>%
  dplyr::arrange(Organ, Element) %>%
  dplyr::mutate(PanelTag = letters[seq_len(dplyr::n())])

df_long <- df_long %>%
  dplyr::left_join(panel_info, by = c("Organ", "Element", "Unit", "ElementLabel"))

## ---------- 6. Helper to reorder CLD letters by descending mean ----------
reorder_cld_letters <- function(letter_vec, mean_vec_named) {
  if (length(letter_vec) == 0) return(letter_vec)

  all_chars <- strsplit(paste(letter_vec, collapse = ""), "")[[1]]
  old_letters <- unique(all_chars[nzchar(all_chars)])

  if (length(old_letters) <= 1) return(letter_vec)

  score_tbl <- tibble::tibble(old_letter = old_letters) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      score = max(mean_vec_named[grepl(old_letter, letter_vec, fixed = TRUE)], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(score), old_letter)

  new_letters <- letters[seq_len(nrow(score_tbl))]
  names(new_letters) <- score_tbl$old_letter

  out <- vapply(letter_vec, function(x) {
    chars <- strsplit(x, "")[[1]]
    mapped <- unname(new_letters[chars])
    mapped <- sort(mapped)
    paste(mapped, collapse = "")
  }, FUN.VALUE = character(1))

  out
}

## ---------- 7. Statistical decision function ----------
analyze_one_variable <- function(dat) {
  dat <- dat %>% tidyr::drop_na(Value)
  dat$Treatment <- factor(dat$Treatment, levels = c("CK", "PE0.5", "PVC5"))

  mean_tbl <- dat %>%
    dplyr::group_by(Treatment) %>%
    dplyr::summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop")
  mean_vec <- mean_tbl$mean_value
  names(mean_vec) <- as.character(mean_tbl$Treatment)

  shapiro_tab <- dat %>%
    dplyr::group_by(Treatment) %>%
    dplyr::summarise(
      shapiro_p = tryCatch(shapiro.test(Value)$p.value, error = function(e) NA_real_),
      .groups = "drop"
    )

  normal_ok <- all(!is.na(shapiro_tab$shapiro_p) & shapiro_tab$shapiro_p > 0.05)

  levene_p <- tryCatch(
    car::leveneTest(Value ~ Treatment, data = dat)$`Pr(>F)`[1],
    error = function(e) NA_real_
  )
  var_equal <- !is.na(levene_p) && levene_p > 0.05

  if (normal_ok && var_equal) {
    method <- "One-way ANOVA + Tukey HSD"
    fit <- aov(Value ~ Treatment, data = dat)
    global_p <- summary(fit)[[1]][["Pr(>F)"]][1]
    pw <- dat %>%
      rstatix::tukey_hsd(Value ~ Treatment) %>%
      dplyr::select(group1, group2, p.adj)
  } else if (normal_ok && !var_equal) {
    method <- "Welch ANOVA + Games-Howell"
    global_p <- dat %>%
      rstatix::welch_anova_test(Value ~ Treatment) %>%
      dplyr::pull(p)
    pw <- dat %>%
      rstatix::games_howell_test(Value ~ Treatment) %>%
      dplyr::select(group1, group2, p.adj)
  } else {
    method <- "Kruskal-Wallis + Dunn (BH)"
    global_p <- dat %>%
      rstatix::kruskal_test(Value ~ Treatment) %>%
      dplyr::pull(p)
    pw <- dat %>%
      rstatix::dunn_test(Value ~ Treatment, p.adjust.method = "BH") %>%
      dplyr::select(group1, group2, p.adj)
  }

  group_levels <- levels(dat$Treatment)

  if (is.na(global_p) || global_p > 0.05 || nrow(pw) == 0) {
    letters_df <- tibble::tibble(
      Treatment = factor(group_levels, levels = group_levels),
      Letters = "a"
    )
  } else {
    pvec <- pw$p.adj
    names(pvec) <- paste(pw$group1, pw$group2, sep = "-")

    letters_raw <- multcompView::multcompLetters(
      pvec,
      threshold = 0.05
    )$Letters

    letters_ordered <- reorder_cld_letters(
      letter_vec = letters_raw,
      mean_vec_named = mean_vec[names(letters_raw)]
    )

    letters_df <- tibble::tibble(
      Treatment = factor(names(letters_ordered), levels = group_levels),
      Letters = unname(letters_ordered)
    ) %>%
      dplyr::right_join(
        tibble::tibble(Treatment = factor(group_levels, levels = group_levels)),
        by = "Treatment"
      ) %>%
      dplyr::arrange(Treatment) %>%
      dplyr::mutate(Letters = ifelse(is.na(Letters), "a", Letters))
  }

  min_shapiro_p <- if (all(is.na(shapiro_tab$shapiro_p))) {
    NA_real_
  } else {
    suppressWarnings(min(shapiro_tab$shapiro_p, na.rm = TRUE))
  }

  stats_df <- tibble::tibble(
    method = method,
    global_p = global_p,
    min_shapiro_p = min_shapiro_p,
    levene_p = levene_p
  )

  pairwise_df <- pw %>%
    dplyr::mutate(
      method = method,
      comparison = paste(group1, "vs", group2)
    )

  list(
    stats = stats_df,
    letters = letters_df,
    pairwise = pairwise_df
  )
}

## ---------- 8. Run stats for each panel ----------
stats_list <- list()
letters_list <- list()
pairwise_list <- list()

for (i in seq_len(nrow(panel_info))) {
  key <- panel_info[i, ]

  dat_i <- df_long %>%
    dplyr::semi_join(key, by = c("Organ", "Element", "Unit", "ElementLabel", "PanelTag"))

  res_i <- analyze_one_variable(dat_i)

  stats_list[[i]] <- dplyr::bind_cols(key, res_i$stats)
  letters_list[[i]] <- dplyr::bind_cols(key, res_i$letters)
  pairwise_list[[i]] <- dplyr::bind_cols(key, res_i$pairwise)
}

stats_tbl <- dplyr::bind_rows(stats_list)
letters_tbl <- dplyr::bind_rows(letters_list)
pairwise_tbl <- dplyr::bind_rows(pairwise_list)

## ---------- 9. Summary for plotting ----------
sum_tbl <- df_long %>%
  dplyr::group_by(Organ, Element, Unit, ElementLabel, PanelTag, Treatment) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    se = sd / sqrt(n),
    ymax_obs = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

panel_range <- df_long %>%
  dplyr::group_by(Organ, Element, Unit, ElementLabel, PanelTag) %>%
  dplyr::summarise(
    ymin_panel = min(Value, na.rm = TRUE),
    ymax_panel = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    offset = dplyr::if_else(
      ymax_panel > ymin_panel,
      0.10 * (ymax_panel - ymin_panel),
      0.10 * ymax_panel + 0.05
    )
  )

letters_plot_tbl <- sum_tbl %>%
  dplyr::left_join(
    letters_tbl,
    by = c("Organ", "Element", "Unit", "ElementLabel", "PanelTag", "Treatment")
  ) %>%
  dplyr::left_join(
    panel_range,
    by = c("Organ", "Element", "Unit", "ElementLabel", "PanelTag")
  ) %>%
  dplyr::mutate(y_pos = ymax_obs + offset)

panel_tag_tbl <- panel_range %>%
  dplyr::transmute(
    Organ,
    Element,
    Unit,
    ElementLabel,
    PanelTag,
    x_tag = -Inf,
    y_tag = Inf,
    TagLabel = PanelTag
  )

## ---------- 10. Save statistics ----------
write.csv(
  stats_tbl,
  file = file.path(out_dir, "figS7_stats_method_globalP.csv"),
  row.names = FALSE
)

write.csv(
  letters_tbl,
  file = file.path(out_dir, "figS7_significance_letters.csv"),
  row.names = FALSE
)

write.csv(
  pairwise_tbl,
  file = file.path(out_dir, "figS7_pairwise_results.csv"),
  row.names = FALSE
)

## ---------- 11. Plot ----------
cols <- c(
  "CK"    = "#4D4D4D",
  "PE0.5" = "#1F78B4",
  "PVC5"  = "#D55E00"
)

p_figS7 <- ggplot(df_long, aes(x = Treatment, y = Value, color = Treatment)) +
  geom_jitter(
    width = 0.08,
    size = 1.8,
    alpha = 0.80,
    show.legend = FALSE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2.6,
    show.legend = FALSE
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.14,
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  geom_text(
    data = letters_plot_tbl,
    aes(x = Treatment, y = y_pos, label = Letters),
    inherit.aes = FALSE,
    family = base_family,
    size = 3.5,
    color = "black",
    vjust = 0
  ) +
  geom_text(
    data = panel_tag_tbl,
    aes(x = x_tag, y = y_tag, label = TagLabel),
    inherit.aes = FALSE,
    family = base_family,
    fontface = "bold",
    size = 4.0,
    hjust = -0.15,
    vjust = 1.15,
    color = "black"
  ) +
  ggh4x::facet_grid2(
    rows = vars(Organ),
    cols = vars(ElementLabel),
    scales = "free_y",
    independent = "y",
    switch = "y"
  ) +
  scale_color_manual(values = cols) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.22))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_family = base_family, base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 0.6),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.6),
    strip.text = element_text(face = "bold", family = base_family, color = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      color = "black",
      family = base_family
    ),
    axis.text.y = element_text(color = "black", family = base_family),
    axis.title = element_text(color = "black", family = base_family),
    strip.placement = "outside",
    legend.position = "none",
    plot.margin = margin(6, 20, 6, 6)
  )

## ---------- 12. Export ----------
pdf_file  <- file.path(out_dir, "figS7_nutrient_concentrations.pdf")
tiff_file <- file.path(out_dir, "figS7_nutrient_concentrations.tiff")

ggsave(
  filename = pdf_file,
  plot = p_figS7,
  width = 10,
  height = 7.6,
  units = "in",
  device = cairo_pdf
)

ggsave(
  filename = tiff_file,
  plot = p_figS7,
  width = 10,
  height = 7.6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  type = "cairo"
)

## ---------- 13. Print summary ----------
print(stats_tbl)
message(
  "Finished:\n",
  "- Figure PDF:  ", pdf_file, "\n",
  "- Figure TIFF: ", tiff_file, "\n",
  "- Stats table: ", file.path(out_dir, "figS7_stats_method_globalP.csv"), "\n",
  "- Letters:     ", file.path(out_dir, "figS7_significance_letters.csv"), "\n",
  "- Pairwise:    ", file.path(out_dir, "figS7_pairwise_results.csv")
)
