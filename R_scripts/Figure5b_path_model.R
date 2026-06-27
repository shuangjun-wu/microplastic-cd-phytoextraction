# Figure 5b source-data script
# Generates the regression-based path model.

suppressPackageStartupMessages({
  pkgs <- c("readr", "dplyr", "tibble", "ggplot2", "broom", "stringr")
  miss <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(miss) > 0) install.packages(miss, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

options(stringsAsFactors = FALSE)
setTimeLimit(cpu = Inf, elapsed = Inf, transient = TRUE)

# -------------------------
# User settings
# -------------------------
fig5_dir <- getwd()
panel_a_dir <- file.path(fig5_dir, "panel_a")
panel_b_dir <- file.path(fig5_dir, "figure_outputs")
dir.create(panel_b_dir, recursive = TRUE, showWarnings = FALSE)

merged_candidates <- c("merged_class_kegg_soil_plant_for_correlation.csv")

# -------------------------
# Helper functions
# -------------------------
find_first_existing <- function(search_dirs, candidates, required = TRUE, label = "file") {
  for (d in search_dirs) {
    for (nm in candidates) {
      p <- file.path(d, nm)
      if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  if (required) stop(label, " not found. Looked for: ", paste(candidates, collapse = ", "))
  NA_character_
}

fmt_p <- function(p) {
  ifelse(is.na(p), "NA",
         ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)))
}

safe_r2 <- function(mod) summary(mod)$r.squared

extract_path_rows <- function(mod, model_name, to_name) {
  broom::tidy(mod) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      model = model_name,
      to = to_name
    )
}

summarise_treatment_labels <- function(path_tbl) {
  path_tbl %>%
    dplyr::filter(stringr::str_detect(from, "^treatment")) %>%
    dplyr::mutate(
      contrast_label = dplyr::case_when(
        from == "treatmentPE0.5" ~ "PE0.5 vs CK",
        from == "treatmentPVC5" ~ "PVC5 vs CK",
        TRUE ~ from
      ),
      beta_txt = sprintf("β = %.2f", beta),
      p_txt = paste0("P ", fmt_p(p_value)),
      line_txt = paste0(contrast_label, ": ", beta_txt, ", ", p_txt)
    ) %>%
    dplyr::group_by(to) %>%
    dplyr::summarise(label = paste(line_txt, collapse = "\n"), .groups = "drop")
}

# -------------------------
# Read input file
# -------------------------
search_dirs <- unique(c(panel_b_dir, panel_a_dir, getwd(), "/mnt/data"))
merged_file <- find_first_existing(search_dirs, merged_candidates, TRUE, "merged file")
message("Using merged file: ", merged_file)

merged_df <- readr::read_csv(merged_file, show_col_types = FALSE)

required_cols <- c(
  "sample_original", "treatment",
  "Plant: BECd", "Plant: SECd",
  "Plant: SDW",
  "Plant: Berry/Total aboveground",
  "Plant: Root / Total Aboveground",
  "Plant: SCd",
  "Soil: SACd"
)

missing_cols <- setdiff(required_cols, names(merged_df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

analysis_df <- merged_df %>%
  dplyr::transmute(
    sample_original = sample_original,
    treatment = as.character(treatment),
    total_extraction = `Plant: BECd` + `Plant: SECd`,
    shoot_dw = `Plant: SDW`,
    berry_allocation = `Plant: Berry/Total aboveground`,
    root_allocation = `Plant: Root / Total Aboveground`,
    shoot_cd = `Plant: SCd`,
    soil_available_cd = `Soil: SACd`
  ) %>%
  dplyr::filter(
    is.finite(total_extraction),
    is.finite(shoot_dw),
    is.finite(berry_allocation),
    is.finite(root_allocation),
    is.finite(shoot_cd),
    is.finite(soil_available_cd),
    !is.na(treatment)
  )

if (nrow(analysis_df) < 8) {
  stop("Too few complete samples after filtering. Need at least 8 complete observations.")
}

analysis_df <- analysis_df %>%
  dplyr::mutate(
    treatment = factor(treatment, levels = c("CK", "PE0.5", "PVC5"))
  )

if (any(is.na(analysis_df$treatment))) {
  stop("Unexpected treatment labels found. Check 'treatment' column.")
}

# -------------------------
# Build growth/allocation composite (PC1)
# -------------------------
bio_pca <- stats::prcomp(
  analysis_df[, c("shoot_dw", "berry_allocation", "root_allocation")],
  center = TRUE, scale. = TRUE
)

analysis_df$biomass_allocation_PC1 <- bio_pca$x[, 1]

# Re-orient PC1 so that larger PC1 means larger shoot dry weight
if (stats::cor(analysis_df$biomass_allocation_PC1, analysis_df$shoot_dw, use = "pairwise.complete.obs") < 0) {
  analysis_df$biomass_allocation_PC1 <- -analysis_df$biomass_allocation_PC1
  bio_pca$rotation[, 1] <- -bio_pca$rotation[, 1]
  bio_pca$x[, 1] <- -bio_pca$x[, 1]
}

pca_loadings <- tibble(
  trait = rownames(bio_pca$rotation),
  PC1_loading = bio_pca$rotation[, 1],
  PC1_variance_explained = (bio_pca$sdev[1]^2) / sum(bio_pca$sdev^2)
)
readr::write_csv(pca_loadings, file.path(panel_b_dir, "fig5b_mediated_soildirect_biomass_pc1_loadings.csv"))

readr::write_csv(
  analysis_df %>% dplyr::select(sample_original, treatment, biomass_allocation_PC1),
  file.path(panel_b_dir, "fig5b_mediated_soildirect_biomass_pc1_scores.csv")
)

# -------------------------
# Standardize continuous variables
# -------------------------
analysis_z <- analysis_df %>%
  dplyr::mutate(
    total_extraction_z = as.numeric(scale(total_extraction)),
    biomass_allocation_PC1_z = as.numeric(scale(biomass_allocation_PC1)),
    shoot_cd_z = as.numeric(scale(shoot_cd)),
    soil_available_cd_z = as.numeric(scale(soil_available_cd))
  )

# -------------------------
# Fit piecewise path model
# -------------------------
mod_soil <- stats::lm(soil_available_cd_z ~ treatment, data = analysis_z)
mod_biomass <- stats::lm(biomass_allocation_PC1_z ~ treatment + soil_available_cd_z, data = analysis_z)
mod_shootcd <- stats::lm(shoot_cd_z ~ treatment + soil_available_cd_z + biomass_allocation_PC1_z, data = analysis_z)
mod_extract <- stats::lm(total_extraction_z ~ soil_available_cd_z + biomass_allocation_PC1_z + shoot_cd_z, data = analysis_z)

analysis_table <- tibble(
  sample_size = nrow(analysis_z),
  pc1_variance_explained = (bio_pca$sdev[1]^2) / sum(bio_pca$sdev^2),
  soil_model_r2 = safe_r2(mod_soil),
  biomass_model_r2 = safe_r2(mod_biomass),
  shootcd_model_r2 = safe_r2(mod_shootcd),
  extraction_model_r2 = safe_r2(mod_extract)
)
readr::write_csv(analysis_table, file.path(panel_b_dir, "fig5b_mediated_soildirect_analysis_table.csv"))

path_tbl <- dplyr::bind_rows(
  extract_path_rows(mod_soil, "soil", "soil_available_cd_z"),
  extract_path_rows(mod_biomass, "biomass", "biomass_allocation_PC1_z"),
  extract_path_rows(mod_shootcd, "shoot_cd", "shoot_cd_z"),
  extract_path_rows(mod_extract, "total_extraction", "total_extraction_z")
) %>%
  dplyr::mutate(
    from = term,
    beta = estimate,
    p_value = p.value,
    direction = dplyr::if_else(beta >= 0, "positive", "negative"),
    support = dplyr::case_when(
      p_value < 0.05 ~ "P<0.05",
      p_value < 0.10 ~ "P<0.10",
      TRUE ~ "NS"
    ),
    path = paste(from, "->", to)
  ) %>%
  dplyr::select(model, from, to, beta, std.error, statistic, p_value, direction, support, path)

readr::write_csv(path_tbl, file.path(panel_b_dir, "fig5b_mediated_soildirect_paths.csv"))

# -------------------------
# Indirect effects to total extraction
# Treatment indirect effects are not collapsed into one value because
# treatment is a factor with two explicit contrasts.
# Soil available Cd now has both direct and indirect paths to total extraction.
# -------------------------
get_beta <- function(tbl, from_var, to_var) {
  x <- tbl %>% dplyr::filter(from == from_var, to == to_var)
  if (nrow(x) == 0) return(NA_real_)
  x$beta[1]
}

b_soil_biomass <- get_beta(path_tbl, "soil_available_cd_z", "biomass_allocation_PC1_z")
b_soil_shootcd <- get_beta(path_tbl, "soil_available_cd_z", "shoot_cd_z")
b_biomass_shootcd <- get_beta(path_tbl, "biomass_allocation_PC1_z", "shoot_cd_z")
b_soil_extract <- get_beta(path_tbl, "soil_available_cd_z", "total_extraction_z")
b_biomass_extract <- get_beta(path_tbl, "biomass_allocation_PC1_z", "total_extraction_z")
b_shootcd_extract <- get_beta(path_tbl, "shoot_cd_z", "total_extraction_z")

indirect_tbl <- tibble(
  effect = c(
    "Soil available Cd -> Total extraction (direct)",
    "Soil available Cd -> Total extraction (indirect via biomass)",
    "Soil available Cd -> Total extraction (indirect via shoot Cd)",
    "Soil available Cd -> Total extraction (indirect via biomass and shoot Cd)",
    "Soil available Cd -> Total extraction (total effect)",
    "Growth/allocation composite -> Total extraction (indirect via shoot Cd)"
  ),
  effect_beta = c(
    b_soil_extract,
    b_soil_biomass * b_biomass_extract,
    b_soil_shootcd * b_shootcd_extract,
    b_soil_biomass * b_biomass_shootcd * b_shootcd_extract,
    b_soil_extract + (b_soil_biomass * b_biomass_extract) + (b_soil_shootcd * b_shootcd_extract) + (b_soil_biomass * b_biomass_shootcd * b_shootcd_extract),
    b_biomass_shootcd * b_shootcd_extract
  )
)
readr::write_csv(indirect_tbl, file.path(panel_b_dir, "fig5b_mediated_soildirect_indirect_effects.csv"))

# -------------------------
# Plot layout
# -------------------------
nodes <- tibble(
  node = c("treatment", "soil_available_cd_z", "biomass_allocation_PC1_z", "shoot_cd_z", "total_extraction_z"),
  label = c(
    "Microplastic\ntreatment",
    paste0("Soil available Cd\nR² = ", sprintf("%.2f", safe_r2(mod_soil))),
    paste0("Growth / allocation\ncomposite (PC1)\nR² = ", sprintf("%.2f", safe_r2(mod_biomass))),
    paste0("Shoot Cd\nR² = ", sprintf("%.2f", safe_r2(mod_shootcd))),
    paste0("Total Cd extraction\nR² = ", sprintf("%.2f", safe_r2(mod_extract)))
  ),
  x = c(0.9, 3.0, 5.2, 5.2, 8.1),
  y = c(3.2, 3.2, 4.4, 2.0, 3.2)
)

# Continuous edges
edges_cont <- path_tbl %>%
  dplyr::filter(!stringr::str_detect(from, "^treatment")) %>%
  dplyr::left_join(nodes %>% dplyr::select(from = node, x_from = x, y_from = y), by = "from") %>%
  dplyr::left_join(nodes %>% dplyr::select(to = node, x_to = x, y_to = y), by = "to") %>%
  dplyr::mutate(
    edge_color = ifelse(direction == "positive", "Positive", "Negative"),
    edge_lty = dplyr::case_when(
      p_value < 0.05 ~ "solid",
      p_value < 0.10 ~ "dashed",
      TRUE ~ "dotted"
    ),
    edge_alpha = dplyr::case_when(
      p_value < 0.05 ~ 0.95,
      p_value < 0.10 ~ 0.75,
      TRUE ~ 0.50
    ),
    label = paste0("β = ", sprintf("%.2f", beta), "\nP ", fmt_p(p_value))
  )

# Treatment labels: one label per downstream node with both contrasts stacked
treat_label_tbl <- summarise_treatment_labels(path_tbl)

treat_edges <- tibble(
  to = c("soil_available_cd_z", "biomass_allocation_PC1_z", "shoot_cd_z"),
  x_from = 0.9,
  y_from = 3.2,
  x_to = c(3.0, 5.2, 5.2),
  y_to = c(3.2, 4.4, 2.0)
) %>%
  dplyr::left_join(treat_label_tbl, by = "to") %>%
  dplyr::mutate(
    xm = c(1.9, 3.25, 3.25),
    ym = c(3.70, 4.95, 1.55)
  )

label_pos <- edges_cont %>%
  dplyr::mutate(
    xm = dplyr::case_when(
      from == "soil_available_cd_z" & to == "biomass_allocation_PC1_z" ~ 4.05,
      from == "soil_available_cd_z" & to == "shoot_cd_z" ~ 4.05,
      from == "soil_available_cd_z" & to == "total_extraction_z" ~ 5.45,
      from == "biomass_allocation_PC1_z" & to == "shoot_cd_z" ~ 6.15,
      from == "biomass_allocation_PC1_z" & to == "total_extraction_z" ~ 6.70,
      from == "shoot_cd_z" & to == "total_extraction_z" ~ 6.85,
      TRUE ~ (x_from + x_to) / 2
    ),
    ym = dplyr::case_when(
      from == "soil_available_cd_z" & to == "biomass_allocation_PC1_z" ~ 4.05,
      from == "soil_available_cd_z" & to == "shoot_cd_z" ~ 2.15,
      from == "soil_available_cd_z" & to == "total_extraction_z" ~ 2.45,
      from == "biomass_allocation_PC1_z" & to == "shoot_cd_z" ~ 3.10,
      from == "biomass_allocation_PC1_z" & to == "total_extraction_z" ~ 4.20,
      from == "shoot_cd_z" & to == "total_extraction_z" ~ 2.20,
      TRUE ~ (y_from + y_to) / 2
    )
  )

p <- ggplot() +
  geom_curve(
    data = treat_edges,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    curvature = 0.12,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed"),
    linewidth = 0.9,
    color = "grey45",
    linetype = "solid",
    alpha = 0.85
  ) +
  geom_curve(
    data = edges_cont,
    aes(
      x = x_from, y = y_from, xend = x_to, yend = y_to,
      color = edge_color, linewidth = abs(beta), alpha = edge_alpha, linetype = edge_lty
    ),
    curvature = 0.12,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed"),
    lineend = "round"
  ) +
  geom_label(
    data = nodes,
    aes(x = x, y = y, label = label),
    fill = "white",
    label.size = 0.35,
    label.padding = unit(0.19, "lines"),
    label.r = unit(0.12, "lines"),
    size = 3.6
  ) +
  geom_label(
    data = label_pos,
    aes(x = xm, y = ym, label = label),
    size = 2.8,
    fill = "white",
    label.size = 0.20,
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.10, "lines")
  ) +
  geom_label(
    data = treat_edges,
    aes(x = xm, y = ym, label = label),
    size = 2.6,
    fill = "white",
    label.size = 0.20,
    label.padding = unit(0.13, "lines"),
    label.r = unit(0.10, "lines")
  ) +
  scale_color_manual(values = c("Positive" = "#D73027", "Negative" = "#4575B4"), name = NULL) +
  scale_linewidth_continuous(range = c(0.7, 2.2), guide = "none") +
  scale_alpha_identity(guide = "none") +
  scale_linetype_identity(guide = "none") +
  coord_cartesian(xlim = c(0.1, 9.35), ylim = c(1.15, 5.2), clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 22, 10, 22)
  )

pdf_file <- file.path(panel_b_dir, "Fig5b_final_NCstyle_v5.pdf")
png_file <- file.path(panel_b_dir, "Fig5b_final_NCstyle_v5.png")

ggsave(pdf_file, p, width = 9.4, height = 5.2, bg = "white")
if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(png_file, p, width = 9.4, height = 5.2, dpi = 300, bg = "white", device = ragg::agg_png)
} else {
  ggsave(png_file, p, width = 9.4, height = 5.2, dpi = 300, bg = "white")
}

message("Done.")
message("Output directory: ", panel_b_dir)
message("Complete cases used: ", nrow(analysis_df))
message("Detected treatment levels: ", paste(levels(analysis_df$treatment), collapse = ", "))
message("PC1 variance explained: ", sprintf("%.3f", (bio_pca$sdev[1]^2) / sum(bio_pca$sdev^2)))
message("Soil model R2: ", sprintf("%.3f", safe_r2(mod_soil)))
message("Biomass model R2: ", sprintf("%.3f", safe_r2(mod_biomass)))
message("Shoot Cd model R2: ", sprintf("%.3f", safe_r2(mod_shootcd)))
message("Extraction model R2: ", sprintf("%.3f", safe_r2(mod_extract)))
message("Figure written to: ", pdf_file)
message("Figure written to: ", png_file)
