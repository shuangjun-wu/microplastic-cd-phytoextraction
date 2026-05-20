# Figure 4: phenotype-associated GSEA summaries
# Generates lollipop plots for biomass- and Cd-associated pathways.

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

pkgs <- c("readr", "dplyr", "stringr", "ggplot2", "tidyr", "purrr", "tibble", "tidytext")
ensure_packages(pkgs)

# User settings
biomass_dir <- path_from_env("FIG4_BIOMASS_DIR", file.path(repo_dir, "data", "fig4", "gsea", "biomass"))
cd_dir      <- path_from_env("FIG4_CD_DIR",      file.path(repo_dir, "data", "fig4", "gsea", "Cd"))
biomass_out_dir <- path_from_env("FIG4_BIOMASS_OUT_DIR", file.path(repo_dir, "outputs", "fig4", "biomass"))
cd_out_dir      <- path_from_env("FIG4_CD_OUT_DIR",      file.path(repo_dir, "outputs", "fig4", "Cd"))
dir.create(biomass_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cd_out_dir, recursive = TRUE, showWarnings = FALSE)

top_n <- 20
strong_fdr_cutoff <- 0.05
moderate_fdr_cutoff <- 0.25

# File definitions
biomass_panels <- c(
  "Shoot_PE0.5_vs_CK_SDW_paired",
  "Shoot_PVC5_vs_CK_SDW_paired",
  "Root_PE0.5_vs_CK_RDW_paired",
  "Root_PVC5_vs_CK_RDW_paired"
)

cd_panels <- c(
  "Shoot_PE0.5_vs_CK_SCd_paired",
  "Shoot_PVC5_vs_CK_SCd_paired",
  "Root_PE0.5_vs_CK_RCd_paired",
  "Root_PVC5_vs_CK_RCd_paired"
)

# Helpers
stop_if_missing_dir <- function(path, label) {
  if (!dir.exists(path)) stop(label, " not found: ", path)
}

make_panel_label <- function(analysis_name) {
  analysis_name %>%
    str_replace("^Shoot_", "Shoot | ") %>%
    str_replace("^Root_",  "Root | ") %>%
    str_replace("_PE0\\.5_vs_CK_", " | PE0.5 vs CK | ") %>%
    str_replace("_PVC5_vs_CK_",  " | PVC5 vs CK | ") %>%
    str_replace("_paired$", "")
}

find_panel_file <- function(dir_path, panel_name) {
  top20_file <- file.path(dir_path, paste0(panel_name, "_pathway_candidates_top20.csv"))
  ranking_file <- file.path(dir_path, paste0(panel_name, "_pathway_ranking.csv"))

  if (file.exists(top20_file)) {
    return(list(path = top20_file, type = "top20"))
  }
  if (file.exists(ranking_file)) {
    return(list(path = ranking_file, type = "ranking"))
  }

  stop(
    "Neither top20 nor ranking file found for panel: ", panel_name,
    "\nExpected one of:\n  ", top20_file, "\n  ", ranking_file
  )
}

add_evidence_group <- function(df) {
  df %>%
    mutate(
      AbsNES = abs(NES),
      EvidenceGroup = case_when(
        p.adjust < strong_fdr_cutoff ~ "FDR < 0.05",
        p.adjust < moderate_fdr_cutoff ~ "0.05 <= FDR < 0.25",
        TRUE ~ "FDR >= 0.25"
      ),
      EvidenceRank = case_when(
        p.adjust < strong_fdr_cutoff ~ 1L,
        p.adjust < moderate_fdr_cutoff ~ 2L,
        TRUE ~ 3L
      )
    )
}

read_one_panel <- function(dir_path, panel_name, top_n = 20) {
  hit <- find_panel_file(dir_path, panel_name)
  df <- readr::read_csv(hit$path, show_col_types = FALSE)

  required_cols <- c("ID", "Description", "NES", "p.adjust", "size")
  miss <- setdiff(required_cols, names(df))
  if (length(miss) > 0) {
    stop("Missing columns in file: ", basename(hit$path), " -> ", paste(miss, collapse = ", "))
  }

  if (hit$type == "ranking") {
    df <- df %>%
      add_evidence_group() %>%
      arrange(EvidenceRank, desc(AbsNES), p.adjust) %>%
      slice_head(n = top_n)
  } else {
    df <- df %>%
      add_evidence_group() %>%
      slice_head(n = top_n)
  }

  panel_label <- make_panel_label(panel_name)

  df %>%
    mutate(
      Panel = panel_label,
      SourceFile = basename(hit$path),
      PathwayLabel = if_else(!is.na(Description) & Description != "", Description, ID),
      PathwayLabel = if_else(p.adjust < strong_fdr_cutoff, paste0(PathwayLabel, " *"), PathwayLabel)
    )
}

make_lollipop_plot <- function(plot_df, plot_title, plot_subtitle) {
  plot_df2 <- plot_df %>%
    group_by(Panel) %>%
    mutate(PathwayLabelFacet = tidytext::reorder_within(PathwayLabel, NES, Panel)) %>%
    ungroup()

  ggplot(plot_df2, aes(x = NES, y = PathwayLabelFacet)) +
    geom_segment(aes(x = 0, xend = NES, yend = PathwayLabelFacet),
                 linewidth = 0.5, alpha = 0.8) +
    geom_point(aes(size = size, fill = EvidenceGroup),
               shape = 21, stroke = 0.7, color = "black") +
    tidytext::scale_y_reordered() +
    facet_wrap(~ Panel, ncol = 2, scales = "free_y") +
    scale_fill_manual(
      values = c(
        "FDR < 0.05" = "#D73027",
        "0.05 <= FDR < 0.25" = "#FDAE61",
        "FDR >= 0.25" = "white"
      ),
      breaks = c("FDR < 0.05", "0.05 <= FDR < 0.25", "FDR >= 0.25")
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "NES",
      y = NULL,
      size = "Pathway size",
      fill = "Adjusted P group"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

save_panel_plot_set <- function(dir_path, out_dir, panel_names, out_prefix, title_text, subtitle_text) {
  plot_df <- purrr::map_dfr(panel_names, ~ read_one_panel(dir_path, .x, top_n = top_n))

  readr::write_csv(plot_df, file.path(out_dir, paste0(out_prefix, "_top20_selected.csv")))

  p <- make_lollipop_plot(
    plot_df,
    plot_title = title_text,
    plot_subtitle = subtitle_text
  )

  ggsave(
    filename = file.path(out_dir, paste0(out_prefix, "_lollipop_4panel.pdf")),
    plot = p,
    width = 14,
    height = 12
  )

  ggsave(
    filename = file.path(out_dir, paste0(out_prefix, "_lollipop_4panel.png")),
    plot = p,
    width = 14,
    height = 12,
    dpi = 600
  )

  invisible(plot_df)
}

# Run
stop_if_missing_dir(biomass_dir, "biomass_dir")
stop_if_missing_dir(cd_dir, "cd_dir")

biomass_df <- save_panel_plot_set(
  dir_path = biomass_dir,
  out_dir = biomass_out_dir,
  panel_names = biomass_panels,
  out_prefix = "Fig4_biomass",
  title_text = "Fig. 4 | Top 20 pathway candidates associated with biomass traits",
  subtitle_text = paste0(
    "Top ", top_n,
    " pathways per panel; * indicates FDR < ", strong_fdr_cutoff,
    "; orange indicates ", strong_fdr_cutoff, " <= FDR < ", moderate_fdr_cutoff
  )
)

cd_df <- save_panel_plot_set(
  dir_path = cd_dir,
  out_dir = cd_out_dir,
  panel_names = cd_panels,
  out_prefix = "FigS_Cd",
  title_text = "Supplementary Figure | Top 20 pathway candidates associated with Cd-related traits",
  subtitle_text = paste0(
    "Top ", top_n,
    " pathways per panel; * indicates FDR < ", strong_fdr_cutoff,
    "; orange indicates ", strong_fdr_cutoff, " <= FDR < ", moderate_fdr_cutoff
  )
)

message("Done.\nBiomass outputs written to: ", biomass_out_dir,
        "\nCd outputs written to: ", cd_out_dir)
