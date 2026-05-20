# Figure 5a: multilayer association network
# Builds a representative association network linking soil, microbial, transcriptomic and plant variables.

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

  pkgs <- c("readr", "dplyr", "stringr", "purrr", "tidyr", "tibble", "readxl", "ggplot2")
  ensure_packages(pkgs)
})

options(stringsAsFactors = FALSE)
setTimeLimit(cpu = Inf, elapsed = Inf, transient = TRUE)

# User settings
input_dir <- path_from_env("FIG5A_DATA_DIR", file.path(repo_dir, "data", "fig5", "panel_a"))
out_dir   <- path_from_env("FIG5A_OUT_DIR",  file.path(repo_dir, "outputs", "fig5", "panel_a"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nomination_p_cutoff <- 0.05
nomination_absrho_cutoff <- 0.60
fdr_report_cutoff <- 0.05

# Number of representative nodes per layer
top_soil_n <- 5
top_mc_n <- 3
top_mf_n <- 3
top_tp_n <- 3
min_pathway_genes <- 10

# Plant endpoints
plant_endpoints <- c(
  "Plant: SDW",
  "Plant: RDW",
  "Plant: SCd"
)

# Input files
merged_candidates  <- c("merged_class_kegg_soil_plant_for_correlation.csv")
expr_candidates    <- c("gene_expression.xlsx")
anno_candidates    <- c("gene_annotation.xlsx")
pathway_candidates <- c("biomass_union_pathway_selection_alltop20.csv")

# Helpers
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

safe_spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 5 || length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(tibble(rho = NA_real_, p_value = NA_real_, n = length(x)))
  }
  ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  tibble(rho = unname(ct$estimate), p_value = ct$p.value, n = length(x))
}

expr_to_sample_original <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "^CKL_\\d+$")  ~ stringr::str_replace(x, "^CKL_(\\d+)$", "CKS_\\1"),
    stringr::str_detect(x, "^PEL_\\d+$")  ~ stringr::str_replace(x, "^PEL_(\\d+)$", "PES_\\1"),
    stringr::str_detect(x, "^PVCL_\\d+$") ~ stringr::str_replace(x, "^PVCL_(\\d+)$", "PVCS_\\1"),
    TRUE ~ NA_character_
  )
}

clean_pathway <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_replace(x, "\\)+$", "")
  dplyr::if_else(x %in% c("", "------", "NA", NA), NA_character_, x)
}

compute_pathway_score <- function(expr_log_mat, genes, min_genes = 10) {
  genes <- intersect(genes, rownames(expr_log_mat))
  if (length(genes) < min_genes) return(NULL)
  sub <- expr_log_mat[genes, , drop = FALSE]
  sds <- apply(sub, 1, stats::sd, na.rm = TRUE)
  sub <- sub[sds > 0 & is.finite(sds), , drop = FALSE]
  if (nrow(sub) < min_genes) return(NULL)
  z <- t(scale(t(sub)))
  score <- colMeans(z, na.rm = TRUE)
  tibble(sample_original = names(score), score = as.numeric(score), n_genes = nrow(sub))
}

classify_tp_axis <- function(pathway) {
  x <- stringr::str_to_lower(pathway)
  dplyr::case_when(
    stringr::str_detect(x, "photosynthesis|antenna proteins|nitrogen metabolism|oxidative phosphorylation|citrate cycle|tca cycle|carbon fixation") ~ "primary_assimilation",
    stringr::str_detect(x, "fatty acid|glycerophospholipid|glycerolipid|carotenoid|monoterpenoid|diterpenoid|sesquiterpenoid|triterpenoid|ubiquinone|biotin metabolism") ~ "lipid_terpenoid",
    stringr::str_detect(x, "glutathione|sulfur metabolism|taurine and hypotaurine|porphyrin metabolism|folate biosynthesis|alanine, aspartate and glutamate metabolism") ~ "redox_detox",
    stringr::str_detect(x, "dna replication|cell cycle") ~ "cell_cycle_growth",
    stringr::str_detect(x, "phenylpropanoid|pentose and glucuronate|propanoate metabolism|flavonoid biosynthesis") ~ "cell_wall_secondary",
    stringr::str_detect(x, "spliceosome|protein processing in endoplasmic reticulum|proteasome|protein export|ribosome biogenesis|n-glycan|rna degradation|mrna surveillance|basal transcription factors") ~ "rna_proteostasis",
    TRUE ~ NA_character_
  )
}

pretty_label <- function(x) {
  manual <- c(
    "Soil: STC" = "Total carbon",
    "Soil: SAHN" = "Alkali-hydrolyzable nitrogen",
    "Soil: SDOC" = "Dissolved organic carbon",
    "Soil: S-UE" = "Urease activity",
    "Soil: SAK" = "Available potassium",
    "Soil: SACd" = "Available Cd",
    "Soil: S-ACP" = "Acid phosphatase activity",
    "Soil: S-β-GC" = "Beta-glucosidase activity",
    "Soil: STP" = "Total phosphorus",
    "Soil: STK" = "Total potassium",
    "Soil: STN" = "Total nitrogen",
    "Soil: SAP" = "Available phosphorus",
    "Soil: pH" = "pH",
    "Soil: SCd" = "Total Cd",
    "Plant: SDW" = "Shoot dry weight",
    "Plant: RDW" = "Root dry weight",
    "Plant: SCd" = "Shoot Cd"
  )
  out <- unname(manual[x])
  need_fallback <- is.na(out)
  if (any(need_fallback)) {
    tmp <- x[need_fallback]
    tmp <- stringr::str_replace(tmp, "^Soil: ", "")
    tmp <- stringr::str_replace(tmp, "^Class: ", "")
    tmp <- stringr::str_replace(tmp, "^Function: ", "")
    tmp <- stringr::str_replace(tmp, "^Pathway: ", "")
    tmp <- stringr::str_replace(tmp, "^Plant: ", "")
    tmp <- stringr::str_replace_all(tmp, "_", " ")
    tmp <- stringr::str_replace(tmp, "^unclassified candidate division ", "Unclassified candidate division ")
    tmp <- stringr::str_replace(tmp, "^unclassified Candidatus ", "Unclassified Candidatus ")
    tmp <- stringr::str_replace(tmp, "^candidate division ", "Candidate division ")
    tmp <- stringr::str_replace(tmp, "^candidatus ", "Candidatus ")
    out[need_fallback] <- tmp
  }
  out
}

node_layer <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "^Soil: ") ~ "soil",
    stringr::str_detect(x, "^Class: ") ~ "microbe_class",
    stringr::str_detect(x, "^Function: ") ~ "microbe_function",
    stringr::str_detect(x, "^Pathway: ") ~ "transcriptome_pathway",
    stringr::str_detect(x, "^Plant: ") ~ "plant_trait",
    TRUE ~ "other"
  )
}

node_subtype <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "^Soil: ") ~ "soil_variable",
    stringr::str_detect(x, "^Class: ") ~ "class_biomarker",
    stringr::str_detect(x, "^Function: ") ~ "function_biomarker",
    stringr::str_detect(x, "^Pathway: ") ~ "pathway_score",
    stringr::str_detect(x, "^Plant: ") ~ "plant_trait",
    TRUE ~ "other"
  )
}

layer_prefix <- function(layer) {
  dplyr::case_when(
    layer == "soil" ~ "S",
    layer == "microbe_class" ~ "MC",
    layer == "microbe_function" ~ "MF",
    layer == "transcriptome_pathway" ~ "TP",
    layer == "plant_trait" ~ "P",
    TRUE ~ "X"
  )
}

nominate_nodes <- function(data_tbl, from_cols, to_cols, top_n = 3, exclude_regex = NULL) {
  if (length(from_cols) == 0 || length(to_cols) == 0) {
    return(list(summary = tibble(), selected = character(), edges = tibble()))
  }

  from_keep <- from_cols
  if (!is.null(exclude_regex)) {
    from_keep <- from_keep[!stringr::str_detect(from_keep, exclude_regex)]
  }

  edge_tbl <- tidyr::expand_grid(from = from_keep, to = to_cols) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      stat = list(safe_spearman(data_tbl[[from]], data_tbl[[to]])),
      rho = stat$rho,
      p_value = stat$p_value,
      n = stat$n
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      abs_effect = abs(rho),
      padj = p.adjust(p_value, method = "BH"),
      keep_nomination = !is.na(p_value) & p_value < nomination_p_cutoff & abs_effect >= nomination_absrho_cutoff
    )

  sig_tbl <- edge_tbl %>% dplyr::filter(keep_nomination)
  if (nrow(sig_tbl) == 0) {
    return(list(summary = tibble(), selected = character(), edges = edge_tbl))
  }

  summary_tbl <- sig_tbl %>%
    dplyr::group_by(from) %>%
    dplyr::summarise(
      endpoint_link_count = dplyr::n_distinct(to),
      endpoint_fdr_supported = sum(!is.na(padj) & padj < fdr_report_cutoff),
      max_abs_effect = max(abs_effect, na.rm = TRUE),
      min_p_value = min(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(endpoint_link_count),
      dplyr::desc(endpoint_fdr_supported),
      dplyr::desc(max_abs_effect),
      min_p_value,
      from
    ) %>%
    dplyr::slice_head(n = top_n)

  list(summary = summary_tbl, selected = summary_tbl$from, edges = edge_tbl)
}

calc_edges <- function(data_tbl, from_cols, to_cols, family_name) {
  if (length(from_cols) == 0 || length(to_cols) == 0) return(tibble())

  tidyr::expand_grid(from = from_cols, to = to_cols) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      stat = list(safe_spearman(data_tbl[[from]], data_tbl[[to]])),
      rho = stat$rho,
      p_value = stat$p_value,
      n = stat$n
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      edge_type = family_name,
      method = "Spearman",
      padj = p.adjust(p_value, method = "BH"),
      abs_effect = abs(rho),
      direction = dplyr::case_when(
        rho > 0 ~ "positive",
        rho < 0 ~ "negative",
        TRUE ~ NA_character_
      ),
      keep_edge = !is.na(p_value) & p_value < nomination_p_cutoff & abs_effect >= nomination_absrho_cutoff,
      support_level = dplyr::case_when(
        !is.na(padj) & padj < fdr_report_cutoff ~ "FDR-supported",
        keep_edge ~ "raw-significant",
        TRUE ~ "not_kept"
      )
    ) %>%
    dplyr::filter(keep_edge)
}

# Read input files
search_dirs <- unique(c(input_dir, getwd(), file.path(repo_dir, "data")))
merged_file  <- find_first_existing(search_dirs, merged_candidates,  TRUE, "merged file")
expr_file    <- find_first_existing(search_dirs, expr_candidates,    TRUE, "expression file")
anno_file    <- find_first_existing(search_dirs, anno_candidates,    TRUE, "annotation file")
pathway_file <- find_first_existing(search_dirs, pathway_candidates, TRUE, "pathway selection file")

message("Using merged file: ", merged_file)
message("Using expression file: ", expr_file)
message("Using annotation file: ", anno_file)
message("Using pathway selection file: ", pathway_file)

merged_df <- readr::read_csv(merged_file, show_col_types = FALSE)

missing_endpoints <- setdiff(plant_endpoints, names(merged_df))
if (length(missing_endpoints) > 0) {
  stop("These plant endpoints were not found in merged_df: ", paste(missing_endpoints, collapse = ", "))
}

soil_cols  <- names(merged_df)[stringr::str_detect(names(merged_df), "^Soil: ")]
class_cols <- names(merged_df)[stringr::str_detect(names(merged_df), "^Class: ")]
func_cols  <- names(merged_df)[stringr::str_detect(names(merged_df), "^Function: ")]

# 1) Nominate class and function nodes from plant endpoints
func_exclude_regex <- stringr::regex(
  "yeast|animal|human|mucin|wnt|axon guidance|phototransduction|mitophagy|carcinogenesis|insulin resistance",
  ignore_case = TRUE
)

mc_nom <- nominate_nodes(merged_df, class_cols, plant_endpoints, top_n = top_mc_n)
mf_nom <- nominate_nodes(merged_df, func_cols, plant_endpoints, top_n = top_mf_n, exclude_regex = func_exclude_regex)

selected_mc <- mc_nom$selected
selected_mf <- mf_nom$selected

readr::write_csv(mc_nom$summary, file.path(out_dir, "fig5a_mc_nomination.csv"))
readr::write_csv(mf_nom$summary, file.path(out_dir, "fig5a_mf_nomination.csv"))

# 2) Pathway scoring from shoot expression
expr_raw <- readxl::read_excel(expr_file)
anno_raw <- readxl::read_excel(anno_file)
pathway_sel_raw <- readr::read_csv(pathway_file, show_col_types = FALSE)

names(expr_raw)[1] <- "GeneID"
shoot_expr_cols <- names(expr_raw)[stringr::str_detect(names(expr_raw), "^(CKL|PEL|PVCL)_\\d+$")]
if (length(shoot_expr_cols) == 0) stop("No shoot expression columns matched ^(CKL|PEL|PVCL)_\\d+$")

sample_map <- tibble::tibble(expr_col = shoot_expr_cols) %>%
  dplyr::mutate(sample_original = expr_to_sample_original(expr_col)) %>%
  dplyr::filter(!is.na(sample_original), sample_original %in% merged_df$sample_original)

if (nrow(sample_map) == 0) stop("No expression samples matched merged sample_original IDs.")

expr_log <- expr_raw %>%
  dplyr::select(GeneID, dplyr::all_of(sample_map$expr_col)) %>%
  dplyr::mutate(dplyr::across(-GeneID, ~ as.numeric(.x))) %>%
  dplyr::distinct(GeneID, .keep_all = TRUE)

expr_mat <- as.matrix(expr_log[, -1, drop = FALSE])
rownames(expr_mat) <- expr_log$GeneID
expr_mat <- log2(expr_mat + 1)
colnames(expr_mat) <- sample_map$sample_original[match(colnames(expr_mat), sample_map$expr_col)]

names(anno_raw)[1] <- "GeneID"
if (!"Pathway_definition" %in% names(anno_raw)) stop("Pathway_definition column not found in annotation file.")

anno_path <- anno_raw %>%
  dplyr::transmute(
    GeneID = as.character(GeneID),
    Pathway_definition = as.character(Pathway_definition)
  ) %>%
  dplyr::filter(!is.na(Pathway_definition), Pathway_definition != "------") %>%
  tidyr::separate_rows(Pathway_definition, sep = ";+") %>%
  dplyr::mutate(Pathway_definition = clean_pathway(Pathway_definition)) %>%
  dplyr::filter(!is.na(Pathway_definition)) %>%
  dplyr::distinct()

pathway_gene_list <- split(anno_path$GeneID, anno_path$Pathway_definition)

pathway_candidates_tbl <- pathway_sel_raw %>%
  dplyr::mutate(
    PathwayLabel = as.character(PathwayLabel),
    Axis = classify_tp_axis(PathwayLabel),
    ShootPanels = stringr::str_count(Panels, stringr::fixed("Shoot |"))
  ) %>%
  dplyr::filter(!is.na(Axis), ShootPanels >= 1)

pathway_score_long <- purrr::map_dfr(pathway_candidates_tbl$PathwayLabel, function(pw) {
  genes <- unique(pathway_gene_list[[pw]])
  if (length(genes) == 0) return(tibble())
  sc <- compute_pathway_score(expr_mat, genes, min_genes = min_pathway_genes)
  if (is.null(sc)) return(tibble())
  meta <- pathway_candidates_tbl %>% dplyr::filter(PathwayLabel == pw) %>% dplyr::slice(1)
  sc %>%
    dplyr::mutate(
      pathway = pw,
      axis = meta$Axis,
      ShootPanels = meta$ShootPanels,
      Recurrence = meta$Recurrence,
      MeanAbsNES = meta$MeanAbsNES
    )
})

if (nrow(pathway_score_long) == 0) stop("No pathway scores could be computed. Check expression / annotation / pathway candidate files.")
readr::write_csv(pathway_score_long, file.path(out_dir, "fig5a_tp_scores.csv"))

# Pathway-plant endpoint associations
pw_endpoint_edges <- tidyr::expand_grid(pathway = unique(pathway_score_long$pathway), to = plant_endpoints) %>%
  purrr::pmap_dfr(function(pathway, to) {
    sc <- pathway_score_long %>%
      dplyr::filter(pathway == !!pathway) %>%
      dplyr::select(sample_original, score)
    dat <- merged_df %>%
      dplyr::select(sample_original, dplyr::all_of(to)) %>%
      dplyr::left_join(sc, by = "sample_original")
    st <- safe_spearman(dat$score, dat[[to]])
    tibble(pathway = pathway, to = to, rho = st$rho, p_value = st$p_value, n = st$n)
  }) %>%
  dplyr::mutate(
    abs_effect = abs(rho),
    padj = p.adjust(p_value, method = "BH"),
    keep_nomination = !is.na(p_value) & p_value < nomination_p_cutoff & abs_effect >= nomination_absrho_cutoff
  ) %>%
  dplyr::left_join(
    pathway_score_long %>%
      dplyr::group_by(pathway) %>%
      dplyr::summarise(
        axis = dplyr::first(axis),
        n_genes = dplyr::first(n_genes),
        ShootPanels = dplyr::first(ShootPanels),
        Recurrence = dplyr::first(Recurrence),
        MeanAbsNES = dplyr::first(MeanAbsNES),
        .groups = "drop"
      ),
    by = "pathway"
  )

nominated_pathways <- pw_endpoint_edges %>%
  dplyr::filter(keep_nomination)

selected_tp <- character()
pathway_stats <- tibble()

if (nrow(nominated_pathways) > 0) {

  pathway_connectivity <- purrr::map_dfr(unique(nominated_pathways$pathway), function(pw) {
    sc <- pathway_score_long %>%
      dplyr::filter(pathway == pw) %>%
      dplyr::select(sample_original, score)
    dat <- merged_df %>% dplyr::left_join(sc, by = "sample_original")

    count_links <- function(cols) {
      if (length(cols) == 0) return(tibble(link_count = 0L, best_p = NA_real_, best_abs = NA_real_))
      out <- purrr::map_dfr(cols, function(v) {
        st <- safe_spearman(dat$score, dat[[v]])
        tibble(var = v, rho = st$rho, p_value = st$p_value, abs_effect = abs(st$rho))
      }) %>%
        dplyr::filter(!is.na(p_value), p_value < nomination_p_cutoff, abs_effect >= nomination_absrho_cutoff)

      if (nrow(out) == 0) return(tibble(link_count = 0L, best_p = NA_real_, best_abs = NA_real_))
      tibble(link_count = nrow(out), best_p = min(out$p_value, na.rm = TRUE), best_abs = max(out$abs_effect, na.rm = TRUE))
    }

    plant_support <- count_links(plant_endpoints)
    mc_support <- count_links(selected_mc)
    mf_support <- count_links(selected_mf)
    soil_support <- count_links(soil_cols)

    tibble(
      pathway = pw,
      plant_link_count = plant_support$link_count,
      mc_link_count = mc_support$link_count,
      mf_link_count = mf_support$link_count,
      soil_link_count = soil_support$link_count,
      total_support = plant_support$link_count + mc_support$link_count + mf_support$link_count + soil_support$link_count,
      upstream_support = mc_support$link_count + mf_support$link_count + soil_support$link_count
    )
  })

  pathway_stats <- nominated_pathways %>%
    dplyr::group_by(pathway, axis) %>%
    dplyr::summarise(
      endpoint_link_count = dplyr::n_distinct(to),
      endpoint_fdr_supported = sum(!is.na(padj) & padj < fdr_report_cutoff),
      max_abs_effect = max(abs_effect, na.rm = TRUE),
      min_p_value = min(p_value, na.rm = TRUE),
      n_genes = dplyr::first(n_genes),
      ShootPanels = dplyr::first(ShootPanels),
      Recurrence = dplyr::first(Recurrence),
      MeanAbsNES = dplyr::first(MeanAbsNES),
      .groups = "drop"
    ) %>%
    dplyr::left_join(pathway_connectivity, by = "pathway") %>%
    dplyr::mutate(
      total_support = dplyr::coalesce(total_support, 0L),
      upstream_support = dplyr::coalesce(upstream_support, 0L),
      soil_link_count = dplyr::coalesce(soil_link_count, 0L),
      mc_link_count = dplyr::coalesce(mc_link_count, 0L),
      mf_link_count = dplyr::coalesce(mf_link_count, 0L)
    ) %>%
    dplyr::arrange(
      dplyr::desc(total_support),
      dplyr::desc(upstream_support),
      dplyr::desc(endpoint_link_count),
      min_p_value,
      dplyr::desc(max_abs_effect),
      dplyr::desc(Recurrence),
      dplyr::desc(MeanAbsNES),
      pathway
    )

  selected_axis_reps <- pathway_stats %>%
    dplyr::group_by(axis) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      dplyr::desc(total_support),
      dplyr::desc(upstream_support),
      dplyr::desc(endpoint_link_count),
      min_p_value,
      dplyr::desc(max_abs_effect),
      dplyr::desc(Recurrence),
      dplyr::desc(MeanAbsNES),
      pathway
    ) %>%
    dplyr::slice_head(n = top_tp_n)

  selected_tp <- selected_axis_reps$pathway
}

readr::write_csv(pathway_stats, file.path(out_dir, "fig5a_tp_nomination.csv"))

# 3) Add selected pathway scores to merged table
merged_plus <- merged_df

if (length(selected_tp) > 0) {
  tp_wide <- pathway_score_long %>%
    dplyr::filter(pathway %in% selected_tp) %>%
    dplyr::mutate(path_col = paste0("Pathway: ", pathway)) %>%
    dplyr::select(sample_original, path_col, score) %>%
    tidyr::pivot_wider(names_from = path_col, values_from = score)
  merged_plus <- merged_plus %>% dplyr::left_join(tp_wide, by = "sample_original")
}

tp_cols <- names(merged_plus)[stringr::str_detect(names(merged_plus), "^Pathway: ")]

# 4) Build edge families using one display rule
edge_pre <- dplyr::bind_rows(
  calc_edges(merged_plus, soil_cols, selected_mc, "soil_class"),
  calc_edges(merged_plus, soil_cols, selected_mf, "soil_function"),
  calc_edges(merged_plus, soil_cols, tp_cols, "soil_pathway"),
  calc_edges(merged_plus, selected_mc, tp_cols, "class_pathway"),
  calc_edges(merged_plus, selected_mf, tp_cols, "function_pathway"),
  calc_edges(merged_plus, selected_mc, plant_endpoints, "class_plant"),
  calc_edges(merged_plus, selected_mf, plant_endpoints, "function_plant"),
  calc_edges(merged_plus, tp_cols, plant_endpoints, "pathway_plant")
) %>%
  dplyr::transmute(
    from, to, edge_type, method,
    effect_value = rho, abs_effect, direction,
    p_value, padj, n, support_level
  )

# Compress soil layer to the best-supported 5 nodes
soil_support <- edge_pre %>%
  dplyr::filter(stringr::str_detect(from, "^Soil: ")) %>%
  dplyr::group_by(from) %>%
  dplyr::summarise(
    link_count = dplyr::n_distinct(to),
    fdr_supported_links = sum(!is.na(padj) & padj < fdr_report_cutoff),
    max_abs_effect = max(abs_effect, na.rm = TRUE),
    min_p_value = min(p_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(link_count),
    dplyr::desc(fdr_supported_links),
    dplyr::desc(max_abs_effect),
    min_p_value,
    from
  ) %>%
  dplyr::slice_head(n = top_soil_n)

selected_soil <- soil_support$from

selected_edges <- edge_pre %>%
  dplyr::filter(!stringr::str_detect(from, "^Soil: ") | from %in% selected_soil) %>%
  dplyr::mutate(
    support_source = dplyr::case_when(
      stringr::str_detect(from, "^Pathway: ") | stringr::str_detect(to, "^Pathway: ") ~
        "merged_class_kegg_soil_plant_for_correlation.csv + gene_expression.xlsx + gene_annotation.xlsx + biomass_union_pathway_selection_alltop20.csv",
      TRUE ~ "merged_class_kegg_soil_plant_for_correlation.csv"
    ),
    selection_rule = paste0(
      "Representative association edge: raw Spearman p < ", nomination_p_cutoff,
      " and |rho| >= ", nomination_absrho_cutoff,
      "; BH-adjusted padj exported for reporting"
    ),
    keep = "yes"
  ) %>%
  dplyr::distinct(from, to, edge_type, .keep_all = TRUE)

# 5) Node table
node_names <- sort(unique(c(selected_edges$from, selected_edges$to)))

nodes_tbl <- tibble(node_name = node_names) %>%
  dplyr::mutate(
    display_label = pretty_label(node_name),
    layer = node_layer(node_name),
    subtype = node_subtype(node_name)
  )

node_stats <- purrr::map_dfr(node_names, function(nn) {
  x <- selected_edges %>% dplyr::filter(from == nn | to == nn)
  tibble(
    node_name = nn,
    degree = nrow(x),
    max_abs_effect = max(x$abs_effect, na.rm = TRUE),
    min_p_value = min(x$p_value, na.rm = TRUE),
    min_padj = suppressWarnings(min(x$padj, na.rm = TRUE)),
    support_sources = paste(sort(unique(x$support_source)), collapse = " | "),
    selection_basis = paste(sort(unique(x$selection_rule)), collapse = " || ")
  )
})

nodes_tbl <- nodes_tbl %>%
  dplyr::left_join(node_stats, by = "node_name") %>%
  dplyr::arrange(
    factor(layer, levels = c("soil", "microbe_class", "microbe_function", "transcriptome_pathway", "plant_trait")),
    dplyr::desc(degree),
    min_p_value,
    display_label
  ) %>%
  dplyr::group_by(layer) %>%
  dplyr::mutate(
    node_rank_within_layer = dplyr::row_number(),
    node_id = paste0(layer_prefix(layer), sprintf("%02d", node_rank_within_layer))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(keep = "yes") %>%
  dplyr::select(
    node_id, node_name, display_label, layer, subtype,
    degree, max_abs_effect, min_p_value, min_padj,
    support_sources, selection_basis, keep
  )

id_map <- nodes_tbl %>% dplyr::select(node_id, node_name)

edges_tbl <- selected_edges %>%
  dplyr::left_join(id_map, by = c("from" = "node_name")) %>%
  dplyr::rename(from_id = node_id) %>%
  dplyr::left_join(id_map, by = c("to" = "node_name")) %>%
  dplyr::rename(to_id = node_id) %>%
  dplyr::mutate(edge_id = paste0("E", sprintf("%03d", dplyr::row_number()))) %>%
  dplyr::select(
    edge_id, from_id, to_id, from, to, edge_type, method,
    effect_value, abs_effect, direction, p_value, padj, n,
    support_level, support_source, selection_rule, keep
  )

readr::write_csv(nodes_tbl, file.path(out_dir, "fig5a_nodes.csv"))
readr::write_csv(edges_tbl, file.path(out_dir, "fig5a_edges.csv"))

# 6) Plot
# Display only selected edge families. No arrows: this is an association network.
display_edge_types <- c(
  "soil_class", "soil_function", "soil_pathway",
  "class_pathway", "function_pathway",
  "class_plant", "function_plant", "pathway_plant"
)

edges_plot <- edges_tbl %>% dplyr::filter(edge_type %in% display_edge_types)
keep_ids <- unique(c(edges_plot$from_id, edges_plot$to_id))
nodes_plot <- nodes_tbl %>% dplyr::filter(node_id %in% keep_ids)

# Bottom -> top: soil -> class -> function -> pathway -> plant
# This matches the intended reading order from environment to plant phenotype.
y_soil <- 0.12
y_mc <- 1.58
y_mf <- 2.68
y_tp <- 3.70
y_plant <- 4.90

spread_x <- function(n, left = 0.9, right = 6.4) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return((left + right) / 2)
  seq(left, right, length.out = n)
}

assign_hpos <- function(df, y, preferred = NULL, left = 0.9, right = 6.4) {
  if (nrow(df) == 0) return(tibble())
  out <- df
  if (!is.null(preferred)) {
    out <- out %>%
      dplyr::mutate(order_key = match(node_name, preferred)) %>%
      dplyr::mutate(order_key = ifelse(is.na(order_key), 999 + dplyr::row_number(), order_key)) %>%
      dplyr::arrange(order_key, dplyr::desc(degree), display_label)
  } else {
    out <- out %>% dplyr::arrange(dplyr::desc(degree), display_label)
  }
  out %>% dplyr::mutate(x = spread_x(dplyr::n(), left, right), y = y)
}

soil_pref <- selected_soil
mc_pref <- selected_mc
mf_pref <- selected_mf
tp_pref <- paste0("Pathway: ", selected_tp)
plant_pref <- plant_endpoints

soil_plot <- assign_hpos(nodes_plot %>% dplyr::filter(layer == "soil"), y_soil, soil_pref, left = 0.55, right = 7.25)
if (nrow(soil_plot) > 0) {
  soil_manual_x <- c(0.55, 2.55, 4.35, 5.95, 7.45)
  soil_plot$x <- soil_manual_x[seq_len(nrow(soil_plot))]
}
mc_plot <- assign_hpos(nodes_plot %>% dplyr::filter(layer == "microbe_class"), y_mc, mc_pref, left = 1.35, right = 7.05)
mf_plot <- assign_hpos(nodes_plot %>% dplyr::filter(layer == "microbe_function"), y_mf, mf_pref, left = 1.30, right = 7.05)
tp_plot <- assign_hpos(nodes_plot %>% dplyr::filter(layer == "transcriptome_pathway"), y_tp, tp_pref, left = 1.35, right = 6.95)
plant_plot <- assign_hpos(nodes_plot %>% dplyr::filter(layer == "plant_trait"), y_plant, plant_pref, left = 1.75, right = 6.45)

node_layout <- dplyr::bind_rows(soil_plot, mc_plot, mf_plot, tp_plot, plant_plot) %>%
  dplyr::mutate(
    display_label_wrapped = dplyr::case_when(
      layer == "soil" ~ stringr::str_wrap(display_label, 15),
      layer == "microbe_class" ~ stringr::str_wrap(display_label, 20),
      layer == "microbe_function" ~ stringr::str_wrap(display_label, 22),
      layer == "transcriptome_pathway" ~ stringr::str_wrap(display_label, 24),
      layer == "plant_trait" ~ stringr::str_wrap(display_label, 18),
      TRUE ~ stringr::str_wrap(display_label, 22)
    )
  )

edges_plot <- edges_plot %>%
  dplyr::left_join(node_layout %>% dplyr::select(from_id = node_id, x_from = x, y_from = y), by = "from_id") %>%
  dplyr::left_join(node_layout %>% dplyr::select(to_id = node_id, x_to = x, y_to = y), by = "to_id") %>%
  dplyr::mutate(
    edge_sign = ifelse(direction == "positive", "Positive", "Negative"),
    edge_alpha = ifelse(support_level == "FDR-supported", 0.92, 0.72)
  )

fill_values <- c(
  "soil" = "#D9D9D9",
  "microbe_class" = "#F4B183",
  "microbe_function" = "#FFD966",
  "transcriptome_pathway" = "#9DC3E6",
  "plant_trait" = "#A9D18E"
)

titles <- tibble(
  label = c("Plant traits", "Transcriptome\npathways", "Function-level\nbiomarkers", "Class-level\nbiomarkers", "Soil variables"),
  x = c(-0.08, -0.08, -0.08, -0.08, -0.08),
  y = c(y_plant, y_tp, y_mf, y_mc, y_soil)
)

panel_label <- tibble(
  label = "a",
  x = -1.35,
  y = 5.52
)

p <- ggplot() +
  geom_curve(
    data = edges_plot,
    aes(
      x = x_from, y = y_from, xend = x_to, yend = y_to,
      color = edge_sign, linewidth = abs_effect, alpha = edge_alpha
    ),
    curvature = 0.12,
    lineend = "round"
  ) +
  geom_label(
    data = node_layout,
    aes(x = x, y = y, label = display_label_wrapped, fill = layer),
    label.size = 0.35,
    label.padding = unit(0.18, "lines"),
    label.r = unit(0.12, "lines"),
    size = 3.2
  ) +
  geom_text(
    data = titles,
    aes(x = x, y = y, label = label),
    fontface = "bold", size = 4.0, hjust = 1
  ) +
  geom_text(
    data = panel_label,
    aes(x = x, y = y, label = label),
    fontface = "bold", size = 6.0, hjust = 0
  ) +
  scale_fill_manual(values = fill_values, guide = "none") +
  scale_color_manual(values = c("Positive" = "#D73027", "Negative" = "#4575B4"), name = NULL) +
  scale_linewidth_continuous(range = c(0.55, 2.05), name = "|Effect size|") +
  scale_alpha_identity(guide = "none") +
  coord_cartesian(xlim = c(-1.75, 8.55), ylim = c(-0.10, 5.65), clip = "off") +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    plot.margin = margin(20, 44, 24, 150),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  )

pdf_file <- file.path(out_dir, "fig5a_network.pdf")
png_file <- file.path(out_dir, "fig5a_network.png")

ggsave(pdf_file, p, width = 11.8, height = 8.4, bg = "white")
if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(png_file, p, width = 11.8, height = 8.4, dpi = 300, bg = "white", device = ragg::agg_png)
} else {
  ggsave(png_file, p, width = 11.8, height = 8.4, dpi = 300, bg = "white")
}

message("Done.")
message("Selected soil nodes: ", ifelse(length(selected_soil) > 0, paste(pretty_label(selected_soil), collapse = "; "), "none"))
message("Selected class nodes: ", ifelse(length(selected_mc) > 0, paste(pretty_label(selected_mc), collapse = "; "), "none"))
message("Selected function nodes: ", ifelse(length(selected_mf) > 0, paste(pretty_label(selected_mf), collapse = "; "), "none"))
message("Selected pathway nodes: ", ifelse(length(selected_tp) > 0, paste(selected_tp, collapse = "; "), "none"))
message("Nodes written to: ", file.path(out_dir, "fig5a_nodes.csv"))
message("Edges written to: ", file.path(out_dir, "fig5a_edges.csv"))
message("Figure written to: ", pdf_file)
message("Figure written to: ", png_file)
