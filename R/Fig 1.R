# Figure 1: read the raw-data sheet, reproduce calculations/statistics and export the final figure.
# Keep fig1.R and Figure1_data.xlsx in the same folder.
# Run: Rscript /full/path/fig1.R [--output=/output/folder] [--input=/data.xlsx] [--font-scale=1]
# By default the six data sheets are refreshed together; --no-save-data exports figures only.
resolve_fig1_dir <- function() {
 for(fr in rev(sys.frames())) if(!is.null(fr$ofile)&&file.exists(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
 ff<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
 if(length(ff)&&file.exists(ff[1])) return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()) {
  ff<-rstudioapi::getActiveDocumentContext()$path
  if(nzchar(ff)&&file.exists(ff)) return(dirname(normalizePath(ff)))
 }
 stop('Run the saved script using Rscript or source its full path.')
}
fig1_dir<-resolve_fig1_dir();args<-commandArgs(trailingOnly=TRUE)
arg_value<-function(prefix,default){a<-args[startsWith(args,prefix)];if(length(a))substring(a[1],nchar(prefix)+1)else default}
input_file<-arg_value('--input=',file.path(fig1_dir,'Figure1_data.xlsx'))
output_dir<-arg_value('--output=',fig1_dir)
font_scale<-suppressWarnings(as.numeric(arg_value('--font-scale=','1')))
if(length(font_scale)!=1L||!is.finite(font_scale)||font_scale<=0)stop('--font-scale must be a positive number.')
if(!file.exists(input_file))stop('Input not found: ',input_file)
input_file<-normalizePath(input_file);input_hash<-tools::md5sum(input_file)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE);output_dir<-normalizePath(output_dir)
figure_dir<-output_dir
save_workbook<-!'--no-save-data'%in%args
if(!requireNamespace('readxl',quietly=TRUE))stop('Please install the R package readxl.')
if(save_workbook&&!requireNamespace('writexl',quietly=TRUE))stop('Please install the R package writexl.')
# 1. Read all supplied records and validate experimental-record keys.
if(tolower(tools::file_ext(input_file))%in%c('xlsx','xls')) {
 raw<-as.data.frame(readxl::read_excel(input_file,sheet='原始数据',.name_repair='minimal'),check.names=FALSE)
}else raw<-read.csv(input_file,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding='UTF-8-BOM')
required <- c("Treatment", "Rep", "SDW", "SCd")
if (!all(required %in% names(raw))) stop("Missing columns: ", paste(setdiff(required, names(raw)), collapse = ", "))
raw$Treatment <- trimws(as.character(raw$Treatment))
for (v in c("SDW", "SCd")) {
  if (!is.numeric(raw[[v]]) || any(!is.finite(raw[[v]])) || any(raw[[v]] <= 0))
    stop(v, " must contain finite positive values; records are not silently dropped.")
}
if (anyNA(raw$Treatment) || any(raw$Treatment == "") || anyNA(raw$Rep)) stop("Missing treatment/Rep key.")
if (anyDuplicated(raw[c("Treatment", "Rep")])) stop("Treatment + Rep must identify unique input records.")
control <- raw$Treatment == "CK"
if (sum(control) < 2) stop("At least two CK records are needed for the control reference.")
if (!any(!control)) stop("No treatment records.")

# 2. Eq. 2: each supplied treatment record is compared with CK arithmetic means.
# The biomass mean and concentration mean are computed separately, without rounding.
# log(mean(B*C)) is not substituted for log(mean(B)*mean(C)).
B_CK <- mean(raw$SDW[control])
C_CK <- mean(raw$SCd[control])
details <- raw[c("Treatment", "Rep", "SDW", "SCd")]
details$ECd_recomputed <- details$SDW * details$SCd
details$CK_biomass_mean <- B_CK
details$CK_concentration_mean <- C_CK
details$RR_biomass <- details$SDW / B_CK
details$RR_concentration <- details$SCd / C_CK
details$lnRR_biomass <- log(details$RR_biomass)
details$lnRR_concentration <- log(details$RR_concentration)
details$lnRR <- details$lnRR_biomass + details$lnRR_concentration
details$lnRR_product_check <- log(details$ECd_recomputed / (B_CK * C_CK))
stopifnot(max(abs(details$lnRR - details$lnRR_product_check)) < 1e-12)

# 3. Eq. 3: BOTH numerator and denominator use absolute log components.
# Zero/zero is undefined: keep NA and report it, rather than inventing a split.
den <- abs(details$lnRR_biomass) + abs(details$lnRR_concentration)
defined <- den > .Machine$double.eps
details$Biomass_Contrib <- details$Conc_Contrib <- NA_real_
details$Biomass_Contrib[defined] <- 100 * abs(details$lnRR_biomass[defined]) / den[defined]
details$Conc_Contrib[defined] <- 100 * abs(details$lnRR_concentration[defined]) / den[defined]
details$contribution_defined <- defined
details$is_control_reference_record <- control
if (any(defined)) stopifnot(max(abs(details$Biomass_Contrib[defined] + details$Conc_Contrib[defined] - 100)) < 1e-10)

# Retain the input replicate order. Plot rank is recomputed from current mean lnRR.
df_raw <- details[!control, c("Treatment", "Rep", "lnRR", "Biomass_Contrib", "Conc_Contrib")]
means <- tapply(df_raw$lnRR, df_raw$Treatment, mean)
treatment_levels <- names(means)[order(as.numeric(means), names(means))]
df_raw$order <- match(df_raw$Treatment, treatment_levels)
df_raw <- df_raw[c("Treatment", "Rep", "order", "lnRR", "Biomass_Contrib", "Conc_Contrib")]
details$plot_order <- match(details$Treatment, treatment_levels)
details$component_direction_biomass <- ifelse(details$lnRR_biomass < 0, "decrease", ifelse(details$lnRR_biomass > 0, "increase", "unchanged"))
details$component_direction_concentration <- ifelse(details$lnRR_concentration < 0, "decrease", ifelse(details$lnRR_concentration > 0, "increase", "unchanged"))
reference <- data.frame(
  source_file = input_file, source_md5 = unname(tools::md5sum(input_file)),
  n_control_records = sum(control), CK_biomass_mean_g = B_CK,
  CK_concentration_mean_mg_per_kg = C_CK,
  CK_product_of_means_ug = B_CK * C_CK,
  CK_mean_of_products_ug = mean(raw$SDW[control] * raw$SCd[control]),
  Eq2_reference = "Separate CK arithmetic means of SDW and SCd",
  contribution_reference = "100 * abs(log component) / sum(abs(log components))"
)
  # 4. Assumption checks and manuscript-prespecified test branches.
  packages <- c("ggplot2", "patchwork", "scales", "car", "rstatix", "multcompView", "ragg", "svglite")
  missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) stop("Data generated; plotting packages missing: ", paste(missing, collapse = ", "),
                            ". Install them before running the figure step.")
  suppressPackageStartupMessages({library(ggplot2); library(patchwork)})
  statdata <- data.frame(Treatment = factor(df_raw$Treatment, levels = treatment_levels), y = df_raw$lnRR)
  fit <- aov(y ~ Treatment, data = statdata)
  shapiro_p <- shapiro.test(residuals(fit))$p.value
  levene_p <- car::leveneTest(y ~ Treatment, data = statdata, center = median)$`Pr(>F)`[1]
  if (shapiro_p >= .05 && levene_p >= .05) {
    tk <- TukeyHSD(fit, "Treatment")$Treatment
    pairs <- do.call(rbind, lapply(seq_len(nrow(tk)), function(i) {
      g <- strsplit(rownames(tk)[i], "-", fixed = TRUE)[[1]]
      data.frame(group1 = g[1], group2 = g[2], p.adj = tk[i, "p adj"])
    }))
    global_p <- summary(fit)[[1]]$`Pr(>F)`[1]
    method_used <- "one-way ANOVA + Tukey HSD"
  } else if (shapiro_p >= .05) {
    pairs <- as.data.frame(rstatix::games_howell_test(statdata, y ~ Treatment, detailed = TRUE))
    global_p <- oneway.test(y ~ Treatment, statdata, var.equal = FALSE)$p.value
    method_used <- "Welch ANOVA + Games-Howell"
  } else {
    pairs <- as.data.frame(rstatix::dunn_test(statdata, y ~ Treatment, p.adjust.method = "BH", detailed = TRUE))
    global_p <- kruskal.test(y ~ Treatment, data = statdata)$p.value
    method_used <- "Kruskal-Wallis + Dunn (BH)"
  }
  if (any(!is.finite(pairs$p.adj))) stop("Post hoc p values unavailable; inspect variance and record counts.")
  # Assign letters in descending mean order so the highest mean starts at a.
  # Reordering letter names must not change any adjusted pairwise decision.
  descending_levels <- rev(treatment_levels)
  p_matrix <- matrix(1, length(descending_levels), length(descending_levels),
                     dimnames = list(descending_levels, descending_levels))
  for (i in seq_len(nrow(pairs))) {
    p_matrix[pairs$group1[i], pairs$group2[i]] <- pairs$p.adj[i]
    p_matrix[pairs$group2[i], pairs$group1[i]] <- pairs$p.adj[i]
  }
  letters <- multcompView::multcompLetters(p_matrix)$Letters
  for (i in seq_len(nrow(pairs))) {
    shared <- length(intersect(strsplit(letters[pairs$group1[i]], "")[[1]],
                               strsplit(letters[pairs$group2[i]], "")[[1]])) > 0
    stopifnot(shared == (pairs$p.adj[i] >= .05))
  }
  res_a <- do.call(rbind, lapply(treatment_levels, function(t) {
    v <- df_raw$lnRR[df_raw$Treatment == t]
    data.frame(Treatment = t, n = length(v), mean = mean(v), se = sd(v) / sqrt(length(v)), Letter = unname(letters[t]))
  }))
  res_a$Treatment <- factor(res_a$Treatment, levels = treatment_levels)
  res_a$label_y <- ifelse(res_a$mean >= 0, res_a$mean + res_a$se + .055, res_a$mean - res_a$se - .055)
  res_a$display_letter <- res_a$Letter
  res_a$label_vjust <- ifelse(res_a$mean >= 0, 0, 1)
  res_b <- do.call(rbind, lapply(treatment_levels, function(t) {
    v <- df_raw[df_raw$Treatment == t, ]
    nb <- sum(is.finite(v$Biomass_Contrib))
    data.frame(Treatment = t, n_contribution = nb,
               Biomass = if (nb) mean(v$Biomass_Contrib, na.rm = TRUE) else NA_real_,
               Concentration = if (nb) mean(v$Conc_Contrib, na.rm = TRUE) else NA_real_)
  }))
  if (any(res_b$n_contribution == 0)) stop("Contribution plot undefined for an entire treatment.")
  stopifnot(max(abs(res_b$Biomass + res_b$Concentration - 100)) < 1e-10)
  long_b <- rbind(data.frame(Treatment = res_b$Treatment, Source = "Biomass", Value = res_b$Biomass),
                  data.frame(Treatment = res_b$Treatment, Source = "Concentration", Value = res_b$Concentration))
  long_b$Treatment <- factor(long_b$Treatment, levels = treatment_levels)
  long_b$Source <- factor(long_b$Source, levels = c("Biomass", "Concentration"))
  diagnostics <- data.frame(method = method_used, shapiro_residual_p = shapiro_p,
                            levene_median_p = levene_p, global_p = global_p,
                            n_treatment_records = nrow(df_raw), n_groups = length(treatment_levels),
                            note = "SE describes supplied treatment-record variation conditional on CK reference means; replication provenance is not validated by this script.")

  # Original measurements remain unchanged; statistical results are calculated in R.
  metadata<-cbind(diagnostics[,setdiff(names(diagnostics),'note'),drop=FALSE],
                  reference[,setdiff(names(reference),c('source_file','source_md5')),drop=FALSE])
  data_sheets<-list('原始数据'=raw,'运算数据'=details,
                   'Panel_a统计'=res_a,
                   'Panel_b贡献率'=res_b,'两两比较'=pairs,'统计方法与参考'=metadata)
  # 5. Publication layout: treatment response and absolute log-component shares.
  # All treatment records are shown; horizontal offsets separate marks only.
  font_family <- "Arial"
  if (!"Arial" %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial=grDevices::pdfFonts("ArialMT")[[1]])
  plot_records <- df_raw
  plot_records$Treatment <- factor(plot_records$Treatment, levels = treatment_levels)
  plot_records$plot_x <- as.integer(plot_records$Treatment) +
    ave(seq_len(nrow(plot_records)), plot_records$Treatment,
        FUN = function(i) if (length(i) == 1) 0 else seq(-.15, .15, length.out = length(i)))
  plot_a_data <- res_a
  lower_envelope <- vapply(treatment_levels, function(t) min(plot_records$lnRR[plot_records$Treatment == t]), numeric(1))
  upper_envelope <- vapply(treatment_levels, function(t) max(plot_records$lnRR[plot_records$Treatment == t]), numeric(1))
  plot_a_data$label_y <- ifelse(plot_a_data$mean < 0,
                              pmin(lower_envelope, plot_a_data$mean - plot_a_data$se) - .055,
                              pmax(upper_envelope, plot_a_data$mean + plot_a_data$se) + .055)
  yrange <- range(c(plot_records$lnRR, plot_a_data$label_y, 0))
  ylim <- c(yrange[1] - .14, yrange[2] + .14)
  axis_colour <- "#30363A"
  common <- theme_classic(base_size = 7 * font_scale, base_family = font_family) +
    theme(panel.grid = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_text(size = 7.2 * font_scale, margin = margin(r = 5)),
          axis.text = element_text(size = 6.8 * font_scale, colour = axis_colour),
          axis.line = element_line(colour = axis_colour, linewidth = .3),
          axis.ticks = element_line(colour = axis_colour, linewidth = .3),
          axis.ticks.length = grid::unit(1.2, "mm"),
          plot.margin = margin(2, 4, 2, 3),
          legend.text = element_text(size = 6.8 * font_scale, colour = axis_colour),
          legend.title = element_blank(),
          plot.tag = element_text(size = 11, face = "bold", colour = "black"))
  plot_A <- ggplot(plot_a_data, aes(Treatment, mean)) +
    geom_col(fill = "#4DBBD5", colour = NA, width = .70) +
    geom_hline(yintercept = 0, linewidth = .35, colour = axis_colour) +
    geom_point(data = plot_records, aes(x = plot_x, y = lnRR), inherit.aes = FALSE,
               shape = 21, fill = "white", colour = "#52616B", stroke = .22,
               size = 1.0, alpha = .8) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .18,
                  linewidth = .4, colour = "#20272C") +
    geom_errorbar(aes(ymin = mean, ymax = mean), width = .22,
                  linewidth = .5, colour = "#20272C") +
    geom_text(aes(y = label_y, label = display_letter, vjust = label_vjust),
              family = font_family, colour = "#20272C", size = 2.3 * font_scale) +
    scale_y_continuous(breaks = scales::breaks_width(.4),
                       labels = scales::label_number(accuracy = .1),
                       expand = expansion(mult = 0)) +
    coord_cartesian(ylim = ylim, expand = TRUE) +
    scale_x_discrete(drop = FALSE) +
    labs(y = expression("ln("*italic(RR)[total]*")")) + common +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.line.x = element_blank())
  plot_B <- ggplot(long_b, aes(Treatment, Value, fill = Source)) +
    geom_col(position = position_stack(reverse = TRUE), width = .70,
             colour = "white", linewidth = .2) +
    geom_hline(yintercept = 50, linetype = "22", linewidth = .3, colour = "#49525A") +
    scale_fill_manual(values = c(Biomass = "#4DBBD5", Concentration = "#E64B35"),
                       breaks = c("Biomass", "Concentration"),
                       labels = c("Biomass", "Cd concentration")) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100), expand = expansion(mult = 0)) +
    coord_cartesian(ylim = c(0, 100), expand = TRUE) +
    scale_x_discrete(drop = FALSE) +
    labs(y = "Absolute contribution (%)") + common +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 5)),
          axis.text.y = element_text(margin = margin(r = 4)),
          legend.position = "top", legend.justification = "right", legend.direction = "horizontal",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 1, 0),
          legend.key.height = grid::unit(2.6, "mm"),
          legend.key.width = grid::unit(4.0, "mm"))
  final_fig1 <- (plot_A / plot_B) + plot_layout(heights = c(1.2, .8), guides = "keep") +
    plot_annotation(tag_levels = "a",
                    theme = theme(plot.tag = element_text(size = 11, face = "bold", family = font_family)))
  width_mm <- 183; height_mm <- 105

  # R grid measures resolved final plot rectangles before all exports.
  require_patchwork_panel_alignment <- function(plot) {
    probe <- tempfile(fileext = ".pdf")
    if (capabilities("aqua")) grDevices::quartz(type="pdf",file=probe,width=width_mm/25.4,height=height_mm/25.4,family=font_family) else grDevices::pdf(probe, width = width_mm / 25.4, height = height_mm / 25.4, family = font_family, useDingbats = FALSE)
    on.exit({grDevices::dev.off(); unlink(probe)})
    g <- patchwork::patchworkGrob(plot)
    cells <- g$layout[grepl("^panel(?:-[0-9]+)?$", g$layout$name, perl = TRUE), , drop = FALSE]
    cells <- cells[order(cells$t, cells$l), , drop = FALSE]
    grid::grid.newpage(); grid::grid.draw(g); grid::grid.force()
    views <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
    panels <- lapply(seq_len(nrow(cells)), function(i) {
      cell <- cells[i, ]; vp <- views[startsWith(views, paste0(cell$name, "."))]
      stopifnot(length(vp) == 1)
      grid::seekViewport(vp)
      lo <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE)
      hi <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"), valueOnly = TRUE)
      grid::upViewport(0)
      list(id = base::letters[i], bbox_pt = 72 * c(lo$x, lo$y, hi$x, hi$y), grid_id = "patchwork-grid-1",
           row_start = as.integer(cell$t - 1), row_stop = as.integer(cell$b),
           col_start = as.integer(cell$l - 1), col_stop = as.integer(cell$r))
    })
    stopifnot(length(panels) == 2)
    edges <- do.call(rbind, lapply(panels, function(x) x$bbox_pt))
    if (abs(diff(edges[, 1])) > 1.5 || abs(diff(edges[, 3])) > 1.5)
      stop("Panel left/right alignment exceeds 1.5 pt; no figures exported.")
  }
  require_patchwork_panel_alignment(final_fig1)
  # SVG retains editable text; TIFF is the only raster figure export.
  svg_device <- function(filename, width, height, ...) svglite::svglite(filename, width = width, height = height, ...)
  ggsave(file.path(figure_dir, "Figure1.svg"), final_fig1, width = width_mm, height = height_mm,
          units = "mm", device = svg_device)
  ggsave(file.path(figure_dir, "Figure1.tiff"), final_fig1, width = width_mm, height = height_mm,
          units = "mm", device = ragg::agg_tiff, dpi = 600, compression = "lzw", bg = "white")
  stopifnot(identical(input_hash,tools::md5sum(input_file)))
  if(save_workbook) {
    # A temporary file keeps the existing workbook intact if writing fails.
    tmp<-tempfile(pattern='fig1_data_',tmpdir=output_dir,fileext='.xlsx')
    writexl::write_xlsx(data_sheets,tmp)
    if(!file.rename(tmp,file.path(output_dir,'Figure1_data.xlsx')))stop('Could not replace Figure1_data.xlsx. Close it in Excel and retry.')
  }
  message('Figure 1 regenerated. Test: ',method_used)
