# Figure 1: screening and contribution analysis
# Generates log-response ratio and contribution panels.

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

required_packages <- c(
  "tidyverse", "agricolae", "patchwork", "scales",
  "car", "FSA", "multcompView"
)
ensure_packages(required_packages)

base_family <- "Arial"
if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}

# 2. Data
data_dir <- path_from_env("FIG1_DATA_DIR", file.path(repo_dir, "data", "fig1"))
out_dir  <- path_from_env("FIG1_OUT_DIR",  file.path(repo_dir, "outputs", "fig1"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

csv_file <- file.path(data_dir, "fig1.csv")
if (!file.exists(csv_file)) {
  csv_file <- file.path(data_dir, "fig1.CSV")
}
if (!file.exists(csv_file)) stop("Cannot find fig1.csv or fig1.CSV in: ", data_dir)

df_raw <- read.csv(csv_file, header = TRUE)

required_cols <- c("Treatment", "lnRR", "Biomass_Contrib", "Conc_Contrib")
missing_cols <- setdiff(required_cols, colnames(df_raw))
if(length(missing_cols) > 0){
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

if("order" %in% colnames(df_raw)){
  df_raw <- df_raw %>% arrange(order)
  treatment_levels <- unique(df_raw$Treatment)
} else {
  treatment_levels <- df_raw %>%
    group_by(Treatment) %>%
    summarise(tmp_mean = mean(lnRR, na.rm = TRUE), .groups = "drop") %>%
    arrange(tmp_mean) %>%
    pull(Treatment)
}

df_raw$Treatment <- factor(df_raw$Treatment, levels = treatment_levels)

# 3. Statistics function
get_stats_letters_fig1 <- function(data, variable, trt_levels) {
  df <- data %>%
    select(Treatment, all_of(variable)) %>%
    drop_na()
  
  df$Treatment <- factor(df$Treatment, levels = trt_levels)
  
  fit <- aov(as.formula(paste(variable, "~ Treatment")), data = df)
  
  shapiro_p <- shapiro.test(residuals(fit))$p.value
  
  levene_p <- car::leveneTest(
    as.formula(paste(variable, "~ Treatment")),
    data = df
  )$`Pr(>F)`[1]
  
  if (shapiro_p >= 0.05 && levene_p >= 0.05) {
    aov_res <- aov(as.formula(paste(variable, "~ Treatment")), data = df)
    hsd <- agricolae::HSD.test(aov_res, "Treatment", group = TRUE)
    
    letter_df <- hsd$groups %>%
      rownames_to_column("Treatment") %>%
      select(Treatment, groups) %>%
      rename(Letter = groups)
    
    method_used <- "one-way ANOVA + Tukey HSD"
  } else {
    dunn <- FSA::dunnTest(
      as.formula(paste(variable, "~ Treatment")),
      data = df,
      method = "bh"
    )$res
    
    pvals <- dunn$P.adj
    names(pvals) <- gsub(" - ", "-", dunn$Comparison)
    
    letters_raw <- multcompView::multcompLetters(pvals)$Letters
    
    letter_df <- tibble(
      Treatment = names(letters_raw),
      Letter = unname(letters_raw)
    )
    
    method_used <- "Kruskal-Wallis + Dunn test (BH-adjusted)"
  }
  
  stats <- df %>%
    group_by(Treatment) %>%
    summarise(
      n = n(),
      mean = mean(.data[[variable]], na.rm = TRUE),
      se   = sd(.data[[variable]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    left_join(letter_df, by = "Treatment") %>%
    mutate(
      Treatment = factor(Treatment, levels = trt_levels),
      label_y = ifelse(mean >= 0,
                       mean + se + 0.05,
                       mean - se - 0.05)
    ) %>%
    arrange(Treatment)
  
  attr(stats, "method_used") <- method_used
  attr(stats, "shapiro_p") <- shapiro_p
  attr(stats, "levene_p") <- levene_p
  
  return(stats)
}

# 4. Panel A
res_a <- get_stats_letters_fig1(df_raw, "lnRR", treatment_levels)

cat("Panel A method used:", attr(res_a, "method_used"), "\n")
cat("Shapiro p =", attr(res_a, "shapiro_p"), "\n")
cat("Levene p  =", attr(res_a, "levene_p"), "\n")

plot_A <- ggplot(res_a, aes(x = Treatment, y = mean)) +
  geom_col(
    fill = "#C99700",
    width = 0.68,
    color = "black",
    linewidth = 0.35
  ) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.16,
    linewidth = 0.35,
    color = "grey35"
  ) +
  geom_text(
    aes(y = label_y, label = Letter),
    family = base_family,
    fontface = "bold",
    size = 4.2
  ) +
  geom_hline(yintercept = 0, linewidth = 0.55, color = "black") +
  scale_y_continuous(
    limits = c(-1.32, 0.28),
    breaks = seq(-1.2, 0.2, 0.2),
    labels = label_number(accuracy = 0.1),
    expand = c(0, 0)
  ) +
  labs(y = expression(bold(ln(italic(RR)[total])))) +
  theme_bw() +
  theme(
    text = element_text(family = base_family),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, face = "bold"),
    axis.text.y  = element_text(size = 13.5, color = "black", face = "bold"),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
    plot.margin  = margin(t = 4, r = 10, b = -2, l = 10)
  )

# 5. Panel B
res_b_summary <- df_raw %>%
  group_by(Treatment) %>%
  summarise(
    Biomass = mean(Biomass_Contrib, na.rm = TRUE),
    Concentration = mean(Conc_Contrib, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total = Biomass + Concentration,
    Biomass = 100 * Biomass / total,
    Concentration = 100 * Concentration / total,
    Treatment = factor(Treatment, levels = treatment_levels)
  ) %>%
  select(-total) %>%
  pivot_longer(
    cols = c(Biomass, Concentration),
    names_to = "Source",
    values_to = "Value"
  )

res_b_summary$Source <- factor(res_b_summary$Source, levels = c("Biomass", "Concentration"))

plot_B <- ggplot(res_b_summary, aes(x = Treatment, y = Value, fill = Source)) +
  geom_col(
    position = "stack",
    width = 0.68,
    color = "black",
    linewidth = 0.35
  ) +
  scale_fill_manual(
    values = c("Biomass" = "#2C7FB8", "Concentration" = "#E67600"),
    breaks = c("Concentration", "Biomass"),
    labels = c("Cd concentration", "Biomass")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 101),
    breaks = seq(0, 100, 20)
  ) +
  labs(y = "Contribution (%)") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_bw() +
  theme(
    text = element_text(family = base_family),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, face = "bold"),
    axis.text.y  = element_text(size = 13.5, color = "black", face = "bold"),
    axis.text.x  = element_text(
      size = 12.5, color = "black", face = "bold",
      angle = 45, hjust = 1, vjust = 1
    ),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13, face = "bold"),
    legend.key.size = grid::unit(0.45, "cm"),
    legend.spacing.x = grid::unit(0.20, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
    plot.margin = margin(t = -2, r = 10, b = 8, l = 10)
  )

# 6. Combine
combined_fig1 <- plot_A / plot_B +
  plot_layout(heights = c(0.79, 1.0)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(face = "bold", size = 18, family = base_family)
  )

# 7. Export
ggsave(
  file.path(out_dir, "fig1_screening.tiff"),
  combined_fig1,
  width = 8.8,
  height = 10.0,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  file.path(out_dir, "fig1_screening.png"),
  combined_fig1,
  width = 8.8,
  height = 10.0,
  units = "in",
  dpi = 400,
  bg = "white"
)

write.csv(res_a, file.path(out_dir, "Figure1A_stats_letters.csv"), row.names = FALSE)

message("Figure 1 outputs written to: ", out_dir)
