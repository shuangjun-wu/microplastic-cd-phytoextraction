# Figure 1 source-data script
# Generates the Fig. 1 panels and associated summary statistics.

required_packages <- c(
  "tidyverse", "agricolae", "patchwork", "scales",
  "car", "FSA", "multcompView"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(agricolae)
library(patchwork)
library(scales)
library(car)
library(FSA)
library(multcompView)

if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}

# -------------------------
# 2. Data
# -------------------------
data_dir <- getwd()
out_dir <- file.path(data_dir, "figure_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_candidates <- c("Source_Data_Fig1.csv", "fig1.csv", "fig1.CSV")
data_file <- data_candidates[file.exists(file.path(data_dir, data_candidates))][1]
if (is.na(data_file)) {
  stop("Input data file not found. Expected Source_Data_Fig1.csv or fig1.csv in the working directory.")
}
df_raw <- read.csv(file.path(data_dir, data_file), header = TRUE)

required_cols <- c("Treatment", "lnRR", "Biomass_Contrib", "Conc_Contrib")
missing_cols <- setdiff(required_cols, colnames(df_raw))
if(length(missing_cols) > 0){
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Use the order column to define treatment order when available.
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

# -------------------------
# 3. Statistics function
# -------------------------
get_stats_letters_fig1 <- function(data, variable, trt_levels) {
  df <- data %>%
    select(Treatment, all_of(variable)) %>%
    drop_na()
  
  df$Treatment <- factor(df$Treatment, levels = trt_levels)
  
  # One-way model for diagnostics.
  fit <- aov(as.formula(paste(variable, "~ Treatment")), data = df)
  
  # Normality test based on ANOVA residuals.
  shapiro_p <- shapiro.test(residuals(fit))$p.value
  
  # Homogeneity of variance test.
  levene_p <- car::leveneTest(
    as.formula(paste(variable, "~ Treatment")),
    data = df
  )$`Pr(>F)`[1]
  
  if (shapiro_p >= 0.05 && levene_p >= 0.05) {
    # Parametric test: ANOVA followed by Tukey HSD.
    aov_res <- aov(as.formula(paste(variable, "~ Treatment")), data = df)
    hsd <- agricolae::HSD.test(aov_res, "Treatment", group = TRUE)
    
    letter_df <- hsd$groups %>%
      rownames_to_column("Treatment") %>%
      select(Treatment, groups) %>%
      rename(Letter = groups)
    
    method_used <- "one-way ANOVA + Tukey HSD"
  } else {
    # Non-parametric test: Kruskal-Wallis followed by Dunn test with BH correction.
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
      # Place labels above positive bars and below negative bars.
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

# -------------------------
# 4. Panel A
# -------------------------
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
    family = "Arial",
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
    text = element_text(family = "Arial"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, face = "bold"),
    axis.text.y  = element_text(size = 13.5, color = "black", face = "bold"),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
    plot.margin  = margin(t = 4, r = 10, b = -2, l = 10)
  )

# -------------------------
# 5. Panel B
# Panel B is displayed as a decomposition result; no additional significance letters are added.
# -------------------------
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

# Biomass is shown at the bottom and Cd concentration at the top.
res_b_summary$Source <- factor(res_b_summary$Source, levels = c("Biomass", "Concentration"))

plot_B <- ggplot(res_b_summary, aes(x = Treatment, y = Value, fill = Source)) +
  geom_col(
    position = "stack",
    width = 0.68,
    color = "black",
    linewidth = 0.35
  ) +
  geom_hline(
    yintercept = 50,
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey30"
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
    text = element_text(family = "Arial"),
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

# -------------------------
# 6. Combine
# -------------------------
final_fig1 <- plot_A / plot_B +
  plot_layout(heights = c(0.79, 1.0)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(face = "bold", size = 18, family = "Arial")
  )

# -------------------------
# 7. Export
# -------------------------
ggsave(
  file.path(out_dir, "Figure1.tiff"),
  final_fig1,
  width = 8.8,
  height = 10.0,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  file.path(out_dir, "Figure1.png"),
  final_fig1,
  width = 8.8,
  height = 10.0,
  units = "in",
  dpi = 400,
  bg = "white"
)

# Export statistical results.
write.csv(res_a, file.path(out_dir, "Figure1A_stats_letters.csv"), row.names = FALSE)

message("Figure 1 files have been written successfully.")
