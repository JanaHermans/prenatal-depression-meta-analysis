################################################################################
# Title:    Meta-analyses first RQ + Figures 
# Project:  ECCN project 
# Author:   Jana Hermans
# Date:     Created in February 2025
################################################################################
library(metafor)
library(meta)
library(grid)
library(ggplot2)
library(tidyverse)
library(gt)
library(patchwork)
library(dplyr)
library(cowplot)
library(ggpubr)

projectdir <- '/Users/janahermans/Documents/PhD/02_project/'
setwd(file.path(projectdir,"results")) 
outdir <- paste0(projectdir,"/results/plots")
outtable <- paste0(projectdir,"/results/tables")
#-----------------------------------Data prep-----------------------------------
cohort_levels <- c("Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", "Generation R", "NINFEA", "PREDO", "TOTAL")
outcomes <- c('int_', 'ext_', 'adhd_', 'asd_', 'fm_', 'gm_', 'lan_', 'nvi_')
results_list <- list()
ALSPAC_samplesize <- readRDS('./ALSPAC_results/descriptives_ALSPAC.RData')

# Helper function to load and format data
load_results <- function(path, outcome, cohort_name) {
  df <- read.csv(path)
  df <- df[df$outcome == outcome, ]
  df$cohort <- cohort_name
  df <- df[c("cohort", "Estimate", "Std..Error", "samplesize")]
  colnames(df) <- c("cohort", "preg_dep_betas", "ses", "N")
  if (cohort_name == 'alspac') {
    df$N <- ALSPAC_samplesize[[paste0("N_",outcome,"pc_CC")]]
  }
  return(df)
}

for (outcome in outcomes) {
  # Load ECCN results
  MAIN_results <- read.csv(paste0("ECCN/results_", outcome, "main_model_ECCN.csv"))
  names(MAIN_results)[1] <- "cohort"
  colnames(MAIN_results)[colnames(MAIN_results) == "Estimate"] <- "preg_dep_betas"
  colnames(MAIN_results)[colnames(MAIN_results) == "Std..Error"] <- "ses"

  # Determine which cohorts to add
  if (outcome %in% c('int_', 'ext_', 'adhd_')) {
    MAIN_results <- rbind(MAIN_results,
                          load_results("ALSPAC_results/results_fulladj_model_ALSPAC.csv", outcome, "alspac"),
                          load_results("PREDOresultsandscript/results_fulladj_model_PREDO.csv", outcome, "predo"))
  } else if (outcome %in% c('fm_', 'gm_', 'lan_', 'nvi_', 'wm_')) {
    MAIN_results <- rbind(MAIN_results,
                          load_results("ALSPAC_results/results_fulladj_model_ALSPAC.csv", outcome, "alspac"))
  } else if (outcome == 'asd_') {
    MAIN_results <- rbind(MAIN_results,
                          load_results("PREDOresultsandscript/results_fulladj_model_PREDO.csv", outcome, "predo"))
  }
  
  # Final formatting
  MAIN_results$preg_dep_betas <- as.numeric(MAIN_results$preg_dep_betas)
  MAIN_results$ses <- as.numeric(MAIN_results$ses)
  MAIN_results$cohort <- toupper(MAIN_results$cohort)
  if (outcome != "wm_") {
    MAIN_results$cohort[MAIN_results$cohort == "GENR"] <- "Generation R"
  }
  
  # Run meta-analysis
  m.model2 <- rma(yi = MAIN_results$preg_dep_betas, sei = MAIN_results$ses)
  leave1out_res <- leave1out(m.model2)
  coefs <- coef(summary(m.model2))
  
  # Add confidence intervals
  MAIN_results <- MAIN_results %>%
    mutate(conf.low = preg_dep_betas - 1.96 * ses,
           conf.high = preg_dep_betas + 1.96 * ses,
           estimate = preg_dep_betas)
  
  MAIN_results$cohort <- factor(MAIN_results$cohort,
                                levels = c("ABCD", "ALSPAC", "DNBC", "EDEN",
                                           "Generation R", "NINFEA", "PREDO"))
  
  # Add meta-analysis result
  MAIN_results$N <- as.character(MAIN_results$N)
  cohorts_to_sum <- nrow(MAIN_results)
  MAIN_results <- bind_rows(MAIN_results,
                            data.frame(cohort = "TOTAL",
                                       estimate = m.model2$beta,
                                       conf.low = m.model2$ci.lb,
                                       conf.high = m.model2$ci.ub,
                                       N = as.character(sum(
                                         as.numeric(
                                           MAIN_results$N[1:cohorts_to_sum])))
                                       ))
  
  # Store results
  base <- gsub("_", "", outcome)
  results_list[[outcome]] <- MAIN_results
  results_list[[paste0(base, "_I2")]] <- sprintf("%.1f", m.model2$I2)
  results_list[[paste0(base, "_tau2")]] <- sprintf("%.2f", m.model2$tau2)
  results_list[[paste0(base, "_Q_pval")]] <- sprintf("%.2f", m.model2$QEp)
  lv1out <- cbind(as.data.frame(leave1out_res),
                  cohort = MAIN_results$cohort[-nrow(MAIN_results)])
  lv1out$N <- NA
  for (cohort in intersect(cohort_levels, lv1out$cohort)) {
    lv1out[lv1out$cohort==cohort,]$N <- as.numeric(results_list[[outcome]][results_list[[outcome]]$cohort=='TOTAL',]$N) - as.numeric(results_list[[outcome]][results_list[[outcome]]$cohort==cohort,]$N)
  }
  results_list[[paste0(base, "_leave1out")]] <- lv1out
  results_list[[paste0(base, "_pvalue")]] <- coefs$pval
}

# Filter the list to only elements ending with "pvalue"
pval_elements <- results_list[grepl("pvalue$", names(results_list))]

# Convert to a data frame
pval_df <- data.frame(
  outcome = names(pval_elements),
  p_value = as.numeric(pval_elements),
  row.names = NULL
)
pval_df$outcome <- sub("pvalue$", "", pval_df$outcome)

# Apply FDR correction (correcting for the number of outcomes, i.e. nine)
pval_df$p_value_adj <- p.adjust(pval_df$p_value, method = "BH")

# No scientific notation
pval_df$p_value <- format(pval_df$p_value, scientific = FALSE)
pval_df$p_value_adj <- format(pval_df$p_value_adj, scientific = FALSE)

# Print significant outcomes after FDR correction
#pval_df %>%
#  filter(p_value_adj < 0.05) %>%
#  print()

pval_df$p_value <- sprintf("%.5f", as.numeric(pval_df$p_value))
pval_df$p_value_adj <- sprintf("%.5f", as.numeric(pval_df$p_value_adj))

# Save
write.csv(pval_df, 
          paste0(projectdir,"/results/tables/pval_results.csv"),
          row.names = FALSE)

#----------------------------Internalising symptoms-----------------------------
res <- results_list$int_
I2 <- results_list$int_I2
tau2 <- results_list$int_tau2
Q_pval <- results_list$int_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,8), xlim=c(-10, 27)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "PREDO", "TOTAL")

res_plot$N_lab <- ifelse(
  is.na(res_plot$N), 
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""), 
  paste0("N = ", res_plot$N)
)

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))
p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 
p_N <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.12, height = 0.88) +  # Cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.12, height = 0.88) +  # Sample size
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.93) +  # Plot
  draw_plot(p_right, x = 0.53, width = 0.48, y = 0.12, height = 0.88)    # Estimate + CI

p_final_INT <- p_combined +
  plot_annotation(
    title = "Internalising symptoms",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#----------------------------Externalising symptoms-----------------------------
res <- results_list$ext_
I2 <- results_list$ext_I2
tau2 <- results_list$ext_tau2
Q_pval <- results_list$ext_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,8), xlim=c(-10, 27)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

# Sample size label
res_plot$N_lab <- ifelse(
  is.na(res_plot$N), 
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""), 
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "PREDO", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 
layout <- c(
  area(t = 2, l = 0, b = 30, r = 2), 
  area(t = 5, l = 2.5, b = 30, r = 5), 
  area(t = 2, l = 4.7, b = 30, r = 7)
)

# Combine plots
p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.12, height = 0.88) +  # Cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.12, height = 0.88) +  # Sample size
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.93) +  # Plot
  draw_plot(p_right, x = 0.53, width = 0.48, y = 0.12, height = 0.88)    # Estimate + CI

# Final annotated plot
p_final_EXT <- p_combined +
  plot_annotation(
    title = "Externalising symptoms",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#--------------------------------------ADHD-------------------------------------
res <- results_list$adhd_
I2 <- results_list$adhd_I2
tau2 <- results_list$adhd_tau2
Q_pval <- results_list$adhd_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,8), xlim=c(-10, 27)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

# Add sample size label
res_plot$N_lab <- ifelse(
  is.na(res_plot$N),
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""),
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "PREDO", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

# Sample size column
p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 
layout <- c(
  area(t = 2, l = 0, b = 30, r = 2), 
  area(t = 5, l = 2, b = 30, r = 5), 
  area(t = 2, l = 5, b = 30, r = 7)
)

# Combine plots
p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.12, height = 0.88) +  # Cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.12, height = 0.88) +  # Sample size
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.93) +  # Plot
  draw_plot(p_right, x = 0.53, width = 0.48, y = 0.12, height = 0.88)    # Estimate + CI

# Final plot with annotation
p_final_ADHD <- p_combined +
  plot_annotation(
    title = "ADHD symptoms",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#--------------------------------------ASD--------------------------------------
res <- results_list$asd_
I2 <- results_list$asd_I2
tau2 <- results_list$asd_tau2
Q_pval <- results_list$asd_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,3), xlim=c(-10, 27)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

# Add sample size label
res_plot$N_lab <- ifelse(
  is.na(res_plot$N),
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""),
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "PREDO", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

# Sample size column
p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab,
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "royalblue3", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 
layout <- c(
  area(t = 2, l = 0, b = 14, r = 2), 
  area(t = 6, l = 2, b = 14, r = 5), 
  area(t = 2, l = 5, b = 14, r = 7)
)

# Combine plots
p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.20, height = 0.67) +  # Cohorts
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.20, height = 0.67) +  # Sample sizes
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.73) +  # Forest
  draw_plot(p_right, x = 0.53, width = 0.48, y = 0.20, height = 0.67)    # Estimates

# Final annotated plot
p_final_ASD <- p_combined +
  plot_annotation(
    title = "ASD symptoms",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#------------------------------Fine motor skills--------------------------------
res <- results_list$fm_
I2 <- results_list$fm_I2
tau2 <- results_list$fm_tau2
Q_pval <- results_list$fm_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "#B5DCDC", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,6), xlim=c(-12, 15)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

# Add sample size labels
res_plot$N_lab <- ifelse(
  is.na(res_plot$N),
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""),
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

# Sample size panel
p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"),
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 

# Combine panels
p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.14, height = 0.88) +  # Cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.14, height = 0.88) +  # Sample sizes
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.92) +  # Forest plot
  draw_plot(p_right, x = 0.51, width = 0.50, y = 0.14, height = 0.88)    # Estimates

# Final annotated plot
p_final_FM <- p_combined +
  plot_annotation(
    title = "Fine motor skills",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#------------------------------Gross motor skills--------------------------------
res <- results_list$gm_
I2 <- results_list$gm_I2
tau2 <- results_list$gm_tau2
Q_pval <- results_list$gm_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "#B5DCDC", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,6), xlim=c(-12, 15)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

# Add sample size labels
res_plot$N_lab <- ifelse(
  is.na(res_plot$N),
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""),
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

# Sample size panel
p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"),
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 

# Combine panels
p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.14, height = 0.88) +  # Cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.14, height = 0.88) +  # Sample sizes
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.92) +  # Forest plot
  draw_plot(p_right, x = 0.51, width = 0.50, y = 0.14, height = 0.88)    # Estimates

# Final plot with annotations
p_final_GM <- p_combined +
  plot_annotation(
    title = "Gross motor skills",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#--------------------------------Language skills--------------------------------
res <- results_list$lan_
I2 <- results_list$lan_I2
tau2 <- results_list$lan_tau2
Q_pval <- results_list$lan_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "#B5DCDC", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,5), xlim=c(-12, 15)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 

# Add sample size column
res_plot$N_lab <- ifelse(
  is.na(res_plot$N),
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""),
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

# Sample size plot
p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab,
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"),
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 

# Combine all plots
p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.16, height = 0.88) +  # cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.16, height = 0.88) +  # sample sizes
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.94) +  # forest plot
  draw_plot(p_right, x = 0.51, width = 0.50, y = 0.16, height = 0.88)    # estimates

# Annotate final plot
p_final_LAN <- p_combined +
  plot_annotation(
    title = "Language skills",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#-----------------------Non-verbal intelligence skills--------------------------
res <- results_list$nvi_
I2 <- results_list$nvi_I2
tau2 <- results_list$nvi_tau2
Q_pval <- results_list$nvi_Q_pval
p <- 
  res |>
  ggplot(aes(y = fct_rev(cohort))) + 
  theme_classic() +
  # Add background highlight for the "TOTAL" row
  geom_rect(data = res[res$cohort == "TOTAL", ], 
            aes(ymin = as.numeric(fct_rev(cohort)) - 0.5, 
                ymax = as.numeric(fct_rev(cohort)) + 0.5),
            xmin = -Inf, xmax = Inf, fill = "#B5DCDC", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x=estimate, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
             shape=15, size=3) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  guides(color = "none") +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high,
                     color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
                 size = .5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Beta estimates", y="") +
  coord_cartesian(ylim=c(1,5), xlim=c(-12, 15)) +
  scale_x_continuous(n.breaks=10) 

res$estimate <- sprintf("%.2f", res$estimate)
res$conf.low <- sprintf("%.2f", res$conf.low)
res$conf.high <- sprintf("%.2f", res$conf.high)
res$estimate_lab <- paste0(res$estimate, " [", res$conf.low, " — ", res$conf.high, "]")
res_plot <- bind_rows(res, data.frame(
  cohort = "Cohort",
  estimate_lab = "Estimate (95% CI)")
) 


# Add sample size labels (make sure 'N' column exists in your data)
res_plot$N_lab <- ifelse(
  is.na(res_plot$N),
  ifelse(res_plot$cohort == "Cohort", "Sample size", ""),
  paste0("N = ", res_plot$N)
)

res_plot$cohort <- forcats::fct_relevel(res_plot$cohort, "Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", 
                                        "Generation R", "NINFEA", "TOTAL")

p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(cohort))) +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(aes(x = 0, label = cohort, color = ifelse(cohort == "TOTAL", "highlight", "normal")), 
            hjust = 0, 
            fontface = "bold") +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

p_N <- res_plot |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0, y = fct_rev(cohort), label = N_lab,
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$N_lab == "Sample size" | res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

p_right <-
  res_plot  |>
  ggplot() +
  scale_color_manual(values = c("highlight" = "#2AACA9", "normal" = "black")) +
  geom_text(
    aes(x = 0,y = fct_rev(cohort), label = estimate_lab, 
        color = ifelse(cohort == "TOTAL", "highlight", "normal")),
    hjust = 0,
    fontface = ifelse((res_plot$estimate_lab == "Estimate (95% CI)"|res_plot$cohort == "TOTAL"), 
                      "bold", "plain")
  ) +
  guides(color = "none") +
  theme_void() 

p_combined <- ggdraw() +
  draw_plot(p_left,  x = 0.00, width = 0.20, y = 0.16, height = 0.88) +  # cohort names
  draw_plot(p_N,     x = 0.20, width = 0.15, y = 0.16, height = 0.88) +  # sample sizes
  draw_plot(p_mid,   x = 0.35, width = 0.40, y = 0.00, height = 0.94) +  # forest plot
  draw_plot(p_right, x = 0.51, width = 0.50, y = 0.16, height = 0.88)    # estimates

p_final_NVI <- p_combined +
  plot_annotation(
    title = "Non-verbal intelligence",
    subtitle = paste0("Heterogeneity: tau² = ", tau2,
                      "; I² = ", I2, "; Cochran's Q p-value = ", Q_pval),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
    )
  )
#---------------------------------Complete plot---------------------------------

#I) MENTAL HEALTH
p_final_ASD <- p_final_ASD + 
  theme(plot.margin = margin(t = 110,  # Increase space above
                             r = 0,  
                             b = 0,  
                             l = 0))  

p_complete <- ggarrange(p_final_INT, p_final_EXT, p_final_ADHD, p_final_ASD, 
                        labels = c("a", "b", "c", "d"), 
                        ncol=2, nrow=2, common.legend = F) 

p_complete + 
  theme(plot.title = element_text(color = "royalblue3",
                                  face="bold", hjust = 0.5, size = 22,
                                  margin = margin(b = 14)))

ggsave("COMPLETE_MH_0109.tiff", units="in", path = outdir, width = 12.5, height = 8.5, device='tiff', dpi=1000)

dev.off()

#II) COGNITION
p_complete <- ggarrange(p_final_FM, p_final_GM, p_final_LAN, p_final_NVI,
                        labels = c("a", "b", "c", "d"), 
                        ncol=2, nrow=2, 
                        heights = c(1, 0.9),
                        common.legend = F) 

p_complete + 
  theme(plot.title = element_text(color = "#2AACA9",
                                  face="bold", hjust = 0.5, size = 24,
                                  margin = margin(b = 14)))

ggsave("COMPLETE_C_0109.tiff", units="in", path = outdir, width = 12.5, height = 7.5, device='tiff', dpi=700)

dev.off()

#----------------------------Print results (in-text)----------------------------
format_result_beta <- function(results_list, outcome) {
  df <- results_list[[outcome]]
  total_row <- df[df$cohort == "TOTAL", ]
  
  est   <- total_row$estimate
  lower <- total_row$conf.low
  upper <- total_row$conf.high
  
  sprintf("β = %.2f, 95%% CI [%.2f, %.2f];", est, lower, upper)
}

for (outcome in outcomes) {
  result_text <- format_result_beta(results_list, outcome)
  cat(outcome, "→", result_text, "\n")
}

format_full_result <- function(results_list, outcome) {
  i2 <- as.numeric(results_list[[paste0(outcome, "I2")]])
  tau2 <- as.numeric(results_list[[paste0(outcome, "tau2")]])
  q_pval <- as.numeric(results_list[[paste0(outcome, "Q_pval")]])
  sprintf("I² = %.1f%%, τ² = %.2f, Cochran's Q p-value = %.2f",
          i2, tau2, q_pval)
}

for (outcome in outcomes) {
  result_text <- format_full_result(results_list, outcome)
  cat(outcome, "→", result_text, "\n")
}

print_significant_results <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    warning("Input must be a non-empty data frame.")
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(df))) {
    cohort <- df$cohort[i]
    est    <- df$estimate[i]
    lower  <- df$ci.lb[i]
    upper  <- df$ci.ub[i]
    
    if (is.na(est) || is.na(lower) || is.na(upper) || is.na(cohort)) next
    
    # Print only if CI does not contain 0
    if (!(lower <= 0 & upper >= 0)) {
      cat(sprintf("%s: β = %.2f, 95%% CI [%.2f, %.2f];\n", cohort, est, lower, upper))
    }
  }
}

print_significant_results(results_list[["fm_leave1out"]])
print_significant_results(results_list[["gm_leave1out"]])
print_significant_results(results_list[["lan_leave1out"]])
print_significant_results(results_list[["nvi_leave1out"]])

print_non_significant_results <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    warning("Input must be a non-empty data frame.")
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(df))) {
    cohort <- df$cohort[i]
    est    <- df$estimate[i]
    lower  <- df$ci.lb[i]
    upper  <- df$ci.ub[i]
    
    if (is.na(est) || is.na(lower) || is.na(upper) || is.na(cohort)) next
    
    # Print only if CI contains 0 (not significant)
    if (lower <= 0 & upper >= 0) {
      cat(sprintf("%s: β = %.2f, 95%% CI [%.2f, %.2f]; (not significant)\n", cohort, est, lower, upper))
    }
  }
}

print_non_significant_results(results_list[["int_leave1out"]])
print_non_significant_results(results_list[["ext_leave1out"]])
print_non_significant_results(results_list[["adhd_leave1out"]])
print_non_significant_results(results_list[["asd_leave1out"]])

#------------------------Save leave1out results to table------------------------
cohorts <- c("ABCD", "ALSPAC", "DNBC", "EDEN", "Generation R", "NINFEA",
             "PREDO")

format_result <- function(df, cohorts) {
  df <- df %>%
    mutate(
      N = as.character(N),
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", ci.lb), ", ", sprintf("%.2f", ci.ub), "]")
    ) %>%
    select(Cohort = cohort, N, `β`, `95% CI`)
  
  # Merge with master cohort list to ensure all cohorts are present
  master_cohorts <- data.frame(Cohort = cohorts)
  df <- left_join(master_cohorts, df, by = "Cohort")
  
  return(df)
}

combine_results_side_by_side <- function(results_list, outcome_names, cohorts) {
  formatted_dfs <- list()
  
  for (outcome in outcome_names) {
    if (!is.null(results_list[[paste0(outcome,"leave1out")]])) {
      df <- format_result(results_list[[paste0(outcome,"leave1out")]], cohorts)
      formatted_dfs[[outcome]] <- df
    }
  }
  
  # Combine into one wide data frame
  combined_df <- do.call(cbind, lapply(formatted_dfs, function(x) x[, -1]))  # Exclude Cohort after first
  combined_df <- cbind(formatted_dfs[[1]][, 1, drop = FALSE], combined_df)   # Add Cohort column
  
  return(combined_df)
}

create_combined_flextable <- function(combined_df, outcome_names) {
  combined_df <- as.data.frame(combined_df)
  
  ft <- flextable(combined_df)
  
  # Clear base header labels
  ft <- set_header_labels(ft, values = setNames(rep("", ncol(combined_df)), names(combined_df)))
  
  # Adjust colwidths for β, CI, N per outcome
  colwidths <- c(1, rep(3, length(outcome_names)))  # 3 columns per outcome
  ft <- add_header_row(ft, values = c("Cohort", outcome_names), colwidths = colwidths)
  
  ft <- ft %>%
    fontsize(size = 7.5, part = "all") %>%
    set_table_properties(layout = "autofit")
  
  return(ft)
}

write_combined_results_to_word <- function(results_list, filename = "combined_results.docx", cohorts) {
  outcome_names <- c("int_", "ext_", "adhd_", "asd_")
  combined_df <- combine_results_side_by_side(results_list, outcome_names, cohorts)
  ft <- create_combined_flextable(combined_df, outcome_names)
  doc <- read_docx() %>%
    body_add_flextable(ft)
  print(doc, target = filename)
}

write_combined_results_to_word(results_list, file.path(outtable, "leaveoneout_MH.docx"), cohorts)

write_combined_results_to_word <- function(results_list, filename = "combined_results.docx", cohorts) {
  outcome_names <- c("fm_", "gm_", "lan_", "nvi_")
  combined_df <- combine_results_side_by_side(results_list, outcome_names, cohorts)
  ft <- create_combined_flextable(combined_df, outcome_names)
  doc <- read_docx() %>%
    body_add_flextable(ft)
  print(doc, target = filename)
}

write_combined_results_to_word(results_list, file.path(outtable, "leaveoneout_C.docx"), cohorts)



