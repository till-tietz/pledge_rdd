rm(list = ls())
library(ggplot2)
library(stargazer)
library(texreg)

# RDD output formating helpers -------------------------------------------------

extract_rdrobust <- function(model, controls = FALSE, country_fes = FALSE, year_fes = FALSE) {
  coef_bc <- model$coef[2]
  coef_robust <- model$coef[3]
  ci_bc <- model$ci[2,]
  ci_robust <- model$ci[3,]
  pvals <- model$pv  # Extract p-values
  
  # Function to add significance stars
  add_stars <- function(coef, pval) {
    stars <- ifelse(pval < 0.01, "***", 
                    ifelse(pval < 0.05, "**", 
                           ifelse(pval < 0.1, "*", "")))
    sprintf("%.2f%s", coef, stars)
  }
  
  coef_bc_str <- add_stars(coef_bc, pvals[2])
  coef_robust_str <- add_stars(coef_robust, pvals[3])
  
  return(c(
    coef_bc_str, sprintf("(%.2f, %.2f)", ci_bc[1], ci_bc[2]),
    coef_robust_str, sprintf("(%.2f, %.2f)", ci_robust[1], ci_robust[2]),
    sum(model$N), sum(model$N_h),
    ifelse(controls, "Yes", "No"),
    ifelse(country_fes, "Yes", "No"),
    ifelse(year_fes, "Yes", "No")
  ))
}

generate_latex_table <- function(models, model_names, controls, country_fes, year_fes, dep_var_name) {
  results <- lapply(seq_along(models), function(i) {
    extract_rdrobust(models[[i]], controls[i], country_fes[i], year_fes[i])
  })
  
  results_matrix <- do.call(cbind, results)
  row_labels <- c(
    "Bias-Corrected Estimate", "95\\% CI",
    "Robust Estimate", "95\\% CI",
    "Observations", "Effective Observations",
    "Controls", "Country FEs", "Year FEs"
  )
  
  # Construct LaTeX Table Header
  latex_table <- c(
    "\\begin{table}[!htbp] \\centering",
    "\\caption{Regression Discontinuity Estimates}",
    "\\label{tab:rdrobust}",
    paste("\\begin{tabular}{@{\\extracolsep{5pt}}l", paste(rep("c", length(model_names)), collapse = ""), "}"),
    "\\hline \\hline",
    " & \\multicolumn{" , length(model_names) , "}{c}{\\textit{Dependent variable:}} \\\\",
    "\\cline{2-", length(model_names) + 1, "}",
    " & \\multicolumn{" , length(model_names) , "}{c}{", dep_var_name, "} \\\\",
    "\\\\[-1.8ex] & ", paste(model_names, collapse = " & "), " \\\\",
    "\\hline"
  )
  
  # Add results row by row
  for (i in seq_along(row_labels)) {
    #latex_table <- c(latex_table, paste(row_labels[i], paste(results_matrix[i, ], collapse = " & "), "& \\\\"))
    latex_table <- c(latex_table, paste(row_labels[i], "&", paste(results_matrix[i, ], collapse = " & "), "\\\\"))
    
    # Insert \hline before observation block
    if (i == 4) {
      latex_table <- c(latex_table, "\\hline")
    }
  }
  
  # Add table footer
  latex_table <- c(
    latex_table,
    "\\hline \\hline",
    "\\textit{Note:} $^{*} p<0.1; ^{**} p<0.05; ^{***} p<0.01$",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  return(paste(latex_table, collapse = "\n"))
}

# set up -----------------------------------------------------------------------

# please set save_output to FALSE unless you wish to save files to dir
# if save_output = TRUE please ensure that the required directories exist 
# or update pahts in if(save_output) blocks as per your directory structure

save_ouput <- TRUE
dir <- "~/papers/pledge_paper/"

# load data
# update paths as per your directory structure
data <- data.table::fread(paste(dir,"data/rdd_pledge_panel.csv", sep = "")) |>
  dplyr::select(-c(V1)) |> 
  as.data.frame()

# generate outcome -------------------------------------------------------------
# LOOCV pledge gap estimation 
# loop is not very efficient but guards against headaches with NA data 
# as num cases is small it's not too bad
estimate_pledge_gap <- function(data, y, x, jackknife = TRUE) {
  formula <- stats::as.formula(
    paste(y, " ~ ", x, " + net_migration + net_migration_lag1 + left_right + factor(year)", sep = "")
  )
  
  data[[paste(y, "resid", sep = "_")]] <- NA
  pval <- rep(NA, nrow(data))
  beta <- rep(NA, nrow(data))
     
  for(i in 1:nrow(data)) {
    d <- data[i,]
    
    if(jackknife) {
      d_fit <- data[setdiff(1:nrow(data),i),]
    } else {
      d_fit <- data  
    }
    
    model <- lm(formula, data = d_fit, weights = (d_fit$legislation_weight))
       
    data[i, paste(y, "resid", sep = "_")] <- (d[[y]] - predict(model, d))
    pval[i] <- summary(model)$coefficients[2,4]
    beta[i] <- summary(model)$coefficients[2,1]
  }
  
  return(list(
    data = data,
    pval = pval,
    beta = beta
  ))
}

gap <- estimate_pledge_gap(data, "quantmig_immigration_restrict", "anti_immigration_position")
data <- gap$data

# get a sense of model fit on a global model
global_outcome_model <- lm(quantmig_immigration_restrict ~ anti_immigration_position + net_migration + net_migration_lag1 + left_right + factor(year), data)

# save global model output
if(save_ouput) {
  stargazer::stargazer(
    global_outcome_model, type = "latex", 
    keep = c("anti_immigration_position", "net_migration", "net_migration_lag1", "left_right"),  
    covariate.labels = c("anti-immigration pledge", "net migration (million)", "net migration lagged (million)", "government left-right position"),
    dep.var.labels = "anti-immigration policy",
    title = "Pledge-Gap Model",
    label = "tab:1",
    add.lines = list(c("Year FEs", "yes")),
    out = paste(dir, "fig_tab/global_outcome_model.tex", sep = ""))
}


# Main RDD Models --------------------------------------------------------------
# we proceed with the quantmig outcome
data_rdd <- data |>
  dplyr::filter(right_wing_in_gov != 1) |> 
  tidyr::drop_na(quantmig_immigration_restrict_resid) |>
  as.data.frame()

# RDD estimation helper
estimate_rdd <- function(
    data, 
    outcome, 
    treatment,
    cutoff = 0,
    fuzzy = NULL,
    controls = NULL, 
    kernel = "tri", 
    weights = NULL,
    bwselect = "mserd", 
    h = NULL,
    b = NULL,
    cluster = NULL,
    FEs = NULL) {
  
  # compute fixed effects
  if(!is.null(FEs)) {
    outcome_demean <- fixest::demean(
      X = data[,outcome],
      f = data[,FEs]
    )
    
    data[[paste(outcome, "demean", sep = "_")]] <- outcome_demean[,1]
    outcome <- paste(outcome, "demean", sep = "_")
  }
  
  # set controls, weights and cluster if not NULL
  if(!is.null(controls)) {
    controls <- eval(parse(text = paste(paste("data[['", controls, "']]", sep = ""), collapse = " + ")))
  }
  
  if(!is.null(weights)) {
    weights <- data[[weights]]
  }
  
  if(!is.null(cluster)) {
    cluster <- data[[cluster]]
  }
  
  if(!is.null(fuzzy)) {
    fuzzy <- data[[fuzzy]]
  }
  
  # compute RDD 
  rdd <- rdrobust::rdrobust(
    y = data[[outcome]],
    x = data[[treatment]],
    c = cutoff,
    fuzzy = fuzzy,
    all = TRUE,
    covs = controls,
    kernel = kernel,
    weights = weights,
    bwselect = bwselect,
    h = h,
    b = b,
    cluster = cluster
  )
  
  return(rdd)
}

# simple model
simple_model <- estimate_rdd(
  data_rdd, 
  outcome = "quantmig_immigration_restrict_resid", 
  treatment = "right_wing_vote_share_centered",
  kernel = "tri",
  bwselect = "mserd",
  cluster = "country_id"
)

summary(simple_model)

# country FE
country_fe_model <- estimate_rdd(
  data_rdd, 
  outcome = "quantmig_immigration_restrict_resid", 
  treatment = "right_wing_vote_share_centered", 
  kernel = "tri",
  h = simple_model$bws[1,],
  b = simple_model$bws[2,],
  cluster = "country_id",
  FEs = "country_id"
)

summary(country_fe_model)

# full model: controls, country FE, year FE
full_model <- estimate_rdd(
  data_rdd, 
  outcome = "quantmig_immigration_restrict_resid", 
  treatment = "right_wing_vote_share_centered", 
  controls = c("net_migration","net_migration_lag1","left_right","opposition_left_right"),
  kernel = "tri",
  h = simple_model$bws[1,],
  b = simple_model$bws[2,],
  cluster = "country_id",
  FEs = c("country_id","year")
)

summary(full_model)

# format output 
main_model_output <- generate_latex_table(
  list(simple_model, country_fe_model, full_model), 
  c("Model 1", "Model 2", "Model 3"), 
  c(FALSE, FALSE, TRUE) ,
  c(FALSE, TRUE, TRUE), 
  c(FALSE, FALSE, TRUE),
  dep_var_name = "anti-immigration pledge-gap"
)

if(save_ouput) {
  writeLines(main_model_output, paste(dir, "fig_tab/main_rdd.tex", sep = ""))
}


# conditional local average treatment effects  
clate_models <- lapply(c(-7.1,-5.1), function(q) {
  if(q == -7.1) {
    d <- data_rdd |> dplyr::filter(gov_rrp_position_diff <= q)
  } else {
    d <- data_rdd |> dplyr::filter(gov_rrp_position_diff >= q)
  }
  
  tryCatch({
    estimate_rdd(
      d, 
      outcome = "quantmig_immigration_restrict_resid", 
      treatment = "right_wing_vote_share_centered", 
      controls = c("net_migration","net_migration_lag1"),
      kernel = "tri",
      h = c(2,2),
      b = c(3,3),
      cluster = "country_id",
      FEs = c("country_id","year")
    )
  }, error = function(e) NULL)
})

# LATE + 90% CI
clate_models <- data.frame(
  coef = c(clate_models[[1]]$coef[2], clate_models[[2]]$coef[2]),
  se = c(clate_models[[1]]$se[2], clate_models[[2]]$se[2]),
  type = c("Left Government", "Right Government")
) |>
  dplyr::mutate(
    ci_low = coef - 1.644 * se,
    ci_high = coef + 1.644 * se
  )

clate_plot <- ggplot(clate_models, aes(x = coef, y = type, color = type)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.1) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual("", values = c("blue","orange")) + 
  labs(x = "CLATE", y = "") + 
  guides(color = "none") + 
  theme_bw()

if(save_ouput) {
  saveRDS(clate_plot, paste(dir, "fig_tab/clate_plot.rds", sep = ""))
}

# RDD plots --------------------------------------------------------------------
data_plot <- data_rdd |>
  dplyr::mutate(right_wing_in_parl = factor(right_wing_in_parl, labels = c("RRP w/o seat(s)", "RRP w seat(s)"))) |>
  dplyr::filter(right_wing_vote_share_centered <= 10)

rdd_plot_1 <- ggplot(data_plot, aes(x = right_wing_vote_share_centered, 
                                    y = quantmig_immigration_restrict_resid, 
                                    color = factor(right_wing_in_parl))) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "loess", se = TRUE) + 
  scale_color_manual("",values = c("blue", "orange")) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(breaks = seq(-5,11,2),labels = function(x) paste0(x, "%")) + 
  scale_y_continuous(breaks = seq(-7,6,1), limits = c(-7,6)) + 
  xlab("Vote Share of Radical Right Parties \n (centered at inclusion threshold)") + 
  ylab("\u0394 Immigration Restriction \n (realized - expected)") + 
  theme_bw() + 
  theme(legend.position = "bottom")

# model based plot
data_plot <- data_plot |>
  dplyr::mutate(bw = ifelse(right_wing_vote_share_centered < 0, simple_model$bws[2,1], NA)) |>
  dplyr::mutate(bw = ifelse(right_wing_vote_share_centered > 0, simple_model$bws[2,2], bw)) |>
  dplyr::filter(abs(right_wing_vote_share_centered) <= bw) |>
  dplyr::mutate(w = rdd::kernelwts(right_wing_vote_share_centered, 0, bw = simple_model$bws[1,2])) 

data_plot$fitted <- stats::predict(lm(stats::as.formula("quantmig_immigration_restrict_resid ~ right_wing_in_parl + right_wing_vote_share_centered + right_wing_in_parl * right_wing_vote_share_centered"), data = data_plot, weights = data_plot$w))

rdd_plot_2 <- ggplot(data_plot, aes(x = right_wing_vote_share_centered, y = quantmig_immigration_restrict_resid, color = right_wing_in_parl)) +
  geom_density2d(aes(group = right_wing_in_parl, show.legend = FALSE), alpha = 0.2) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, aes(y = fitted), show.legend = FALSE) +
  scale_color_manual("", values = c("blue", "orange")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(breaks = seq(-2,2,0.5),labels = function(x) paste0(x, "%")) + 
  xlab("Vote Share of Radical Right Parties \n (centered at inclusion threshold)") + 
  ylab("\u0394 Immigration Restriction \n (realized - expected)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

rdd_plot <- cowplot::plot_grid(rdd_plot_1, rdd_plot_2, align = "hv", axis = "l")

if(save_ouput) {
  saveRDS(rdd_plot, paste(dir, "fig_tab/rdd_plot.rds", sep = ""))
}

# Robustness -------------------------------------------------------------------

# manipulation around cutoff 
density <- rddensity::rddensity(data_rdd$right_wing_vote_share_centered, c = 0)
summary(density)

density_plot <- rddensity::rdplotdensity(
  density, data_rdd$right_wing_vote_share_centered,
  lcol = c("blue","orange"), 
  CIcol = c("blue","orange"), 
  histFillCol = "grey30") 

density_plot <- density_plot$Estplot +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(breaks = seq(-5,15,2.5),labels = function(x) paste0(x, "%")) + 
  xlab("Vote Share of Radical Right Parties (centered at inclusion threshold)") + 
  ylab("Observation Density") 

density_plot <- ggpubr::ggarrange(density_plot)
density_plot <- ggpubr::annotate_figure(
  density_plot,
  fig.lab = "Manipulation Test",
  fig.lab.face = "bold",
  fig.lab.size = 15,
  top = ggpubr::text_grob(
    paste("P-value: {T = ", round(density$test$t_jk, 2), 
          ",", 
          " P > |T| = ", round(density$test$p_jk, 2),
          "}",
          sep = ""),
    hjust = 1.1, x = 1, size = 10, face = "italic"
  )
)

if(save_ouput) {
  saveRDS(density_plot, paste(dir, "fig_tab/manipulation_test.rds", sep = ""))
}


# placebo cutoffs 
cutoffs <- c(-0.025, 0.025, 0, seq(-0.8, 0.8, by = 0.1))

placebo_cutoffs <- lapply(cutoffs, function(cut) {
  tryCatch({
    rdd <- estimate_rdd(
      data_rdd, 
      outcome = "quantmig_immigration_restrict_resid", 
      treatment = "right_wing_vote_share_centered", 
      controls = c("net_migration","net_migration_lag1","left_right","opposition_left_right"),
      c = cut,
      kernel = "tri",
      bwselect = "mserd",
      cluster = "country_id",
      FEs = c("country_id","year")
    )
    
    data.frame(
      cutoff = cut,
      est = rdd$coef[2,],
      ci.low = rdd$ci[2,][1],
      ci.high = rdd$ci[2,][2]
    )
  }, error = function(e) NULL)
}) |> dplyr::bind_rows() 

placebo_plot <- ggplot(placebo_cutoffs, aes(x = cutoff, y = est)) + 
  stat_smooth(data = subset(placebo_cutoffs, cutoff < 0),aes(ymin = ci.low, ymax = ci.high, fill = "LATE 95% CI \nPlacebo Thresholds"), alpha = 0.2, geom = "ribbon", method = "loess") + 
  stat_smooth(data = subset(placebo_cutoffs, cutoff > 0),aes(ymin = ci.low, ymax = ci.high, fill = "LATE 95% CI \nPlacebo Thresholds"), alpha = 0.2, geom = "ribbon", method = "loess") + 
  geom_point(data = subset(placebo_cutoffs, cutoff == 0), aes(color = "LATE \nTrue Threshold"), size = 3) + 
  geom_errorbar(data = subset(placebo_cutoffs, cutoff == 0), 
                aes(ymin = ci.low, ymax = ci.high, color = "LATE \nTrue Threshold"), width = 0.01) + 
  scale_fill_manual("", values = "blue") + 
  scale_color_manual("", values = "orange") + 
  scale_x_continuous(limits = c(-0.65,0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") +
  labs(x = "Placebo Treatment Assignment Thresholds", y = "LATE") + 
  theme_bw() +
  theme(legend.position = "bottom") 

if(save_ouput) {
  saveRDS(placebo_plot, paste(dir, "fig_tab/placebo_plot.rds", sep = ""))
}

# bandwidth test 
bws <- seq(0.5, 15, 0.5)

bws_test <- lapply(bws, function(bw) {
  tryCatch({
    rdd <- estimate_rdd(
      data_rdd, 
      outcome = "quantmig_immigration_restrict_resid", 
      treatment = "right_wing_vote_share_centered", 
      controls = c("net_migration","net_migration_lag1","left_right","opposition_left_right"),
      kernel = "tri",
      h = bw,
      cluster = "country_id",
      FEs = c("country_id","year")
    )
    
    data.frame(
      bw = bw,
      est = rdd$coef[2,],
      ci.low = rdd$ci[2,][1],
      ci.high = rdd$ci[2,][2]
    )
  }, error = function(e) NULL)
}) |> dplyr::bind_rows() 

bw_plot <- ggplot(bws_test, aes(x = bw, y = est)) + 
  geom_point(color = "orange") + 
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1, color = "orange") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Bandwidth", y = "LATE") + 
  theme_bw()

if(save_ouput) {
  saveRDS(bw_plot, paste(dir, "fig_tab/bw_plot.rds", sep = ""))
}


# jackknife --> netherlands are a key driver of results
# jackknife countries 
countries <- sort(unique(data_rdd$country_name))

jackknife_country <- lapply(countries, function(country) {
  d <- data_rdd |> 
    dplyr::filter(country_name != country)
  
  tryCatch({
    rdd <- estimate_rdd(
      d, 
      outcome = "quantmig_immigration_restrict_resid", 
      treatment = "right_wing_vote_share_centered", 
      controls = c("net_migration","net_migration_lag1","left_right","opposition_left_right"),
      kernel = "tri",
      bwselect = "mserd",
      cluster = "country_id",
      FEs = c("country_id","year")
    )
    
    data.frame(
      excluding = country,
      est = rdd$coef[2,],
      ci.low = rdd$ci[2,][1],
      ci.high = rdd$ci[2,][2]
    )
  }, error = function(e) NULL)
}) |> 
  dplyr::bind_rows() |>
  dplyr::mutate(influential = ifelse(est > 0, "influential observation", NA))


jackknife_plot <- ggplot(jackknife_country, aes(x = est, y = forcats::fct_reorder(excluding, est), color = influential)) + 
  geom_point(size = 3) + 
  geom_errorbarh(aes(xmin = ci.low, xmax = ci.high), height = 0.1) + 
  scale_color_manual("", values = c("orange")) + 
  labs(x = "LATE", y = "Excluding") + 
  theme_bw() +
  theme(legend.position = "bottom")

if(save_ouput) {
  saveRDS(jackknife_plot, paste(dir, "fig_tab/jackknife_plot.rds", sep = ""))
}


# Descriptive plots ------------------------------------------------------------

# are the ideological positions of Gov & RRP different 
data_plot <- data_rdd |>
  dplyr::distinct(country_name, year, .keep_all = TRUE) |>
  dplyr::select(country_name, year, anti_immigration_position, 
                right_wing_anti_immigration_position, left_right, 
                quantmig_immigration_restrict, right_wing_in_parl) |>
  dplyr::mutate(
    anti_im_lgov = ifelse(left_right < 2.36, anti_immigration_position, NA),
    anti_im_rgov = ifelse(left_right >= 2.36, anti_immigration_position, NA)
  ) 

position_plot <- ggplot(data_plot, aes(x = year)) +
  stat_smooth(aes(y = anti_im_rgov, fill = "Right Government", color = "Right Government"), alpha = 0.2) + 
  stat_smooth(aes(y = anti_im_lgov, fill = "Left Government", color = "Left Government"), alpha = 0.2) + 
  stat_smooth(aes(y = right_wing_anti_immigration_position, fill = "Radical Right Opposition", color = "Radical Right Opposition"), alpha = 0.05) +
  scale_color_manual("", values = c("blue","orange","darkviolet"), breaks = c("Radical Right Opposition", "Right Government", "Left Government")) + 
  scale_fill_manual("", values = c("blue","orange","darkviolet"), breaks = c("Radical Right Opposition", "Right Government", "Left Government")) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "", y = "Anti Immigration Position") + 
  theme_bw() + 
  theme(legend.position = "bottom")

if(save_ouput) {
  saveRDS(position_plot, paste(dir, "fig_tab/position_plot.rds", sep = ""))
}





