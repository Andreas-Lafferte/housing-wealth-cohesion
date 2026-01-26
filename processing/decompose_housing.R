#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Analysis code for a research paper on "Housing wealth and social cohesion: Evidence from Chile"
# Responsable: Technical assistant
# Executive Summary: This script contains the code to perform decomposicion analysis
# Date: January 21, 2026
#******************************************************************************************************************************************************

options(scipen=999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               here,
               sjmisc, 
               estimatr,
               sandwich,
               texreg,
               broom,
               fixest,
               conflicted)

conflict_prefer_all("dplyr", "tidyr")
conflict_prefer_all("dplyr", "stats")
conflicted::conflicts_prefer(dplyr::filter)
conflict_prefer_all("dplyr", "tidylog")
conflict_prefer_all("tidyr", "tidylog")

# 2. Data -----------------------------------------------------------------

load(here("output/data/df_study1_V2.RData"))
load(here("output/data/db_long_V2.RData"))

glimpse(df_study1)
conflicts_prefer(dplyr::filter)
# convert wave to factor for FE
df_study1 <- df_study1 %>% 
  mutate(ola = factor(ola, levels = c("2016", "2017", "2018", "2019")),
         ola = sjlabelled::set_label(ola, "Wave"),
         housing = factor(housing, 
                          levels = c("Owned home with mortgage payments",
                                     "Owned and fully paid-off home",
                                     "Rented housing",
                                     "Other regime")),
         age_2 = (age)^2)

df_study1 <- subset(df_study1, housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments"))

# 3. Decompose base -------------------------------------------------------

# Decomposition exactly as in the note (eq. 1–7)

decompose_housing_note <- function(data,
                                   vardep,
                                   W = "dummy_decile_uf2018",
                                   cluster = "idencuesta",
                                   mediators = c(E = "educyear",
                                                 C = "isei",
                                                 I = "ln_inc_eq_real_sqrt"),
                                   weights = NULL) {
  
  stopifnot(vardep %in% names(data), W %in% names(data), cluster %in% names(data))
  if (!all(unname(mediators) %in% names(data))) {
    miss <- unname(mediators)[!unname(mediators) %in% names(data)]
    stop("Missing mediators in data: ", paste(miss, collapse = ", "))
  }
  
  requireNamespace("dplyr")
  requireNamespace("tibble")
  requireNamespace("estimatr")
  
  # ---- 0) Common sample (same N everywhere, as assumed by the identity) ----
  keep_vars <- unique(c(vardep, W, cluster, unname(mediators), weights))
  d <- data %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    stats::na.omit()
  
  # ---- Helper: lm_robust with CR2 clustered SEs ----
  fit_cr2 <- function(formula, data) {
    if (is.null(weights)) {
      estimatr::lm_robust(
        formula  = formula,
        data     = data,
        clusters = data[[cluster]],
        se_type  = "CR2"
      )
    } else {
      estimatr::lm_robust(
        formula  = formula,
        data     = data,
        weights  = data[[weights]],
        clusters = data[[cluster]],
        se_type  = "CR2"
      )
    }
  }
  
  # ---- (1) Unconditional / total: Y = a0 + b0 W ----
  m_total <- fit_cr2(stats::as.formula(paste0(vardep, " ~ ", W)), d)
  b0 <- stats::coef(m_total)[W]
  
  # ---- (2) Conditional / residual: Y = a1 + b1 W + g1 E + t1 C + d1 I ----
  rhs_long <- paste(c(W, unname(mediators)), collapse = " + ")
  m_long <- fit_cr2(stats::as.formula(paste0(vardep, " ~ ", rhs_long)), d)
  
  b1 <- stats::coef(m_long)[W]                         # beta1
  g1 <- stats::coef(m_long)[ mediators[["E"]] ]        # gamma1
  t1 <- stats::coef(m_long)[ mediators[["C"]] ]        # theta1
  d1 <- stats::coef(m_long)[ mediators[["I"]] ]        # delta1
  
  # ---- (3) E = aE + g2 W ----
  m_E <- fit_cr2(stats::as.formula(paste0(mediators[["E"]], " ~ ", W)), d)
  g2 <- stats::coef(m_E)[W]                             # gamma2
  
  # ---- (4) C = aC + t2 W ----
  m_C <- fit_cr2(stats::as.formula(paste0(mediators[["C"]], " ~ ", W)), d)
  t2 <- stats::coef(m_C)[W]                             # theta2
  
  # ---- (5) I = aI + d2 W ----
  m_I <- fit_cr2(stats::as.formula(paste0(mediators[["I"]], " ~ ", W)), d)
  d2 <- stats::coef(m_I)[W]                             # delta2
  
  # ---- (7) Decomposition of beta0 ----
  ind_E <- g1 * g2
  ind_C <- t1 * t2
  ind_I <- d1 * d2
  
  sum_indirect <- ind_E + ind_C + ind_I
  check_sum <- b1 + sum_indirect
  
  out <- tibble::tibble(
    component = c(
      "Total (beta0): W -> Y",
      "Direct/residual (beta1): W -> Y | E,C,I",
      paste0("Indirect via E: gamma1*gamma2 (", mediators[["E"]], ")"),
      paste0("Indirect via C: theta1*theta2 (", mediators[["C"]], ")"),
      paste0("Indirect via I: delta1*delta2 (", mediators[["I"]], ")"),
      "Sum indirect",
      "Check: beta1 + sum(indirect)"
    ),
    estimate = c(b0, b1, ind_E, ind_C, ind_I, sum_indirect, check_sum),
    share_of_beta0 = c(
      1,
      b1 / b0,
      ind_E / b0,
      ind_C / b0,
      ind_I / b0,
      sum_indirect / b0,
      check_sum / b0
    )
  )
  
  list(
    data_used = d,
    models = list(total = m_total, long = m_long, E = m_E, C = m_C, I = m_I),
    coefs = list(beta0 = b0, beta1 = b1,
                 gamma1 = g1, gamma2 = g2,
                 theta1 = t1, theta2 = t2,
                 delta1 = d1, delta2 = d2),
    table = out
  )
}

# Example:
res <- decompose_housing_note(df_study1, 
                              vardep = "size_network", 
                              W = "dummy_decile_uf2018",
                              cluster = "idencuesta",
                              mediators = c(E="educyear", 
                                            C="isei", 
                                            I="ln_inc_eq_real_sqrt"))
res$table

varsdep <- c(
  "identification", "friends", "size_network",
  "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
  "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
  "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)

# Run all outcomes

decomp_list <- map(
  varsdep,
  ~ decompose_housing_note(
    data = df_study1,
    vardep = .x,
    W = "dummy_decile_uf2018",           
    cluster = "idencuesta",
    mediators = c(E = "educyear",
                  C = "isei",
                  I = "ln_inc_eq_real_sqrt")
  )
)
names(decomp_list) <- varsdep

decomp_long <- imap_dfr(decomp_list, ~ {
  .x$table %>%
    mutate(vardep = .y) %>%
    select(vardep, component, estimate, share_of_beta0)
})

decomp_long

decomp_list$trust_inst$table

# 4. Decompose residual ---------------------------------------------------

decompose_housing_resid <- function(data,
                                    vardep,
                                    W = "dummy_decile10",
                                    wave = "ola",      # ya es factor
                                    age = "age",
                                    cluster = "idencuesta",
                                    mediators = c(E = "educyear",
                                                  C = "isei",
                                                  I = "ln_inc_eq_real_sqrt"),
                                    weights = NULL) {
  
  stopifnot(vardep %in% names(data), W %in% names(data),
            wave %in% names(data), age %in% names(data), cluster %in% names(data))
  if (!all(unname(mediators) %in% names(data))) {
    miss <- unname(mediators)[!unname(mediators) %in% names(data)]
    stop("Missing mediators in data: ", paste(miss, collapse = ", "))
  }
  
  requireNamespace("dplyr")
  requireNamespace("tibble")
  requireNamespace("estimatr")
  
  # ---- 0) Common sample ----
  keep_vars <- unique(c(vardep, W, wave, age, cluster, unname(mediators), weights))
  d <- data %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    stats::na.omit()
  
  # ---- helper: residualize var on age + wave FE ----
  ctrl_rhs <- paste(c(age, wave), collapse = " + ")
  
  resid_on_controls <- function(varname) {
    f <- stats::as.formula(paste0(varname, " ~ ", ctrl_rhs))
    if (is.null(weights)) {
      stats::resid(stats::lm(f, data = d))
    } else {
      stats::resid(stats::lm(f, data = d, weights = d[[weights]]))
    }
  }
  
  # residualized variables
  d$.Y <- resid_on_controls(vardep)
  d$.W <- resid_on_controls(W)
  d$.E <- resid_on_controls(mediators[["E"]])
  d$.C <- resid_on_controls(mediators[["C"]])
  d$.I <- resid_on_controls(mediators[["I"]])
  
  # ---- helper: lm_robust CR2 clustered ----
  fit_cr2 <- function(formula, data) {
    if (is.null(weights)) {
      estimatr::lm_robust(
        formula  = formula,
        data     = data,
        clusters = data[[cluster]],
        se_type  = "CR2"
      )
    } else {
      estimatr::lm_robust(
        formula  = formula,
        data     = data,
        weights  = data[[weights]],
        clusters = data[[cluster]],
        se_type  = "CR2"
      )
    }
  }
  
  # ---- (1) total on residuals ----
  m_total <- fit_cr2(.Y ~ .W, d)
  b0 <- stats::coef(m_total)[".W"]
  
  # ---- (2) conditional on residuals ----
  m_long <- fit_cr2(.Y ~ .W + .E + .C + .I, d)
  b1 <- stats::coef(m_long)[".W"]
  g1 <- stats::coef(m_long)[".E"]
  t1 <- stats::coef(m_long)[".C"]
  d1 <- stats::coef(m_long)[".I"]
  
  # ---- (3–5) first stages on residuals ----
  m_E <- fit_cr2(.E ~ .W, d); g2 <- stats::coef(m_E)[".W"]
  m_C <- fit_cr2(.C ~ .W, d); t2 <- stats::coef(m_C)[".W"]
  m_I <- fit_cr2(.I ~ .W, d); d2 <- stats::coef(m_I)[".W"]
  
  # ---- decomposition ----
  ind_E <- g1 * g2
  ind_C <- t1 * t2
  ind_I <- d1 * d2
  sum_ind <- ind_E + ind_C + ind_I
  check_sum <- b1 + sum_ind
  
  out <- tibble::tibble(
    component = c(
      "Total (beta0): W -> Y (net of age + wave FE)",
      "Direct/residual (beta1): W -> Y | E,C,I (net of age + wave FE)",
      paste0("Indirect via E: gamma1*gamma2 (", mediators[["E"]], ")"),
      paste0("Indirect via C: theta1*theta2 (", mediators[["C"]], ")"),
      paste0("Indirect via I: delta1*delta2 (", mediators[["I"]], ")"),
      "Sum indirect",
      "Check: beta1 + sum(indirect)"
    ),
    estimate = c(b0, b1, ind_E, ind_C, ind_I, sum_ind, check_sum),
    share_of_beta0 = c(1, b1/b0, ind_E/b0, ind_C/b0, ind_I/b0, sum_ind/b0, check_sum/b0)
  )
  
  list(
    data_used = d,
    models = list(total = m_total, long = m_long, E = m_E, C = m_C, I = m_I),
    table = out
  )
}

decomp_list_resid <- setNames(
  map(varsdep, ~ decompose_housing_resid(
    data = df_study1,
    vardep = .x,
    W = "dummy_decile_uf2018",   
    cluster = "idencuesta",
    wave = "ola",
    age = "age",
    mediators = c(E = "educyear", 
                  C = "isei", 
                  I = "ln_inc_eq_real_sqrt")
  )),
  varsdep
)


decomp_long_resid <- imap_dfr(decomp_list_resid, ~ .x$table %>% mutate(vardep = .y))

decomp_list_resid$trust_inst$table


# 5. Boostrap -------------------------------------------------------------

# Cluster bootstrap wrapper for decomposition objects that return $table
# Assumes decomp_fun(data, vardep, ...) returns list(table = tibble(component, estimate, share_of_beta0), ...)
cluster_boot_decomp <- function(decomp_fun,
                                data,
                                vardep,
                                cluster = "idencuesta",
                                B = 999,
                                alpha = 0.05,
                                seed = 123,
                                quiet = TRUE,
                                ...) {
  
  set.seed(seed)
  
  # 1) point estimate on original data
  point <- decomp_fun(data = data, vardep = vardep, cluster = cluster, ...)
  point_tab <- point$table %>%
    select(component, estimate, share_of_beta0) %>%
    mutate(vardep = vardep, .before = 1)
  
  # 2) prep clusters
  cl_ids <- unique(data[[cluster]])
  K <- length(cl_ids)
  
  # helper: build bootstrap sample with new cluster id
  make_boot_sample <- function() {
    draw <- sample(cl_ids, size = K, replace = TRUE)
    
    # bind clusters; assign new cluster id per draw position
    boot <- map2_dfr(draw, seq_along(draw), function(cid, j) {
      dat_j <- data[data[[cluster]] == cid, , drop = FALSE]
      dat_j$boot_cluster <- j
      dat_j
    })
    
    boot
  }
  
  # helper: run one bootstrap replication safely
  run_one <- function(b) {
    boot <- make_boot_sample()
    
    # call decomposition using boot_cluster as cluster variable
    res <- tryCatch(
      decomp_fun(data = boot, vardep = vardep, cluster = "boot_cluster", ...),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      if (!quiet) message("bootstrap replicate failed: ", b)
      return(NULL)
    }
    
    res$table %>%
      select(component, estimate, share_of_beta0) %>%
      mutate(.rep = b, .before = 1)
  }
  
  boots <- map(seq_len(B), run_one) %>%
    compact() %>%
    bind_rows()
  
  if (nrow(boots) == 0) {
    stop("All bootstrap replications failed. Check inputs / model stability.")
  }
  
  # 3) SE + percentile CI by component
  ci_lo <- alpha / 2
  ci_hi <- 1 - alpha / 2
  
  boot_sum <- boots %>%
    group_by(component) %>%
    summarise(
      B_used = n(),
      se = sd(estimate),
      conf.low = unname(quantile(estimate, probs = ci_lo, na.rm = TRUE)),
      conf.high = unname(quantile(estimate, probs = ci_hi, na.rm = TRUE)),
      share_se = sd(share_of_beta0),
      share_conf.low = unname(quantile(share_of_beta0, probs = ci_lo, na.rm = TRUE)),
      share_conf.high = unname(quantile(share_of_beta0, probs = ci_hi, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # 4) merge point + bootstrap inference
  out <- point_tab %>%
    left_join(boot_sum, by = "component") %>%
    arrange(match(component, point_tab$component))
  
  list(
    point = point,
    point_table = point_tab,
    boot_draws = boots,
    summary = out
  )
}

## Example

res_bs <- cluster_boot_decomp(
  decomp_fun = decompose_housing_note,
  data = df_study1,
  vardep = "size_network",
  cluster = "idencuesta",
  B = 100, # iterations
  alpha = 0.05,
  W = "dummy_decile_uf2018",
  mediators = c(E="educyear", C="isei", I="ln_inc_eq_real_sqrt")
)

res_bs$summary


## 5.1 Base decomposing bootstrap ----

boot_base_list <- setNames(
  map(varsdep, ~ cluster_boot_decomp(
    decomp_fun = decompose_housing_note,
    data = df_study1,
    vardep = .x,
    cluster = "idencuesta",
    B = 100,
    alpha = 0.05,
    seed = 123,
    W = "dummy_decile_uf2018",
    mediators = c(E="educyear", C="isei", I="ln_inc_eq_real_sqrt")
  )),
  varsdep
)

boot_base_tbl <- imap_dfr(boot_base_list, ~ .x$summary %>% mutate(vardep = .y, .before = 1))
boot_base_tbl

## 5.2 Residual decomposition bootstrap ----

boot_resid_list <- setNames(
  map(varsdep, ~ cluster_boot_decomp(
    decomp_fun = decompose_housing_resid,
    data = df_study1,
    vardep = .x,
    cluster = "idencuesta",
    B = 100,
    alpha = 0.05,
    seed = 123,
    W = "dummy_decile_uf2018",
    wave = "ola",
    age = "age",
    mediators = c(E="educyear", C="isei", I="ln_inc_eq_real_sqrt")
  )),
  varsdep
)

boot_resid_tbl <- imap_dfr(boot_resid_list, ~ .x$summary %>% mutate(vardep = .y, .before = 1))
boot_resid_tbl

# 6. Save and export ------------------------------------------------------

save(
  decomp_list,
  decomp_long,
  decomp_list_resid,
  decomp_long_resid,
  boot_base_list,
  boot_base_tbl,
  boot_resid_list,
  boot_resid_tbl,
  file = here("output/decomposition/decomposition_models_all.RData")
)
