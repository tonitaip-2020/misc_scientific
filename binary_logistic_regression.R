# Binary logistic regression analysis script
# Reads: streaming_latest.csv (comma-separated)
# DV: q2_prim_prod  (Batch vs {Hybrid (batch + streaming), Streaming})
# IVs (Likert 0..4):
#   q4_freshness, q5d_reliability, q5g_ease, q5j_cost, q5m_management,
#   q9a_team_fam_stream, q13_team_autonomy
#
# The script:
#   1) Loads and prepares data
#   2) Checks key assumptions for binary logistic regression and prints PASS/FAIL/REVIEW
#   3) Fits logistic regression and prints report-ready outputs:
#      LR chi-square, pseudo-R^2, coefficients with SE, z, p, OR and OR 95% CI
#
# Notes:
# - Some diagnostic tests require optional packages. If missing, the corresponding items are marked REVIEW.

options(stringsAsFactors = FALSE)
suppressWarnings(suppressMessages({
  # base only by default
}))

cat("=== Binary Logistic Regression (Streaming vs Batch) ===\n\n")

# ----------------------------
# Helpers
# ----------------------------
status_line <- function(name, status, details = NULL) {
  cat(sprintf("[%s] %s\n", status, name))
  if (!is.null(details) && nzchar(details)) {
    cat(details, "\n")
  }
  cat("\n")
}

has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), NA, formatC(x, digits = digits, format = "f"))
}

# ----------------------------
# Input / columns
# ----------------------------
file_path <- "streaming_latest.csv"

dv <- "q2_prim_prod"
ivs <- c("q4_freshness_req", "q5d_reliability", "q5g_ease", "q5j_cost", "q5m_management",
         "q9a_team_fam_stream", "q13_team_autonomy")

needed_cols <- c(dv, ivs)

if (!file.exists(file_path)) {
  stop(sprintf("File not found: %s (expected in working directory: %s)", file_path, getwd()))
}

dat0 <- read.csv(file_path, sep = ",", header = TRUE, check.names = FALSE)

missing_cols <- setdiff(needed_cols, names(dat0))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

dat <- dat0[, needed_cols, drop = FALSE]

# ----------------------------
# DV collapse / encoding
# ----------------------------
dv_raw <- dat[[dv]]

# Accept exact strings; otherwise attempt a trim match
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
dv_raw2 <- trim(as.character(dv_raw))

hybrid_label <- "Hybrid (batch + streaming)"
stream_label <- "Streaming"
batch_label  <- "Batch"

collapsed <- ifelse(dv_raw2 %in% c(hybrid_label, stream_label), 1,
                    ifelse(dv_raw2 %in% c(batch_label), 0, NA))

dat$DV_bin <- collapsed

# ----------------------------
# Coerce IVs to numeric and check Likert range
# ----------------------------
for (v in ivs) {
  dat[[v]] <- suppressWarnings(as.numeric(dat[[v]]))
}

# ----------------------------
# Missingness
# ----------------------------
miss_per_col <- sapply(dat[, c("DV_bin", ivs), drop = FALSE], function(x) mean(is.na(x)))
miss_tbl <- data.frame(variable = names(miss_per_col),
                       missing_prop = as.numeric(miss_per_col),
                       missing_pct = 100 * as.numeric(miss_per_col),
                       row.names = NULL)

complete_idx <- complete.cases(dat[, c("DV_bin", ivs), drop = FALSE])
dat_cc <- dat[complete_idx, , drop = FALSE]

n_total <- nrow(dat)
n_cc <- nrow(dat_cc)

cat("Data file:", file_path, "\n")
cat("Rows (total):", n_total, "\n")
cat("Rows (complete cases):", n_cc, "\n\n")

# Missingness assumption: handled / low missingness (heuristic)
# Thresholds are contextual; here we use <5% per variable as PASS, else REVIEW (not FAIL).
miss_max <- max(miss_per_col, na.rm = TRUE)
miss_details <- paste0(
  "Missingness by variable (%):\n",
  paste(sprintf("  - %s: %s", miss_tbl$variable, fmt_num(miss_tbl$missing_pct, 2)), collapse = "\n"),
  sprintf("\nMax missingness: %s%%", fmt_num(100 * miss_max, 2)),
  sprintf("\nComplete-case N: %d (dropped %d)", n_cc, n_total - n_cc)
)
if (miss_max <= 0.05) {
  status_line("Missing data not excessive (<=5% per variable, complete-case used)", "PASS", miss_details)
} else {
  status_line("Missing data not excessive (<=5% per variable, complete-case used)", "REVIEW", miss_details)
}

# ----------------------------
# IVs are within expected Likert range 0..4 (data integrity check)
# ----------------------------
range_violations <- lapply(ivs, function(v) {
  x <- dat_cc[[v]]
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)
  bad <- which(!(x %in% 0:4))
  if (length(bad) > 0) {
    return(list(var = v,
                n_bad = length(bad),
                min = min(x, na.rm = TRUE),
                max = max(x, na.rm = TRUE)))
  }
  NULL
})
range_violations <- Filter(Negate(is.null), range_violations)

if (length(range_violations) == 0) {
  status_line("IV Likert range check (0..4)", "PASS", "All IV values in complete cases are within 0..4.")
} else {
  det <- paste0("Violations detected:\n",
                paste(sapply(range_violations, function(z) {
                  sprintf("  - %s: n_bad=%d, observed range [%s,%s]",
                          z$var, z$n_bad, fmt_num(z$min, 2), fmt_num(z$max, 2))
                }), collapse = "\n"))
  status_line("IV Likert range check (0..4)", "FAIL", det)
  stop("IV range violations detected; fix/clean data before modeling.")
}

# ----------------------------
# Assumption #1: DV is binary and properly coded
# ----------------------------
dv_unique <- sort(unique(na.omit(dat$DV_bin)))
dv_ok <- identical(dv_unique, c(0, 1)) || identical(dv_unique, c(0)) || identical(dv_unique, c(1))
dv_counts <- table(dat$DV_bin, useNA = "ifany")
dv_details <- paste0(
  "Collapsed DV coding:\n",
  "  - 1 = {", hybrid_label, ", ", stream_label, "}\n",
  "  - 0 = ", batch_label, "\n\n",
  "Counts (including NA):\n",
  paste(capture.output(print(dv_counts)), collapse = "\n")
)
if (dv_ok && length(dv_unique) == 2) {
  status_line("DV is binary after collapse", "PASS", dv_details)
} else if (dv_ok && length(dv_unique) == 1) {
  status_line("DV has only one class after collapse (cannot fit logistic regression)", "FAIL", dv_details)
  stop("DV has only one class after collapse; logistic regression requires both outcomes.")
} else {
  status_line("DV collapse produced unexpected values (check labels / data)", "FAIL", dv_details)
  stop("Unexpected DV values after collapse; verify the input labels.")
}

# ----------------------------
# Assumption #2: Independence of observations
# ----------------------------
status_line("Independence of observations (study design)", "REVIEW",
            "Cannot be inferred from the CSV alone. Review sampling/collection to ensure independent observations.")
            
# ----------------------------
# Assumption #3: Missingness (check above)
# ----------------------------

# ----------------------------
# Assumption #4: Adequate sample size / Events Per Variable (EPV)
# ----------------------------
events <- sum(dat_cc$DV_bin == 1, na.rm = TRUE)
nonevents <- sum(dat_cc$DV_bin == 0, na.rm = TRUE)
k_params <- length(ivs) + 1  # + intercept
epv_events <- events / k_params
epv_nonevents <- nonevents / k_params
epv_details <- sprintf("Complete-case N=%d; Events(1)=%d; Non-events(0)=%d; Params(incl. intercept)=%d\nEPV_events=%s; EPV_nonevents=%s\nHeuristic PASS if both >=10.",
                       n_cc, events, nonevents, k_params, fmt_num(epv_events, 2), fmt_num(epv_nonevents, 2))
if (epv_events >= 10 && epv_nonevents >= 10) {
  status_line("Sample size adequacy (EPV heuristic)", "PASS", epv_details)
} else if (epv_events >= 5 && epv_nonevents >= 5) {
  status_line("Sample size adequacy (EPV heuristic)", "REVIEW", epv_details)
} else {
  status_line("Sample size adequacy (EPV heuristic)", "FAIL", epv_details)
}

# ----------------------------
# Fit model
# ----------------------------
form <- as.formula(paste("DV_bin ~", paste(ivs, collapse = " + ")))
m0 <- glm(DV_bin ~ 1, data = dat_cc, family = binomial())
m1 <- glm(form, data = dat_cc, family = binomial())

# ----------------------------
# Assumption #5: No complete/quasi separation
# ----------------------------
if (has_pkg("detectseparation")) {
  sep_obj <- tryCatch(
    detectseparation::detect_separation(form, data = dat_cc, family = binomial()),
    error = function(e) e
  )
  if (inherits(sep_obj, "error")) {
    status_line("Separation (detectseparation::detect_separation)", "REVIEW",
                paste0("Separation check failed: ", sep_obj$message))
  } else {
    # The returned object typically carries a logical 'separation' flag; be defensive across versions
    sep_flag <- NA
    if (is.list(sep_obj) && "separation" %in% names(sep_obj)) sep_flag <- sep_obj$separation
    if (isTRUE(sep_flag)) {
      status_line("Separation (detectseparation::detect_separation)", "FAIL",
                  "Complete or quasi-complete separation detected (some MLEs infinite). Consider bias-reduced methods (e.g., brglm2::brglm_fit) or model re-specification.")
    } else if (identical(sep_flag, FALSE)) {
      status_line("Separation (detectseparation::detect_separation)", "PASS",
                  "No separation detected by detectseparation.")
    } else {
      status_line("Separation (detectseparation::detect_separation)", "REVIEW",
                  paste0("Separation check ran, but could not locate a definitive flag in the returned object.\nReturned object class: ",
                         paste(class(sep_obj), collapse = ", "),
                         "\nInspect the printed object for details:\n",
                         paste(capture.output(print(sep_obj)), collapse = "\n")))
    }
  }
} else {
  status_line("Separation (requires detectseparation)", "REVIEW",
              "Package 'detectseparation' not available. Install it to run detect_separation().")
}

# ----------------------------
# Assumption #6: Multicollinearity (VIF)
# ----------------------------
if (has_pkg("car")) {
  vif_vals <- car::vif(m1)
  # For models with factors, car::vif may return matrix; here should be numeric vector.
  if (is.matrix(vif_vals)) {
    vif_out <- apply(vif_vals, 1, function(r) r[1])
  } else {
    vif_out <- vif_vals
  }
  vif_max <- max(vif_out, na.rm = TRUE)
  vif_details <- paste0(
    "VIFs:\n",
    paste(sprintf("  - %s: %s", names(vif_out), fmt_num(as.numeric(vif_out), 3)), collapse = "\n"),
    sprintf("\nMax VIF: %s (heuristic PASS if < 5; REVIEW if 5-10; FAIL if >=10)", fmt_num(vif_max, 3))
  )
  if (vif_max < 5) {
    status_line("Multicollinearity (VIF)", "PASS", vif_details)
  } else if (vif_max < 10) {
    status_line("Multicollinearity (VIF)", "REVIEW", vif_details)
  } else {
    status_line("Multicollinearity (VIF)", "FAIL", vif_details)
  }
} else {
  status_line("Multicollinearity (VIF requires car)", "REVIEW",
              "Package 'car' not available. Install it to compute VIF.")
}

# ----------------------------
# Assumption #7: Linearity of the logit for (treated-as-continuous) predictors
# Box-Tidwell style test: include x*log(x) terms; needs x > 0, so add 0.5 offset.
# Note: For strictly ordinal Likert, this is often a modeling choice; interpret accordingly.
# ----------------------------
offset <- 0.5
dat_bt <- dat_cc
for (v in ivs) {
  dat_bt[[paste0(v, "_log")]] <- (dat_bt[[v]] + offset) * log(dat_bt[[v]] + offset)
}
form_bt <- as.formula(paste("DV_bin ~", paste(ivs, collapse = " + "), "+",
                            paste(paste0(ivs, "_log"), collapse = " + ")))
m_bt <- glm(form_bt, data = dat_bt, family = binomial())

bt_coef <- summary(m_bt)$coefficients
bt_terms <- paste0(ivs, "_log")
bt_p <- bt_coef[bt_terms, "Pr(>|z|)"]
bt_p <- bt_p[!is.na(bt_p)]

bt_details <- paste0(
  "Box-Tidwell-style interaction terms (x*log(x+0.5)) p-values:\n",
  paste(sprintf("  - %s: p=%s", names(bt_p), fmt_num(bt_p, 4)), collapse = "\n"),
  "\nHeuristic PASS if all p > 0.05 (no evidence of non-linearity in the logit)."
)

if (length(bt_p) == length(ivs) && all(bt_p > 0.05)) {
  status_line("Linearity in the logit (Box-Tidwell-style)", "PASS", bt_details)
} else if (length(bt_p) == length(ivs)) {
  status_line("Linearity in the logit (Box-Tidwell-style)", "REVIEW", bt_details)
} else {
  status_line("Linearity in the logit (Box-Tidwell-style)", "REVIEW",
              paste0(bt_details, "\nSome terms unavailable; review model output for warnings."))
}

# ----------------------------
# Assumption #8: Influential observations / outliers (Cook's D, leverage, DFBETAs)
# ----------------------------
n <- nrow(dat_cc)
p <- length(coef(m1))  # includes intercept

cooks <- cooks.distance(m1)
lev <- hatvalues(m1)
dfb <- dfbetas(m1)

cook_thr <- 4 / n
lev_thr <- 2 * p / n

n_cook_hi <- sum(cooks > cook_thr, na.rm = TRUE)
n_lev_hi <- sum(lev > lev_thr, na.rm = TRUE)

# DFBETAs: common heuristic |dfbeta| > 2/sqrt(n)
dfb_thr <- 2 / sqrt(n)
n_dfb_hi <- sum(apply(abs(dfb), 1, max, na.rm = TRUE) > dfb_thr, na.rm = TRUE)

inf_details <- paste0(
  sprintf("Cook's D: max=%s; threshold=4/n=%s; count(>thr)=%d\n", fmt_num(max(cooks, na.rm = TRUE), 4), fmt_num(cook_thr, 6), n_cook_hi),
  sprintf("Leverage (hat): max=%s; threshold=2p/n=%s; count(>thr)=%d\n", fmt_num(max(lev, na.rm = TRUE), 4), fmt_num(lev_thr, 6), n_lev_hi),
  sprintf("DFBETAs: threshold=2/sqrt(n)=%s; rows with any |DFBETA|>thr: %d\n", fmt_num(dfb_thr, 6), n_dfb_hi),
  "Indices of potentially influential rows (complete-case row numbers):\n",
  "  - Cook's D >", fmt_num(cook_thr, 6), ": ", paste(which(cooks > cook_thr), collapse = ", "), "\n",
  "  - Leverage >", fmt_num(lev_thr, 6), ": ", paste(which(lev > lev_thr), collapse = ", "), "\n",
  "  - Any |DFBETA| >", fmt_num(dfb_thr, 6), ": ", paste(which(apply(abs(dfb), 1, max, na.rm = TRUE) > dfb_thr), collapse = ", ")
)

# Decide PASS/REVIEW/FAIL: if none flagged => PASS; else REVIEW (need inspection), unless extreme
if (n_cook_hi == 0 && n_lev_hi == 0 && n_dfb_hi == 0) {
  status_line("Influential observations (Cook's D / leverage / DFBETAs)", "PASS", inf_details)
} else {
  # extreme if any Cook's D > 1
  if (any(cooks > 1, na.rm = TRUE)) {
    status_line("Influential observations (Cook's D / leverage / DFBETAs)", "FAIL", inf_details)
  } else {
    status_line("Influential observations (Cook's D / leverage / DFBETAs)", "REVIEW", inf_details)
  }
}

# ----------------------------
# Assumption #9: Overdispersion (binomial)
# ----------------------------
disp <- summary(m1)$deviance / summary(m1)$df.residual
disp_details <- sprintf("Dispersion statistic = Deviance/df = %s (heuristic PASS if <= 1.5; REVIEW otherwise).",
                       fmt_num(disp, 4))
if (!is.na(disp) && disp <= 1.5) {
  status_line("Overdispersion", "PASS", disp_details)
} else {
  status_line("Overdispersion", "REVIEW", disp_details)
}

# ----------------------------
# Model fit: Hosmer-Lemeshow (optional)
# ----------------------------
if (has_pkg("ResourceSelection")) {
  # hoslem.test needs numeric y and predicted probabilities
  phat <- fitted(m1)
  # Use g=10 groups by default
  hl <- tryCatch(ResourceSelection::hoslem.test(dat_cc$DV_bin, phat, g = 10), error = function(e) e)
  if (inherits(hl, "error")) {
    status_line("Hosmer-Lemeshow goodness-of-fit (ResourceSelection)", "REVIEW",
                paste0("Test failed: ", hl$message))
  } else {
    hl_p <- hl$p.value
    hl_details <- paste(capture.output(print(hl)), collapse = "\n")
    # HL p>0.05 often interpreted as "no evidence of poor fit" (but has limitations).
    if (!is.na(hl_p) && hl_p > 0.05) {
      status_line("Hosmer-Lemeshow goodness-of-fit (ResourceSelection)", "PASS", hl_details)
    } else {
      status_line("Hosmer-Lemeshow goodness-of-fit (ResourceSelection)", "REVIEW", hl_details)
    }
  }
} else {
  status_line("Hosmer-Lemeshow goodness-of-fit (requires ResourceSelection)", "REVIEW",
              "Package 'ResourceSelection' not available. Install it to run hoslem.test().")
}

# ----------------------------
# Assumption #10: Correct specification / omitted variables
# ----------------------------
status_line("Model specification (omitted variables, measurement validity, functional form)", "REVIEW",
            "Requires substantive/domain review; cannot be determined from the CSV alone.")

# ----------------------------
# Report-ready model outputs
# ----------------------------
cat("=== Model Output (Report-Ready) ===\n\n")

# Likelihood-ratio test vs null
ll0 <- as.numeric(logLik(m0))
ll1 <- as.numeric(logLik(m1))
lr_stat <- 2 * (ll1 - ll0)
lr_df <- (attr(logLik(m1), "df") - attr(logLik(m0), "df"))
lr_p <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

cat(sprintf("Likelihood-ratio test vs null: X^2(%d) = %s, p = %s\n",
            lr_df, fmt_num(lr_stat, 4), fmt_num(lr_p, 6)))

# Pseudo-R^2
n <- nrow(dat_cc)
mcfadden <- 1 - (ll1 / ll0)
coxsnell <- 1 - exp((2 / n) * (ll0 - ll1))
nagelkerke <- coxsnell / (1 - exp((2 / n) * ll0))

cat(sprintf("Pseudo-R^2: McFadden = %s; Cox-Snell = %s; Nagelkerke = %s\n\n",
            fmt_num(mcfadden, 4), fmt_num(coxsnell, 4), fmt_num(nagelkerke, 4)))

# Coefficients table
sm <- summary(m1)
co <- sm$coefficients

beta <- co[, "Estimate"]
se <- co[, "Std. Error"]
z <- co[, "z value"]
pval <- co[, "Pr(>|z|)"]

or <- exp(beta)
or_l <- exp(beta - 1.96 * se)
or_u <- exp(beta + 1.96 * se)

out <- data.frame(
  Term = rownames(co),
  Beta = beta,
  SE = se,
  z = z,
  p = pval,
  OR = or,
  OR_2.5 = or_l,
  OR_97.5 = or_u,
  row.names = NULL,
  check.names = FALSE
)

# Print with reasonable formatting
print_out <- out
num_cols <- c("Beta", "SE", "z", "p", "OR", "OR_2.5", "OR_97.5")
for (cc in num_cols) print_out[[cc]] <- as.numeric(print_out[[cc]])

cat("Coefficient table (Wald):\n")
print(print_out, digits = 6, row.names = FALSE)
cat("\n")

# Additional items often reported
cat("Additional model diagnostics:\n")
cat(sprintf("AIC = %s\n", fmt_num(AIC(m1), 4)))
cat(sprintf("BIC = %s\n", fmt_num(BIC(m1), 4)))
cat(sprintf("Null deviance = %s on %d df\n", fmt_num(m1$null.deviance, 4), m1$df.null))
cat(sprintf("Residual deviance = %s on %d df\n", fmt_num(m1$deviance, 4), m1$df.residual))
cat(sprintf("Dispersion (deviance/df) = %s\n", fmt_num(disp, 4)))
cat("\n")

cat("Done.\n")
