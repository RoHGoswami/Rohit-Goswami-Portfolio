library(seminr)

# Load data
plsdata <- read.csv("Playdata1.csv")

# ---------------------------
# Measurement model (adds AREA and REGION as single numeric-coded items; no dummies)
# ---------------------------
measurements <- constructs(
  # Reflective constructs
  reflective("DLIT", multi_items("D12_", 1:6)),      # Digital Literacy
  reflective("HLIT", multi_items("D12_", 7:10)),     # Health Literacy
  
  # Formative/composite constructs
  composite("DSS",  multi_items("D12_", 11:16),weights = mode_B),  # Digital Soft Skills
  composite("EHS",  multi_items("D12_", 17:20), weights = mode_B)  # eHealth Skills
)

# ---------------------------
# Structural model
# ---------------------------
structure <- relationships(
  paths(from = "DLIT",    to = "DSS"),
  paths(from = "HLIT",    to = "EHS")
  
)

# ---------------------------
# Estimate PLS + bootstrap
# ---------------------------
pls_model <- estimate_pls(
  data = plsdata,
  measurement_model = measurements,
  structural_model  = structure
)
summary(pls_model)


boot_estimates <- bootstrap_model(pls_model, nboot = 5000, cores = 2)
summary(boot_estimates)



plot(boot_estimates, title = "Digital Competence PLS SEM Model")
save_plot("Final Plot.pdf")

sum_pls <- summary(pls_model)
print(sum_pls$validity$vif_items)

### Extracting Latent scores

# Extract construct (latent variable) scores
construct_scores <- pls_model$construct_scores

# Inspect first rows
head(construct_scores)

# Save to file
write.csv(construct_scores, "construct_scores.csv", row.names = FALSE)


# Scale PLS construct scores to 0–100 and save
construct_scores <- as.data.frame(pls_model$construct_scores)
cols <- intersect(names(construct_scores), c("DLIT","HLIT","DSS","EHS"))
construct_scores[paste0(cols, "_index")] <- lapply(construct_scores[cols], function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[2] == r[1]) rep(50, length(x)) else (x - r[1]) / (r[2] - r[1]) * 100
})
write.csv(construct_scores, "construct_scores_scaled.csv", row.names = FALSE)

library(readr)
library(dplyr)
library(tidyverse)
library(nlmeU)  
library(nlme)
library(lme4)
library(lattice)
library(corrplot)
library(plot.matrix)
library(ggplot2)
library(lmerTest)
library(performance)
library(broom.mixed)
library(splines)

# Data
df <- read.csv("Regr.csv")

##### Only age as non linear 

# Categorical predictors + grouping factor
df[c("SEX","AMPLITUDE","EMPLOYMENT_STATUS","HIGHEST_QUALIFICATION","PROFESSION","D1_1","REGION")] <-
  lapply(df[c("SEX","AMPLITUDE","EMPLOYMENT_STATUS","HIGHEST_QUALIFICATION","PROFESSION","D1_1","REGION")], factor)

# DLIT model AGE modeled nonlinearly via natural spline

M1_DLIT <- lm(
  DLIT ~ ns(AGE, df = 3) + SEX + AMPLITUDE + EMPLOYMENT_STATUS + HIGHEST_QUALIFICATION + PROFESSION + D1_1 + REGION,
  data = df
)

summary(M1_DLIT)

# Diagnostics (as before)
par(mfrow = c(1, 1))
plot(M1_DLIT)
qqnorm(residuals(M1_DLIT)); qqline(residuals(M1_DLIT))

#### Model for HLIT

M1_HLIT <- lm(
  HLIT ~ ns(AGE, df = 3) + SEX + AMPLITUDE + EMPLOYMENT_STATUS + HIGHEST_QUALIFICATION + PROFESSION + D1_1 + REGION,
  data = df
)

summary(M1_HLIT)

# Diagnostics (as before)
par(mfrow = c(1, 1))
plot(M1_HLIT)
qqnorm(residuals(M1_HLIT)); qqline(residuals(M1_HLIT))


library(broom.mixed)  # for tidy()
library(knitr)        # for kable()
library(dplyr)

## DLIT coefficients
coef_DLIT <- tidy(M1_DLIT, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

kable(coef_DLIT,
      caption = "Table 1. Regression coefficients for DLIT model")

## HLIT coefficients
coef_HLIT <- tidy(M1_HLIT, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

kable(coef_HLIT,
      caption = "Table 2. Regression coefficients for HLIT model")

library(performance)

perf_DLIT <- model_performance(M1_DLIT) %>%
  mutate(Outcome = "DLIT")

perf_HLIT <- model_performance(M1_HLIT) %>%
  mutate(Outcome = "HLIT")

fit_tab <- bind_rows(perf_DLIT, perf_HLIT) %>%
  select(Outcome, RMSE, R2, R2_adjusted, AIC, BIC, Sigma) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

kable(fit_tab,
      caption = "Table 3. Overall model fit for DLIT and HLIT models")




## ---------------------------------------------------------
## 0. Packages for tidying and writing to Excel
## ---------------------------------------------------------
# install.packages("writexl")   # run once if needed
# install.packages("car")       # for Type III Anova (optional)

library(dplyr)
library(broom)        # broom::tidy for lm
library(performance)  # model_performance()
library(car)
library(writexl)

## ---------------------------------------------------------
## 1. Coefficient tables with 95% CI
## ---------------------------------------------------------

coef_DLIT <- broom::tidy(M1_DLIT, conf.int = TRUE) %>%
  mutate(model = "DLIT")

coef_HLIT <- broom::tidy(M1_HLIT, conf.int = TRUE) %>%
  mutate(model = "HLIT")

## If you want rounded numbers (optional):
coef_DLIT <- coef_DLIT %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

coef_HLIT <- coef_HLIT %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

## ---------------------------------------------------------
## 2. Overall model fit table (DLIT vs HLIT)
## ---------------------------------------------------------

perf_DLIT <- performance::model_performance(M1_DLIT) %>%
  mutate(model = "DLIT")

perf_HLIT <- performance::model_performance(M1_HLIT) %>%
  mutate(model = "HLIT")

fit_tab <- bind_rows(perf_DLIT, perf_HLIT) %>%
  select(model, RMSE, R2, R2_adjusted, AIC, BIC, Sigma) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

## ---------------------------------------------------------
## 3. Type III tests per predictor (optional but very useful)
## ---------------------------------------------------------


## ---------------------------------------------------------
## 4. Write everything to a single Excel file
## ---------------------------------------------------------

write_xlsx(
  list(
    "DLIT_coefficients"  = coef_DLIT,
    "HLIT_coefficients"  = coef_HLIT,
    "Model_fit"          = fit_tab
  ),
  path = "Regression_results.xlsx"
)



library(dplyr)
library(ggplot2)

## Function to plot the AGE spline effect for a given model
plot_age_spline <- function(model, df, outcome_label) {
  
  # 1. Age grid over the observed range
  age_seq <- seq(min(df$AGE, na.rm = TRUE),
                 max(df$AGE, na.rm = TRUE),
                 by = 1)
  
  # 2. Hold all other covariates at their reference (first) level
  newdat <- data.frame(
    AGE = age_seq,
    SEX = levels(df$SEX)[1],
    AMPLITUDE = levels(df$AMPLITUDE)[1],
    EMPLOYMENT_STATUS = levels(df$EMPLOYMENT_STATUS)[1],
    HIGHEST_QUALIFICATION = levels(df$HIGHEST_QUALIFICATION)[1],
    PROFESSION = levels(df$PROFESSION)[1],
    D1_1 = levels(df$D1_1)[1],
    REGION = levels(df$REGION)[1]
  )
  
  # 3. Predictions from the spline model (with SE for CI)
  pred <- predict(model, newdata = newdat, se.fit = TRUE)
  
  plot_data <- newdat %>%
    mutate(
      fit   = pred$fit,
      se    = pred$se.fit,
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
  
  # 4. ggplot of the age effect
  ggplot(plot_data, aes(x = AGE, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(size = 1) +
    labs(
      x = "Age",
      y = paste("Predicted", outcome_label),
      title = paste("Non-linear effect of Age on", outcome_label),
      subtitle = "Natural spline (df = 3); other covariates at reference levels"
    ) +
    theme_minimal()
}

## ----- DLIT age spline plot -----
p_DLIT_age <- plot_age_spline(M1_DLIT, df, "DLIT")
p_DLIT_age

## ----- HLIT age spline plot -----
p_HLIT_age <- plot_age_spline(M1_HLIT, df, "HLIT")
p_HLIT_age


library(dplyr)
library(ggplot2)

## Pretty age-spline plot ----------------------------------------------
plot_age_spline_pretty <- function(model, df, outcome_label,
                                   highlight_ages = NULL) {
  
  # 1. Age grid over observed range (1-year steps)
  age_seq <- seq(min(df$AGE, na.rm = TRUE),
                 max(df$AGE, na.rm = TRUE),
                 by = 1)
  
  # 2. Hold other covariates at their reference (first) level
  newdat <- data.frame(
    AGE = age_seq,
    SEX = levels(df$SEX)[1],
    AMPLITUDE = levels(df$AMPLITUDE)[1],
    EMPLOYMENT_STATUS = levels(df$EMPLOYMENT_STATUS)[1],
    HIGHEST_QUALIFICATION = levels(df$HIGHEST_QUALIFICATION)[1],
    PROFESSION = levels(df$PROFESSION)[1],
    D1_1 = levels(df$D1_1)[1],
    REGION = levels(df$REGION)[1]
  )
  
  # 3. Predictions + 95% CI
  pred <- predict(model, newdata = newdat, se.fit = TRUE)
  
  plot_data <- newdat %>%
    mutate(
      fit   = pred$fit,
      se    = pred$se.fit,
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
  
  # Optional: highlight specific ages (e.g. c(25, 50))
  highlight_data <- NULL
  if (!is.null(highlight_ages)) {
    hd <- data.frame(
      AGE = highlight_ages,
      SEX = levels(df$SEX)[1],
      AMPLITUDE = levels(df$AMPLITUDE)[1],
      EMPLOYMENT_STATUS = levels(df$EMPLOYMENT_STATUS)[1],
      HIGHEST_QUALIFICATION = levels(df$HIGHEST_QUALIFICATION)[1],
      PROFESSION = levels(df$PROFESSION)[1],
      D1_1 = levels(df$D1_1)[1],
      REGION = levels(df$REGION)[1]
    )
    ph <- predict(model, newdata = hd, se.fit = TRUE)
    highlight_data <- hd %>%
      mutate(fit = ph$fit)
  }
  
  # 4. Plot
  p <- ggplot(plot_data, aes(x = AGE, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "grey80", alpha = 0.6) +
    geom_line(size = 1.1, colour = "black") +
    labs(
      x = "Age (years)",
      y = paste("Predicted", outcome_label),
      title = paste("Effect of Age on", outcome_label),
      subtitle = "Natural spline (df = 3); other covariates at reference levels"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  if (!is.null(highlight_data)) {
    p <- p +
      geom_point(data = highlight_data,
                 aes(x = AGE, y = fit),
                 size = 3, colour = "black") +
      geom_text(
        data = highlight_data,
        aes(label = paste0("Age ", AGE, "\n", round(fit, 1)),
            x = AGE, y = fit),
        vjust = -1, size = 4
      )
  }
  
  p
}

## ----- DLIT age spline (with highlighted ages 25 and 50) -----
p_DLIT_age <- plot_age_spline_pretty(M1_DLIT, df, "DLIT",
                                     highlight_ages = c(25, 50))
p_DLIT_age

## ----- HLIT age spline -----
p_HLIT_age <- plot_age_spline_pretty(M1_HLIT, df, "HLIT",
                                     highlight_ages = c(25, 50))
p_HLIT_age

## For high-quality print output you can save, for example:
ggsave("DLIT_age_spline.png", p_DLIT_age, width = 7, height = 5, dpi = 300)
ggsave("HLIT_age_spline.png", p_HLIT_age, width = 7, height = 5, dpi = 300)










