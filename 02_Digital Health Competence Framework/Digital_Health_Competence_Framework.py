### Digital Competence Framework(Rohit Goswami)

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

# ═══════════════════════════════════════════════════════════
#  PART 1 — PLS-SEM  (R seminr equivalent)
# ═══════════════════════════════════════════════════════════
# Note: Python has no direct seminr equivalent. We use semopy
# for SEM estimation. For a full PLS-PM workflow identical to
# seminr, consider the R–Python bridge (rpy2) or the plspm
# package if available.
# ═══════════════════════════════════════════════════════════

import semopy

# ── Load data ──
plsdata = pd.read_csv("Playdata1.csv")

# ── Define measurement + structural model in semopy syntax ──
# Reflective constructs use =~ (latent measured BY indicators)
# Formative/composite constructs use <~ (latent formed BY indicators)
# Structural paths use ~
model_spec = """
# --- Measurement model ---
# Reflective: DLIT (Digital Literacy)
DLIT =~ D12_1 + D12_2 + D12_3 + D12_4 + D12_5 + D12_6

# Reflective: HLIT (Health Literacy)
HLIT =~ D12_7 + D12_8 + D12_9 + D12_10

# Formative / composite: DSS (Digital Soft Skills)
# semopy does not natively support mode_B composites;
# we approximate with regression-style indicators.
# For true PLS mode_B, see the plspm note below.
DSS =~ D12_11 + D12_12 + D12_13 + D12_14 + D12_15 + D12_16

# Formative / composite: EHS (eHealth Skills)
EHS =~ D12_17 + D12_18 + D12_19 + D12_20

# --- Structural model ---
DSS ~ DLIT
EHS ~ HLIT
"""

# ── Estimate SEM ──
sem_model = semopy.Model(model_spec)
sem_model.fit(plsdata)

print("=== SEM Parameter Estimates ===")
estimates = sem_model.inspect()
print(estimates.to_string(index=False))


# ── Bootstrap (semopy supports bootstrapping via semopy.semplot or manual) ──
# semopy does not have a built-in bootstrap_model(); we implement manually.
def bootstrap_sem(data, model_spec, n_boot=5000, seed=42):
    """Bootstrap SEM parameter estimates."""
    rng = np.random.default_rng(seed)
    n = len(data)
    boot_params = []

    for i in range(n_boot):
        idx = rng.integers(0, n, size=n)
        boot_data = data.iloc[idx].reset_index(drop=True)
        try:
            m = semopy.Model(model_spec)
            m.fit(boot_data)
            params = m.inspect()
            params["boot_iter"] = i
            boot_params.append(params)
        except Exception:
            continue  # skip failed iterations

    return pd.concat(boot_params, ignore_index=True)


print("\nRunning bootstrap (5000 iterations) — this may take a few minutes...")
boot_df = bootstrap_sem(plsdata, model_spec, n_boot=5000)

# Summarise bootstrap
boot_summary = (
    boot_df.groupby(["lval", "op", "rval"])["Estimate"]
    .agg(
        [
            "mean",
            "std",
            lambda x: np.percentile(x, 2.5),
            lambda x: np.percentile(x, 97.5),
        ]
    )
    .rename(
        columns={
            "mean": "boot_mean",
            "std": "boot_se",
            "<lambda_0>": "ci_lower",
            "<lambda_1>": "ci_upper",
        }
    )
    .reset_index()
)
print("\n=== Bootstrap Summary ===")
print(boot_summary.to_string(index=False))

# ── Construct scores (latent variable scores) ──
construct_scores = sem_model.predict(plsdata)
# Keep only latent constructs
latent_cols = ["DLIT", "HLIT", "DSS", "EHS"]
available_cols = [c for c in latent_cols if c in construct_scores.columns]
construct_scores = construct_scores[available_cols]

print("\n=== Construct Scores (first 6 rows) ===")
print(construct_scores.head(6))
construct_scores.to_csv("construct_scores.csv", index=False)

# ── Scale construct scores to 0–100 ──
construct_scores_scaled = construct_scores.copy()
for col in available_cols:
    x = construct_scores_scaled[col]
    r_min, r_max = x.min(), x.max()
    if r_max == r_min:
        construct_scores_scaled[f"{col}_index"] = 50.0
    else:
        construct_scores_scaled[f"{col}_index"] = (x - r_min) / (r_max - r_min) * 100

construct_scores_scaled.to_csv("construct_scores_scaled.csv", index=False)
print("\nScaled scores saved to construct_scores_scaled.csv")


# ═══════════════════════════════════════════════════════════
#  PART 2 — OLS Regression with Natural Splines
# ═══════════════════════════════════════════════════════════

import statsmodels.api as sm
import statsmodels.formula.api as smf
from patsy import dmatrix, bs  # patsy provides natural spline basis via cr()

# ── Load regression data ──
df = pd.read_csv("Regr.csv")

# Convert categorical predictors to category dtype
cat_cols = [
    "SEX",
    "AMPLITUDE",
    "EMPLOYMENT_STATUS",
    "HIGHEST_QUALIFICATION",
    "PROFESSION",
    "D1_1",
    "REGION",
]
for col in cat_cols:
    df[col] = df[col].astype("category")

# ── DLIT model: AGE modelled non-linearly via natural spline (df=3) ──
# patsy's cr() is equivalent to R's ns()
formula_DLIT = (
    "DLIT ~ cr(AGE, df=3) + C(SEX) + C(AMPLITUDE) + C(EMPLOYMENT_STATUS) "
    "+ C(HIGHEST_QUALIFICATION) + C(PROFESSION) + C(D1_1) + C(REGION)"
)

M1_DLIT = smf.ols(formula_DLIT, data=df).fit()
print("\n" + "=" * 60)
print("MODEL 1 — DLIT")
print("=" * 60)
print(M1_DLIT.summary())

# ── HLIT model ──
formula_HLIT = (
    "HLIT ~ cr(AGE, df=3) + C(SEX) + C(AMPLITUDE) + C(EMPLOYMENT_STATUS) "
    "+ C(HIGHEST_QUALIFICATION) + C(PROFESSION) + C(D1_1) + C(REGION)"
)

M1_HLIT = smf.ols(formula_HLIT, data=df).fit()
print("\n" + "=" * 60)
print("MODEL 2 — HLIT")
print("=" * 60)
print(M1_HLIT.summary())


# ═══════════════════════════════════════════════════════════
#  Diagnostics
# ═══════════════════════════════════════════════════════════


def plot_diagnostics(model, label):
    """Residual diagnostics: Residuals vs Fitted + Q-Q plot."""
    residuals = model.resid
    fitted = model.fittedvalues

    fig, axes = plt.subplots(1, 2, figsize=(12, 5))

    # Residuals vs Fitted
    axes[0].scatter(fitted, residuals, alpha=0.4, edgecolors="k", s=20)
    axes[0].axhline(0, color="red", linestyle="--")
    axes[0].set_xlabel("Fitted values")
    axes[0].set_ylabel("Residuals")
    axes[0].set_title(f"{label} — Residuals vs Fitted")

    # Q-Q plot
    stats.probplot(residuals, dist="norm", plot=axes[1])
    axes[1].set_title(f"{label} — Normal Q-Q")

    plt.tight_layout()
    plt.savefig(f"{label}_diagnostics.png", dpi=300)
    plt.show()


plot_diagnostics(M1_DLIT, "DLIT")
plot_diagnostics(M1_HLIT, "HLIT")


# ═══════════════════════════════════════════════════════════
#  Coefficient tables + model performance
# ═══════════════════════════════════════════════════════════


def tidy_model(model, model_name):
    """Mimic R broom::tidy() — return tidy coefficient DataFrame."""
    params = model.params
    bse = model.bse
    tvalues = model.tvalues
    pvalues = model.pvalues
    ci = model.conf_int()

    tidy_df = pd.DataFrame(
        {
            "term": params.index,
            "estimate": params.values.round(3),
            "std_error": bse.values.round(3),
            "statistic": tvalues.values.round(3),
            "p_value": pvalues.values.round(3),
            "conf_low": ci[0].values.round(3),
            "conf_high": ci[1].values.round(3),
            "model": model_name,
        }
    )
    return tidy_df


coef_DLIT = tidy_model(M1_DLIT, "DLIT")
coef_HLIT = tidy_model(M1_HLIT, "HLIT")

print("\n=== DLIT Regression Coefficients ===")
print(coef_DLIT.to_string(index=False))

print("\n=== HLIT Regression Coefficients ===")
print(coef_HLIT.to_string(index=False))


def model_performance(model, label):
    """Mimic R performance::model_performance() for OLS."""
    n = model.nobs
    k = model.df_model
    resid = model.resid
    rmse = np.sqrt(np.mean(resid**2))
    sigma = np.sqrt(model.mse_resid)

    return {
        "Outcome": label,
        "RMSE": round(rmse, 3),
        "R2": round(model.rsquared, 3),
        "R2_adjusted": round(model.rsquared_adj, 3),
        "AIC": round(model.aic, 3),
        "BIC": round(model.bic, 3),
        "Sigma": round(sigma, 3),
    }


perf_DLIT = model_performance(M1_DLIT, "DLIT")
perf_HLIT = model_performance(M1_HLIT, "HLIT")
fit_tab = pd.DataFrame([perf_DLIT, perf_HLIT])

print("\n=== Model Fit Comparison ===")
print(fit_tab.to_string(index=False))


# ═══════════════════════════════════════════════════════════
#  Export to Excel (R writexl equivalent)
# ═══════════════════════════════════════════════════════════

with pd.ExcelWriter("Regression_results.xlsx", engine="openpyxl") as writer:
    coef_DLIT.to_excel(writer, sheet_name="DLIT_coefficients", index=False)
    coef_HLIT.to_excel(writer, sheet_name="HLIT_coefficients", index=False)
    fit_tab.to_excel(writer, sheet_name="Model_fit", index=False)

print("\nRegression results saved to Regression_results.xlsx")


# ═══════════════════════════════════════════════════════════
#  Age Spline Plots (ggplot2 → matplotlib/seaborn)
# ═══════════════════════════════════════════════════════════


def plot_age_spline(model, df, outcome_label, highlight_ages=None, save_path=None):
    """
    Plot the non-linear AGE effect from an OLS model with natural splines.
    Mirrors the R function plot_age_spline_pretty().
    """
    age_seq = np.arange(df["AGE"].min(), df["AGE"].max() + 1, 1)

    # Build prediction DataFrame — hold categoricals at their first level
    newdat = pd.DataFrame({"AGE": age_seq})
    for col in cat_cols:
        newdat[col] = df[col].cat.categories[0]
        newdat[col] = newdat[col].astype(df[col].dtype)

    # Predict with confidence intervals
    pred = model.get_prediction(newdat)
    pred_summary = pred.summary_frame(alpha=0.05)

    newdat["fit"] = pred_summary["mean"]
    newdat["lower"] = pred_summary["mean_ci_lower"]
    newdat["upper"] = pred_summary["mean_ci_upper"]

    # Plot
    fig, ax = plt.subplots(figsize=(7, 5))
    ax.fill_between(
        newdat["AGE"],
        newdat["lower"],
        newdat["upper"],
        color="grey",
        alpha=0.3,
        label="95% CI",
    )
    ax.plot(newdat["AGE"], newdat["fit"], color="black", linewidth=1.2)

    # Highlight specific ages
    if highlight_ages is not None:
        for age in highlight_ages:
            hdat = pd.DataFrame({"AGE": [age]})
            for col in cat_cols:
                hdat[col] = df[col].cat.categories[0]
                hdat[col] = hdat[col].astype(df[col].dtype)
            h_pred = model.get_prediction(hdat).summary_frame(alpha=0.05)
            y_val = h_pred["mean"].values[0]
            ax.plot(age, y_val, "ko", markersize=6)
            ax.annotate(
                f"Age {age}\n{y_val:.1f}",
                xy=(age, y_val),
                xytext=(0, 12),
                textcoords="offset points",
                ha="center",
                fontsize=9,
            )

    ax.set_xlabel("Age (years)", fontsize=12)
    ax.set_ylabel(f"Predicted {outcome_label}", fontsize=12)
    ax.set_title(f"Effect of Age on {outcome_label}", fontweight="bold", fontsize=14)
    ax.text(
        0.5,
        1.02,
        "Natural spline (df = 3); other covariates at reference levels",
        transform=ax.transAxes,
        ha="center",
        fontsize=10,
        style="italic",
    )
    ax.grid(True, alpha=0.3)
    sns.despine()
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches="tight")
        print(f"Saved: {save_path}")
    plt.show()


# ── DLIT age spline plot ──
plot_age_spline(
    M1_DLIT, df, "DLIT", highlight_ages=[25, 50], save_path="DLIT_age_spline.png"
)

# ── HLIT age spline plot ──
plot_age_spline(
    M1_HLIT, df, "HLIT", highlight_ages=[25, 50], save_path="HLIT_age_spline.png"
)
