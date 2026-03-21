### Student Data Low

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.nonparametric.smoothers_lowess import lowess
from statsmodels.stats.diagnostic import het_breuschpagan

# ══════════════════════════════════════════════
#  Load & Inspect
# ══════════════════════════════════════════════
data = pd.read_csv("Student_data_low.csv")
print(data.dtypes)
print(data.describe())

# Step 2: Replace blanks with NaN
data.replace("", np.nan, inplace=True)
print("\nMissing values:\n", data.isna().sum())

# Step 3: Summary
print(data.describe(include="all"))


# ══════════════════════════════════════════════
#  Step 4: Mode Imputation
# ══════════════════════════════════════════════
def get_mode(series):
    return series.mode()[0]


for col in ["Parental_Education_Level", "Distance_from_Home", "Teacher_Quality"]:
    if col in data.columns and data[col].isna().any():
        data[col].fillna(get_mode(data[col]), inplace=True)

# ══════════════════════════════════════════════
#  Step 5: Encode Categorical Variables
# ══════════════════════════════════════════════
encoded_data = data.copy()

# Ordinal encoding (1-indexed to match R factor(..., ordered=TRUE))
ordinal_vars = {
    "Motivation_Level": ["Low", "Medium", "High"],
    "Parental_Involvement": ["Low", "Medium", "High"],
    "Access_to_Resources": ["Low", "Medium", "High"],
    "Family_Income": ["Low", "Medium", "High"],
    "Teacher_Quality": ["Low", "Medium", "High"],
    "Parental_Education_Level": ["High School", "College", "Postgraduate"],
    "Distance_from_Home": ["Near", "Moderate", "Far"],
}

for var, levels in ordinal_vars.items():
    if var in encoded_data.columns:
        cat_type = pd.CategoricalDtype(categories=levels, ordered=True)
        encoded_data[var] = encoded_data[var].astype(cat_type).cat.codes + 1

# Binary encoding
binary_vars = {
    "Internet_Access": ("Yes", "No"),
    "Learning_Disabilities": ("Yes", "No"),
    "Extracurricular_Activities": ("Yes", "No"),
    "School_Type": ("Public", "Private"),
    "Gender": ("Female", "Male"),
}

for var, (pos, neg) in binary_vars.items():
    if var in encoded_data.columns:
        encoded_data[var] = (encoded_data[var] == pos).astype(int)

# Peer_Influence as category (factor)
if "Peer_Influence" in encoded_data.columns:
    encoded_data["Peer_Influence"] = encoded_data["Peer_Influence"].astype("category")

# ══════════════════════════════════════════════
#  Step 2b: Log Transformation
# ══════════════════════════════════════════════
encoded_data["Log_Exam_Score"] = np.log(encoded_data["Exam_Score"] + 1)

# ══════════════════════════════════════════════
#  Reduced Model (Fixed Effects Only — OLS)
# ══════════════════════════════════════════════
formula_fixed = (
    "Log_Exam_Score ~ Attendance + Hours_Studied + Motivation_Level "
    "+ Internet_Access + Access_to_Resources "
    "+ Sleep_Hours + Tutoring_Sessions "
    "+ Previous_Scores "
    "+ Gender + Parental_Involvement "
    "+ Extracurricular_Activities + C(Peer_Influence) "
    "+ Teacher_Quality * School_Type "
    "+ Teacher_Quality * Learning_Disabilities "
    "+ Learning_Disabilities * Distance_from_Home "
    "+ Physical_Activity * Learning_Disabilities"
)

Reduced_model = smf.ols(formula_fixed, data=encoded_data).fit()
print("\n=== Reduced Model (OLS) ===")
print(Reduced_model.summary())

# ══════════════════════════════════════════════
#  Full Model (Mixed Effects — random intercept
#  for Parental_Education_Level)
# ══════════════════════════════════════════════
encoded_data["Parental_Education_Level_grp"] = encoded_data[
    "Parental_Education_Level"
].astype(str)

formula_mixed = (
    "Log_Exam_Score ~ Attendance + Hours_Studied + Motivation_Level "
    "+ Internet_Access + Access_to_Resources "
    "+ Sleep_Hours + Tutoring_Sessions "
    "+ Previous_Scores "
    "+ Gender + Parental_Involvement "
    "+ Extracurricular_Activities + C(Peer_Influence) "
    "+ Teacher_Quality * School_Type "
    "+ Teacher_Quality * Learning_Disabilities "
    "+ Learning_Disabilities * Distance_from_Home "
    "+ Physical_Activity * Learning_Disabilities"
)

log_model = smf.mixedlm(
    formula_mixed,
    data=encoded_data,
    groups=encoded_data["Parental_Education_Level_grp"],
    re_formula="1",
).fit(reml=False)

print("\n=== Full Model (Mixed Effects) ===")
print(log_model.summary())

# ══════════════════════════════════════════════
#  Likelihood Ratio Test (Full vs Reduced)
# ══════════════════════════════════════════════
ll_full = log_model.llf
ll_reduced = Reduced_model.llf
lrt_stat = -2 * (ll_reduced - ll_full)
lrt_df = 1  # one extra parameter (random intercept variance)
lrt_pval = 1 - stats.chi2.cdf(lrt_stat, df=lrt_df)

print(f"\n=== Likelihood Ratio Test ===")
print(f"Chi²:    {lrt_stat:.4f}")
print(f"df:      {lrt_df}")
print(f"p-value: {lrt_pval:.6f}")

# ══════════════════════════════════════════════
#  Step 5: Residual Analysis
# ══════════════════════════════════════════════
residuals_full = log_model.resid
fitted_values = log_model.fittedvalues

# Q-Q Plot
fig, ax = plt.subplots(figsize=(6, 5))
stats.probplot(residuals_full, dist="norm", plot=ax)
ax.set_title("Q-Q Plot of Residuals")
plt.tight_layout()
plt.savefig("low_qq_plot.png", dpi=150)
plt.show()

# Breusch-Pagan Test
bp_stat, bp_p, _, _ = het_breuschpagan(residuals_full, log_model.model.exog)
print(f"\n=== Breusch-Pagan Test ===")
print(f"Statistic: {bp_stat:.4f},  p-value: {bp_p:.6f}")

# ══════════════════════════════════════════════
#  Step 6: Scale-Location Plot
# ══════════════════════════════════════════════
fig, ax = plt.subplots(figsize=(7, 5))
ax.scatter(
    fitted_values, np.sqrt(np.abs(residuals_full)), alpha=0.5, color="blue", s=10
)
smooth = lowess(np.sqrt(np.abs(residuals_full)), fitted_values, frac=0.6)
ax.plot(smooth[:, 0], smooth[:, 1], color="red", linewidth=1)
ax.set_xlabel("Fitted Values")
ax.set_ylabel("√|Standardized Residuals|")
ax.set_title("Scale-Location Plot")
plt.tight_layout()
plt.savefig("low_scale_location.png", dpi=150)
plt.show()

# ══════════════════════════════════════════════
#  Step 7: Random Effects Validation (Nakagawa R²)
# ══════════════════════════════════════════════
var_fixed = np.var(log_model.fittedvalues)
var_random = float(log_model.cov_re.iloc[0, 0])
var_resid = log_model.scale

r2_marginal = var_fixed / (var_fixed + var_random + var_resid)
r2_conditional = (var_fixed + var_random) / (var_fixed + var_random + var_resid)

print(f"\n=== Nakagawa R² ===")
print(f"Marginal R²:    {r2_marginal:.4f}")
print(f"Conditional R²: {r2_conditional:.4f}")

# ══════════════════════════════════════════════
#  Step 8: Backward Feature Selection
# ══════════════════════════════════════════════
print("\n=== Backward Feature Selection ===")
current_formula = formula_mixed

while True:
    model = smf.mixedlm(
        current_formula,
        data=encoded_data,
        groups=encoded_data["Parental_Education_Level_grp"],
        re_formula="1",
    ).fit(reml=False)

    pvals = model.pvalues.drop("Intercept", errors="ignore")

    if len(pvals) == 0:
        break

    max_p = pvals.max()
    if max_p < 0.05:
        print("All remaining variables are significant (p < 0.05). Stopping.")
        break

    remove_var = pvals.idxmax()
    print(f"  Removing: {remove_var}  (p = {max_p:.4f})")

    current_formula = current_formula.replace(f" + {remove_var}", "").replace(
        f"+ {remove_var}", ""
    )

    try:
        test_model = smf.mixedlm(
            current_formula,
            data=encoded_data,
            groups=encoded_data["Parental_Education_Level_grp"],
            re_formula="1",
        ).fit(reml=False)
        log_model = test_model
    except Exception as e:
        print(f"  Could not refit after removing {remove_var}: {e}")
        break

print("\n=== Final Model Summary ===")
print(log_model.summary())

# ══════════════════════════════════════════════
#  Fitted vs Residuals Plot (final model)
# ══════════════════════════════════════════════
fitted_final = log_model.fittedvalues
resid_final = log_model.resid

fig, ax = plt.subplots(figsize=(7, 5))
ax.scatter(fitted_final, resid_final, alpha=0.5, color="blue", s=10)
smooth = lowess(resid_final, fitted_final, frac=0.6)
ax.plot(smooth[:, 0], smooth[:, 1], color="black", linewidth=1)
ax.set_xlabel("Fitted Values")
ax.set_ylabel("Residuals")
ax.set_title("Fitted vs Residuals Plot")
plt.tight_layout()
plt.savefig("low_fitted_vs_residuals.png", dpi=150)
plt.show()
