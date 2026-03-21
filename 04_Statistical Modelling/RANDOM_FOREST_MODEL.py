## Random forest Model
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score

# ══════════════════════════════════════════════
#  Load & Inspect Data
# ══════════════════════════════════════════════
data = pd.read_csv("Student_data_low.csv")
print(data.dtypes)
print(data.describe())

# Step 2: Clean missing values — replace empty strings with NaN
data.replace("", np.nan, inplace=True)
print("\nMissing values per column:")
print(data.isna().sum())

# Step 3: Summary
print(data.describe(include="all"))

# ══════════════════════════════════════════════
#  Step 4: Impute missing values using mode
# ══════════════════════════════════════════════
mode_cols = ["Parental_Education_Level", "Distance_from_Home", "Teacher_Quality"]
for col in mode_cols:
    if col in data.columns and data[col].isna().any():
        data[col].fillna(data[col].mode()[0], inplace=True)

# ══════════════════════════════════════════════
#  Step 5: Encode categorical variables
# ══════════════════════════════════════════════
encoded_data = data.copy()

# Ordinal encoding
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
        encoded_data[var] = (
            encoded_data[var].astype(cat_type).cat.codes + 1
        )  # 1-indexed

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

# Peer_Influence → one-hot encode (factor in R)
if "Peer_Influence" in encoded_data.columns:
    encoded_data = pd.get_dummies(
        encoded_data, columns=["Peer_Influence"], drop_first=True
    )

# ══════════════════════════════════════════════
#  Random Forest — Full Dataset
# ══════════════════════════════════════════════
X = encoded_data.drop(columns=["Exam_Score"])
y = encoded_data["Exam_Score"]

np.random.seed(123)
rf_model = RandomForestRegressor(
    n_estimators=500, max_features=4, random_state=123, n_jobs=-1
)
rf_model.fit(X, y)

predicted = rf_model.predict(X)
mse = mean_squared_error(y, predicted)
rmse = np.sqrt(mse)
mae = mean_absolute_error(y, predicted)
r2 = r2_score(y, predicted)

print(f"\n=== Full-Data Metrics ===")
print(f"MSE:  {mse:.4f}")
print(f"RMSE: {rmse:.4f}")
print(f"MAE:  {mae:.4f}")
print(f"R²:   {r2:.4f}")

# ══════════════════════════════════════════════
#  Train / Test Split — Check for Overfitting
# ══════════════════════════════════════════════
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=123
)

rf_model_split = RandomForestRegressor(
    n_estimators=500, max_features=4, random_state=123, n_jobs=-1
)
rf_model_split.fit(X_train, y_train)

test_pred = rf_model_split.predict(X_test)
test_rmse = np.sqrt(mean_squared_error(y_test, test_pred))
print(f"\nTest RMSE: {test_rmse:.4f}")

# ══════════════════════════════════════════════
#  Feature Importance
# ══════════════════════════════════════════════
importance_df = pd.DataFrame(
    {
        "Feature": X.columns,
        "Importance": rf_model_split.feature_importances_,
    }
).sort_values("Importance", ascending=True)

fig, ax = plt.subplots(figsize=(8, 7))
ax.barh(
    importance_df["Feature"],
    importance_df["Importance"],
    color="blue",
    edgecolor="black",
)
ax.set_xlabel("Importance")
ax.set_ylabel("Features")
ax.set_title("Feature Importance (Random Forest) Student data low")
plt.tight_layout()
plt.savefig("rf_feature_importance.png", dpi=150)
plt.show()

# ══════════════════════════════════════════════
#  Residual Histogram
# ══════════════════════════════════════════════
residuals = y.values - predicted

fig, ax = plt.subplots(figsize=(7, 4))
ax.hist(residuals, bins=20, color="steelblue", edgecolor="black")
ax.set_xlabel("Residuals")
ax.set_title("Residual Histogram")
plt.tight_layout()
plt.savefig("rf_residual_histogram.png", dpi=150)
plt.show()

# ══════════════════════════════════════════════
#  Correlation Matrix
# ══════════════════════════════════════════════
numeric_X = X.select_dtypes(include=[np.number])
corr = numeric_X.corr()

fig, ax = plt.subplots(figsize=(12, 10))
sns.heatmap(
    corr, annot=True, fmt=".2f", cmap="coolwarm", ax=ax, cbar_kws={"shrink": 0.8}
)
ax.set_title("Correlation Matrix")
plt.tight_layout()
plt.savefig("rf_correlation_matrix.png", dpi=150)
plt.show()

# ══════════════════════════════════════════════
#  Cross-Validated Hyperparameter Tuning
#  (R caret::train with method="rf", cv=5)
# ══════════════════════════════════════════════
param_grid = {"max_features": [2, 4, 6, 8, 10]}

tuned = GridSearchCV(
    RandomForestRegressor(n_estimators=500, random_state=123, n_jobs=-1),
    param_grid,
    cv=5,
    scoring="neg_root_mean_squared_error",
    return_train_score=True,
)
tuned.fit(X_train, y_train)

print("\n=== Cross-Validated Tuning Results ===")
results_df = pd.DataFrame(tuned.cv_results_)[
    ["param_max_features", "mean_test_score", "std_test_score"]
]
results_df["mean_test_RMSE"] = -results_df["mean_test_score"]
print(
    results_df[["param_max_features", "mean_test_RMSE", "std_test_score"]].to_string(
        index=False
    )
)
print(f"\nBest max_features: {tuned.best_params_['max_features']}")
