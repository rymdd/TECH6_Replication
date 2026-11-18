"""Extension B: "poor" index for each municipality.
Lasso (LassoCV, 5 folds) that predicts the high school share of women 15-20 in 2000 from things fixed before 1994.
Trained only on municipalities without an Islamic mayor, so the prediction is the schooling we would expect without one.
Political variables are left out so poor does not overlap with pious. Called from R/08_extension_b.R, which writes the input csv first"""


import pandas as pd
from sklearn.linear_model import LassoCV
from sklearn.metrics import r2_score
from sklearn.model_selection import KFold, cross_val_predict
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

df = pd.read_csv("data/derived/ext_b_lasso_input.csv")
predictors = [c for c in df.columns if c not in ["muni_id", "hischshr1520f", "i94"]]

untreated = df["i94"] == 0
X = df.loc[untreated, predictors]
y = df.loc[untreated, "hischshr1520f"]

# fixed seed, so the folds are the same in every run
folds = KFold(n_splits=5, shuffle=True, random_state=20261120)
lasso = make_pipeline(StandardScaler(), LassoCV(cv=folds))
lasso.fit(X, y)

# for the untreated use out-of-fold predictions, so no municipality is predicted with its own outcome
df["pred_schooling"] = lasso.predict(df[predictors])
oof = cross_val_predict(lasso, X, y, cv=folds)
df.loc[untreated, "pred_schooling"] = oof
df[["muni_id", "pred_schooling"]].to_csv("data/derived/ext_b_predicted_schooling.csv", index=False)

fit = pd.DataFrame({
    "n_train": [untreated.sum()],
    "folds": [5],
    "alpha": [lasso[-1].alpha_],
    "r2_in_sample": [lasso.score(X, y)],
    "r2_out_of_fold": [r2_score(y, oof)],
})
fit.to_csv("output/tables/ext_b_lasso_fit.csv", index=False)

coefs = pd.DataFrame({"predictor": predictors, "coefficient_per_sd": lasso[-1].coef_})
coefs.to_csv("output/tables/ext_b_lasso_coefficients.csv", index=False)
print(coefs)
