import pandas as pd
import numpy as np
import model as model
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer

# --- Load and prepare preprocessing ---
data = pd.read_csv("../data/data.csv", sep=';')
data.columns = data.columns.str.strip().str.replace('"', '')

X_cols = data.drop(columns=['Target'])
numeric_cols = X_cols.select_dtypes(include=[np.number]).columns.tolist()
categorical_cols = X_cols.select_dtypes(include=['object']).columns.tolist()

preprocessor = ColumnTransformer([
    ('num', StandardScaler(), numeric_cols),
    ('cat', OneHotEncoder(), categorical_cols)
])
preprocessor.fit(X_cols)

model = model.model  # your trained LogisticRegressionBinary model

def predict_student(data_dict):
    """
    Returns:
        tuple: (predicted_label:str, probability:float, top_dropout_factors:list)
    """
    df = pd.DataFrame([data_dict])
    X = preprocessor.transform(df)

    # --- Model prediction ---
    probs = model.predict_proba(X)
    pred_label = (probs >= 0.5).astype(int)[0][0]
    pred_prob = float(probs[0][0] if pred_label == 1 else 1 - probs[0][0])

    label = "Dropout" if pred_label == 1 else "Graduate"

    # --- Compute feature contributions ---
    coefs = model.W.flatten()
    feature_names = (
        numeric_cols +
        preprocessor.named_transformers_['cat']
        .get_feature_names_out(categorical_cols)
        .tolist()
    )

    # multiply each feature value by its weight → contribution
    X_array = X.toarray() if hasattr(X, "toarray") else X
    contributions = X_array[0] * coefs
    contrib_series = pd.Series(contributions, index=feature_names).sort_values(ascending=False)

    # --- Top features indicating higher dropout likelihood ---
    top_dropout_factors = contrib_series.head(5).index.tolist()

    # Make names readable
    top_dropout_factors = [
        name.replace("Curricular units 1st sem", "1st semester")
            .replace("Curricular units 2nd sem", "2nd semester")
            .replace("_", " ")
            .title()
        for name in top_dropout_factors
    ]

    return label, pred_prob, top_dropout_factors
