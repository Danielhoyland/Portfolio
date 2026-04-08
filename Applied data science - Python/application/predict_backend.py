import pandas as pd
import numpy as np
from model import model, cat_encoder, numeric_cols, categorical_cols, preprocessor

def predict_student(data_dict):
    """
    Predicts student dropout likelihood and returns:
        (predicted_label:str, probability:float, top_dropout_factors:list)
    """

    # Convert input to DataFrame
    df = pd.DataFrame([data_dict])

    # Ensure all expected columns exist
    expected_cols = numeric_cols.tolist() + categorical_cols.tolist()
    for col in expected_cols:
        if col not in df.columns:
            df[col] = 0  # Fill missing numeric/categorical columns with 0

    # --- Preprocess features ---
    X_processed = preprocessor.transform(df[expected_cols])

    # --- Model prediction ---
    probs = model.predict_proba(X_processed)  # shape: (n_samples, 1)
    pred_prob = float(probs[0])  # probability of class 1 (Dropout)
    pred_label = int(pred_prob >= 0.5)
    label = "Dropout" if pred_label == 1 else "Graduate"
    probability = round(pred_prob * 100, 2) if pred_label == 1 else round((1 - pred_prob) * 100, 2)


    # --- Feature contributions for top factors ---
    feature_names_num = numeric_cols.tolist()
    feature_names_cat = cat_encoder.get_feature_names_out(categorical_cols).tolist()
    feature_names = feature_names_num + feature_names_cat

    # Align feature names with model weights (avoid mismatch)
    W = model.W.flatten()
    min_len = min(len(feature_names), len(W))
    feature_names = feature_names[:min_len]
    W = W[:min_len]

    # Compute feature contributions based on current input
    X_row = X_processed[0][:min_len]
    contributions = X_row * W

    # Create Series sorted by impact
    contrib_series = pd.Series(contributions, index=feature_names).sort_values(ascending=False)

    # --- Top 5 dropout risk factors ---
    top_dropout_factors = contrib_series.head(5).index.tolist()
    top_dropout_factors = [
        name.replace("Curricular units 1st sem", "1st semester")
            .replace("Curricular units 2nd sem", "2nd semester")
            .replace("_", " ")
            .replace("X0_", "")
            .title()
        for name in top_dropout_factors
    ]

    return label, probability, top_dropout_factors
