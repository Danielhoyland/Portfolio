#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.metrics import roc_curve, auc
from sklearn.preprocessing import label_binarize
from sklearn.model_selection import StratifiedKFold
import seaborn as sns


# In[2]:


import sys
sys.path.append("../dataProcessing")

from data_processing import data
print(data['Target'])


# In[3]:


print(data['Nacionality'])


# In[4]:


data = data.dropna(subset=['Target'])
def map_nacionality(nationality):
    if nationality.strip().lower() == "purtuguese":
        return 1
    else:
        return 2

data['Nacionality'] = data['Nacionality'].apply(map_nacionality)


# In[5]:


numeric_cols = data.select_dtypes(include=[np.number]).columns.drop('Target')
categorical_cols = data.select_dtypes(include=['object']).columns

preprocessor = ColumnTransformer([
    ('num', StandardScaler(), numeric_cols),
    ('cat', OneHotEncoder(drop='first'), categorical_cols)
])

X_processed = preprocessor.fit_transform(data.drop('Target', axis=1))
y = data['Target'].values
print(data['Nacionality'])


# In[6]:


#X_train, X_test, y_train, y_test = train_test_split(X_processed, y, test_size=0.2, random_state=42, stratify=y)


# In[7]:


class LogisticRegressionBinary:
    def __init__(self, learning_rate=0.1, epochs=50):
        self.lr = learning_rate
        self.epochs = epochs
        self.W = None
        self.b = None
        self.losses = [] 

    def sigmoid(self, z):
        return 1 / (1 + np.exp(-z))

    def binary_cross_entropy(self, y_true, y_pred):
        epsilon = 1e-15 
        y_pred = np.clip(y_pred, epsilon, 1-epsilon)
        return -np.mean(y_true*np.log(y_pred) + (1-y_true)*np.log(1-y_pred))

    def fit(self, X, y):
        n_samples, n_features = X.shape
        self.W = np.zeros((n_features, 1))
        self.b = 0

        for _ in range(self.epochs):
            linear = X @ self.W + self.b
            y_pred = self.sigmoid(linear)
            dw = (1/n_samples) * (X.T @ (y_pred - y.reshape(-1,1)))
            db = (1/n_samples) * np.sum(y_pred - y.reshape(-1,1))
            self.W -= self.lr * dw
            self.b -= self.lr * db

            loss = self.binary_cross_entropy(y, y_pred)
            self.losses.append(loss)

    def predict_proba(self, X):
        return self.sigmoid(X @ self.W + self.b)

    def predict(self, X, threshold=0.5):
        return (self.predict_proba(X) >= threshold).astype(int).ravel()
    def top_dropout_features(self, X_student, feature_names, top_n=5):
        """
        Find top features contributing most to dropout likelihood for one student.

        Parameters
        ----------
        X_student : array-like, shape (n_features,)
            The preprocessed feature vector for one student.
        feature_names : list of str
            Names of the features in the same order as model.W.
        top_n : int, optional (default=5)
            Number of top features to return.

        Returns
        -------
        pandas.Series : feature → contribution value
        """
        if X_student.ndim > 1:
            X_student = X_student.flatten()

        contributions = X_student * self.W.flatten()
        contrib_series = pd.Series(contributions, index=feature_names)

        # Positive weights → push toward dropout
        top_feats = contrib_series.sort_values(ascending=False).head(top_n)
        return top_feats

# In[8]:


#model = LogisticRegressionBinary(learning_rate=0.1, epochs=5000)
#model.fit(X_train, y_train)
#y_pred = model.predict(X_test)
#accuracy = accuracy_score(y_test, y_pred)
#print(f"Test Accuracy: {accuracy:.4f}")
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)

accuracies = []
fold = 1

for train_index, test_index in skf.split(X_processed, y):
    X_train, X_test = X_processed[train_index], X_processed[test_index]
    y_train, y_test = y[train_index], y[test_index]

    model = LogisticRegressionBinary(learning_rate=0.1, epochs=500)
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)

    acc = accuracy_score(y_test, y_pred)
    print(f"Fold {fold} Accuracy: {acc:.4f}")
    accuracies.append(acc)
    fold += 1

print(f"\nAverage Accuracy: {np.mean(accuracies):.4f} ± {np.std(accuracies):.4f}")


# In[9]:





# In[10]:


corr = data[numeric_cols.tolist() + ['Target']].corr()



# In[11]:


numeric_cols = data.select_dtypes(include=['int64', 'float64']).columns
corr_with_target = data[numeric_cols].corr()['Target'].drop('Target')
corr_with_target = corr_with_target.sort_values()



# In[13]:




# In[14]:


# In[15]:


cat_encoder = OneHotEncoder(drop='first', handle_unknown='ignore')
cat_encoder.fit(data[categorical_cols])

feature_names_num = numeric_cols.tolist()
feature_names_cat = cat_encoder.get_feature_names_out(categorical_cols).tolist()
feature_names = feature_names_num + feature_names_cat

feature_names = feature_names[:model.W.shape[0]]

importance_grad = -model.W.flatten()
importance_grad = pd.Series(importance_grad, index=feature_names).sort_values(ascending=False)


importance_dropout = pd.Series(model.W.flatten(), index=feature_names).sort_values(ascending=False)




# In[20]:


# In[21]:


y_test_bin = label_binarize(y_test, classes=[0, 1])
probs_test = model.predict_proba(X_test)


# In[22]:


fpr, tpr, thresholds = roc_curve(y_test_bin, probs_test)
roc_auc = auc(fpr, tpr)


# In[23]:




# In[16]:
# Ensure preprocessor is fitted before export
if not hasattr(preprocessor.named_transformers_['cat'], "categories_"):
    print("⚙️ Fitting preprocessor before saving model.py...")
    X = data.drop(columns=['Target'])
    preprocessor.fit(X)


__all__ = ['model', 'preprocessor', 'numeric_cols', 'categorical_cols']


# In[ ]:




