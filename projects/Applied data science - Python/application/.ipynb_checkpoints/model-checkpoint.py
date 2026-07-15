#!/usr/bin/env python
# coding: utf-8

# In[36]:


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


# In[37]:


data = pd.read_csv('../data/data.csv', sep=';')
data.columns = data.columns.str.strip().str.replace('"', '')

data = data[data['Target'] != 'Enrolled']

target_mapping = {'Graduate': 0, 'Dropout': 1}
data['Target'] = data['Target'].map (target_mapping)

print(data.head())
print(data.columns)


# In[38]:


numeric_cols = data.select_dtypes(include=[np.number]).columns.drop('Target')
categorical_cols = data.select_dtypes(include=['object']).columns

preprocessor = ColumnTransformer([
    ('num', StandardScaler(), numeric_cols),
    ('cat', OneHotEncoder(), categorical_cols)
])
X_processed = preprocessor.fit_transform(data.drop('Target', axis=1))
y = data['Target'].values


# In[39]:


X_train, X_test, y_train, y_test = train_test_split(
    X_processed, y, test_size=0.2, random_state=42
)


# In[40]:


n_classes = len(np.unique(y))
Y_train_onehot = np.zeros((len(y_train), n_classes))
for i, val in enumerate(y_train):
    Y_train_onehot[i, val] = 1

Y_test_onehot = np.zeros((len(y_test), n_classes))
for i, val in enumerate(y_test):
    Y_test_onehot[i, val] = 1


# In[41]:


class SoftmaxRegression:
    def __init__(self, learning_rate=0.1, epochs=5000):
        self.lr = learning_rate
        self.epochs = epochs
        self.W = None
        self.b = None

    def softmax(self, z):
        exp_z = np.exp(z - np.max(z, axis=1, keepdims=True))
        return exp_z / np.sum(exp_z, axis=1, keepdims=True)

    def fit(self, X, Y):
        n_samples, n_features = X.shape
        n_classes = Y.shape[1]
        self.W = np.zeros((n_features, n_classes))
        self.b = np.zeros((1, n_classes))

        for _ in range(self.epochs):
            logits = X @ self.W + self.b
            probs = self.softmax(logits)
            dW = (1/n_samples) * (X.T @ (probs - Y))
            db = (1/n_samples) * np.sum(probs - Y, axis=0, keepdims=True)
            self.W -= self.lr * dW
            self.b -= self.lr * db

    def predict(self, X):
        logits = X @ self.W + self.b
        probs = self.softmax(logits)
        return np.argmax(probs, axis=1)


# In[42]:


model = SoftmaxRegression(learning_rate=0.1, epochs=5000)
model.fit(X_train, Y_train_onehot)
y_test_pred = model.predict(X_test)
y_test_true = y_test
accuracy = accuracy_score(y_test_true, y_test_pred)
print(f"Test Accuracy: {accuracy:.4f}")


# In[43]:


plt.figure(figsize=(8,5))
plt.hist(data[data['Target']==0]['Admission grade'], bins=20, alpha=0.6, label='Graduate', color='blue')
plt.hist(data[data['Target']==1]['Admission grade'], bins=20, alpha=0.6, label='Dropout', color='red')
plt.xlabel('Admission grade')
plt.ylabel('Count')
plt.title('Admission Grade Distribution by Target')
plt.legend()
plt.show()


# In[44]:


corr = data[numeric_cols.tolist() + ['Target']].corr()
plt.figure(figsize=(12,8))
plt.imshow(corr, cmap='coolwarm', interpolation='nearest')
plt.colorbar()
plt.xticks(range(len(corr)), corr.columns, rotation=90)
plt.yticks(range(len(corr)), corr.columns)
plt.title('Correlation Heatmap')
plt.show()


# In[45]:


cm = confusion_matrix(y_test_true, y_test_pred)
print("Confusion Matrix:")
print(cm)


# In[46]:


fig, ax = plt.subplots()
im = ax.imshow(cm, cmap='Blues')

for i in range(len(cm)):
    for j in range(len(cm[i])):
        ax.text(j, i, cm[i, j], ha='center', va='center', color='black')

ax.set_xlabel('Predicted label')
ax.set_ylabel('True label')
ax.set_xticks([0, 1])
ax.set_yticks([0, 1])
ax.set_xticklabels(['Graduate', 'Dropout'])
ax.set_yticklabels(['Graduate', 'Dropout'])
plt.title("Confusion Matrix")
plt.show()

print("\nClassification Report:")
print(classification_report(y_test_true, y_test_pred, target_names=['Graduate', 'Dropout']))


# In[47]:


enrolled_data = pd.read_csv('../data/data.csv', sep=';')
enrolled_data.columns = enrolled_data.columns.str.strip().str.replace('"', '')


# In[48]:


enrolled_rows = enrolled_data[enrolled_data['Target'] == 'Enrolled']

X_enrolled = enrolled_rows.drop('Target', axis=1)

X_enrolled_processed = preprocessor.transform(X_enrolled)


# In[49]:


logits = X_enrolled_processed @ model.W + model.b
probs = model.softmax(logits) 


# In[50]:


pred_class = np.argmax(probs, axis=1)
pred_mapping = {0: 'Graduate', 1: 'Dropout'}

enrolled_rows['Predicted'] = [pred_mapping[c] for c in pred_class]


# In[51]:


print("\nPredicted outcomes for Enrolled students:")
print(enrolled_rows[['Admission grade', 'Predicted']].head(10))


# In[52]:


y_test_bin = label_binarize(y_test_true, classes=[0, 1])


# In[53]:


logits_test = X_test @ model.W + model.b
probs_test = model.softmax(logits_test)


# In[54]:


fpr, tpr, thresholds = roc_curve(y_test_bin.ravel(), probs_test[:,1].ravel())
roc_auc = auc(fpr, tpr)


# In[55]:


plt.figure(figsize=(8,6))
plt.plot(fpr, tpr, color='blue', lw=2, label=f'ROC curve (AUC = {roc_auc:.3f})')
plt.plot([0,1], [0,1], color='gray', lw=1, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver Operating Characteristic (ROC) Curve')
plt.legend(loc="lower right")
plt.grid(True)
plt.show()


# In[ ]:




