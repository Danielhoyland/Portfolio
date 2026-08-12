import numpy as np

class LogisticRegression:
    def __init__(self, learning_rate=0.01, epochs=1000):
        self.lr = learning_rate
        self.epochs = epochs
        self.weights = None
        self.bias = None

    def sigmoid(self, z):
        return 1 / (1 + np.exp(-z))

    def fit(self, X, y):
        n_samples, n_features = X.shape
        self.weights = np.zeros(n_features)
        self.bias = 0

        for _ in range(self.epochs):
            # Linear model
            lin_model = np.dot(X, self.weights) + self.bias
            y_pred = self.sigmoid(lin_model)
            
            # Gradients
            dw = (1/n_samples) * np.dot(X.T, (y_pred - y))
            db = (1/n_samples) * np.sum(y_pred - y)
            
            # Update parameters
            self.weights -= self.lr * dw
            self.bias -= self.lr * db

    def predict_proba(self, X):
        lin_model = np.dot(X, self.weights) + self.bias
        return self.sigmoid(lin_model)

    def predict(self, X):
        proba = self.predict_proba(X)
        return (proba >= 0.5).astype(int)
        
    def accuracy(self, true_values, predictions):
        return np.mean(true_values == predictions)