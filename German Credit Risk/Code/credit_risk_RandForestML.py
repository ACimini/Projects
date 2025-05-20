import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import random

from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import (
    accuracy_score, classification_report, confusion_matrix,
    roc_curve, auc, roc_auc_score
)

# Set seeds for reproducibility
np.random.seed(1001)
random.seed(1001)

# Load data
test_data_path = "C:/Users/alexc/Desktop/Projects/German Credit Risk/Data/german_credit_data.csv"
data = pd.read_csv(test_data_path)
data = data.drop(columns=["Unnamed: 0"])  # Remove unnecessary index column

# Feature Engineering

#Credit duration amount ratio for an added "risk" metric
data['Credit_Duration_Amount_Ratio'] = data['Duration'] / data['Credit amount']

#Checks to see if they have a checking account or not
data['Has_Checking_Account'] = data['Checking account'].apply(lambda x: 0 if x == 'NA' else 1)

job_map = {
    'unskilled and non-resident': 0,
    'unskilled resident': 1,
    'skilled': 2,
    'highly skilled': 3
}
data['Job_Level'] = data['Job'].map(job_map)

data['Stable_Housing'] = data['Housing'].apply(lambda x: 1 if x == 'own' else 0)
data['Own_And_Has_Savings'] = np.where(
    (data['Housing'] == 'own') &
    (data['Saving accounts'] != 'little') &
    (data['Saving accounts'] != 'NA'), 1, 0
)

# Define target and features
target = "Risk"
x = data.drop(columns=[target])
y = data[target]

# One-hot encode categorical variables
X = pd.get_dummies(x, drop_first=True)

# Check class distribution
print("Class distribution:\n", y.value_counts(normalize=True))

# Train-Test Split
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=1001, stratify=y
)

# Initialize classifier with class balancing
classifier = RandomForestClassifier(random_state=1001, class_weight='balanced')
classifier.fit(X_train, y_train)

# Cross-validation for ROC-AUC
cv_scores = cross_val_score(classifier, X, y, cv=5, scoring='roc_auc')
print(f"Cross-Validated ROC-AUC: {np.mean(cv_scores):.4f}")

# Feature Importance Plot
importances = classifier.feature_importances_
feature_names = X.columns

feature_importance_df = pd.DataFrame({
    'Feature': feature_names,
    'Importance': importances
}).sort_values(by='Importance', ascending=False)

plt.figure(figsize=(10, 6))
sns.barplot(x='Importance', y='Feature', data=feature_importance_df.head(15))
plt.title('Top 15 Feature Importances')
plt.tight_layout()
plt.show()

# Evaluation on test set
y_pred = classifier.predict(X_test)

print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))
print("Confusion Matrix:\n", confusion_matrix(y_test, y_pred))

# ROC-AUC
y_probs = classifier.predict_proba(X_test)[:, 1]
fpr, tpr, thresholds = roc_curve(y_test, y_probs, pos_label='good')
roc_auc = auc(fpr, tpr)

plt.figure(figsize=(8, 6))
plt.plot(fpr, tpr, label=f'ROC curve (AUC = {roc_auc:.2f})')
plt.plot([0, 1], [0, 1], 'k--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver Operating Characteristic (ROC) Curve')
plt.legend(loc='lower right')
plt.grid()
plt.show()

print("Test ROC-AUC Score:", roc_auc_score(y_test, y_probs))


# Features that were tested but not implemented
'''
# 3. Simplified purpose categories
investment_purposes = ['education', 'business', 'repairs']
data['Purpose_Type'] = data['Purpose'].apply(lambda x: 'Investment' if x in investment_purposes else 'Consumer')

# 4. High-risk purchase intent
risky_purposes = ['radio/TV', 'car', 'furniture/equipment', 'domestic appliances', 'vacation/others']
data['High_Risk_Purpose'] = data['Purpose'].apply(lambda x: 1 if x in risky_purposes else 0)

Binning age groups
data['Age_Bin'] = pd.cut(data['Age'], bins=[0, 25, 50, 100], labels=['Young', 'Adult', 'Senior'])
'''

