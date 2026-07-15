#!/usr/bin/env python
# coding: utf-8

# In[35]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


# In[54]:


data = pd.read_csv('../data/data.csv', sep=';')
data.columns = data.columns.str.strip().str.replace('"', '')

target_mapping = {'Graduate': 0, 'Dropout': 1}
data['Target'] = data['Target'].map (target_mapping)

print(data.head())
print(data.columns)


# In[55]:


nationality_map = {
    1: 'Portuguese', 2: 'German', 6: 'Spanish', 11: 'Italian', 13: 'Dutch',
    14: 'English', 17: 'Lithuanian', 21: 'Angolan', 22: 'Cape Verdean',
    24: 'Guinean', 25: 'Mozambican', 26: 'Santomean', 32: 'Turkish',
    41: 'Brazilian', 62: 'Romanian', 100: 'Moldovan', 101: 'Mexican',
    103: 'Ukrainian', 105: 'Russian', 108: 'Cuban', 109: 'Colombian'
}
tempdata = data.copy()

tempdata['Nacionality'] = tempdata['Nacionality'].map(nationality_map).fillna('Other')

plt.figure(figsize=(12,6))
tempdata['Nacionality'].value_counts().plot(kind='bar', color='teal', edgecolor='black')

plt.title('Distribution of Student Nationalities', fontsize=14)
plt.xlabel('Nationality', fontsize=12)
plt.ylabel('Number of Students', fontsize=12)
plt.xticks(rotation=45, ha='right')
plt.tight_layout()
plt.savefig('nationality_distribution.png', dpi=300, bbox_inches='tight')
plt.show()
plt.close()
nationality_counts = tempdata['Nacionality'].value_counts()

for nationality, count in nationality_counts.items():
    print(f"{nationality}: {count}")


# In[56]:


tempdata = data.copy()

tempdata['Nacionality'] = tempdata['Nacionality'].map(nationality_map).fillna('Other')

non_portuguese = tempdata[tempdata['Nacionality'] != 'Portuguese']

plt.figure(figsize=(12,6))
non_portuguese['Nacionality'].value_counts().plot(kind='bar', color='teal', edgecolor='black')

plt.title('Distribution of Non-Portuguese Student Nationalities', fontsize=14)
plt.xlabel('Nationality', fontsize=12)
plt.ylabel('Number of Students', fontsize=12)
plt.xticks(rotation=45, ha='right')
plt.tight_layout()
plt.savefig('non_portuguese_distribution.png')
plt.show()
plt.close()


# In[57]:


data['Nacionality'] = data['Nacionality'].map(nationality_map)
data['Nacionality'] = data['Nacionality'].apply(lambda x: 'Purtuguese' if x == 'Portuguese' else 'Other')

plt.figure(figsize=(6,6))
data['Nacionality'].value_counts().plot(kind='bar', color=['teal', 'orange'], edgecolor='black')

plt.title('Portuguese vs Other Nationalities', fontsize=14)
plt.xlabel('Nationality Group', fontsize=12)
plt.ylabel('Number of Students', fontsize=12)
plt.xticks(rotation=0)
plt.tight_layout()
plt.savefig('porto_vs_other.png')
plt.show()
plt.close()


# In[58]:


summary = (
    data.groupby('Nacionality')['International']
    .mean()
    .reset_index()
    .rename(columns={'International': 'Percentage'})
)
summary['Percentage'] *= 100 

plt.figure(figsize=(6,5))
bars = plt.bar(summary['Nacionality'], summary['Percentage'], 
               color=['teal', 'orange'], edgecolor='black')

plt.title('Percentage of International Students: Portuguese vs Others', fontsize=14)
plt.ylabel('Percentage of International Students (%)', fontsize=12)
plt.xlabel('Nationality Group', fontsize=12)
plt.ylim(0, 110)  

for bar in bars:
    height = bar.get_height()
    if height > 90:
        plt.text(bar.get_x() + bar.get_width()/2, height - 5, f"{height:.1f}%", 
                 ha='center', va='top', color='white', fontsize=11)
    else:
        plt.text(bar.get_x() + bar.get_width()/2, height + 1, f"{height:.1f}%", 
                 ha='center', va='bottom', fontsize=11)

plt.tight_layout()
plt.savefig('international_students_comparison.png', dpi=300, bbox_inches='tight')
plt.show()
plt.close()

print(summary)


# In[59]:


marital_status_map = {1: 'Single', 2: 'Married', 3: 'Widower', 4: 'Divorced', 5: 'Facto Union', 6: 'legally separated'}
attendance_map = {1: 'Daytime', 0: 'Evening'}
gender_map = {0: 'Male', 1: 'Female'}


# In[60]:


tempdata = data.copy()

tempdata['Marital status'] = tempdata['Marital status'].map(marital_status_map).fillna('Other')
tempdata['Daytime/evening attendance'] = tempdata['Daytime/evening attendance'].map(attendance_map).fillna('Unknown')
tempdata['Gender'] = tempdata['Gender'].map(gender_map).fillna('Unknown')


# In[61]:


plt.figure(figsize=(6,4))
sns.countplot(x='Marital status', data=tempdata, palette='pastel', edgecolor='black')
plt.title('Distribution of Marital Status')
plt.ylabel('Number of Students')
plt.xlabel('Marital Status')
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()


# In[62]:


plt.figure(figsize=(6,4))
sns.countplot(x='Daytime/evening attendance', data=tempdata, palette='Set2', edgecolor='black')
plt.title('Daytime vs Evening Attendance')
plt.ylabel('Number of Students')
plt.xlabel('Attendance')
plt.tight_layout()
plt.show()


# In[63]:


plt.figure(figsize=(6,4))
sns.countplot(x='Gender', data=tempdata, palette='Set1', edgecolor='black')
plt.title('Gender Distribution')
plt.ylabel('Number of Students')
plt.xlabel('Gender')
plt.tight_layout()
plt.show()


# In[ ]:




