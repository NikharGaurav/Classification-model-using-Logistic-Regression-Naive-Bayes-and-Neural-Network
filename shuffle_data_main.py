from sklearn.utils import shuffle
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
import matplotlib.pyplot as plt

data=pd.read_csv(r'C:\Users\Prathwish\Documents\Courses\3. DS5220 - Supervised Machine Learning\Project\Nikhar\PERM_ChangesNew.csv')

list(data)
data_encoded = pd.get_dummies(data, columns=['RECEIVED_YEAR','FW_OWNERSHIP_INTEREST','PW_LEVEL_9089',
                                         'JI_LIVE_IN_DOM_SVC_CONTRACT',
                                         'RECR_INFO_COLL_TEACH_COMP_PROC','RI_COLL_TCH_BASIC_PROCESS',
                                         'RI_POSTED_NOTICE_AT_WORKSITE','RI_US_WORKERS_CONSIDERED',
                                         'FOREIGN_WORKER_INFO_EDUCATION','FW_INFO_TRAINING_COMP', 'FW_INFO_REQ_EXPERIENCE',
                                         'FW_INFO_ALT_EDU_EXPERIENCE','FW_INFO_REL_OCCUP_EXP',
                                         'EMPLOYER_YR_ESTAB_Cat', 'JOB_INFO_WORK_STATE_Cat',
                                         'COUNTRY_OF_CITIZENSHIP_Cat',
                                         'CLASS_OF_ADMISSION_Cat'
                                         ], drop_first=True)


data=data_encoded

xdata=np.array(data.iloc[:,1:655],dtype="int64")
ydata=np.array(data.iloc[:,0],dtype="int64")

from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()
scaler.fit(xdata)
xdata = scaler.transform(xdata)

xdata=scale(xdata)

pca = PCA(n_components=654)

pca.fit(xdata)

var= pca.explained_variance_ratio_

var1=np.cumsum(np.round(pca.explained_variance_ratio_, decimals=4)*100)

print(var1)

plt.plot(var1)

pca = PCA(n_components=500)
pca.fit(xdata)
X1=pca.fit_transform(xdata)
##########################################################

data=pd.DataFrame(np.column_stack((ydata, X1)))
data = data.rename(columns={0: 'CASE_STATUS'})
data=shuffle(data)

train=data.iloc[0:int(round(len(data)*.60)),:]
test=data.iloc[int(round(len(data)*.60)):int(round(len(data)*.80)),]
validate=data.iloc[int(round(len(data)*.80)):len(data),]

Xtrain=np.array(train.iloc[:,1:501],dtype="int64")
Ytrain=np.array(train.iloc[:,0],dtype="int64")


#from imblearn.under_sampling import RandomUnderSampler
#rus = RandomUnderSampler(return_indices=True)
#X_resampled, y_resampled, idx_resampled = rus.fit_sample(Xtrain, Ytrain)
#resampledTrain=pd.DataFrame(np.column_stack((y_resampled, X_resampled)))
#resampledTrain = resampledTrain.rename(columns={0: 'CASE_STATUS'})
#resampledTrain=shuffle(resampledTrain)
