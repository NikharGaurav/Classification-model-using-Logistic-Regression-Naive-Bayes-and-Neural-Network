
# coding: utf-8

# In[142]:


import numpy as np
import pandas as pd
import math as mt
from random import randrange
from sklearn.utils import resample
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
from sklearn.preprocessing import StandardScaler
from sklearn.utils import shuffle


# In[137]:


#splitting the dataset
def train_test_split1(data,beg,end):
    test=[]
    #train_size=ratio*len(data)
    size=end-beg
    train=list(data)
    while len(test)<size:
        test.append(train.pop(beg))
        #beg=beg+1
    data_x_train=[]
    data_y_train=[]
    data_x_test=[]
    data_y_test=[]
    for i in range(len(train)):
        data_x_train.append(train[i][1:])
        data_y_train.append(train[i][0])
    for i in range(len(test)):
        data_x_test.append(test[i][1:])
        data_y_test.append(test[i][0])
    return np.asarray(data_x_train), np.asarray(data_y_train),np.asarray(data_x_test),np.asarray(data_y_test)


# In[3]:


#Function to calculate mean and standard deviation 
def mean(x):
    return sum(x)/len(x)

def standard_deviation(x):
    average=mean(x)
    variance = sum([pow(i-average,2) for i in x])/float(len(x)-1)
    return mt.sqrt(variance)


# In[4]:


#Function to calculate mean and standard deviation for each variable
def summarise(data):
    summ=[(mean(i),standard_deviation(i))for i in zip(*data)]
    del summ[-1]
    return summ


# In[5]:


#Separating dataset for each classification
def distribute_class(data):
    distribute={}
    for i in range(len(data)):
        vector= data[i]
        if ( vector[-1] not in distribute):
            distribute[vector[-1]]=[]
        distribute[vector[-1]].append(vector)
    return distribute


# In[6]:


#Calculating mean and standard deviation for each class
def summ_class(data_x,data_y):
    data=[]
    for i in range(len(data_x)):
        value=list(np.append(data_x[i],data_y[i]))
        data.append(value)
    distribute=distribute_class(data)
    summary={}
    for key, value in distribute.items():
        summary[key]=summarise(value)
    return summary


# In[7]:


#Calculating Gaussian probability density function
def gaussian_probability(x,mean,std):
    if(std==0):
        std=1
    exponent=mt.exp(-(mt.pow(x-mean,2)/(2*mt.pow(std,2))))
    return (1/(mt.sqrt(2*mt.pi)*std))*exponent    


# In[8]:


#Calculating probability for class using gaussian probability density function
def class_probability(summary,inputvector):
    probability={}
    for key, value in summary.items():
        probability[key]=1
        for i in range(len(value)):
            mean,std= value[i]
            x=inputvector[i]
            probability[key] *= gaussian_probability(x,mean,std)
    return probability


# In[9]:


#finding class with largest probability
def predict(summary,inputvector):
    probability= class_probability(summary,inputvector)
    classlable=''
    prob=-1
    for key,value in probability.items():
        if classlable=='' or value>prob:
            prob=value
            classlable=key
    return classlable
        


# In[10]:


#making preddiction for test data
def getPredictions(summary,test):
    prediction=[]
    for i in range(len(test)):
        result=predict(summary,test[i])
        prediction.append(result)
    return prediction


# In[11]:


#calculating the accuracy
def accuracy(test,prediction):
    correct=0
    for i in range(len(test)):
        if test[i]==prediction[i]:
            correct=correct+1
    return(correct/float(len(test)))


# In[12]:


#Calculating true positive, true negative, false positive and false negative
def measure(y_act, y_pred):
    TP=0
    FP=0
    TN=0
    FN=0
    for i in range(len(y_act)):
        if y_act[i]==y_pred[i]==1:
            TP += 1
        if y_pred[i]==1 and y_act[i]!=y_pred[i]:
            FP += 1
        if y_act[i]==y_pred[i]==0:
            TN += 1
        if y_pred[i]==0 and y_act[i]!=y_pred[i]:
            FN += 1
    return(TP,FP,TN,FN)


# In[169]:


#Reading the data
Data= pd.read_csv("C:/Users/Nikhar/Desktop/SML/Project/Changed_data/PERM_Changes.csv")


# In[170]:


#one hot encoding
Data1=Data
data_encoded = pd.get_dummies(Data1, columns=['RECEIVED_YEAR','FW_OWNERSHIP_INTEREST','PW_LEVEL_9089',
                                         'JI_LIVE_IN_DOM_SVC_CONTRACT',
                                         'RECR_INFO_COLL_TEACH_COMP_PROC','RI_COLL_TCH_BASIC_PROCESS',
                                         'RI_POSTED_NOTICE_AT_WORKSITE','RI_US_WORKERS_CONSIDERED',
                                         'FOREIGN_WORKER_INFO_EDUCATION','FW_INFO_TRAINING_COMP', 'FW_INFO_REQ_EXPERIENCE',
                                         'FW_INFO_ALT_EDU_EXPERIENCE','FW_INFO_REL_OCCUP_EXP',
                                         'EMPLOYER_YR_ESTAB_Cat', 'JOB_INFO_WORK_STATE_Cat',
                                         'COUNTRY_OF_CITIZENSHIP_Cat',
                                         'CLASS_OF_ADMISSION_Cat'
                                         ], drop_first=True)
Data1=data_encoded
xdata=np.array(Data1.iloc[:,1:655],dtype="int64")
ydata=np.array(Data1.iloc[:,0],dtype="int64")


# In[171]:


#scaling the data
scaler = StandardScaler()
scaler.fit(xdata)
xdata = scaler.transform(xdata)
xdata=scale(xdata)


# In[172]:


#implementing the pca
pca = PCA(n_components=654)
pca.fit(xdata)
pca = PCA(n_components=500)
pca.fit(xdata)
X1=pca.fit_transform(xdata)
data1=pd.DataFrame(np.column_stack((ydata, X1)))
data1 = data1.rename(columns={0: 'CASE_STATUS'})
data_naive=shuffle(data1)


# In[90]:


#reading data
#Data1= pd.read_csv("C:/Users/Nikhar/Desktop/SML/Project/Changed_data/Add_PCA.csv")


# In[174]:


#Downsampling the data
Data1_majority=data_naive[data_naive['CASE_STATUS']==1]
Data1_minority=data_naive[data_naive['CASE_STATUS']==0]


# In[175]:


Data1_majority_downsample= resample(Data1_majority, n_samples=23130,replace=False,random_state=100)


# In[176]:


#Data1_majority_downsample.shape


# In[177]:


Data2=pd.concat([Data1_majority_downsample, Data1_minority])


# In[178]:


#Data2.shape


# In[179]:


Data2 = Data2.sample(frac=1).reset_index(drop=True)


# In[180]:


Data_Training=Data2.iloc[0:37008]
Data_Testing=Data2.iloc[37008:46260]


# In[181]:


Data3=Data_Training.values.tolist()


# In[184]:


size=Data_Training.shape[0]
k=5

accurate=[]
best_accuracy=0
for fold in range(k):
    beg= int((size/k)*fold)
    end= int((size/k)*(fold+1))
    train_x,train_y,test_x,test_y=train_test_split1(Data3,beg,end)
    modal=summ_class(train_x,train_y)
    p=getPredictions(modal,test_x)
    acc=accuracy(test_y,p)
    #print (acc)
    if (acc>best_accuracy or best_accuracy==0):
        modal_final=modal
        best_accuracy=acc




# In[185]:


print("Best training accuracy is {}".format(best_accuracy)) 


# In[130]:


#Data_Testing
Xtest=np.array(Data_Testing.iloc[:,1:501])
Ytest=np.array(Data_Testing.iloc[:,0])


# In[132]:


Y_predicted=getPredictions(modal_final,Xtest)
acc=accuracy(Ytest, Y_predicted)
TP,FP,TN,FN=measure(Ytest, Y_predicted)


# In[186]:


print("Testing accuracy is {}".format(acc))  


# In[134]:


precission=TP/(TP+FP)
recall=(TP/(TP+FN))


# In[135]:


print("Precision is {} and recall is {}".format(precission,recall))

