
# coding: utf-8

# In[568]:


import numpy as np
import pandas as pd
from random import randrange
np.random.seed(100)
from sklearn.utils import resample
from sklearn.utils import shuffle
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
from sklearn.preprocessing import StandardScaler


# In[2]:


#splitting the dataset
def train_test_split2(data,beg,end):
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


# In[587]:


#Creating the logistic resression class
class logistic_regression(object):
    
    def __init__(self,alpha, lmda, iterations, intercept):
        self.alpha= alpha
        self.lmda = lmda
        self.iterations = iterations
        self.intercept = intercept

#Defining the sigmoid function        
    def sigmoid(self, value):
        sig = 1/(1+np.exp((-1)*value))
        return sig


    
#Calculating the weights    
    def logistic_reg(self, X, Y):
        
        if self.intercept == True:
            X = np.column_stack((np.ones((X.shape[0],1)), X))
            
        theta=np.zeros(X.shape[1])
        for i in range(self.iterations):
            Y_pred = self.sigmoid(np.dot(X,theta))
            theta = theta + self.alpha*(X.T.dot(Y - Y_pred) + self.lmda*theta)/Y.size
            loss = self.logistic_loss(Y, Y_pred, theta)
            if i%2000 == 0:
                print("Logistic loss:{}".format(loss))
        return theta

#Calculating the logistic loss    
    def logistic_loss(self, Y, Y_pred, theta):
        loss = (-Y.T.dot(np.log(Y_pred)) - ((1-Y).T.dot(np.log(1-Y_pred)))
                 + ((self.lmda/2)*np.sum(np.dot(theta.T, theta))) )
        return loss

#Predicting the probability
    def predict(self, X, theta):
        if self.intercept == True:
            X = np.column_stack((np.ones((X.shape[0],1)), X))
        classes = np.where((np.dot(X, theta))>=0, 1, 0)
        return classes

    
    def predict_prob(self, X, theta):
        if self.intercept == True:
            X = np.column_stack((np.ones((X.shape[0],1)), X))
        prob = self.sigmoid(np.dot(X, theta))
        return prob

#Calculating the accuracy
    def accuracy(self, Y, Y_pred):
        acc = (np.sum(Y==Y_pred)/(Y==Y_pred).shape[0])*100
        #err = 100 - acc
        return acc


# In[4]:


#Calculating the precision and recall
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


# In[569]:


#Reading the data
Data= pd.read_csv("C:/Users/Nikhar/Desktop/SML/Project/Changed_data/PERM_Changes.csv")


# In[570]:


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


# In[571]:


#scaling the data
scaler = StandardScaler()
scaler.fit(xdata)
xdata = scaler.transform(xdata)
xdata=scale(xdata)


# In[572]:


#implementing the pca
pca = PCA(n_components=654)
pca.fit(xdata)
pca = PCA(n_components=500)
pca.fit(xdata)
X1=pca.fit_transform(xdata)
data1=pd.DataFrame(np.column_stack((ydata, X1)))
data1 = data1.rename(columns={0: 'CASE_STATUS'})
Data_logistic=shuffle(data1)


# In[6]:


#Data_logistic= pd.read_csv("C:/Users/Nikhar/Desktop/SML/Project/Changed_data/Add_PCA.csv")


# In[573]:


#Downsampling the data
Data_logistic_majority=Data_logistic[Data_logistic['CASE_STATUS']==1]
Data_logistic_minority=Data_logistic[Data_logistic['CASE_STATUS']==0]


# In[574]:


#Data_logistic[Data_logistic['CASE_STATUS']==1].shape


# In[575]:



Data1_majority_downsample= resample(Data_logistic_majority, n_samples=23130,replace=False,random_state=100)


# In[576]:


#Data1_majority_downsample.shape


# In[577]:


Data2=pd.concat([Data1_majority_downsample, Data_logistic_minority])


# In[578]:


Data2 = Data2.sample(frac=1).reset_index(drop=True)


# In[579]:


Data_Training=Data2.iloc[0:37008]
Data_Testing=Data2.iloc[37008:46260]


# In[580]:



Data3=Data_Training.values.tolist()


# In[588]:


logistic_obj = logistic_regression( alpha=0.0001,lmda=0.0001, iterations=20000, intercept=True)


# In[586]:


size=Data_Training.shape[0]
TP=[]
FP=[]
TN=[]
FN=[]
accurate=[]
k=5
best_accuracy=0
for fold in range(k):
    beg= int((size/k)*fold)
    end= int((size/k)*(fold+1))
    train_x,train_y,test_x,test_y=train_test_split2(Data3,beg,end)
    theta_logistic = logistic_obj.logistic_reg(X=train_x, Y=train_y)
    Y_predicted = logistic_obj.predict(test_x, theta_logistic)
    acc=logistic_obj.accuracy(test_y, Y_predicted)
    print (acc)
    if (acc>best_accuracy or best_accuracy==0):
        model_final=theta_logistic
        best_accuracy=acc
    


# In[583]:


print("Best training accuracy is {}".format(best_accuracy))   


# In[302]:


#Data_Testing
Xtest=np.array(Data_Testing.iloc[:,1:501])
Ytest=np.array(Data_Testing.iloc[:,0])


# In[552]:


Y_predicted=logistic_obj.predict(Xtest, model_final)
acc=logistic_obj.accuracy(Ytest, Y_predicted)
TP,FP,TN,FN=measure(Ytest, Y_predicted)



# In[554]:


print("Testing accuracy is {}".format(acc))  


# In[555]:


precission=TP/(TP+FP)
recall=(TP/(TP+FN))


# In[557]:


print("Precision is {} and recall is {}".format(precission,recall))

