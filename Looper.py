import time
import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score
from sklearn.metrics import recall_score
from sklearn.metrics import precision_score
from sklearn.metrics import confusion_matrix
import os
import gc

os.chdir(r'C:\Users\Prathwish\Desktop\model')


nn_hdim_list=[1,2,3,4,5,6,7,8,9,10,100,500]
m_list=[1000,500,100,10,1]
reg_lambda_list=[0.1,0.01,0.001,0.0001]
epsilon_list=[0.1,0.01,0.001,0.0001]



#Data Prep for models
Xtrain=np.array(train.iloc[:,1:501],dtype="int64")
Ytrain=np.array(train.iloc[:,0],dtype="int64")
Xvalidate=np.array(validate.iloc[:,1:501],dtype="int64")
Yvalidate=np.array(validate.iloc[:,0],dtype="int64")
Xtest=np.array(test.iloc[:,1:501],dtype="int64")
Ytest=np.array(test.iloc[:,0],dtype="int64")

del var,var1,xdata,ydata,X1,test,validate,data,data_encoded
gc.collect()
 
d=[]
for nn_hdim in nn_hdim_list:
    for m in m_list:
        print('m',m)
        for reg_lambda in reg_lambda_list:
            print('reg_lambda',reg_lambda)
            for epsilon in epsilon_list:
                print('epsilon',epsilon)
                start=time.clock()
                #--insert model code here---#
                model=neuralNet(train,nn_hdim,2,m,reg_lambda,epsilon,4)
                end=time.clock()
                tntrain, fptrain, fntrain, tptrain=confusion_matrix(Ytrain, predict(model,Xtrain)).ravel()
                tnval, fpval, fnval, tpval=confusion_matrix(Yvalidate, predict(model,Xvalidate)).ravel()
                tntest, fptest, fntest, tptest=confusion_matrix(Ytest, predict(model,Xtest)).ravel()
                d.append((nn_hdim,
                          m,
                          reg_lambda,
                          epsilon,
                          tntrain, fptrain, fntrain, tptrain,
                          tnval, fpval, fnval, tpval,
                          tntest, fptest, fntest, tptest,
                          end - start))
                
            gc.collect()
        with open("model"+str(nn_hdim)+"_"+str(m)+".csv",'w') as f:
            f.write( ','.join(str(d)) )
 
data=pd.DataFrame(d, columns=('nn_hdim', 'm','reg_lambda', 'epsilon','tntrain', 'fptrain', 'fntrain', 'tptrain','tnval',
                              'fpval', 'fnval', 'tpval','tntest', 'fptest', 'fntest', 'tptest','time'))        




 