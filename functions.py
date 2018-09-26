def predict(model, x):
    W1, b1, W2, b2 = model['W1'], model['b1'], model['W2'], model['b2']
    z1 = x.dot(W1) + b1
    a1 = np.tanh(z1)
    z2 = a1.dot(W2) + b2
    exp_scores = np.exp(z2)
    probs = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)
    return np.argmax(probs, axis=1)


def softmax(x):
    #Compute the softmax of vector x in a numerically stable way.
    shiftx = x - np.max(x)
    exps = np.exp(shiftx)
    return exps / np.sum(exps)


def neuralNet(data,nn_hdim,nn_output_dim,m,reg_lambda,epsilon,num_passes):
    np.random.seed(0)
    df_1=data.loc[data['CASE_STATUS']==1]
    df_0=data.loc[data['CASE_STATUS']==0]
    nn_input_dim=data.shape[1]-1
    W1 = np.random.randn(nn_input_dim, nn_hdim) / np.sqrt(nn_input_dim)
    b1 = np.zeros((1, nn_hdim))
    W2 = np.random.randn(nn_hdim, nn_output_dim) / np.sqrt(nn_hdim)
    b2 = np.zeros((1, nn_output_dim))
    # This is what we return at the end
    model = {}
    for i in range(num_passes):
        dw2=0
        db2=0
        dw1=0
        db1=0
        for i in range(0,df_1.shape[0],m):
            df1s=df_1.iloc[i:i+m]
            df0s=df_0.sample(m)
            dfmain=df1s.append(df0s)
            X=np.array(dfmain.iloc[:,1:501],dtype="int64")
            y=np.array(dfmain.iloc[:,0],dtype="int64")
            
            z1 = X.dot(W1) + b1
            a1 = np.tanh(z1)
            
            z2 = a1.dot(W2) + b2
            exp_scores = np.exp(z2)
            probs = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)
            #probs=softmax(z2)
            # Backpropagation
            delta3 = probs
            delta3[range(dfmain.shape[0]), y] -= 1
            dW2 = (a1.T).dot(delta3)
            db2 = np.sum(delta3, axis=0, keepdims=True)
            delta2 = delta3.dot(W2.T) * (1 - np.power(a1, 2))
            dW1 = np.dot(X.T, delta2)
            db1 = np.sum(delta2, axis=0)
         
            # Add regularization terms (b1 and b2 don't have regularization terms)
        dW2 += reg_lambda * W2
        dW1 += reg_lambda * W1
         
            # Gradient descent parameter update
            # Assign new parameters to the model
        W1 += -epsilon * dW1
        b1 += -epsilon * db1
        W2 += -epsilon * dW2
        b2 += -epsilon * db2
        model = { 'W1': W1, 'b1': b1, 'W2': W2, 'b2': b2}
    return model

def neuralNet2(data,nn_hdim1,nn_hdim2,nn_output_dim,m,reg_lambda,epsilon,num_passes):
    np.random.seed(0)
    df_1=data.loc[data['CASE_STATUS']==1]
    df_0=data.loc[data['CASE_STATUS']==0]
    nn_input_dim=data.shape[1]-1
    W1 = (0.01**2)+np.random.randn(nn_input_dim, nn_hdim1)*0
    b1 = np.zeros((1, nn_hdim1))
    
    W2 = (0.01**2)+np.random.randn(nn_hdim1, nn_hdim2)*0
    b2 = np.zeros((1, nn_hdim2))
    
    W3 = np.random.randn(nn_hdim2, nn_output_dim) / np.sqrt(nn_hdim2)
    b3 = np.zeros((1, nn_output_dim))
    
    model = {}
    for i in range(num_passes):
        dw3=0
        db3=0
        dw2=0
        db2=0
        dw1=0
        db1=0
        for i in range(0,df_1.shape[0],m):
            df1s=df_1.iloc[i:i+m]
            df0s=df_0.sample(m)
            dfmain=df1s.append(df0s)
            X=np.array(dfmain.iloc[:,1:501],dtype="int64")
            y=np.array(dfmain.iloc[:,0],dtype="int64")
            
            z1 = X.dot(W1) + b1
            a1 = np.tanh(z1)
            ##level 2
            z2 = a1.dot(W2) + b2
            a2 = np.tanh(z2)
            #level3
            z3 = a2.dot(W3) + b3
            exp_scores = np.exp(z3)
            probs = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)

            # Backpropagation
            delta4 = probs
            delta4[range(dfmain.shape[0]), y] -= 1
            dW3 = (a2.T).dot(delta4)
            db3 = np.sum(delta4, axis=0, keepdims=True)
            ###########
            delta3 = delta4.dot(W3.T) * (1 - np.power(a2, 2))
            dW2 = np.dot(a1.T, delta3)
            db2 = np.sum(delta3, axis=0)
            ###############
            delta2 = delta3.dot(W2.T) * (1 - np.power(a1, 2))
            dW1 = np.dot(X.T, delta2)
            db1 = np.sum(delta2, axis=0)
        dW3 += reg_lambda * W3
        dW2 += reg_lambda * W2
        dW1 += reg_lambda * W1
        W1 += -epsilon * dW1
        b1 += -epsilon * db1
        W2 += -epsilon * dW2
        b2 += -epsilon * db2
        W3 += -epsilon * dW3
        b3 += -epsilon * db3        
        model = { 'W1': W1, 'b1': b1, 'W2': W2, 'b2': b2,'W3': W3, 'b3': b3}
    return model