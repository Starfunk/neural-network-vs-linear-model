# Neural Networks vs. Linear Models - Simulated Housing Dataset

#1. SETTING UP THE NEURAL NETWORK
#2. VISUALIZE RESULTS
#3. CALCULATE CROSS VALIDATION VALUE

#1. ----[SETTING UP THE NEURAL NETWORK]-----------------------------------------

# Setting up training and testing datasets. The training data make up 75% of
# the dataset and are drawn randomly from the original dataset. The training
# data also has appended to it, the outliers data generated in the
# Data_Generation.R file. The test data is the remaining 25% of the data.
(index <- sample(1:nrow(housing.data),round(0.75*nrow(housing.data))))
train <- rbind(housing.data[index,], outliers)
test <- housing.data[-index,]

# Finding the max values for each column in the dataset.
maxs <- apply(housing.data, 2, max) 
mins <- apply(housing.data, 2, min)

# Scaled takes the original dataset and normalizes it based on the max and 
# min values. Normalized training and testing data is then created.
scaled <- as.data.frame(scale(housing.data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

# Train the linear model on the training data and then test it on the testing
# data. Note that we don't use the scaled datasets here since it is not
# necessary to train the linear model on normalized values.
lm.fit <- glm(value~., data=train)
pr.lm <- predict(lm.fit,test)
pr.lm_ <- as.vector(pr.lm)

# We note that the linear model does a great job discovering the hidden 
# coefficients of each of the variables. It is not surprising that the linear
# model does a great job fitting the simulated housing data because the 
# simulated housing data does indeed follow a linear function!
summary(lm.fit)

# Set n equal to a vector containing the names for each of the columns in train_
n <- names(train_)

# # Creating the formula that stores how we want the neural network to fit the 
# data.
f <- as.formula(paste("value ~", paste(n[!n %in% "value"], collapse = " + ")))

# Training the neural network (NN). See Setup.R for a description of the 
# arguments used.
nn <- neuralnet(f,data=train_,hidden=hidden.layers.sim,linear.output=T, 
                threshold=threshold.sim)

# Compute the predicted values for the neural network.
pr.nn <- compute(nn,test_[,1:(ncol(train_)-1)])

# Transform the normalized prediction data back to absolute values. 
pr.nn_ <- pr.nn$net.result * (max(housing.data$value) - min(housing.data$value))
                        + min(housing.data$value)

# Calculate the root mean square for the predictions.
(RMSE.lm <- rootMeanSqError(as.vector(pr.lm), as.vector(test$value)))
(RMSE.nn <- rootMeanSqError(as.vector(pr.nn_), as.vector(test$value)))

# Compute the proportional root mean square error which is normalized value
# with respect to the mean output value of the original dataset. These
# values allow us to compare to the performance of both the neural network and
# linear model ACROSS datasets.
prop.lm.sim <- propRMSE(as.vector(pr.lm), as.vector(test$value), 
                    mean(housing.data$value))
prop.nn.sim <- propRMSE(as.vector(pr.nn_), as.vector(test$value), 
                    mean(housing.data$value))

# Computes how many times RMSE.nn goes into RMSE.lm and is a score that shows
# how well the neural network did compared to the linear mode.
(score.sim <- RMSE.lm / RMSE.nn)

#2. ----[VISUALIZE RESULTS]-----------------------------------------------------

# Plot both figures in the same frame.
par(mfrow=c(2,1))

# We plot housing values against prestige because we know from designing the
# underlying function that prestige is strongly correlated with housing values.

# Plot the linear model (red) against the test data (black).
plot(test$prestige, test$value, pch=19, ylab="Housing Values", xlab="Prestige", 
     main="Linear Model")
points(test$prestige, pr.lm_, col="red", pch=19)

# Plot the neural network (blue) against the test data (black).
plot(test$prestige, test$value, pch=19, ylab="Housing Values", xlab="Prestige", 
     main="Neural Network")
points(test$prestige, as.vector(pr.nn_), col="blue", pch=19)

#3. ----[CALCULATE CROSS VALIDATION VALUE]--------------------------------------

# Run the cross validation algorithm to get the average RMSE values for the 
# neural network and the linear model. Note that the training process for the 
# neural network may converge on suboptimal weights, resulting in suboptimal 
# predictions. We are only interested when the neural network converges on 
# optimal weights, thus cross validation is not a great way to benchmark the 
# neural network's performance. 

# Returns the average RMSE values for the linear model and for 
# the neural network by running k learning experiences. Note this takes a few 
# minutes to run.
CV <- kFoldValidationSim(housing.data, outliers, k.sim)

# Print the average RMSE across k learning experiences.
print(paste("The average RMSE across", k.sim, "loops for the neural network is", 
            CV[1], sep=" "))
print(paste("The average RMSE across", k.sim, "loops for the linear model is", 
            CV[2], sep=" "))



