# Using Neural Networks - NN vs. Linear Model On Squared Number Dataset

# Load the neuralnet library
library(neuralnet)

# Prepare our nonlinear dataset
Input <- sample(1:100, 1000, replace=T)
sq.data <- as.data.frame(Input)

# Training output
Output <- (Input)^2

sq.data <- cbind(sq.data, Output)
colnames(sq.data) <- c("Input","Output")

View(sq.data)

plot(sq.data$Input,sq.data$Output)

(index <- sample(1:nrow(sq.data),round(0.75*nrow(sq.data))))
train <- sq.data[index,]
test <- sq.data[-index,]

maxs <- apply(sq.data, 2, max) 
mins <- apply(sq.data, 2, min)
#Column bind the data into one variable


sq.scaled <- as.data.frame(scale(sq.data, center = mins, scale = maxs - mins))
sq.train <- sq.scaled[index,]
sq.test <- sq.scaled[-index,]

View(sq.train)

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sq <- neuralnet(Output~Input, sq.train, hidden=c(8,3), threshold=0.01, 
                  stepmax= 1000000)


pr.nn.sq <- compute(net.sq, sq.test[,1]) #Run them through the neural network

pr.nn.sq_ <- pr.nn.sq$net.result * (max(sq.data$Output) - 
                          min(sq.data$Output)) + min(sq.data$Output)

lm.fit <- glm(Output ~ Input, data=train)
pr.lm <- predict(lm.fit, test)
pr.lm_ <- as.vector(pr.lm)


rootMeanSqError(pr.lm_, as.vector(test$Output))
rootMeanSqError(as.vector(pr.nn.sq_), as.vector(test$Output))





#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)
