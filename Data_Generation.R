# Using Neural Networks - Generating Data

# Median housing values for a neighborhood in Van

# AGE
(age.van <- sample(20:80, 100, replace = TRUE))

# CRIMINAL
(criminal.van <- runif(100, min = 0, max = 1))

hist(criminal.van,breaks=10)

# PRESTIGE
(prestige.van <- runif(100, min = 0, max = 1))
hist(prestige.van)
?hist
# TRANSPORT
(transport.van <- runif(100, min = 0, max = 1))


ageValue <- function(age) {
  age.value <- -(0.1 * age - 6)^2 + 40
  return(age.value)
}

calcMedHousingValues <- function(criminal, prestige, transport, age) {
  value <- 20 * -criminal + 100 * prestige + 20 * transport + ageValue(age)
  return(value)
}

(median.housing.values <- calcMedHousingValues(criminal.van, prestige.van, 
                                              transport.van, age.van))

outliers <- housing.data[which(housing.data[,5] < 50),]
nrow(outliers)


(housing.data <- data.frame(
  age = age.van,
  criminal = criminal.van,
  prestige = prestige.van,
  transport = transport.van,
  value = median.housing.values
))

View(housing.data)
# Setting up training and testing datasets
(index <- sample(1:nrow(housing.data),round(0.75*nrow(housing.data))))
train <- housing.data[index,]
test <- housing.data[-index,]
maxs <- apply(housing.data, 2, max) 
mins <- apply(housing.data, 2, min)
scaled <- as.data.frame(scale(housing.data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

View(housing.data)

(n <- names(train_))
f <- as.formula(paste("value ~", paste(n[!n %in% "value"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(10,10),linear.output=T)

pr.nn <- compute(nn,test_[,1:4])
pr.nn_ <- pr.nn$net.result*(max(housing.data$value)
                            -min(housing.data))+min(housing.data)
print(pr.nn_)

plot(nn)

