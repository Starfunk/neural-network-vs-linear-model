# Neural Networks vs. Linear Models - Generating Data
#
# This script generates simulated housing market data. The VARIABLES section
# describes how the variables are generated. The VISUALIZE DATA section allows
# for a visualization of the variables in histograms and scatter plots.
# The PREPARE HOUSING DATA SECTION creates the housing.data dataframe. The final
# section, SAVING HOUSING DATA, allows for the saving of housing.data in a csv
# file.

#1. VARIABLES
#2. VISUALIZE DATA
#3. PREPARE HOUSING DATA
#4. OUTLIER DATA
#5. SAVING HOUSING DATA

#1. ----[VARIABLES]-------------------------------------------------------------

# AGE: The mean age for the neighborhood. The vector, age, stores the mean ages
# for each neighborhood.
age <- rnorm(length.data, mean=45, sd=5.8)

# CRIMINAL: The average number of reported criminal cases per neighborhood, 
# averaged over some span of time.
criminal <- 80/age + noZero(rnorm(length.data, mean=0, sd=2))

# PRESTIGE: The higher the prestige of the neighborhood, the lower the crime 
# rate of the neighborhood and vice versa. Prestige is guaranteed to be defined
# for all values of the criminal variable because all values in criminal are
# defined. We conceptualize the prestige value being gathered through a survey
# where citizens across the city rank the prestige of each of the neighborhoods 
# in the city. The prestige values for each neighborhood is then the mean values 
# from these surveys.
prestige <- 10/criminal + noZero(rnorm(length.data, mean=0, sd=1))

# AMENITIES: This variable reflects how close each neighborhood is to grocery
# stores, public parks, transport, etc. Higher values mean the neighborhood
# has access to more ammenities and vice versa.
amenities <- noZero(rnorm(length.data, mean=10, sd=1) - criminal)

# SMELL: A measure of which neighborhood smells the best. The better-smelling 
# neighborhoods have higher values.
smell <- noNegative(rnorm(length.data, mean=0, sd=0.5))

# BLOCK: A measure for the mean neighborhood block length.
block <- rnorm(length.data, mean=200, sd=20)


#2. ----[VISUALIZE DATA]--------------------------------------------------------

# Visualizing age.
hist(age)

# Visualizing criminality.
hist(criminal)

# Visualizing prestige.
hist(prestige)

# Visualizing amenities.
hist(amenities)

# Visualizing smell.
hist(smell)

# Visualizing block.
hist(block)

# Generate the housing values based on the variables above.
(housing.values <- calcHousingValues(age, criminal, prestige, amenities, 
                                     smell, block))

# CREATE HOUSING.DATA
# Save the housing data variables in the dataframe, housing.data.
(housing.data <- data.frame(
  age = age,
  criminal = criminal,
  prestige = prestige,
  amenities = amenities,
  smell = smell,
  block = block,
  value = housing.values
))

# Visualize the spread of housing values.
hist(housing.values)

# Plot the housing values against mean neighborhood ages
plot(housing.data$age, housing.data$value, xlab="age", ylab="value")

# Plot the housing values against criminal activity
plot(housing.data$criminal, housing.data$value, xlab="criminal", ylab="value")

# Plot the housing values against access to amenities
plot(housing.data$amenities, housing.data$value, xlab="amenities", ylab="value")

# Plot the distribution of housing values against smell.
plot(housing.data$smell, housing.data$value, xlab="smell", ylab="value")

# Plot the distribution of housing values against block.
plot(housing.data$block, housing.data$value, xlab="block", ylab="value")


#3. ----[OUTLIER DATA]----------------------------------------------------------

# In order to test if the outlier data points affect the neural networks 
# prediction accuracy, the following section copies housing.data where the 
# value is below a given threshold. Noise is then added and transforms the data 
# from exact copies of housing.data rows to rows that are similar and can be 
# used  to train the network on outlier data. This "outliers" dataset can then 
# be appended to the training dataset.

# Returns a subset of housing.data where themvalues for each neighborhood
# is less than outlier.threshold.
outliers <- housing.data[which(housing.data[,ncol(housing.data)] 
                               < outlier.threshold),]
# Creates a larger outlier dataset to train on based on the value of
# outlier.loop. If outlier.loop = 2, the outliers dataset will be 4 times as
# large as when outlier.loop = 1, if outlier.loop = 3 the outliers dataset will 
# be 6 times as large as when outlier.loop = 1, and so on.
for(i in outlier.loop) {
  outliers <- rbind(outliers, outliers)
}

# Now we subtly change the values of each of the variables to create unique
# data.
r.outlier <- nrow(outliers)
for(i in 1:ncol(outliers)) {
  noise <- noZero(rnorm(r.outlier, mean=0, sd=0.01))
  outliers[,i] <- outliers[,i] + noise
}

# Finally, we recompute the housing values for each neighborhood.
for(i in 1:nrow(outliers)) {
  outliers[i,5] <- calcHousingValues(outliers[i,1], outliers[i,2], 
                                     outliers[i,3], outliers[i,4],
                                     outliers[i,5], outliers[i,6])
}

#4. ----[SAVING HOUSING DATA]---------------------------------------------------

# If you wish to overwrite the saved housing.data that came with the repository 
# and use your own custom housing data, you can save the data by running the 
# code below.

# Naming the housing.data file.
housing.data.name <- "simulated-housing.csv"

# Saving the housing.data.
write.csv(housing.data, paste(p.data, housing.data.name, 
                                          sep = ""), row.names = FALSE) 

