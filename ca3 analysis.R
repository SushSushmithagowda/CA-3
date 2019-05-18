install.packages("pwr")

library(pwr)

library(dplyr)

#effect size is calculated to know the sample size
effect_size <- cohen.ES(test = "r", size = "large")
effect_size

#The sample size is calculated by using the effect size obtained
sample_size <- pwr.r.test(r = effect_size$effect.size, sig.level = 0.05, power = 0.8)
sample_size

#approximate correlation power calculaion is obatianed by plotting the sample size
plot(sample_size)

#Reading the two csv files
Business_expenditure_on_technology <- read.csv("Business_expenditure_on_Technology.csv")
Crop_yield_potato <- read.csv("crop_yield.csv")

#merging the two datasets to a variable called "data"
data <- merge(Business_expenditure_on_technology, Crop_yield_potato)
head(data)

sample_data <- sample_n(data, 29)
head(sample_data) #To display the first six records of sample_data
nrow(sample_data) #To display the nrow of sample_data

#using cor.test to see whether the two columns have a relation 
#and to find the p-value and confidence interval
cor.test(sample_data$Business_expenditure_on_Technology, 
         sample_data$Crop_Yield_per_Hectare_Tonnes)