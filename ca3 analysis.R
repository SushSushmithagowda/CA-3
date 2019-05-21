



#To check whether Crop_Yield_per_Hectare_Tonnes is normally distributed or not
#shapiro test is executed

#Normality test for Crop_Yield_per_Hectare_Tonnes
normality_test_Crop_Yield_per_Hectare_Tonnes <- shapiro.test(crop_yield$
                                                               Crop_Yield_per_Hectare_Tonnes)
normality_test_Crop_Yield_per_Hectare_Tonnes$p.value



# Normality test for Business_expenditure_on_technology 
normality_test_Business_expenditure_on_Technology <- shapiro.test(business_expenditure_on_technology
                                                                  $Business_expenditure_on_Technology)
normality_test_Business_expenditure_on_Technology$p.value

#installing pwr package for power analysis
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

#Loading the two csv files 
Business_expenditure_on_technology <- read.csv("business_expenditure_on_Technology.csv")
Crop_yield <- read.csv("crop_yield.csv")


#merging the two datasets to a variable called "data"
data <- merge(Business_expenditure_on_technology, Crop_yield)
head(data)

sample_data <- sample_n(data, 29)
head(sample_data) #To display the first six records of sample_data
nrow(sample_data) #To display the nrow of sample_data

#using spearman nonparametric test to see whether the two columns have a relation 
#and to find the p-value and confidence interval
Cor_test <- cor.test(sample_data$Crop_Yield_per_Hectare_Tonnes, 
         sample_data$Business_expenditure_on_Technology, method = "spearman" )
Cor_test