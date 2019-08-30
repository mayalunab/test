install.packages('tinytex')
tinytex::install_tinytex()

tinytex:::is_tinytex()

install.packages("dplyr")
library(dplyr)

setwd("C:/Users/max/Documents/R/ass_1")

##actual work now ##

b_data <- read.csv("boston.csv")

head(b_data)

summary(b_data)



## question 1 ## 

income.control.tab <- table(income = b_data$income, control = b_data$treatment)
print(income.control.tab)

addmargins(income.control.tab)

mean(b_data$income[b_data$treatment == 0])
mean(b_data$income[b_data$treatment == 1])
median(b_data$income[b_data$treatment == 0])
median(b_data$income[b_data$treatment == 1])


males.control <- table(treatment = b_data$treatment, males = b_data$male)

## control group proportion of men ## 

males.control[1,2]/sum(males.control[,1])

## treatment group proportion of men ## 

males.control[2,2]/sum(males.control[,2])


## question 2 ## 

control_dif_means <- (mean(b_data$numberim.post[b_data$treatment == 0], na.rm = T)
                      -
                        mean(b_data$numberim.pre[b_data$treatment == 0], na.rm = T))

print(control_dif_means)


treatment_dif_means <- (mean(b_data$numberim.post[b_data$treatment == 1], na.rm = T)
                        -
                          mean(b_data$numberim.pre[b_data$treatment == 1], na.rm = T))

print(treatment_dif_means)


## question 3 ## 

b_data$type <- NA
b_data$type[b_data$college == 1 & b_data$treatment == 0] <- "EduControl" 
b_data$type[b_data$college == 0 & b_data$treatment == 0] <- "UneduControl" 
b_data$type[b_data$college == 1 & b_data$treatment == 1] <- "EduTreatment"
b_data$type[b_data$college == 0 & b_data$treatment == 1] <- "UneduTreatment"

b_data$type <- as.factor(b_data$type)


tapply(b_data$numberim.pre, b_data$type, mean, na.rm = T)
tapply(b_data$numberim.post, b_data$type, mean, na.rm = T)
