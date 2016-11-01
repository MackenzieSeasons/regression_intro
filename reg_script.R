library(tidyverse)

population_data <- read_csv("population_data.csv")

glimpse(population_data)

set.seed(1)
#ensures we get the same random sample as David
sample1_analytic_data <- sample_n(population_data, size=200)
#takes a random sample

glimpse(sample1_analytic_data)

#now we can conduct the regression
#performance is the DV (criterion), and IQ is the IV (predictor)
#if i multiply a number by the inverse of a number, i get 1

sample_lm_results <- lm(performance ~ IQ + 1, data=sample1_analytic_data)
#criterion, predictor, intercept - the intercept is implicitly there, so you can leave it off in this scenario
sample_lm_results <- lm(performance ~ IQ, data=sample1_analytic_data)
summary(sample_lm_results)
#intercept is 50.59, slope is .24
#intercept is elevation, slope is the angle
#r squared is .30
#b weight vs beta weights - b weights are the raw data (b value, unstandardized slope, slope, regression coefficient)
#another type of slope is beta weights - same analysis, they take the performance column and they transform is into a z score
#beta weights have a mean of 0 and an SD of 1
#they run the IQ column and do the same thing for a beta weight
#results in no intercept, and a beta weight
# a weight for the standardized variables

library(apaTables)
apa.reg.table(sample_lm_results)
#time saving, shows us CI's
#b weight of .24, CI [.19, .29]

#now we're going to do it again with the pop data
population_data <- read_csv("population_data.csv")
glimpse(population_data)

set.seed(1)

population_lm_results <- lm(performance ~ IQ, data=population_data)
summary(population_lm_results)
#got a slope of .20 - within the CI of our sample slope!
#when there's only one predictor, the beta and the correlation are the same, but when there's more than one, they'll be wildly different

#CI - predicted value for Y given a SINGLE value on X
#prediction value for one person
x_axis_range <- data.frame(IQ=c(120))
#data frame with 1 row, and 1 column, with an IQ of 120

CI_data <- predict(sample_lm_results,
                   newdata=x_axis_range, interval = "confidence", level=.95)
#putting in our new value, x axis range into our regression data from before, then give us a CI
CI_data
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
#this puts the IQ back on this line, so our CI the mean for an IQ of 120 is 78 to 80
CI_data

#NOW we want to do it for the whole regression line, predicted value for entire x-axis range
min_predictor <- min(sample1_analytic_data$IQ)
max_predictor <- max(sample1_analytic_data$IQ)
#choosing the min and max values from the IQ column
#we need a range of values on the x axis

x_axis_range <- data_frame(IQ=seq(min_predictor, max_predictor, by=.5))
x_axis_range

#now we want to create predicted values for each of these numbers to get the regression line
x_axis_range <- data.frame(IQ=seq(min_predictor, max_predictor, by=.5))
CI_data <- predict(sample_lm_results,
                   newdata=x_axis_range, interval = "confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data


#PI data
PI_data <- predict(sample_lm_results,
                   newdata=x_axis_range, interval = "prediction", level=.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
PI_data


head(CI_data)
head(PI_data)
#the PI is much wider than the CI

#lets make a graph
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
#use the new CI data
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
#adds PI data
print(reg_plot)

#shortcut time for if you just want the CI
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE) #this is the CI shortcut
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
print(reg_plot)









