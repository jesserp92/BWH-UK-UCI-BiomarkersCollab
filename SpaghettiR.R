#Load the following libraries to run in R
library(dplyr)
library(ggplot2)
library(lcsm) #needed for generating spaghetti plots
library(stringr)#Simple, Consistent Wrappers for Common String Operations
library(tidyverse)
library(tidyr)
library(tsibble)#special tidy time series data frame. 
library(brolgar)  #an R package to BRowse Over Longitudinal Data Graphically and Analytically in R (brolgar)
# install.packages("remotes")
#remotes::install_github("njtierney/brolgar")

data <- read.csv("Spag bland.csv", header = TRUE)  #header = true denotes that the first row in the .csv file is a header or used to mark the group of each column, rather than data to be read when constructing the dataframe. 
print(data)
library(reshape2)
#generated scatterplot to see point spread
p <- ggplot(data = data, aes(x = time, y = correction, group = id)) + labs(x = "Plasma Draw (Timepoint in years)", y = "Corrected Plasma NT1 (pg/mL)")
p + geom_point()

spag<- ggplot(data=data, aes(x=time, y = correction, group = id, colour = id)) + labs(x = "Plasma Draw (Timepoint in years)", y = "Corrected Plasma NT1 (pg/mL)") + geom_line()
# add a line using locally weighted regression (lowess) to “smooth” over all the variability and give a sense of the overall or average trend.
spag + stat_smooth(aes(group = 1), method = "lm", se = FALSE) + 
  stat_summary(aes(x = 8, yintercept = ..y.., group = 1), fun = "median", color = "black", geom = "hline")
#If the lowess looks fairly linear, choose a linear smooth and turn off the standard error shading.
spag + geom_line() + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),
                                                                geom = "point", fun.y = mean, shape = 17, size = 3)
#Suppose that between time 1 and 2, an intervention occurred, and we wish to fit a piecewise linear model rather than an overall smooth. We can do this by creating a dummy variable (pre/post intervention) and its interaction with time. 
spag + geom_line() + stat_smooth(aes(group = 1), method = "lm", formula = y ~
                                x * I(x > 1), se = FALSE) + stat_summary(aes(group = 1), fun.y = mean, geom = "point",
                                                                         shape = 17, size = 3)

#Generate Bland-Analysis
mean_diff <- mean(data$diffm)

#find lower 95% confidence interval limits
lower <- mean_diff - 1.96*sd(data$diffm)

lower
#find upper 95% confidence interval limits
upper <- mean_diff + 1.96*sd(data$diffm)

upper

#remove NA
data[!is.na(data$diffm),]

#create Bland-Altman plot. Change diff to match column title, and change mean to match column title
ggplot(data, aes(x = meanm, y = diffm)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")