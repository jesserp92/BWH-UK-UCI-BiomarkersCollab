#Load the following libraries to run in R

# load packages
library(dplyr) #used for cleaning dataframes and data manipulation 
library(tibble) #used for creating a base data frame and also displays data along with data types. 
library(tidyverse) #used in data sciences for grammar, philosophy, and data structure. Tidies data.
library(ggplot2) #dedicated to data visualization
library(ggpubr) #dedicated to data visualization usually used in biological sciences research
library(corrplot) #used to generate correlation
library(Hmisc) #used to generate R base correlation and correlation matrices
library(PerformanceAnalytics) #provides performance metrics and analysis for normally and non-normally distributed data. Can also use on correlation matrices

#Read the file in the following format: "Titleofdocument.csv". From here on, the data will be abbreviated to "PL". 
P <- read.csv("Tau.csv", header = TRUE)
print(P)
P <- tibble::as_tibble(P)
library(reshape2)

#Creating a correlation Matrix 
#[,unlist] step is necessary to make sure the only columns read are the ones with numerics. Any column with alphas or non-numerics will read as an error. 
#The following selects a subset of all numeric variables in our data using the unlist, lapply, and is.numeric functions as shown below:
P.cor = cor(P[, unlist(lapply(P, is.numeric))], method = c("spearman"))  # Properly apply cor

#flattenCorrelationMatrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#[,unlist] step is necessary to make sure the only columns read are the ones with numerics. Any column with alphas or non-numerics will read as an error. 
#The following runs a correlation matrix with p-values. 
P.rcorr=rcorr(as.matrix(P)[, unlist(lapply(P, is.numeric))])
PL.rcorr
flattenCorrMatrix(P.rcorr$r,P.rcorr$P)

#Next we are going to generate one table of correlation coefficients and one with p-values. 
P.coeff = P.rcorr$r
P.p = P.rcorr$P

#Create correlation matrix based on the information generated above. 
corrplot(P.cor)

#Create a correlogram to better visualize the graphical display
w <- corrplot(P.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Combine correlogram with significance test

# Insignificant correlation are crossed
corrplot(P.coeff, type="upper", order="hclust", 
         p.mat = P.p, sig.level = 0.04, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(P.coeff, type="upper", order="hclust", 
         p.mat = P.p, sig.level = 0.04, insig = "blank")

#now to display a chart of a correlation matrix
chart.Correlation(P, histogram=TRUE, pch=19)