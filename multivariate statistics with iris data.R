library(onewaytests)
library(magrittr)
library((GGally))
library(xts)
library(ts)
attach(iris)

"""
Before starting, this script is for people who have advanced understanding of statistics, but want to improve
their coding ability in R. This script does not contain many of the preceeding steps
that usually go into analysis, because the data set is well known and again, the script is for people
who have a high understanding of statistics so they will be able to follow and interpret results easily. 

Note: The pipe operator (%>%) comes from the magrittr library and where 'data %>% head()' it is the same as 'head(data)'

If you cannot extract the dataset read it in using line 20 
Any questions you can reach me on LinkedIn (Robin Mahachi)
"""

# iris <- read.csv("C:\\Users\\Robin\\Documents\\iris_dataset.csv") # amend as necessary

# data inspection
iris %>% head()
iris %>% anyNA() # check for any missing data

# normality tests with Jarque-Bera, Brown-Forsythe and Bartlett's test

jarque.bera.test(iris$Sepal.Length) # p val > 0.05
jarque.bera.test(iris$Sepal.Width) # p val > 0.05
jarque.bera.test(iris$Petal.Length) # p val < 0.05
jarque.bera.test(iris$Petal.Width) # p val < 0.05


# use Brown-Forsythe test shows significant differences exist | output suggests bartlett.test can be used
bf.test(Petal.Width ~ Species, data=iris)
bf.test(Petal.Length ~ Species, data=iris)

# Bartlett test of homogeneity of variances

bartlett.test(iris[,1:4]) # BF test confirmed -- differences are statistically significant

# data visuals
par(mfrow=c(2,2)) # prepare a 2x2 grid for histograms
hist(iris$Sepal.Length, main = "Sepal length", xlab="Sepal Length", col="orange")
hist(iris$Petal.Length, main = "Petal length", xlab="Petal Length", col="maroon")
hist(iris$Sepal.Width, main = "Sepal Width", xlab="Sepal Width", col="grey") # notice this approximates a norm dist
hist(iris$Petal.Width, main = "Petal Wdith", xlab="Petal Length", col="royalblue")


# Identify key variables using Principal Components Analysis
pca.iris = prcomp(iris[1:4], scale=TRUE)
summary(pca.iris) # use judgement call to include PC2 or not, as Standard Deviation is near 1, and adds 22.7% to proportion of variance
pca.iris$rotation # view loadings, use $ operator function on pca.iris to view other info. With/without PC2, Petals still standard out

# multivariate regression using MANOVA with Petal data as variables to understand species
manova.iris = manova(cbind(Petal.Width, Petal.Length) ~ Species, data=iris)
summary(manova.iris, test="Pillai")
summary(manova.iris, test="Wilks") # notice Wilks lambda is the inverse of Pillai's test
summary(manova.iris, test="Hotelling-Lawley")
summary(manova.iris, test="Roy") # all 4 tests seem good


# use a linear model to find the R2 and p-value values. 
linearmod.iris = lm(iris$Petal.Length + Petal.Width ~ iris$Species)
summary(linearmod.iris) # high R-Sqr, low P-Value. Can conclude Petal offers a greater explanation to Species than Sepal. 
