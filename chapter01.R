# File-Name:       chapter01.R           
# Date:            2014-7-7                                
# Author:          Andrew Kireru
# Email:           gitobuk@gmail.com
# Purpose:         Code for Chapter 1. Showcases tools for exploratory data analysis.
# Data Used:       Poverty_Rate__by_District.csv
# Packages Used:   ggplot2

# All source code is copyright (c) 2014 
# data taken from: http://www.Open Kenya _ Transparent Africa

# All rights reserved.

# Load in the data set from disk.
pov.rate<-read.csv("Poverty_Rate__by_District.csv" , sep=',',header=TRUE)

head(pov.rate)
summary(pov.rate)
# Create a numeric vector containing just the Number of the Poor per district data.
Number.of.Poor <- with(pov.rate, Number.of.Poor..2005.06.)

# Create a numeric vector containing  the poverty rate per district
Poverty.Rate<-with(pov.rate, Poverty.Rate..2005.06.)
summary(Number.of.Poor)

# Define our own mean and median functions.
my.mean <- function(x)
{
  return(sum(x) / length(x))
}

my.median <- function(x)
{
  sorted.x <- sort(x)
  if (length(x) %% 2 == 0)
  {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

# Confirm that our mean and median functions produce the correct answer.
my.mean(Number.of.Poor)
[1] 219417.7
my.median(Number.of.Poor)
[1] 192204

mean(Number.of.Poor) - my.mean(Number.of.Poor)
#[1] 0

median(Number.of.Poor) - my.median(Number.of.Poor)
#[1] 0

# Experiment with functions for assessing the range of a data set.
min(Number.of.Poor)
#[1] 21249

max(Number.of.Poor)
#[1] 660220

c(min(Number.of.Poor), max(Number.of.Poor))
#[1]  21249 660220

range(Number.of.Poor)
#[1]  21249 660220
# Try out the quantile function for computing quantiles.
quantile(Number.of.Poor)
#   0%    25%    50%    75%   100% 
# 21249 117373 192204 276570 660220 
quantile(Number.of.Poor, probs = seq(0, 1, by = 0.20))
#     0%      20%      40%      60%      80%     100% 
# 21249.0 102822.4 139896.2 220976.8 337252.8 660220.0 

# Define a variance function to assess the spread of data.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / length(x))
}

# Test our variance function for correctness.
my.var(Number.of.Poor) - var(Number.of.Poor)
#[1] -310701709

# Update the variance function to make it unbiased.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}

# Test our variance function again for correctness.
my.var(Number.of.Poor) - var(Number.of.Poor)
#[1] 0

# Check the range predicted by the variance function.
c(mean(Number.of.Poor) - var(Number.of.Poor), mean(Number.of.Poor) + var(Number.of.Poor))
#[1] -21438198474  21438637309 	# what a number!

range(Number.of.Poor)
#[1]  21249 660220			#lets stick with this for now

# Switch to standard deviations instead for thinking about ranges.
my.sd <- function(x)
{
  return(sqrt(my.var(x)))
}

# Test our standard deviation function for correctness.
my.sd(Number.of.Poor) - sd(Number.of.Poor)
#[1] 0

c(mean(Number.of.Poor) - sd(Number.of.Poor), mean(Number.of.Poor) + sd(Number.of.Poor))
#[1]  72999.04 365836.32
range(Number.of.Poor)
#[1]  21249 660220

c(quantile(Number.of.Poor, probs = 0.25), quantile(Number.of.Poor, probs = 0.75))
#   25%    75% 
# 117373 276570 

# Start visualizing data using the ggplot2 package.
library('ggplot2')
head(pov.rate)
aggregate<- with(pov.rate, aggregate(Number.of.Poor, by = list(District.Name), sum))
# Experiment with ggplot.
m <- ggplot(aggregate, aes(Number.of.Poor))
m + geom_bar()
m + geom_bar(fill = "white", colour = "red")

#plot2
b <- ggplot(pov.rate, aes(x = District.Name, y = Number.of.Poor))
 b + geom_point(size = 2)+theme(axis.text.x=element_text(angle=90,hjust=0))
	 
#plot 3
d <- ggplot(pov.rate, aes(x = District.Name, y = Number.of.Poor))
 d + geom_point(size = 2)+theme(axis.text.x=element_text(angle=90,hjust=0)) +geom_bar(stat = "identity", 
  fill = "white", colour = "red")+labs(title = "Number of poor people in Kenya by District")

#plot4
g <- ggplot(pov.rate, aes(x = District.Name, y = Poverty.Rate))
g + geom_point(size = 2)+theme(axis.text.x=element_text(angle=90,hjust=0),
axis.text.y=element_text(colour="grey20",size=6,angle=0,hjust=0,face="plain")) +geom_bar(stat = "identity", 
  fill = "white", colour = "purple")+labs(title = "percentage of poor people in Kenya by District")


# Load in the data set from disk.
health.rate<-read.csv("Health_Facilities.csv" , sep=',',header=TRUE)
summary(health.rate)
datasum <- with(health.rate, aggregate(Dispensary, by = list(District), sum))
#plot5
h <- ggplot(health.rate, aes(x = District, y = Poverty.Rate))
h + geom_point(size = 2)+theme(axis.text.x=element_text(angle=90,hjust=0),
axis.text.y=element_text(colour="grey20",size=6,angle=0,hjust=0,face="plain")) +geom_bar(stat = "identity", 
  fill = "white", colour = "purple")+labs(title = "percentage of poor people in Kenya by District")




