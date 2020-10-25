library(tidyverse)
MechaCar_mpg <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
head(MechaCar_mpg)

names(MechaCar_mpg)[names(MechaCar_mpg)=="vehicle length"] <- "len"
names(MechaCar_mpg)[names(MechaCar_mpg)=="vehicle weight"] <- "weight"
names(MechaCar_mpg)[names(MechaCar_mpg)=="spoiler angle"] <- "angle"
names(MechaCar_mpg)[names(MechaCar_mpg)=="ground clearance"] <- "clear"

lm(mpg ~ len + weight + angle + clear, data=MechaCar_mpg) #create linear model
summary(lm(mpg ~ len + weight + angle + clear, data=MechaCar_mpg)) #summarize linear model

cor(MechaCar_mpg$mpg,MechaCar_mpg$len) #calculate correlation coefficient for length
cor(MechaCar_mpg$mpg,MechaCar_mpg$weight) #calculate correlation coefficient for weight
cor(MechaCar_mpg$mpg,MechaCar_mpg$angle) #calculate correlation coefficient for angle
cor(MechaCar_mpg$mpg,MechaCar_mpg$clear) #calculate correlation coefficient for clearance

# Highest correlation of the four variables is car length at .61 and ground clearance at .32

plt <- ggplot(MechaCar_mpg,aes(x=len,y=mpg)) #import dataset into ggplot2
plt + geom_line()
model <- lm(mpg ~ len,MechaCar_mpg) #create linear model
yvals <- model$coefficients['len']*MechaCar_mpg$len + model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

# Create same ggplot for ground clearance
plt <- ggplot(MechaCar_mpg,aes(x=clear,y=mpg)) #import dataset into ggplot2
plt + geom_line()
model <- lm(mpg ~ clear,MechaCar_mpg) #create linear model
yvals <- model$coefficients['clear']*MechaCar_mpg$clear + model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


# Read suspension coil data
SuspensionCoil <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
head(SuspensionCoil)

summary(SuspensionCoil$PSI) # Stats Summary

minPSI <- min(SuspensionCoil$PSI) # create metric for stats summary
maxPSI <- max(SuspensionCoil$PSI)
meanPSI <- mean(SuspensionCoil$PSI)
medPSI <- median(SuspensionCoil$PSI)
varPSI <- var(SuspensionCoil$PSI)
stdevPSI <- sd(SuspensionCoil$PSI)

library(tibble)
Stats_Labels = c("min","max","mean","median","variance","standard deviation")
Stats_Results = c(minPSI,maxPSI,meanPSI,medPSI,varPSI,stdevPSI )
Statistics_Table = tibble(Stats_Labels, Stats_Results)
Statistics_Table

plt <- ggplot(SuspensionCoil, aes(x=PSI)) # Plot ggplot
plt + geom_density() # Visualize distribution using density plot


sample_table <- SuspensionCoil %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table, aes(x=PSI)) #import dataset into ggplot2
plt + geom_density() # Visualize distribution using density plot

t.test(sample_table$PSI,mu=mean(SuspensionCoil$PSI)) #compare sample versus population means
