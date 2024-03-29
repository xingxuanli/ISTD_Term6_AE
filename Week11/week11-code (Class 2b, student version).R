
# Script for ...


######## 0. Set the working environemnt, load the survival package and the heart.csv file

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Set the working folder
# getwd()
# setwd()

# Load the "survival" package
if(!require(survival)){
  install.packages("survival")
  library(survival)
}

# heart.csv data (These data come from the Stanford Heart Transplantation Program)
#
# The goal is to estimate the survival of patients from the data and understand the effect that other explanatory variables may have
# (on whether transplantation helps). Note that in some cases the appropriate heart for transplant might note be available, and patients
# need to wait for it. This study was conducted in April 1, 1974. The survival time is censored if the patient drops out of program
# (no follow up information) or the patient is alive at the time if the end of the study.
#
# We start by reading the "heart.csv" file
heart <- read.csv("heart.csv")
str(heart)
# 172 observations for 7 variables:
# start, stop: entry and exit times for this time interval (in days)
# event: 1 = dead, 0 = alive
# age: age at the start of the programme
# surgery: prior bypass surgery (1 = yes, 0 = no)
# transplant: received transplant (1 = yes, 0 = no)
# id: patient ID
#
unique(heart$id) # so we have a total of 103 patients
#
# Let's take a look at some specific patients:
subset(heart,id==1)   # Patient 1 died at the age of 31 without going through a transplant
subset(heart,id==4)   # Patient 4 waited 36 days for a transplant. He/she then died after 3 days.
subset(heart,id==25)  # Patient 25 was alive at the end of the programme.
subset(heart,id==102) # patient 102 dropped the program after 11 days


######## 1. Kaplan-Meier estimator

# Let's check the "survfit" and "Surv" functions
?Surv    # Create a survival object, usually used as a response variable in a model formula.
?survfit # Create survival curves

# Estimate the survival function and store it in the variable "km"
km <- survfit(Surv(start,stop,event)~1,data=heart)
summary(km,censored=TRUE) # summary of the model, with patients' survival probability 

# plot the Kaplan-Meier curve along with 95% confidence interval
plot(km) 

# let's take a detailed look at the results
subset(heart,stop==1)
subset(heart,stop==2)


######## 2. Cox proportional hazard model

# We now fit a Cox proportional hazard model, where the age, 
# surgery and transplantvariables are used to explain the hazard rate.

# Check "coxph" function
?coxph

# Modelling
cox <- coxph(..)
summary(cox)

# Visual analysis
plot(survfit(cox))
summary(survfit(cox))

