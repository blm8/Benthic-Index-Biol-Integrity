############################################
## Benthic Index for Biological Integrity ##
############################################

rm(list=ls()) # Clear your workspace
setwd("~/Desktop") # Set your working directory

# Download the B-IBI_Data.csv file from Canvas
# Copy and paste this link into your browser and fill out the atttributes for each taxon:
# https://www.pugetsoundstreambenthos.org/Taxa-Attributes.aspx

IBI <- read.csv("B-IBI_Data.csv", header=TRUE)
View(IBI) # View your data in spreadsheet form
colnames(IBI) # See your column names
levels(IBI$SITE) # See your "levels" in a given column
levels(IBI$ENVIRON)
nlevels(IBI$ENVIRON) # See your number of levels in a given column # You will calculate the B-IBI for each of these levels or environments

shade <- subset(IBI, SITE=="BigBeefCreek" & ENVIRON=="Shade")
View(shade)

# Calculate the metrics of the B-IBI associated with composition and richness:
IBI1 <- function(x) { # Function thresholds are based on B-IBI_Criteria.pdf
  if (x <= 14) {
    result <- 1
  }
  else if (x > 28) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
totTaxa <- IBI1(length(shade$FAMILY)) # Number of families in sample
totTaxa # Score returned based on function criteria

IBI2 <- function(x) {
  if (x <= 3.5) {
    result <- 1
  }
  else if (x > 7) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
mayflyTaxa <- IBI2(nrow(shade[shade$ORDER=="Ephemeroptera", ])) # Number of mayfly families in sample
mayflyTaxa # Score returned based on function criteria

IBI3 <- function(x) {
  if (x <= 2.7) {
    result <- 1
  }
  else if (x > 5.3) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
stoneflyTaxa <- IBI3(nrow(shade[shade$ORDER=="Plecoptera", ])) # Number of stonefly families in sample
stoneflyTaxa # Score returned based on function criteria

IBI4 <- function(x) {
  if (x <= 2.7) {
    result <- 1
  }
  else if (x > 5.3) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
caddisflyTaxa <- IBI4(nrow(shade[shade$ORDER=="Tricoptera", ])) # Number of caddisfly families in sample
caddisflyTaxa # Score returned based on function criteria

IBI5 <- function(x) {
  if (x <= 4) {
    result <- 1
  }
  else if (x > 8) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
longLived <- IBI5(nrow(shade[shade$LONGLIVED=="Yes", ])) # Number of long-lived families in sample
longLived # Score returned based on function criteria

# Calculate the metrics of the B-IBI associated with pollution tolerances:
IBI6 <- function(x) {
  if (x <= 2) {
    result <- 1
  }
  else if (x > 4) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
tolerantTaxa <- IBI6(nrow(shade[shade$TOLERANT=="Yes", ])) # Number of pollution-tolerant families in sample
tolerantTaxa # Score returned based on function criteria

IBI7 <- function(x) {
  if (x >= 0.44) {
    result <- 1
  }
  else if (x < 0.27) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
percTolerant <- IBI7(nrow(shade[shade$TOLERANT=="Yes", ])/length(shade$FAMILY)) # Percentage of pollution-tolerant families in sample
percTolerant # Score returned based on function criteria

# Calculate the metrics of the B-IBI associated with feeding ecologies:
IBI8 <- function(x) {
  if (x <= 0.045) {
    result <- 1
  }
  else if (x > 0.09) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
percPredators <- IBI8(nrow(shade[shade$FEEDING_GROUP=="Predator", ])/length(shade$FAMILY)) # Percentage of predator families in sample
percPredators # Score returned based on function criteria

IBI9 <- function(x) {
  if (x <= 8) {
    result <- 1
  }
  else if (x > 16) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
clingerTaxa <- IBI9(nrow(shade[shade$CLINGER=="Yes", ])) # Number of clinger families in sample
clingerTaxa # Score returned based on function criteria

# Calculate the metrics of the B-IBI associated with community attributes:
IBI10 <- function(x) {
  if (x >= 0.75) {
    result <- 1
  }
  else if (x < 0.55) {
    result <- 5
  }
  else {
    result <- 3
  }
  return(result)
}
dominantTaxa <- head(shade[order(-shade$COUNT),],3)
percDominant <- IBI10(sum(dominantTaxa$COUNT)/sum(shade$COUNT)) # Percentage of three most abundant families in sample
percDominant # Score returned based on function criteria

# Calculate the B-IBI for our shaded reach of Big Beef Creek:
shadeIBI <- sum(totTaxa, mayflyTaxa, stoneflyTaxa, caddisflyTaxa, longLived,
                tolerantTaxa, percTolerant, percPredators, clingerTaxa, percDominant)
shadeIBI # A ten-metric score of 28-36 is considered "Fair"

# Repeat this exercise with our sunny reach of Big Beef Creek
# Send your code to blm8@uw.edu
