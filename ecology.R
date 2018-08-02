
# Manjil Shrestha
# 20180802
# Course: Data Analysis and Statistics
# Data Analysis Final Assignment

# This script is to investigate how the availability and variability of resources affect the individual
# fitness, the total reproductive output and the age of female Orcas at death. The investigation
# seeks to support a better understanding of the effect of the environment on both the fitness
# of an individual Orca and the reproductive ability of an entire Orca population.
# The investigation will be done by means of a simulation programmed in R. You will gradually
# build up and then run a simulation of the reproductive life histories of thousands of female
# Orcas that allows you to generate data that can be analyzed with respect to the given
# research objective

#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

# co, track the condition of each individual female
# At recruitment (i.e. when they become reproductively active), females
# have a normal distribution of conditions with mean 100 and standard deviation 10.
mean <- 100
standard_deviation <- 10
co <- rnorm(100, mean, standard_deviation)

# variable age to track the age of a single female. Set age to the appropriate
# value for a female that has just become recruited
age <- 15

# The variable alive will take the value 1 if a female is still alive in any given year,
# and zero if it has died. Set it to 1, to begin with.
alive <- 1

# The quality and variability of the environment will determine the ability of an individual to
# improve its condition. The influence of the environment will be represented by the
# variables enMu and enSD. A high value of enMu will indicate a rich environment and a
# high value of enSD will indicate a variable environment. To begin with, set these values
# to 0 and 20 respectively.
enMu <- 0
enSD <- 20

# The variable sr will represent the survival rate within the population of reproductive
# female whales (i.e. probability of a randomly selected whale from that population of
# surviving a year). To begin with, set sr to 0.8.
sr <- 0.8

# The variables s0 and s1 are used to describe the relationship between the condition of a
# whale and its probability of survival and hence will be used to model the influence of the
# whales condition on its probability of survival. Set s0 to -2 and s1 to 0.05.
s0 <- -2
s1 <- 0.05

# loop that runs while a female is alive and of a reproductive age.
while (alive == 1 && age >= 15 && age <= 40) {
    # allow the whale to survive with the probability sr
    survival_probability = rbinom(1, 1, sr)

    # if it does, increment its age by 1.
    if (survival_probability > sr) {
        age = age + 1
    }
}
print.default(age)
