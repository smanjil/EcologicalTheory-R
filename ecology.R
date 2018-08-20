
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

# store the total number of offspring that reach recruitment from each female whale and
# the age at which females stop breeding
recruits <- c()
ages <- c()
weaned <- c()

# simulate 1000 female life histories
# loop that runs while a female is alive and of a reproductive age.
i <- 1
while (i <= 1000) {
    co <- rnorm(1, mean, standard_deviation)

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

    # default
    enMu <- 0
    enSD <- 20

    # scenario 1
    #enMu <- 20
    #enSD <- 1

    # scenario 2
    #enMu <- 2
    #enSD <- 1

    # scenario 3
    #enMu <- 20
    #enSD <- 30

    # scenario 4
    #enMu <- 2
    #enSD <- 30

    # The variable sr will represent the survival rate within the population of reproductive
    # female whales (i.e. probability of a randomly selected whale from that population of
    # surviving a year). To begin with, set sr to 0.8.
    sr <- 0.8

    # The variables s0 and s1 are used to describe the relationship between the condition of a
    # whale and its probability of survival and hence will be used to model the influence of the
    # whales condition on its probability of survival. Set s0 to -2 and s1 to 0.05.
    s0 <- -2
    s1 <- 0.05

    # number of calves
    number_calves <- 0

    # if a female has calf, set to 1 else 0
    calf <- 0

    # track the age of a calf
    calfage <- 0

    # track the number of calves that become independent of their mothers
    offspring <- 0

    # breeding probability of a reproductive female whales
    b <- 0

    # the relationship between the condition of a whale and its breeding probability and hence will
    # be used to model the influence of the whales condition on its breeding probability
    b0 <- -10
    b1 <- 0.1

    # effect of annual maternal investment on a female’s condition
    inv <- 10

    while (alive == 1 && age <= 40) {
        # allow the whale to survive with the probability sr
        # We assume that these annual condition increments are normally distributed and independent
        # between successive years. enMu represents the mean and enSD represents the standard deviation
        # of that distribution. annual increment of the individual condition

        x1 <- rbinom(1, 1, sr)
        x2 <- rnorm(1, enMu, enSD)

        co <- co + x2
        sr <- exp(s0 + s1 * co) / (1 + exp(s0 + s1 * co))

        if (x1 == 1) {
            age <- age + 1
        } else {
            alive <- 0
        }

        if (calf == 0) {
            b <- exp(b0 + b1 * co) / (1 + exp(b0 + b1 * co))

            # Bernoulli trial (i.e. a binomial experiment with one trial), to simulate the event of birth
            x3 <- rbinom(1, 1, b)
            if (x3 == 1) {
                calf <- 1
            }
        } else if (calf == 1) {
            #b0 <- b0 -inv
            co <- co -inv

            # check calfs survival rate with fixed probability, set calf and calfage to 0 if died, else increment age by 1
            calf_survival <- rbinom(1, 1, 0.80)
            if (calf_survival == 0) {
                calf <- 0
                calfage <- 0
            } else if (calf_survival == 1) {
                calfage <- calfage + 1
            }
        }

        # check calf reached the age of independence, if yes, increase number of mothers offspring
        # and, reset calf and calfage to 0
        if (calfage > 5) {
            offspring <- offspring + 1
            calf <- 0
            calfage <- 0
            if (rbinom(1, 1, 0.98) == 1) {      # checking if probability of calves survival after 5th year is 0.98
                number_calves <- number_calves + 1
            }
        }
    }
    # storing the female's final age and its total number of recruited offspring into the lists
    # ages and recruits respectively
    recruits <- c(recruits, number_calves)
    weaned <- c(weaned, offspring)
    ages <- c(ages, age)

    i <- i + 1
}

# plot histogram of ages
hist(ages, col=c("blue"))
legend("topright", c("Ages"), fill=c("blue"))

hist(recruits, col=c("red"))
legend("topright", c("Recruits"), fill=c("red"))

average_recruits <- mean(recruits)
