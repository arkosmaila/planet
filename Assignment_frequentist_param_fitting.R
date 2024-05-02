###############################################################
#Assginment for week 1 of the Advanced Modelling methods course
###############################################################

# Use the MLE method to obtain parameter estimates for b=β/N using an
# SIR model and the biweekly data for outbreaks of measles in three communities 
#within Niamey, Niger (Grais et al. 2006) are provided on the course website. To download and plot the data, do, e.g.,
# Use the data in community A to answer the following questions

# read in the measles data
niamey <- read.csv("http://kingaa.github.io/clim-dis/parest/niamey.csv")

#P.S - use the data in community A to obtain the parameter estimate of the 
#transmission term b=β/N

# plot the data


# write your disease model (SIR)



# write your likelihood and the nll functions using a Poisson loglikelihood


# fit the parameter b using the optim function

#We can get an idea about the uncertainty and in particular obtain confidence 
#intervals by comparing the likelihoods we get at different values of the parameter.

# calculate the nll over this range of bs 
#b=seq(8e-5,1.2e-4,length=100)

# plot the profile of the nlls over the range of bs showing the MLE on the plot


# Try and repeat the exercise for communities B and C too