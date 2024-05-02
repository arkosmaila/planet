# Use the MLE method to obtain parameter estimates for b=β/N using an
# SIR model and the biweekly data for outbreaks of measles in three communities 
# within Niamey, Niger (Grais et al. 2006) are provided on the course website. To download and plot the data, do, e.g.,
# Use the data in community A to answer the following questions

# read in the measles data
niamey <- read.csv("http://kingaa.github.io/clim-dis/parest/niamey.csv")
View(niamey)

# Extract data for Community A
community_A <- subset(niamey, community == "A")

# Extract data for Community B and Community C
community_B <- subset(niamey, community == "B")
community_C <- subset(niamey, community == "C")

# Plot the data for all communities
plot(community_A$biweek, community_A$measles, type = "l", xlab = "Biweek", ylab = "Measles Cases", 
     main = "Measles Outbreaks in Communities A, B, and C", col = "blue", lwd = 2)
lines(community_B$biweek, community_B$measles, col = "red", lwd = 2)
lines(community_C$biweek, community_C$measles, col = "green", lwd = 2)

# Add legend
legend("topleft", legend = c("Community A", "Community B", "Community C"), 
       col = c("blue", "red", "green"), lty = 1, lwd = 2, bg = "white", cex = 0.8)

#Visualizing the data for only community A
community_A %>% 
  ggplot(aes(biweek, measles)) +
  geom_point(col="red") +
  geom_line(aes(biweek, measles), col="blue") +
  ggtitle("Measles Cases in Community A Over Time")



# write your disease model (SIR)
SIR_model <- function(b, N, I_initial, R_initial, t) {
  S <- rep(0, length(t))
  I <- rep(0, length(t))
  R <- rep(0, length(t))
  S[1] <- N - I_initial
  I[1] <- I_initial
  R[1] <- R_initial
  dt <- t[2] - t[1]
  for (i in 2:length(t)) {
    dS <- -b * S[i - 1] * I[i - 1] * dt / N
    dI <- (b * S[i - 1] * I[i - 1] * dt / N) - (I[i - 1] * dt)
    dR <- I[i - 1] * dt
    S[i] <- S[i - 1] + dS
    I[i] <- I[i - 1] + dI
    R[i] <- R[i - 1] + dR
  }
  return(data.frame(S = S, I = I, R = R))
}

# write your likelihood and the nll functions using a Poisson loglikelihood
likelihood <- function(params, data) {
  b <- params
  N <- 50000
  I_initial <- data$measles[1]
  R_initial <- 0
  t <- seq_along(data$biweek)
  SIR_result <- SIR_model(b, N, I_initial, R_initial, t)
  expected_cases <- SIR_result$I
  log_likelihood <- sum(dpois(data$measles, lambda = pmax(expected_cases, 1e-10), log = TRUE))
  if (is.nan(log_likelihood) || is.infinite(log_likelihood)) {
    return(NA)
  }
  return(-log_likelihood)
}

# fit the parameter b using the optim function
initial_guess <- 1e-6 # Adjust initial guess
fit <- optim(par = initial_guess, fn = likelihood, data = community_A, method = "BFGS", control = list(fnscale = -1))
if (fit$convergence != 0) {
  print("Optimization did not converge.")
} else {
  b_estimate <- fit$par
  cat("Estimated value of b:", b_estimate, "\n")
}



#2:Negative b results
# Fit the parameter b using the optim function with box constraints
fit <- optim(par = initial_guess, fn = likelihood, data = community_A, method = "L-BFGS-B", 
             lower = 0, upper = Inf)

b_estimate <- fit$par
cat("Estimated value of b:", b_estimate, "\n")


#We can get an idea about the uncertainty and in particular obtain confidence 
#intervals by comparing the likelihoods we get at different values of the parameter.

# calculate the nll over this range of bs 
b <- seq(8e-5, 1.2e-4, length = 100)
nll_values <- rep(0, length(b))
for (i in 1:length(b)) {
  nll_values[i] <- likelihood(b[i], community_A)
}

# plot the profile of the nlls over the range of bs showing the MLE on the plot
plot(b, nll_values, type = "l", xlab = "b", ylab = "Negative Log Likelihood", main = "Profile Likelihood of b for Community A")
abline(v = b_estimate, col = "red")


# plot the profile of the nlls over the range of bs showing the MLE on the plot
plot(b, nll_values, type = "l", xlab = "b", ylab = "Negative Log Likelihood", 
     main = "Profile Likelihood of b for Community A", col = "blue", lwd = 2)
abline(v = b_estimate, col = "red", lty = 2)
grid() # Add grid lines
legend("topright", legend = c("Negative Log Likelihood", "MLE Estimate"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1), bg = "white")

















# Define the SIR model function
SIR_model <- function(b, N, I_initial, R_initial, t) {
  S <- rep(0, length(t))
  I <- rep(0, length(t))
  R <- rep(0, length(t))
  S[1] <- N - I_initial
  I[1] <- I_initial
  R[1] <- R_initial
  dt <- t[2] - t[1]
  for (i in 2:length(t)) {
    dS <- -b * S[i - 1] * I[i - 1] * dt / N
    dI <- (b * S[i - 1] * I[i - 1] * dt / N) - (I[i - 1] * dt)
    dR <- I[i - 1] * dt
    S[i] <- S[i - 1] + dS
    I[i] <- I[i - 1] + dI
    R[i] <- R[i - 1] + dR
  }
  return(data.frame(S = S, I = I, R = R))
}

# Parameters
N <- sum(community_A$measles)  # Total population
I_initial <- community_A$measles[1]  # Initial infected individuals
R_initial <- 0  # Initial recovered individuals
t <- seq_along(community_A$biweek)  # Time points

# Fit the SIR model using the estimated value of b
b_estimate <- 1.625703  # Estimated value of b
SIR_fit <- SIR_model(b_estimate, N, I_initial, R_initial, t)

# Plot the results
plot(t, community_A$measles, type = "l", xlab = "Biweek", ylab = "Measles Cases", 
     main = "SIR Model Fit to Measles Outbreak in Community A")
lines(t, SIR_fit$I, col = "red", lwd = 2)  # Plot infected cases from SIR model
legend("topright", legend = c("Observed", "SIR Model"), col = c("black", "red"), lty = 1, lwd = 2, bg = "white")




