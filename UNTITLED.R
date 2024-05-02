# Load necessary libraries
library(ggplot2)

# Read the data
niamey <- read.csv("http://kingaa.github.io/clim-dis/parest/niamey.csv")

# Filter data for community A
community_A <- subset(niamey, community == "A")

# Plot the data
ggplot(community_A, aes(x = biweek, y = measles)) +
  geom_point() +
  labs(title = "Measles Outbreak in Community A",
       x = "Time",
       y = "Cases")

# Define SIR model
SIR_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # SIR equations'
    N=S+I+R
    dS <- -b * S * I/N
    dI <- (b * S * I)/N - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


# Define likelihood function
likelihood <- function(b, data) {
  with(data, {
    # Initial conditions
    N=50000
    I0 <- data$measles[1]
    R0 <- 2/3*N
    S0 <- N-R0-I0
    
    # Solve the differential equations
    model <- ode(y = c(S = S0, I = I0, R = R0),
                 times = biweek,
                 func = SIR_model,
                 parms = list(b = b, gamma = gamma))  # Pass parameters as a list
    
    # Calculate expected cases
    expected_cases <- model[, "I"]
    
    # Calculate Poisson log-likelihood
    log_likelihood <- sum(dpois(data$measles, lambda = expected_cases, log = TRUE))
    
    return(-log_likelihood)
  })
}


# Fit the parameter b using optim function

initial_guess <- 1e-6  # Initial guess for parameter b
N <- 50000  # Total population
gamma <- 1.4  # Recovery rate

# Run optimization
fit <- optim(par = initial_guess,
             fn = likelihood,
             data = community_A)

beta_val<-fit$par


# Define a sequence of values for b
b_seq <- seq(8e-5, 1.2e-4, length = 100)

# Calculate negative log-likelihood for each value of b
nll_values <- sapply(b_seq, function(b) likelihood(b*50000, community_A))

# Find the index of the MLE
mle_index <- which.min(nll_values)
mle_b <- b_seq[mle_index]

# Plot the profile of the negative log-likelihood
ggplot(data = data.frame(b = b_seq, nll = nll_values), aes(x = b, y = nll)) +
  geom_line() +
  geom_vline(xintercept = mle_b, color = "red", linetype = "dashed") +
  labs(title = "Profile of Negative Log-Likelihood",
       x = "Parameter b",
       y = "Negative Log-Likelihood") +
  theme_minimal()






# Simulate the SIR model using the estimated beta
initial_conditions <- c(S = N - community_A$measles[1]-2/3*N, I = community_A$measles[1], R = 2/3 * N)
times <- community_A$biweek
parameters <- list(b = beta_val, gamma = gamma)

# Solve the differential equations
model_solution <- ode(y = initial_conditions,
                      times = times,
                      func = SIR_model,
                      parms = parameters)

# Create a data frame for the model solution
model_data <- as.data.frame(model_solution)

# Print the simulated data
print(model_data)














