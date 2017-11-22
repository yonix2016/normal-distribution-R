# Calculate stock price at time t+1
#  Input
#  st = stock price at time t
#  r = expected annual stock return
#  sigma = annualized volatility in prices
#  t = time in years
#  n = number of steps
#  e = epsilon
nextPrice = function(st, r, sigma, t, n, e) {
  dt = t / n # dt = size of the nit step size
  return(st * exp(((r - (0.5 * (sigma ^ 2))) * dt) + (sigma * e * sqrt(dt))))
}

#########################################################
# Step 1
# Set initial values
#########################################################
st = 10 # Initial stock price
r = 0.15 # 15 % expected annual stock return per year
sigma = 0.2 # 20% annualized volatility in prices
t = 1 # 1 year
n = 100 # 100 steps
epsilon = 0.15

#########################################################
# Step 2
# Calculate the expected value of the stock price at
# the end of every successive dt interval of time
#########################################################
priceMovement = function(st, r, sigma, t, n, epsilon, randomEpsilon) {
  prices = c()
  prices[1] = st
  for (i in 1:n) {
    if (randomEpsilon) {
      epsilon = rnorm(1)
    }
    prices[i + 1] = nextPrice(prices[i], r, sigma, t, n, epsilon)
  }
  return(prices)
}

prices = priceMovement(st, r, sigma, t, n, epsilon, FALSE)

#########################################################
# Step 3
# Plot the entire movement of prices over tht T 
# period under observation
#########################################################
plot(prices,
  main = 'Fixed epsilon in each time step',
  xlab = 'Step', 
  ylab = 'Stock Price', 
  xlim = c(0, 100), 
  ylim = c(0, 20), 
  type='l')

#########################################################
# Step 4
# Randomly assign values to ε from a standard normal 
# distribution
#########################################################
prices = priceMovement(st, r, sigma, t, n, epsilon, TRUE)
plot(prices, 
  main = 'Random epsilon in normal distribution in each time step',
  xlab = 'Step', 
  ylab = 'Stock Price', 
  xlim = c(0, 100), 
  ylim = c(0, 20), 
  type='l')

#########################################################
# Step 5
# Perform 5 trials of 100 steps to plot probable 
# movement of stock prices over a 1 year period.
# Plot each trajectory of prices as a separate line
#########################################################
colors = rainbow(5)
plot(
  c(0, 100),
  c(0, 18), 
  main = '5 trials of random epsilon in normal distribution in each time step',
  xlab = 'Step',
  ylab = 'Stock Price',
  type='n')

for (trial in 1:5) {
  prices = priceMovement(st, r, sigma, t, n, epsilon, TRUE)
  lines(prices, col = colors[trial])
  text(90, prices[101], paste('Trial', trial), cex = 1.0, col = colors[trial])
}