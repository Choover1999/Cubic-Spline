#Problem 3

#Building the cubic spline interpolation curve

yield_data <- data.frame(Time_to_Maturity = c(0.25, 0.5, 1, 2, 5, 10, 20), Yield_Rate = c(3.2, 3.23, 3.3, 3.55, 4.1, 4.50, 4.72))

#Create our "A" matrix

A_matrix <- matrix(0, nrow = length(yield_data$Time_to_Maturity) + 2, ncol = length(yield_data$Yield_Rate) + 2)

A_matrix[1:7, 1] <- 1

for (i in 1:length(yield_data$Time_to_Maturity)) {
  A_matrix[i, 2] <- max(yield_data$Time_to_Maturity[i] - yield_data$Time_to_Maturity[1], 0)
  A_matrix[i, 3] <- max(yield_data$Time_to_Maturity[i] - yield_data$Time_to_Maturity[1], 0)^2
  A_matrix[i, 4] <- max(yield_data$Time_to_Maturity[i] - yield_data$Time_to_Maturity[1], 0)^3
}

for (i in 1:length(yield_data$Time_to_Maturity)) {
  for (j in 5:9) {
    A_matrix[i, j] <- max(yield_data$Time_to_Maturity[i] - yield_data$Time_to_Maturity[j - 3], 0)^3
  }
}

A_matrix[8, 3] <- 2
A_matrix[9, 3] <- 2

for (i in 4:9) {
  A_matrix[9, i] <- 6 * (yield_data$Time_to_Maturity[7] - yield_data$Time_to_Maturity[i-3])
}

#Invert the A matrix

A_inverse <- solve(A_matrix)

#Get the rates matrix and add 0's for twice differentiable condition

twice_differentiable <- c(yield_data$Yield_Rate, c(0, 0))

#Multiply by the inverse to solve the system of equations

coeffs <- A_inverse %*% twice_differentiable

#Plot the scatter plot of discrete points and plot the function given by the cubic spline
t1 <- yield_data$Time_to_Maturity[1]

cubic_spline <- function(x) {
  spline <- rep(0, length(x))
  for (i in 4:9) {
    spline <- spline + coeffs[i] * pmax(x - yield_data$Time_to_Maturity[(i - 3)], 0)^3
  }
  return(coeffs[1] + coeffs[2] * pmax(x - t1, 0) + coeffs[3] * pmax(x - t1, 0)^2 + spline)
}

# Plot the scatter plot of discrete points
plot(yield_data$Time_to_Maturity, yield_data$Yield_Rate, pch = 16, col = "blue", main = "Yield to Maturity", xlab = "Time to Maturity (Years)", ylab = "Yield Rate")

# Plot the function given by the cubic spline
curve(cubic_spline(x), from = 0, to = 20, col = "red", add = TRUE)
legend("topleft", legend = c("Discrete Points", "Cubic Spline Interpolation"), col = c("blue", "red"), lty = 1, pch = c(16, NA))