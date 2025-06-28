# ICT110 Task 2: Weather Data Analysis for NSW
#Ashton Winnett, 1184199, 2025

#Set the working directory to where the dataset is located 
setwd("D:/UNIVERSITY/BS COMPUTER SCIENCE/SEMESTER 1/ICT110 Indroduction to Data Science/ASSESSMENT 2")

# 1. Data Setup
# Load the dataset
library(readr)
weather <- read.csv("weatherAUS.csv")
View(weather)

# Add State column based on Location (NSW locations)
nsw_locations <- c("Albury", "BadgerysCreek", "Cobar", "CoffsHarbour", "Moree", 
                   "Newcastle", "NorahHead", "NorfolkIsland", "Penrith", "Richmond", 
                   "Sydney", "SydneyAirport", "WaggaWagga", "Williamtown", "Wollongong")
weather$State <- ifelse(weather$Location %in% nsw_locations, "NSW", "Other")

# Filter for NSW
weather_nsw <- weather[weather$State == "NSW", ]

# Handle missing values: Replace NA with median for numeric columns
numeric_cols <- sapply(weather_nsw, is.numeric)
for (col in names(weather_nsw)[numeric_cols]) {
  weather_nsw[[col]][is.na(weather_nsw[[col]])] <- median(weather_nsw[[col]], na.rm = TRUE)
}

# Convert RainToday and RainTomorrow to factors
weather_nsw$RainToday <- as.factor(weather_nsw$RainToday)
weather_nsw$RainTomorrow <- as.factor(weather_nsw$RainTomorrow)

# 2. Exploratory Data Analysis
# 2.1 One-variable analysis: Rainfall distribution
png("rainfall_histogram.png")
hist(weather_nsw$Rainfall, breaks = 50, col = "blue", border = "black", 
     main = "Distribution of Rainfall in NSW", xlab = "Rainfall (mm)", ylab = "Count")
dev.off()

# 2.2 Two-variable analysis: Temperature vs. Humidity
png("temp_humidity_scatter.png")
plot(weather_nsw$Temp9am, weather_nsw$Humidity9am, pch = 16, col = rgb(0, 0.5, 0, 0.5),
     main = "Temperature vs. Humidity at 9am in NSW", xlab = "Temperature (°C)", ylab = "Humidity (%)")
dev.off()

# 2.3 One-variable analysis: WindGustSpeed distribution
png("windgust_boxplot.png")
boxplot(weather_nsw$WindGustSpeed, col = "orange", 
        main = "Distribution of Wind Gust Speed in NSW", ylab = "Wind Gust Speed (km/h)")
dev.off()

# 2.4 Two-variable analysis: Rainfall vs. RainTomorrow
rain_means <- aggregate(Rainfall ~ RainTomorrow, data = weather_nsw, FUN = mean)
png("rainfall_raintomorrow_bar.png")
barplot(rain_means$Rainfall, names.arg = rain_means$RainTomorrow, col = "purple",
        main = "Average Rainfall by Rain Tomorrow in NSW", xlab = "Rain Tomorrow", ylab = "Average Rainfall (mm)")
dev.off()

# 3. Advanced Analysis
# 3.1 Linear Regression: Predict MaxTemp using MinTemp
model1 <- lm(MaxTemp ~ MinTemp, data = weather_nsw)
print(summary(model1))
png("regression_plot_maxtemp.png")
plot(weather_nsw$MinTemp, weather_nsw$MaxTemp, pch = 16, col = rgb(0, 0, 0, 0.5),
     main = "Linear Regression: MaxTemp vs. MinTemp in NSW", xlab = "MinTemp (°C)", ylab = "MaxTemp (°C)")
abline(model1, col = "red", lwd = 2)
dev.off()

# 3.2 Linear Regression: Predict Humidity9am using Temp9am
model2 <- lm(Humidity9am ~ Temp9am, data = weather_nsw)
print(summary(model2))
png("regression_plot_humidity.png")
plot(weather_nsw$Temp9am, weather_nsw$Humidity9am, pch = 16, col = rgb(0, 0.5, 0, 0.5),
     main = "Linear Regression: Humidity9am vs. Temp9am in NSW", xlab = "Temp9am (°C)", ylab = "Humidity9am (%)")
abline(model2, col = "blue", lwd = 2)
dev.off()

# 3.3 Clustering: K-means on Humidity9am and Rainfall
set.seed(123)
# Scale data and check for issues
cluster_data <- scale(weather_nsw[, c("Humidity9am", "Rainfall")])
if (any(is.na(cluster_data))) stop("NAs found in scaled cluster data")
# K-means function
kmeans_base <- function(data, k, max_iter = 100) {
  n <- nrow(data)
  # Randomly initialise centroids
  centroids <- data[sample(1:n, k), , drop = FALSE]
  clusters <- numeric(n)
  for (iter in 1:max_iter) {
    # Assign points to nearest centroid
    for (i in 1:n) {
      distances <- sqrt(rowSums((t(centroids) - data[i, ])^2))
      clusters[i] <- which.min(distances)
    }
    # Store old centroids
    old_centroids <- centroids
    # Update centroids
    for (j in 1:k) {
      if (sum(clusters == j) > 0) {
        centroids[j, ] <- colMeans(data[clusters == j, , drop = FALSE])
      } else {
        # Reinitialise empty cluster centroid
        centroids[j, ] <- data[sample(1:n, 1), ]
      }
    }
    # Check convergence
    if (all(old_centroids == centroids)) break
  }
  # Verify cluster assignments
  return(list(cluster = clusters, centers = centroids))
}
# Run k-means with 3 clusters
kmeans_result <- kmeans_base(cluster_data, k = 3)
weather_nsw$Cluster <- as.factor(kmeans_result$cluster)
# Plot clusters with explicit axis limits
png("clustering_plot.png")
x_range <- range(cluster_data[, 1], kmeans_result$centers[, 1], na.rm = TRUE)
y_range <- range(cluster_data[, 2], kmeans_result$centers[, 2], na.rm = TRUE)
plot(cluster_data[, 1], cluster_data[, 2], col = kmeans_result$cluster, pch = 16,
     xlim = x_range, ylim = y_range,
     main = "K-means Clustering: Humidity vs. Rainfall in NSW", 
     xlab = "Humidity9am (scaled)", ylab = "Rainfall (scaled)")
points(kmeans_result$centers, col = 1:3, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 16)
dev.off()