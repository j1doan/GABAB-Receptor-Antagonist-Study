# Load the ggplot2 package
library(ggplot2)

# Use the Iris dataset (already available in R)
df <- iris

# Calculate mean petal length by species
mean_petal_length <- aggregate(Petal.Length ~ Species, data = df, FUN = mean)

# Create a bar chart
ggplot(mean_petal_length, aes(x = Species, y = Petal.Length)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Species", y = "Mean Petal Length", title = "Average Petal Length by Species") +
  theme_minimal() +  # Customize the theme (optional)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Create a bar chart with custom colors
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#FF9999", "#66B3FF", "#99FF99")) +
  labs(x = "Species", y = "Sepal Length", title = "Customized Bar Chart")

# Simulated data with standard errors
error_data <- data.frame(
  Species = levels(iris$Species),
  Mean_Sepal_Length = tapply(iris$Sepal.Length, iris$Species, mean),
  SE = tapply(iris$Sepal.Length, iris$Species, function(x) sd(x) / sqrt(length(x)))
)

# Create a bar chart with error bars
ggplot(error_data, aes(x = Species, y = Mean_Sepal_Length)) +
  geom_bar(stat = "identity", fill = "#FF9999") +
  geom_errorbar(aes(ymin = Mean_Sepal_Length - SE, ymax = Mean_Sepal_Length + SE), width = 0.2) +
  labs(x = "Species", y = "Mean Sepal Length", title = "Bar Chart with Error Bars")

# Create a bar chart with rotated x-axis labels
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "identity", fill = "#66B3FF") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "Sepal Length", title = "Bar Chart with Rotated Labels")

# Create a bar chart with narrower bars
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "identity", fill = "#99FF99", width = 0.6) +
  labs(x = "Species", y = "Sepal Length", title = "Bar Chart with Narrower Bars")