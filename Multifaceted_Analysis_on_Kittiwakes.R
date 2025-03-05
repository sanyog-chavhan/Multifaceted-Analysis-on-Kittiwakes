# Understanding Kittiwake Trends: A Multifaceted Analysis

# ---------------------------- Task 1 ------------------------------------


library(reshape2)

obs_csv <- read.csv("data/Kittiwake_Observation.csv")

head(obs_csv)
View(obs_csv)

# Summary statistics for the observation data
summary(obs_csv)

dusk_data <- obs_csv$dusk
head(dusk_data)

print(paste("Mean of kittiwakes at dusk",mean(dusk_data)))

confidence_interval <- t.test(dusk_data, conf.level = 0.99)$conf.int

print(paste("99% confidence interval for the mean number of kittiwakes observed at dusk:", 
            round(confidence_interval[1], digits = 4), 
            "to", 
            round(confidence_interval[2], digits = 4)))

dusk_data <- c(67, 92, 86, 86, 66, 86, 75, 111, 96, 94, 110, 96, 100, 69, 77, 86, 103, 62, 85, 84, 100, 84, 82, 91, 85, 103, 72, 98)

dusk_dataframe <- data.frame(Dusk = dusk_data)

dusk_dataframe$Date <- 1:28

plot(dusk_dataframe$Date, dusk_dataframe$Dusk, type = "l", col = "purple", xlab = "Day", ylab = "Number of Kittiwakes", main = "Time Series Plot for dusk observations of Kittiwakes", xaxt = "n")

axis(1, at = seq(1, 28, by = 1), labels = seq(1, 28, by = 1))


# ----------------------------- Task 2 ----------------------------------

library(ggplot2)

hist_csv <- read.csv("data/Kittiwake_Historical.csv")

View(hist_csv)
str(hist_csv)

# Perform ANOVA test
anova_res <- aov(Site.D ~ X, data = hist_csv)
print(summary(anova_res))

# Hypothesis
# Null Hypothesis (H0): The decline in the number of kittiwakes over time is independent of the site.
# Alternative Hypothesis (H1): There are significant differences in the mean number of breeding pairs across different sites over time.

p_val <- 0.106
alpha <- 0.05

if (p_val < alpha) {
  print("Reject the Null Hypothesis")
} else {
  print("Failed to reject the Null Hypothesis")
}

# There is not enough evidence to conclude that there are significant differences in the mean number of breeding pairs across different sites over time.

hist_csv <- data.frame(
  Year = c(2000, 2004, 2008, 2012, 2016, 2020),
  Site.A = c(36, 33, 43, 40, 31, 34),
  Site.B = c(71, 71, 60, 60, 47, 58),
  Site.C = c(38, 49, 35, 33, 29, 33),
  Site.D = c(38, 28, 36, 33, 29, 23),
  Site.E = c(56, 48, 40, 40, 31, 21)
)

# Subset data for Site D
site_d_data <- data.frame(Year = hist_csv$Year, Breeding.pairs = hist_csv$Site.D)
print(site_d_data)

# Fit a linear model
linear_model <- lm(Breeding.pairs ~ Year, data = site_d_data)

# Predict breeding pairs for 2014
est_2014 <- predict(linear_model, newdata = data.frame(Year = 2014))

print(paste("Estimated number of breeding pairs at Site D in 2014:", round(est_2014, digits = 2)))

# Plot 
ggplot(site_d_data, aes(x = Year, y = Breeding.pairs)) +
  geom_point(color = "#66c2a5", size = 5, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "#fc8d62", linetype = "solid", size = 1.5) +
  labs(title = "Number of Breeding Pairs at Site D Over the Years",
       x = "Year",
       y = "Number of Breeding Pairs",
       caption = "Source: Ornithologist's Data") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "italic", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")


# ----------------------------Task 3---------------------------------


# ---------------------------- 3a ----------------------------


measure_csv <-read.csv("data/Kittiwake_Measurement.csv")
summary(measure_csv)

# Pairwise scatter-plot matrix
pairs(measure_csv[, c("Weight", "Wingspan", "Culmen")], main = "Scatterplot Matrix for Measurement Data")


# ---------------------------- 3b ----------------------------

library(ggplot2)
library(tidyr)

correlation_red_legged <- cor.test(measure_csv$Wingspan[measure_csv$Sub.species == "Red-legged"], measure_csv$Culmen[measure_csv$Sub.species == "Red-legged"])
print(correlation_red_legged)

# Interpretation
# For the Red-legged subspecies, the correlation coefficient (0.2934) points to a weakly positive association between wing span and culmen length.

correlation_black_legged <- cor.test(measure_csv$Wingspan[measure_csv$Sub.species == "Black-legged"], measure_csv$Culmen[measure_csv$Sub.species == "Black-legged"])
print(correlation_black_legged)

# Interpretation
# The correlation coefficient (0.6477) suggests a moderate to strong positive correlation between wing span and culmen length for the Black-legged sub-species.


# Scatter-plot for both Black-legged and Red-legged Sub-species
plot(c(measure_csv$Wingspan[measure_csv$Sub.species == "Black-legged"],
       measure_csv$Wingspan[measure_csv$Sub.species == "Red-legged"]),
     c(measure_csv$Culmen[measure_csv$Sub.species == "Black-legged"],
       measure_csv$Culmen[measure_csv$Sub.species == "Red-legged"]),
     main = "Scatterplot for Kittiwakes",
     xlab = "Wing Span", ylab = "Culmen Length", pch = 16, col = c(rep("blue", sum(measure_csv$Sub.species == "Black-legged")),
                                                                   rep("red", sum(measure_csv$Sub.species == "Red-legged"))))


measure_df <- data.frame(measure_csv)

measure_long <- pivot_longer(measure_df, 
                             cols = c(Wingspan, Culmen), 
                             names_to = "Measurement", 
                             values_to = "Value")

p <- ggplot(measure_long, aes(x = Sub.species, y = Value, fill = Sub.species)) +
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  facet_wrap(~Measurement, scales = 'free') +
  labs(title = "Wingspan and Culmen Length by Sub-species",
       x = "Sub-species",
       y = "Measurement",
       fill = "Sub-species") +
  theme_minimal() +
  theme(legend.position = "none")

print(p)


# ---------------------------- 3c ----------------------------


# Null Hypothesis (H₀): There is no difference in the mean weights between the Black-legged and Red-legged sub-species.
# Alternative Hypothesis (H₁): There is a significant difference in the mean weights between the Black-legged and Red-legged sub-species.

t_test_weights <- t.test(measure_csv$Weight[measure_csv$Sub.species == "Black-legged"],
                         measure_csv$Weight[measure_csv$Sub.species == "Red-legged"])
print(t_test_weights)

# Box plot to compare weights of two sub-species
ggplot(measure_csv, aes(x = Sub.species, y = Weight, fill = Sub.species)) +
  geom_boxplot() +
  labs(title = "Comparison of Bird Weights by Sub-species",
       x = "Sub-species",
       y = "Weight") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal()

# Interpretation

# The t-value of -0.81326 indicates the difference in means scaled by the standard error. In this case, it is negative, suggesting that the mean weight of Black-legged birds is slightly lower than the mean weight of Red-legged birds.
# The p-value (0.4245) is greater than the commonly used significance level of 0.05. Therefore, you do not have enough evidence to reject the null hypothesis.
# The 95% confidence interval includes zero, further supporting the idea that the true difference in means could be zero. This is consistent with the non-significant p-value.


# ---------------------------- 3d ----------------------------


install.packages("car")
library(car)

# Perform MANOVA
manova_result <- manova(cbind(Wingspan, Culmen, Weight) ~ Sub.species, data = measure_csv)

# Display the MANOVA results
print(manova_result)

# Display summary with Pillai's trace statistic
summary(manova_result, test = "Pillai")

# Interpretation:
# The MANOVA test indicates a significant effect of Sub.species on the combined dependent variables (Wingspan, Culmen, Weight), as evidenced by the low p-value (0.002404). 
# This result suggests that there are significant differences in the combined means of the three variables between the sub-species groups.


# ---------------------------- Task 4 -------------------------------


# ---------------------------- 4a ----------------------------


location_csv <- read.csv("data/Kittiwake_Location.csv")
dfloc_csv <- data.frame(location_csv)
print(dfloc_csv)
View(location_csv)

# Fitting a linear model
linear_model <- lm(Breeding.pairs ~ ., data = dfloc_csv)
print(summary(linear_model))

# Interpretation
# The model suggests that sandeel concentration and cliff height are significant predictors of the number of breeding pairs.

# Testing linear model to predict the number of breeding pair
new_data <- data.frame(Coast.direction = "West", sandeel = 0.9, Summer.temp = 22.9, cliff.height = 2.53)
predictions_lm <- predict(linear_model, newdata = new_data, level = 0.8, interval = "confidence")
print(predictions_lm)

# Interpretation
# In summary, the model predicts that the number of breeding pairs is approximately 30.79, 
# and we have a confidence interval (at the specified confidence level) ranging from 19.87 to 41.70.


# ---------------------------- 4b ----------------------------


# Fitting a linear model to the logarithm of the number of breeding pair
log_linear_model <- lm(log(Breeding.pairs) ~ ., data = dfloc_csv)
print(summary(log_linear_model))

# Interpretation:
# Sandeel concentration and cliff height are significant predictors of the log number of breeding pairs.
# The coast direction also appears to have an effect, with North and South directions associated with a decrease in the log number of breeding pairs compared to other directions.
# Summer temperature does not seem to have a significant effect on the log number of breeding pairs.

# Testing linear model to the logarithm of the number of breeding pair
new_data <- data.frame(Coast.direction = "West", sandeel = 0.9, Summer.temp = 22.9, cliff.height = 2.53)
predictions <- predict(log_linear_model, newdata = new_data, level = 0.8, interval = "confidence")
print(predictions)

# Interpretation

# In summary, based on the provided data and model, we estimate that the log-transformed number of breeding pairs is approximately 3.742, 
# and we are 80% confident that the true value falls within the range of 3.643 to 3.841.


# ---------------------------- 4c and 4d ----------------------------


install.packages("margins")

library(ggplot2)
library(car)
library(MASS)
library(margins)


plot(linear_model, which = 1, main = "Linear Residuals vs Fitted")
plot(linear_model, which = 2, main = "Normal Q-Q Plot for Linear Model") # Suggests departures from normality as the points deviate from the straight line

plot(log_linear_model, which = 1, main = "Log Linear Residuals vs Fitted") # More linear relationship than the above model with a few outliers present
plot(log_linear_model, which = 2, main = "Normal Q-Q Plot for Log Linear Model") # The residuals follow a better normal distribution in case of log_linear_model

# The log-linear model seems to perform better in terms of meeting the assumptions of homoscedasticity and normality of residuals based on these diagnostic plots. 

# Calculating R-squared for both the models
print(summary(linear_model)$r.squared)
print(summary(log_linear_model)$r.squared)

# The log-linear model has a slightly higher R-squared value, suggesting that it explains a slightly larger proportion of the variability in the breeding pairs compared to the linear model.

# Interpretability of the coefficients for linear_model
linear_coeff <- coef(linear_model)
print(linear_coeff)

# Interpretability of the coefficients for log_linear_model
log_linear_coeff <- coef(log_linear_model)
print(log_linear_coeff)

# Likelihood ratio test to compare linear_model and log_linear_model
lr_test <- anova(linear_model, log_linear_model, test = "Chisq")
print(lr_test)

# Sandeel and cliff.height have very small p-values (<< 0.05)
# Coast.direction and Summer.temp have p-values greater than 0.05

# Considering Residual vs Fitted and Normal Q-Q plot, R-squared, interpretability of the coefficients and LRT, it seems like log_linear_model performs better.

# Effect of the covariates on the number of breeding pairs
marginal_effects <- margins(linear_model)
print(marginal_effects)


# ---------------------------- 4e ----------------------------


new_data_breeding_pair <- data.frame(Coast.direction = "East", sandeel = 1.84, 
                       Summer.temp = 17.8, cliff.height = 3.45)
new_data_breeding_pair_pred <- predict(log_linear_model, newdata = new_data_breeding_pair, level = 0.98, interval="confidence")
print(new_data_breeding_pair_pred)

# The predicted number of breeding pairs is approximately 4.791.
# And considering this model, we can be 98% confident that the true number of breeding pairs at any site with a given set of characteristics falls within the range of 4.682 to 4.900

# ------------------------- END -------------------------------
