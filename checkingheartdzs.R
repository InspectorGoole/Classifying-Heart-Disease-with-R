library(tidyverse)
library(yardstick)
library(Metrics)

hd_data <- read.csv('hearts.csv')

head(hd_data, 5)


# create a new feature to represent a binary outcome for the current class variable for whether a patient has heart disease or not

hd_data %>% mutate(hd = ifelse(class > 0, 1, 0))-> hd_data


# This code creates a new binary column hd in your dataframe.

# If the existing class column is greater than 0, hd is set to 1. Otherwise, it's set to 0.

# The result overwrites the original hd_data object.'''


head(hd_data, 5)

# use statistical tests to check which features impact heart disease

hd_sex <- chisq.test(hd_data$sex, hd_data$hd)
print(hd_sex)

# Tests if there's a significant association between sex and heart disease

# Tells us if gender is related to having heart disease (categorical vs categorical)

# Output shows: p-value - if low (<0.05), sex is a significant predictor

# The result is stored in the object hd_sex.

hd_age <- t.test(hd_data$age ~ hd_data$hd)
print(hd_age)

# Tests if there's a significant difference in age between patients with vs without heart disease

# Tells us if age differs significantly between the two groups (numeric vs categorical)

# Output shows: p-value - if low (<0.05), age is a significant predictor

# check the thalach variable

hd_heartrate <- t.test(hd_data$thalach ~ hd_data$hd)
print(hd_heartrate)

# Group 0 (No heart disease): Average max heart rate = 158.4

# Group 1 (Heart disease): Average max heart rate = 139.3

# Difference: 19.1 beats per minute

# p-value = 9.106e-14 (extremely small - essentially 0.00000000000009)

# This means the difference is highly statistically significant

# People without heart disease have significantly higher maximum heart rates during exercise compared to those with heart disease. 


# so I am going to remove the class column and use hd column as our target variable

hd_data$class <- NULL

head(hd_data, 5)

# save the highly significant features to a list

highly_significant <- list('age', 'sex', 'thalach')

hd_data %>% mutate(hd_labelled = ifelse(hd == 0, 'No disease', 'Disease')) -> hd_data
head(hd_data, 5)

# visualize the gender associations
ggplot(hd_data, aes(x = hd_labelled, fill = factor(sex))) + 
  geom_bar(position = 'fill') + 
  labs(x = 'Heart Disease Status', y = 'Proportion', fill = 'Sex')

# visualize the age associations
ggplot(data = hd_data, aes(x = hd_labelled, y = age)) + geom_boxplot()
# Box position: The "Disease" group appears to have a higher median age than the "No disease" group

# Box spread: Shows the age distribution (25th to 75th percentile) for each group

# Whiskers: Show the range of typical ages in each group

# visualize the thalach associations
ggplot(data = hd_data, aes(x = hd_labelled, y = thalach)) + geom_boxplot()

# the disease group has lower median heart rate compared to no heart disease
# there are few outliers in no heart disease category with low heart rate

# build a model to predict heart disease using significant features as predictors
model <- glm(data = hd_data, hd ~ age + sex + thalach, family = 'binomial')

# Extract the model summary
summary(model)

# Predict the probability
pred_prob <- predict(model, hd_data, type='response')


# create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main dataframe

hd_data$pred_hd <- ifelse(pred_prob >= 0.5, 1, 0)

# Calculate a decision rule using probability 0.5 as cutoff and save the predicted decision into the main dataframe


# Calculate and print accuracy score
accuracy <- accuracy(hd_data$hd, hd_data$pred_hd)
print(paste("Accuracy=", accuracy))

precision <- precision(hd_data$hd, hd_data$pred_hd)
print(paste('precision=', precision))

# Calculate and print the confusion matrix
confusion <- conf_mat(table(hd_data$hd, hd_data$pred_hd))
confusion

head(hd_data, 5)

# now we will try to get better model with more features


# copy into new df
new_df <- hd_data

new_df$hd_labelled <- NULL
new_df$pred_hd <- NULL

head(new_df, 5)

full_model <- glm(hd ~ ., data = new_df, family = 'binomial')
summary(full_model)

pred_prb <- predict(full_model, new_df, type='response')
new_df$pred <- ifelse(pred_prob >= 0.5, 1, 0)

accuracy <- accuracy(new_df$hd, new_df$pred)
print(paste(accuracy))

precision <- precision(new_df$hd, new_df$pred)
print(paste(precision))


conf <- conf_mat(table(new_df$hd, new_df$pred))
conf 

# looks like there is not much difference between the 2 models we created except that in the new model we found out what are the best 
# features for this model

# Create the model with significant predictors
new_df$pred <- NULL

best_model <- glm(hd ~ sex + cp + trestbps + thalach + exang + ca + thal, 
                  data = new_df, family = 'binomial')

# View model summary
summary(best_model)

new_df$pred_prob <- NULL
new_df$pred_class <- NULL
head(new_df, 5)

# Make predictions
new_df$pred_prob <- predict(best_model, new_df, type = 'response')
new_df$pred_class <- ifelse(new_df$pred_prob >= 0.5, 1, 0)


head(new_df, 5)
# Calculate accuracy
library(Metrics)
accuracy(new_df$hd, new_df$pred_class)

accuracy(as.numeric(new_df$hd), as.numeric(new_df$pred_class))

accuracy(factor(new_df$hd, levels = c(0, 1)), 
         factor(new_df$pred_class, levels = c(0, 1)))

accuracy(as.integer(new_df$hd), as.integer(new_df$pred_class))

table(new_df$hd, useNA = "always")
table(new_df$pred_class, useNA = "always")

# looks like we had some NAs in our pred_class, so now we remove them an check accuracy
# Remove rows where pred_class is NA

clean_df <- new_df[!is.na(new_df$pred_class), ]

# Calculate accuracy
accuracy(clean_df$hd, clean_df$pred_class)

# accuracy is 0.85 which is great
confusion <- conf_mat(table(clean_df$hd, clean_df$pred_class))
confusion

install.packages("caret")

library(caret)

cm <- confusionMatrix(
  factor(clean_df$pred_class, levels = c(0, 1)),
  factor(clean_df$hd, levels = c(0, 1)),
  positive = "1"
)

print(cm)

# we have 0.81 for sensitivity score which is my priority metric.

