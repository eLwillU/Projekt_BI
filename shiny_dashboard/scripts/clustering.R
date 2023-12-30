source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)

data <- get_raw_clinical_data()


par(mfrow=c(2 ,2))

data %>%
  ggplot(aes(x = factor(death_from_cancer), y = age_at_diagnosis)) +
  geom_boxplot() +
  labs(x = "Survival", y = "Age",title = "Death from cancer") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 9)
  )

ggplot(data = data, aes(sample = age_at_diagnosis)) + 
  geom_qq() + 
  geom_qq_line() +
  facet_grid(. ~ death_from_cancer)

data %>%
  ggplot(aes(x = death_from_cancer, y = mutation_count)) +
  geom_boxplot() +
  labs(x = "Survival", y = "Mutation_count") 

ggplot(data = data, aes(sample=mutation_count)) +
  geom_qq() + 
  geom_qq_line() +
  facet_grid(. ~death_from_cancer)


data %>%
  ggplot(aes(x = overall_survival, y = tumor_size)) +
  geom_boxplot() +
  labs(x = "Survival", y = "Tumor size")

QQlabels = c("no" = "overall_survival=no","yes" = "overall_survival=yes")
ggplot(data = data, aes(sample=tumor_size)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(. ~death_from_cancer, labeller=labeller(death_from_cancer=QQlabels))


hist(data$tumor_size)


# The logarithm transformation
data_n <- select_if(data, is.numeric) 
data$tumor_size_T <- log10((data$tumor_size)); data$tumor_size <- NULL
data$mutation_count_T <- log10((data$mutation_count)); data$mutation_count <- NULL

# Set up the plotting layout
par(mfrow = c(2, 2))

# Plot histogram for death_from_cancer = "yes"
hist(data$tumor_size_T[data$death_from_cancer == "yes"], main = "Tumor Size (Death = Yes)", xlab = "Tumor Size", col = "red")

# Plot histogram for death_from_cancer = "no"
hist(data$tumor_size_T[data$death_from_cancer == "no"], main = "Tumor Size (Death = No)", xlab = "Tumor Size", col = "blue")

# Plot histogram for mutation_count with death_from_cancer = "yes"
hist(data$mutation_count_T[data$death_from_cancer == "yes"], main = "Mutation Count (Death = Yes)", xlab = "Mutation Count", col = "green")

# Plot histogram for mutation_count with death_from_cancer = "no"
hist(data$mutation_count_T[data$death_from_cancer == "no"], main = "Mutation Count (Death = No)", xlab = "Mutation Count", col = "purple")


correlation_matrix <- cor(data[, sapply(data, is.numeric)], data$death_from_cancer)
correlation_with_target <- correlation_matrix[, "death_from_cancer"]
significant_columns <- names(sort(correlation_with_target, decreasing = TRUE))

library(randomForest)

your_data_numeric <- model.matrix(death_from_cancer ~ ., data)[,-1]
rf_model <- randomForest(death_from_cancer ~ ., data = data)
feature_importance <- importance(rf_model)
significant_columns <- rownames(feature_importance)

df <- data.frame(col2 = feature_importance)

df <- df %>%
  arrange(desc(MeanDecreaseGini))

df




