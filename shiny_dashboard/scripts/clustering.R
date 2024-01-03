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
  ggplot(aes(x = death_from_cancer, y = tumor_size)) +
  geom_boxplot() +
  labs(x = "Survival", y = "Tumor size")

QQlabels = c("no" = "death_from_cancer=no","yes" = "death_from_cancer=yes")
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



library(randomForest)

your_data_numeric <- model.matrix(death_from_cancer ~ ., data)[,-1]
rf_model <- randomForest(death_from_cancer ~ ., data = data)
feature_importance <- importance(rf_model)
significant_columns <- rownames(feature_importance)

df <- data.frame(col2 = feature_importance)

df <- df %>%
  arrange(desc(MeanDecreaseGini))

df


data2 <- get_raw_data()

sum(data2$mutation_count)

hist(data$age_at_diagnosis)


cohortPlotData <- data %>%
  mutate(cohort = as.numeric(as.character(cohort))) %>%
  group_by(cohort) %>%
  summarize(count = n())

ggplot(cohortPlotData, aes(x = "", y = count, fill = factor(cohort))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  geom_text(aes(label = cohort),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Cohort"))



hormonePlotData <- data %>%
  mutate(hormone_therapy = data$hormone_therapy) %>%
  group_by(hormone_therapy) %>%
  summarize(count = n())

ggplot(hormonePlotData, aes(x = "", y = count, fill = factor(hormone_therapy))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  geom_text(aes(label = hormone_therapy),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Hormone Therapy"))

