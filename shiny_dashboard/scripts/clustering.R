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


QQlabels = c("0" = "overall_survival=0","1" = "overall_survival=1")
ggplot(data = data, aes(sample=tumor_size)) + 
  stat_qq() + 
  stat_qqline() + 
  facet_grid(.~overall_survival,labeller=labeller(overall_survival=QQlabels))

# The logarithm transformation
data$tumor_size_T <- log10((data$tumor_size)); data$tumor_size <- NULL
data$mutation_count_T <- log10((data$mutation_count)); data$mutation_count <- NULL

# plot  data
par(mfrow=c(2 ,2))
hist(data_n$tumor_size , main = "", xlab = "Tumor size", freq = FALSE )
curve(dnorm(x, mean = mean(data_n$tumor_size), sd = sd(data_n$tumor_size)),col = "red", add = TRUE)
#boxplot(data_n$tumor_size, xlab = "Tumor size")

hist(data$tumor_size_T , main = "", xlab = "Tumor size ", freq = FALSE )
curve(dnorm(x, mean = mean(data$tumor_size_T), sd = sd(data$tumor_size_T)),col = "red", add = TRUE)
#boxplot(data$tumor_size_T, xlab = "Tumor size ")

hist(data_n$mutation_count, main = "", xlab = "Mutation count", freq = FALSE )
curve(dnorm(x, mean = mean(data_n$mutation_count), sd = sd(data_n$mutation_count)),col = "red", add = TRUE)
#boxplot(data$mutation_count, xlab = "Mutation count")

hist(data$mutation_count_T, main = "", xlab = "Mutation count", freq = FALSE )
curve(dnorm(x, mean = mean(data$mutation_count_T), sd = sd(data$mutation_count_T)),col = "red", add = TRUE)
#boxplot(data$mutation_count_T, xlab = "Mutation count ")
