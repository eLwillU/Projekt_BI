source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(plotly)

data <- get_raw_clinical_data()

str(data)

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

# Boxplot of age and death from cancer.
fig <- plot_ly(x=data$death_from_cancer, y=data$age_at_diagnosis, type="box",  quartilemethod="linear", name="Linear Quartile Mode")
fig <- fig %>% layout(title = "Age distribution: death_from_cancer")
fig


fig1 <- ggplot(data = data, aes(sample = age_at_diagnosis)) + 
  geom_qq() + 
  geom_qq_line() +
  facet_grid(. ~ death_from_cancer)
  
ggplotly(fig1)

# Boxplot Mutation Count
fig2 <- data %>%
  ggplot(aes(x = death_from_cancer, y = mutation_count)) +
  geom_boxplot() +
  labs(x = "Survival", y = "Mutation_count") 
ggplotly(fig2)

ggplot(data = data, aes(sample=mutation_count)) +
  geom_qq() + 
  geom_qq_line() +
  facet_grid(. ~death_from_cancer)

# Boxplot tumor size
fig3 <- data %>%
  ggplot(aes(x = death_from_cancer, y = tumor_size)) +
  geom_boxplot() +
  labs(x = "Survival", y = "Tumor size")

ggplotly(fig3)


# Bar Charts of the tumor sizes
p1_data <- data %>% 
  filter(death_from_cancer=="yes")
p2_data <- data %>% 
  filter(death_from_cancer=="no")

p1 <- plot_ly(p1_data, x=~tumor_size ) %>% 
  add_histogram()
p2 <- plot_ly(p2_data, x=~tumor_size ) %>% 
  add_histogram()
subplot(p1, p2)

p1 <- plot_ly(x=p1_data$tumor_size, type="histogram")
p1 %>% add_histogram(p2)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  
}

  
tumor_data$tumor_size <- data %>% 
  pull(tumor_size, death_from_cancer) %>% 
  remove_outliers() %>% 
  na.omit() %>% 
  normalize() 

no_outliers_normalised <- data %>%
  select(tumor_size, death_from_cancer) %>%
  mutate(tumor_size = remove_outliers(tumor_size)) %>%
  filter(!is.na(tumor_size)) %>%
  mutate(tumor_size = normalize(tumor_size))
  


no_outliers <- data %>%
  select(tumor_size, death_from_cancer) %>%
  mutate(tumor_size = remove_outliers(tumor_size)) %>%
  filter(!is.na(tumor_size)) 
 



p3 <- plot_ly(no_outliers_normalised, x=~tumor_size, type="histogram", nbinsx=20,  
              marker = list(color = 'lightblue',
                            line = list(color = 'navyblue',
                            width = 1.5)))


hist(data$tumor_size)



fig <- plot_ly(x=data$tumor_size, type="histogram", histfunc='sum', nbins=12)
fig <- fig %>% layout(yaxis=list(type='linear'))
fig


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



get_Cohort_Piechart <- function() { 
  cohortData <- data.frame("Category"= paste("Cohort", rownames(cohortPlotData)), cohortPlotData)
  plotData <- cohortData[,c('Category', "count")]
  fig <- plot_ly(plotData, labels = ~Category, values = ~count, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'))
  fig <- fig %>% layout(title = 'Cohorts of the METABRIC dataset',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  plot <- ggplotly(fig)
  return(plot)
}


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
