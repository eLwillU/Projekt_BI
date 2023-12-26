library(dplyr)

raw_data <- read.csv("METABRIC_RNA_Mutation.csv", sep = ",")

## TODO: fehlerhafte daten
raw_data[raw_data == ""] <- NA
raw_data[raw_data == "UNDEF"] <- NA
raw_data$cancer_type_detailed[raw_data$cancer_type_detailed == "Breast"] <- NA
raw_data$oncotree_code[raw_data$oncotree_code == "BREAST"] <- NA

## missing values
sum(is.na(raw_data))
length(unique(raw_data$patient_id)) == nrow(raw_data) # there are no duplicate patients
# per column
missing_column <- sapply(raw_data, function(x) sum(is.na(x)) / length(x) * 100)
missing_column <- sort(missing_column, decreasing = TRUE)
# per row
missing_row <- apply(raw_data, 1, function(x) sum(is.na(x)) / length(x) * 100)
missing_row <- sort(missing_row, decreasing = TRUE)
# removing them
raw_data <- subset(raw_data, select = -tumor_stage) # high missing of 26%
raw_data <- subset(raw_data, select = -cancer_type) # all values are the same
raw_data <- subset(raw_data, select = -patient_id) # no use for us. No duplicate patients
raw_data <- na.omit(raw_data) # TODO: Zuerst noch besser mit fehlenden Werten umgehen

## converting
# all character columns are factors
raw_data <- raw_data %>% 
  mutate(across(where(is.character), as.factor))
# additional factors that were numbers
raw_data$chemotherapy <- factor(
  raw_data$chemotherapy,
  levels = c(0,1),
  labels = c("no", "yes")
  )
raw_data$cohort <- as.factor(raw_data$cohort)
raw_data$neoplasm_histologic_grade <- as.factor(raw_data$neoplasm_histologic_grade)
raw_data$hormone_therapy <- factor(
  raw_data$hormone_therapy,
  levels = c(0,1),
  labels = c("no", "yes")
  )
raw_data$overall_survival <- factor(
  raw_data$overall_survival,
  levels = c(0,1),
  labels = c("no", "yes")
  )
raw_data$radio_therapy <- factor(
  raw_data$radio_therapy,
  levels = c(0,1),
  labels = c("no", "yes")
  )
# unordered to ordered factors if applicable
raw_data$cellularity <- factor(
  raw_data$cellularity, 
  levels = c("Low", "Moderate", "High"), 
  ordered = TRUE
  ) 
raw_data$neoplasm_histologic_grade <- factor(
  raw_data$neoplasm_histologic_grade, 
  levels = c(1,2,3), 
  ordered = TRUE
) 

## outlier
#TODO: discuss how to handle outliers. If removed only 6 rows remain :D
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}
raw_data <- raw_data %>% 
  mutate(across(where(is.numeric), remove_outliers))
raw_data <- na.omit(raw_data)

## TODO: Standardisierung / Normalisierung