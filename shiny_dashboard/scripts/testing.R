source("scripts/preprocessing.R")
source("scripts/models.R")

model <- get_logistic_clinical_model()
df <- get_raw_clinical_data()

summary(model$model)

new_patient <- data.frame(
  cellularity = "High",  # Replace 'YourValue' with the actual value
  pam50_._claudin.low_subtype = "LumA",  # Replace with actual value
  her2_status = "Positive",  # Replace with actual value
  hormone_therapy = "yes",  # Replace with actual value
  lymph_nodes_examined_positive = 3,  # Replace with actual number
  nottingham_prognostic_index = 4,  # Replace with actual number
  tumor_size = 120  # Replace with actual number
)

output <- predict(model$model, new_patient, type = "response")
