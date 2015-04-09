rm(list = ls())
gc()
run_on_server <- TRUE
if (!run_on_server)
  setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
data_dir <- "data"
fig_dir <- "fig"
results_dir <- "results"
dir.create(fig_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)
dir.create("submit", showWarnings = FALSE)
get_notifications <- ifelse(run_on_server, TRUE, FALSE)
if (get_notifications) {
  library(RPushbullet)
  # options(error = function() { # Be notified when there is an error
  #   pbPost("note", "Error!", geterrmessage(), recipients = c(1, 2))
  # })
}
#----------------------------------------------------------------------
write_submission <- function(probs, model_name) {
  file_path <- file.path("submit", paste0(model_name, ".csv"))
  submit <- read.csv("data/SubmissionFormat.csv")
  submit[, 2:ncol(submit)] <- probs
  if (file.exists(file_path))
    stop(paste0(file_path, " already exists!"))
  write.csv(submit, file.path(file_path), row.names = FALSE)
  message(paste0("Results written to ", file_path))
}
#----------------------------------------------------------------------
seed <- 12345
set.seed(seed)
library(caret)
library(e1071)
# List all models in caret
# names(getModelInfo())

# Load data
prop_missing_cutoff <- 0.8
load(file = file.path(data_dir, paste0("data_dummy_", prop_missing_cutoff, ".Rdata")))
if (prop_missing_cutoff == 0.5) {
  train <- data_dummy_0.5$train
  test <- data_dummy_0.5$test
  ytrain <- data_dummy_0.5$ytrain
  
} else {
  train <- data_dummy_0.8$train
  test <- data_dummy_0.8$test
  ytrain <- data_dummy_0.8$ytrain
}

# Create data partitions of 80% and 20%
ntrain <- nrow(train)
train_indices <- sample(1:ntrain)[1:floor(ntrain*0.8)]
train_val <- train[-train_indices, ]

# Set up caret models
train_control <- trainControl(method = "cv", number = 10, returnResamp = "none")

mod_types <- c("knn")
mod <- list()
probs <- matrix(NA, nrow(test), ncol(ytrain))
for (mod_type in mod_types) {
  for (svc_index in 1:ncol(ytrain)) {
    
    # Testing!!!
    # mod_type <- "svmRadial"
    # train_indices <- 1:1000
    # svc_index <- 1
    
    # Train all the models with train data
    mod[[svc_index]] <- train(train[train_indices, ], ytrain[train_indices, svc_index], 
                              method = mod_type, trControl = train_control)
    
    # Predict on test data
    probs[, svc_index] <- predict(object = mod[[svc_index]], newdata = test, 
                                  type = "prob")$yes
    
    # Get predictions for each model and add them back to themselves
    # train_val[[paste0(mod_type, "_PROB"]] <- predict(mod[[svc_index]], train_val, type = "prob")
    # test[[paste0(mod_type, "_PROB"]] <- predict(mod[[svc_index]], test, type = "prob")
    
    # Run an ensemble model to blend all the predicted probabilities
    # mod_ensemble[[svc_index]] <- train(train_val, ytrain[-train_indices, svc_index], 
    #                                    method = "lasso", trControl = train_control)
    
    # Predict on test data
    # preds <- predict(mod_ensemble[[svc_index]], test, type = "prob")
  }
  write_submission(probs, paste0(mod_type, "_cutoff", prop_missing_cutoff))
  save(mod, file = file.path(results_dir, 
                             paste0("mod_", mod_type, "_cutoff", prop_missing_cutoff, ".rda")))
  if (get_notifications)
    pbPost(type = "note", 
           title = "stat222", 
           body = paste0(mod_type, " done!"),
           recipients = c(1, 2))
}
