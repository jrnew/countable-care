## @knitr main
rm(list = ls())
gc()
run_on_server <- TRUE ###
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

write_submission <- function(probs, model_name) {
  file_path <- file.path("submit", paste0(model_name, ".csv"))
  submit <- read.csv("data/SubmissionFormat.csv")
  submit[, 2:ncol(submit)] <- probs
  if (file.exists(file_path))
    stop(paste0(file_path, " already exists!"))
  write.csv(submit, file.path(file_path), row.names = FALSE)
  message(paste0("Results written to ", file_path))
}

seed <- 12345
set.seed(seed)
library(caret)
library(e1071)
# List all models in caret
# names(getModelInfo())

# Load data
prop_missing_cutoff <- 0.5
load(file = file.path(data_dir, paste0("data_cutoff", prop_missing_cutoff, ".rda")))
train <- data$train
test <- data$test
ytrain <- data$ytrain

# Create data partitions of 80% and 20%
ntrain <- nrow(train)
train_indices <- sample(1:ntrain)[1:floor(ntrain*0.8)]
train_val <- train[-train_indices, ]

# Set up caret models
train_control <- trainControl(method = "cv", number = 10, returnResamp = "none")

# mod_types <- c("gbm", "rf")
mod_types <- c("rf")
mod <- list()
probs <- matrix(NA, nrow(test), ncol(ytrain))
for (mod_type in mod_types) {
  for (svc_index in 1:ncol(ytrain)) {
    
    # Testing!!!
    # mod_type <- "rf"
    # train_indices <- 1:100
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
#----------------------------------------------------------------------
# # Random probability drawn from U(0, 1)
# probs <- matrix(runif(nrow(test)*(ncol(ytrain))), nrow(test), ncol(ytrain))
# write_submission(probs, paste0("unifseed", seed))
# # Constant probability of 0.5
# probs <- matrix(0.5, nrow(test), ncol(ytrain))
# write_submission(probs, "constant0.5")
# # Constant probability of proportion for each service
# ytrain_props <- apply(ytrain, 2, mean)
# probs <- matrix(rep(ytrain_props, each = nrow(test)), nrow(test), ncol(ytrain))
# write_submission(probs, "constantprop")
#----------------------------------------------------------------------
# # Random forest
# library(randomForest)
# probs <- matrix(NA, nrow(test), ncol(ytrain) - 1)
# for (i in 1:ncol(ytrain)) {
#   target <- ytrain[, i]
#   model <- randomForest(target ~ ., data = train, 
#                         ntree = 1000, importance = TRUE)
#   save(model, file = file.path(results_dir, paste0("rf_ntrees1000", i)))
#   probs[, i] <- predict(model, newdata = test, type = "prob")
# }
# write_submission(probs, paste0("rf_ntrees1000seed", seed))
# if (get_notifications)
#   pbPost(type = "note", 
#          title = "stat222", 
#          body = "RF done!",
#          recipients = c(1, 2))
# 
# # GBM
# library(gbm)
# for (i in 1:ncol(ytrain)) {
#   target <- ytrain[, i]
#   model <- gbm(target ~ ., data = train, 
#                distribution = "bernoulli", 
#                n.trees = 1000, verbose = TRUE)
#   save(model, file = file.path(results_dir, paste0("gbm_ntrees1000", i)))
#   probs[, i] <- predict(object = model, newdata = test, type = "response", n.trees = 50)
# }
# write_submission(probs, paste0("gbm_ntrees1000seed", seed))
# if (get_notifications)
#   pbPost(type = "note", 
#          title = "stat222", 
#          body = "GBM done!",
#          recipients = c(1, 2))

# library(e1071)  
# mod_svm <- svm(train[train_indices, ], ytrain[train_indices, svc_index])
# svm_predictions<-predict(svm_fit,newdata=testing)  
#----------------------------------------------------------------------