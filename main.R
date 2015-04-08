rm(list = ls())
gc()
run_on_server <- FALSE
if (run_on_server)
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
library(caret)
library(e1071)
# List all models in caret
# names(getModelInfo())

# Load data
# Treat ordinal as categorical or continuous? #############
load(file.path(data_dir, "data.rda"))
load(file.path(data_dir, "data_dummy.rda"))
load(file.path(data_dir, "data_dummyc.rda"))
train <- data$train
test <- data$test
ytrain <- data$ytrain

# Create data partitions of 80% and 20%
ntrain <- nrow(train)
train_indices <- sample(1:ntrain)[1:floor(ntrain*0.8)]
train_val <- train[-train_indices, ]

# Set up caret models
train_control <- trainControl(method = "cv", number = 10, returnResamp = "none")

# mod_types <- c("rf", "gbm", "svmRadial")
# mod_types <- c("gbm")
mod_types <- c("pls")
mod <- list()
probs <- matrix(NA, nrow(test), ncol(ytrain))
for (mod_type in mod_types) {
  for (svc_index in 1:ncol(ytrain)) {
    
    # Testing!!!
    # mod_type <- "pls"
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
  write_submission(probs, paste0(mod_type, "seed", seed))
  save(mod, file = file.path(results_dir, paste0("mod_", mod_type, ".rda")))
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
# SL.library <- c("SL.gam", "SL.gbm", "SL.glm", "SL.glmnet",
#                 "SL.nnet", "SL.randomForest",  
#                 "SL.ridge", "SL.step", "SL.svm", "SL.mean")
# 
# # elastic net
# library(glmnet)
# set.seed(seed)
# for (alpha in seq(0.1, 0.9, 0.1)) {
#   cv_mod_ridge <- cv.glmnet(as.matrix(train_data[, -ncol(train_data)]), train_data$y, alpha = 0)
#   plot(cv_mod_ridge)
#   pred_ridge <- predict(cv_mod_ridge, s = cv_mod_ridge$lambda.min, newx = as.matrix(test_data))
#   write_submission(ytest = pred_ridge, model_name = paste0("elasticnet", alpha))
# }
# 
# # ridge regression
# library(glmnet)
# set.seed(seed)
# cv_mod_ridge <- cv.glmnet(as.matrix(train_data[, -ncol(train_data)]), train_data$y, alpha = 0)
# plot(cv_mod_ridge)
# pred_ridge <- predict(cv_mod_ridge, s = cv_mod_ridge$lambda.min, newx = as.matrix(test_data))
# write_submission(ytest = pred_ridge, model_name = "ridge1")
# 
# # lasso regression
# library(glmnet)
# set.seed(204832)
# 
# cv_mod_lasso <- cv.glmnet(as.matrix(train_data[, -ncol(train_data)]), train_data$y, alpha = 1)
# plot(cv_mod_lasso)
# pred_lasso <- predict(cv_mod_lasso, s = cv_mod_lasso$lambda.min, newx = as.matrix(test_data))
# cv_mod_lasso$lambda.min
# write_submission(ytest = pred_lasso, model_name = paste0("lasso1_lambda", cv_mod_lasso$lambda.min))
# 
# # pcr
# library(pls)
# set.seed(seed)
# mod_pcr <- pcr(y ~ ., data = train_data, scale = TRUE, validation = "CV")
# validationplot(mod_pcr, val.temp = "MSEP")
# pred_pcr <- predict(mod_pcr, test_data, ncomp = 
# 
# # gam
# # library(mgcv)
# # mod_gam <- gam(y ~ ., data = train_data)
# # predict(mod_gam, test_data)
# 
# if (FALSE) {
# #----------------------------------------------------------------------
# # mean(ytrain)
# write_submission(ytest = rep(mean(ytrain), ntest), model_name = "meanytrain")
# #----------------------------------------------------------------------
# # median(ytrain)
# write_submission(ytest = rep(median(ytrain), ntest), model_name = "medianytrain")
# #----------------------------------------------------------------------
# # random forest
# library(randomForest)
# set.seed(seed)
# mod_rf <- randomForest(y ~ ., data = train_data,
#                        importance = TRUE,
#                        # ntree = 2000, mtry = 3, 
#                        # nodesize = 10, maxnodes = 500, replace = FALSE, 
#                        do.trace = 10)
# pred_rf <- predict(mod_rf, test_data)
# save(mod_rf, file = "output/mod_rf1.rda")
# write_submission(ytest = pred_rf, model_name = "rf1")
# if (get_notifications)
#   pbPost(type = "note", 
#          title = paste0("STAT154"), 
#          body = ("Random forest done!"),
#          recipients = c(1, 2))
# #----------------------------------------------------------------------
# # GBM
# library(gbm)
# mod_gbm <- gbm(y ~ ., data = train_data, 
#                distribution = "gaussian", verbose = TRUE)
# pred_gbm <- predict(object = mod_gbm, newdata = test_data, n.trees = 100)
# save(mod_gbm, file = "output/mod_gbm1.rda")
# write_submission(ytest = pred_gbm, model_name = "gbm1")
# if (get_notifications)
#   pbPost(type = "note", 
#          title = paste0("STAT154"), 
#          body = ("GBM done!"),
#          recipients = c(1, 2))
# #----------------------------------------------------------------------
# # random forest
# library(randomForest)
# set.seed(seed)
# mod_rf <- randomForest(y ~ ., data = train_data_small,
#                        importance = TRUE,
#                        do.trace = 10)
# pred_rf <- predict(mod_rf, test_data_small)
# save(mod_rf, file = "output/mod_rf1_datasmall.rda")
# write_submission(ytest = pred_rf, model_name = "rf1_smalldata")
# if (get_notifications)
#   pbPost(type = "note", 
#          title = paste0("STAT154"), 
#          body = ("Random forest done (small data)!"),
#          recipients = c(1, 2))
# }
# #----------------------------------------------------------------------
# # GBM
# library(gbm)
# mod_gbm <- gbm(y ~ ., data = train_data_small, 
#                distribution = "gaussian", verbose = TRUE)
# pred_gbm <- predict(object = mod_gbm, newdata = test_data_small, n.trees = 100)
# save(mod_gbm, file = "output/mod_gbm1_datasmall.rda")
# write_submission(ytest = pred_gbm, model_name = "gbm1_smalldata")
# if (get_notifications)
#   pbPost(type = "note", 
#          title = paste0("STAT154"), 
#          body = ("GBM done (small data)!"),
#          recipients = c(1, 2))
# #----------------------------------------------------------------------
# # SuperLearner
# library(SuperLearner)
# set.seed(seed)
# SL.library <- c("SL.gam", "SL.gbm", "SL.glm", "SL.glmnet",
#                 "SL.nnet", "SL.randomForest",  
#                 "SL.ridge", "SL.step", "SL.svm", "SL.mean")
# mod_sl <- SuperLearner(Y = train_data_small$y,
#                        X = train_data_small[, -ncol(train_data_small)],
#                        newX = test_data_small,
#                        SL.library = SL.library,
#                        family = gaussian(),
#                        verbose = TRUE)
# save(mod_sl, file = "output/mod_sl1_datasmall.rda")
# write_submission(ytest = mod_sl$SL.predict, model_name = "sl1_smalldata")
# if (get_notifications)
#   pbPost(type = "note", 
#          title = paste0("STAT154"), 
#          body = ("SL done (small data)!"),
#          recipients = c(1, 2))
# # mod_slcv <- CV.SuperLearner(Y = train_data$y,
# #                             X = train_data[, -ncol(train_data)],
# #                             V = 10,
# #                             SL.library = SL.library,
# #                             family = gaussian(),
# #                             verbose = TRUE)
# }
