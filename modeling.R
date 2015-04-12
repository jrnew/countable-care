## @knitr modeling

#======================================================================
# Modeling
#======================================================================
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
prop_missing_cutoff <- 0.9
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

mod_types <- c("gbm", "rf")
if (FALSE) {
mod <- list()
probs <- matrix(NA, nrow(test), ncol(ytrain))
for (mod_type in mod_types) {
  for (svc_index in 1:ncol(ytrain)) {
    # Train all the models with train data
    mod[[svc_index]] <- train(train[train_indices, ], ytrain[train_indices, svc_index], 
                              method = mod_type, trControl = train_control)
    
    # Predict on test data
    probs[, svc_index] <- predict(object = mod[[svc_index]], newdata = test, 
                                  type = "prob")$yes
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
}

# Ensemble the models
mod_ensemble_types <- c("glm")
mod_ensemble <- list()
probs_ensemble <- matrix(NA, nrow(test), ncol(ytrain))
for (mod_ensemble_type in mod_ensemble_types) {
  for (svc_index in 1:ncol(ytrain)) {
    for (mod_type in mod_types) {
      load(file.path(results_dir, 
                     paste0("mod_", mod_type, "_cutoff", prop_missing_cutoff, ".rda")))
      
      # Get predictions for each model and add them back to themselves
      train_val[[paste0(mod_type, "_PROB")]] <- predict(object = mod[[svc_index]], 
                                                       newdata = train_val, 
                                                       type = "prob")$yes
      test[[paste0(mod_type, "_PROB")]] <- predict(object = mod[[svc_index]], 
                                                  newdata = test, 
                                                  type = "prob")$yes
    }
    # Run an ensemble model to blend all the predicted probabilities
    mod_ensemble[[svc_index]] <- train(train_val[, grepl("_PROB", names(train_val))], 
                                       ytrain[-train_indices, svc_index], 
                                       method = mod_ensemble_type, trControl = train_control)
    
    # Predict on test data
    probs_ensemble[, svc_index] <- predict(object = mod_ensemble[[svc_index]], 
                                           newdata = test, 
                                           type = "prob")$yes
  }
  write_submission(probs_ensemble, paste0(mod_ensemble_type, "_cutoff", prop_missing_cutoff))
  save(mod_ensemble, file = file.path(results_dir, 
                                      paste0("mod_ensemble_", mod_ensemble_type, "_cutoff", 
                                             prop_missing_cutoff, ".rda")))
  if (get_notifications)
    pbPost(type = "note", 
           title = "stat222", 
           body = paste0(mod_ensemble_type, " done!"),
           recipients = c(1, 2))
}

# Set predicted prob to 0 for services d and n in survey release b
if (FALSE) {
prop_missing_cutoff <- 0.9 # does not matter which one is used here
load(file = file.path(data_dir, paste0("data_cutoff", prop_missing_cutoff, ".rda")))
test <- data$test
# submit_files <- list.files("submit")
# for (file in submit_files) {
  file <- "gbm_cutoff0.9.csv"
  preds <- read.csv(file.path("submit", file))
  pdf(file.path(fig_dir, "preds-svcsdn.pdf"), width = 10, 4)
  par(mar = c(4.5, 4.5, 1, 1), mfrow = c(1, 2))
  preds_svc_d <- preds$service_d[test$release == "b"]
  hist(preds_svc_d, freq = FALSE, breaks = 50,
       main = "", col = "lightgrey",
       xlab = "Predicted probability for service d")
  legend("topright", legend = c(paste0("Mean = ", round(mean(preds_svc_d), digits = 3), 
                                    "\nMedian = ", round(median(preds_svc_d), digits = 3),
                                    "\nSD = ", round(sd(preds_svc_d), digits = 3))),
       bty = "n")
  preds_svc_n <- preds$service_n[test$release == "b"]
  hist(preds_svc_n, freq = FALSE, breaks = 50,
       main = "", col = "lightgrey",
       xlab = "Predicted probability for service n")
  legend("topright", legend = c(paste0("Mean = ", round(mean(preds_svc_n), digits = 3), 
                                      "\nMedian = ", round(median(preds_svc_n), digits = 3),
                                      "\nSD = ", round(sd(preds_svc_n), digits = 3))),
         bty = "n")
  dev.off()
  preds$service_d[test$release == "b"] <- 0
  preds$service_n[test$release == "b"] <- 0
  write.csv(preds, file.path("submit", gsub("\\.csv", "_releaseb_svcsdn0.csv", file)), 
            row.names = FALSE)
# }
}

# Ridge Regression (Sample with 50% cutoff)
# load("data/data_dummy_0.5.rda")
train <- data_dummy_0.5$train
test <- data_dummy_0.5$test
ytrain <- data_dummy_0.5$ytrain
train <- as.matrix(train)
class(train) <- "numeric"
test <- as.matrix(test)
class(test) <- "numeric"
ytrain <- as.matrix(ytrain)
ytrain[ytrain=="yes"] <- 1
ytrain[ytrain=="no"] <- 0
class(ytrain) <- "numeric"
# Convert train, test and ytrain to numeric matrices, to accomodate glmnet.

library(glmnet)

# Use parallel to speed up
nCores <- parallel::detectCores()
doParallel::registerDoParallel(nCores)

# Keep track of the lambdas that give the smallest MSE
lambdas.min <- rep(0,14)
result <- matrix(rep(0,nrow(test)*ncol(ytrain)), ncol=ncol(ytrain))

for (i in 1:14){
  print(i)
  cv_mod_ridge <- cv.glmnet(train, ytrain[,i], alpha = 0, parallel=TRUE)
  #Use 10-fold cross validation on 100 default lambda values.
  lambdas.min[i] <- cv_mod_ridge$lambda.min
  pred_ridge <- predict(cv_mod_ridge, s = cv_mod_ridge$lambda.min, newx = test)
  result[,i] <- pred_ridge
}

# Visualize the lambdas
plot(x=1:14, y=lambdas.min, xlab="Column# in Y", ylab="Lambda.min", type="h",
     main="Lambdas with Smallest MSE Using Ridge_CV")

# Deal with values outside of 0 and 1:
colmean = colMeans(ytrain)
for (i in 1:14){
  result[which(result[,i] < 0),i] <- colmean[i]
  result[which(result[,i] > 1),i] <- colmean[i]
}

# Write Submission
write_submission(result, "Ridge0.5")

# Benchmark models
# Random probability drawn from U(0, 1)
probs <- matrix(runif(nrow(test)*(ncol(ytrain))), nrow(test), ncol(ytrain))
write_submission(probs, paste0("unifseed", seed))
# Constant probability of 0.5
probs <- matrix(0.5, nrow(test), ncol(ytrain))
write_submission(probs, "constant0.5")
# Constant probability of proportion for each service
ytrain_props <- sapply(ytrain, mean)
probs <- matrix(rep(ytrain_props, each = nrow(test)), nrow(test), ncol(ytrain))
write_submission(probs, "constantprop")
