## @knitr modeling

#======================================================================
# Modeling
#======================================================================
data_dir <- "data"
fig_dir <- "fig"
results_dir <- "results"
dir.create(fig_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)
dir.create("submit", showWarnings = FALSE)

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
# Note: Load data coded with dummy variables for knn
# load(file = file.path(data_dir, paste0("data_dummy_cutoff",
#                                        prop_missing_cutoff, ".rda")))
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
}

# Set predicted prob to 0 for services d and n in survey release b
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
