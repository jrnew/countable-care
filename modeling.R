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

mod_types <- c("gbm", "rf") # mod_types <- "knn"
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

#======================================================================
# Ridge Regression (Sample with 50% cutoff)
#======================================================================
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

for (i in 1:ncol(ytrain)) {
  print(i)
  cv_mod_ridge <- cv.glmnet(train, ytrain[,i], alpha = 0, parallel=TRUE)
  # Use 10-fold cross validation on 100 default lambda values
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

# Write submission
write_submission(result, "Ridge0.5")

#======================================================================
# Multinomial logistic model
#======================================================================
# Load data
x <- data$train
y <- data$ytrain
t <- data$test

# Multinomial logistic regression
# Convert a type of Y train data(factor -> numeric dummy)
genY <- function(y.dat) {
  y.dat1 <- lapply(y.dat, function(x)
    if (is.factor(x)) {
      as.numeric(x)
    }
  )
  y.dat1 <- rapply(y.dat1, c)
  y.dat1 <- data.frame(service_a= y.dat1[1:14644], service_b= y.dat1[14645:29288], 
                       service_c=y.dat1[29289:43932], service_d=y.dat1[43933:58576],
                       service_e=y.dat1[58577:73220], service_f=y.dat1[73221:87864],
                       service_g=y.dat1[87865:102508], service_h=y.dat1[102509:117152],
                       service_i=y.dat1[117153:131796], service_j=y.dat1[131797:146440],
                       service_k=y.dat1[146441:161084], service_l=y.dat1[161085:175728],
                       service_m=y.dat1[175729:190372], service_n=y.dat1[190373:205016])
  rownames(y.dat1) <- NULL
  y.dat1[y.dat1 == 1] <- 0
  y.dat1[y.dat1 == 2] <- 1
  
  return(y.dat1)
}

# new Y from this function
y <- genY(y)

# Correlations between Y variables
temp = cor(y)
image(temp)

# Generate 4 categories(00, 01, 10, 11) as factors
service_ab <- with(y, factor(paste(service_a, service_b, sep="")))
service_jk <- with(y, factor(paste(service_j, service_k, sep="")))
service_lm <- with(y, factor(paste(service_l, service_m, sep="")))

# Store categorical variables column in X train data frame
x$service_ab <- service_ab
x$service_jk <- service_jk
x$service_lm <- service_lm

# Fit the Multinomial logistic regression model with all numeric variables
# These are expample of 50% cut-off data. 80% and 90% have more dependent variables
test_ab <- with(x, multinom(service_ab ~ n_0002 + n_0005 + n_0012 + n_0019 + n_0034 
                            + n_0038 + n_0064 + n_0067 + n_0078 + n_0083 + n_0086
                            + n_0099 + n_0100 + n_0102 + n_0108 + n_0109 + n_0110))
test_jk <- with(x, multinom(service_jk ~ n_0002 + n_0005 + n_0012 + n_0019 + n_0034 
                            + n_0038 + n_0064 + n_0067 + n_0078 + n_0083 + n_0086
                            + n_0099 + n_0100 + n_0102 + n_0108 + n_0109 + n_0110))
test_lm <- with(x, multinom(service_lm ~ n_0002 + n_0005 + n_0012 + n_0019 + n_0034 
                            + n_0038 + n_0064 + n_0067 + n_0078 + n_0083 + n_0086
                            + n_0099 + n_0100 + n_0102 + n_0108 + n_0109 + n_0110))

# Predicted probality for each category
pred_ab <- predict(test_ab, newdata = t, "probs")
pred_jk <- predict(test_jk, newdata = t, "probs")
pred_lm <- predict(test_lm, newdata = t, "probs")

# Predicted probability for each service
service_a <- pred_ab[,3] + pred_ab[,4]
service_b <- pred_ab[,2] + pred_ab[,4]
service_j <- pred_jk[,3] + pred_jk[,4]
service_k <- pred_jk[,2] + pred_jk[,4]
service_l <- pred_lm[,3] + pred_lm[,4]
service_m <- pred_lm[,2] + pred_lm[,4]


# Merging the predicted probability with the other columns
# from GMB method
submit[["service_a"]] <- service_a
submit[["service_b"]] <- service_b
submit[["service_j"]] <- service_j
submit[["service_k"]] <- service_k
submit[["service_l"]] <- service_l
submit[["service_m"]] <- service_m

# Write csv file to be submitted
write.csv(submit, file = "category0.5.csv")
#======================================================================
# Benchmark models
#======================================================================
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
