rm(list = ls())
gc()
setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
fig_dir <- "fig"
results_dir <- "results"
dir.create(fig_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)
dir.create("submit", showWarnings = FALSE)
get_notifications <- FALSE
if (get_notifications) {
  library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost("note", "Error!", geterrmessage(), recipients = c(1, 2))
  })
}
#----------------------------------------------------------------------
write_submission <- function(probs, model_name) {
  file_path <- file.path("submit", paste0(model_name, ".csv"))
  submit <- read.csv("data/SubmissionFormat.csv")
  # if (!all.equal(dim(submit[, 2:ncol(submit)]), dim(probs)))
  #   stop(paste0("probs should be of dimensions ", dim(submit[, 2:ncol(submit)]), "!"))
  submit[, 2:ncol(submit)] <- probs
  if (file.exists(file_path))
    stop(paste0(file_path, " already exists!"))
  write.csv(submit, file.path(file_path), row.names = FALSE)
  message(paste0("Results written to ", file_path))
}

seed <- 12345
train <- read.csv("data/train_values.csv")
ytrain <- read.csv("data/train_labels.csv")
test <- read.csv("data/test_values.csv")

# Check for columns with only missing values or constant values and drop them
cols_missingvalues <- apply(train, 2, function(x) if (is.factor(x)) all(x == "") else all(is.na(x)))
sum(cols_missingvalues) # 14
names(train)[cols_missingvalues]
cols_constant <- apply(train, 2, function(x) length(unique(x)) == 1)
sum(cols_constant) # 20
names(train)[cols_constant]
dim(train); dim(test)
train <- train[, !cols_missingvalues & !cols_constant]
test <- test[, !cols_missingvalues & !cols_constant]
dim(train); dim(test)

# RF can only digest factors with up to 32 levels
cols_nlevels <- apply(train, 2, function(x) length(unique(x)))
cols_morethan32levels <- (cols_ordinal | cols_categorical) & cols_nlevels > 32
names(train)[cols_morethan32levels]
sum(cols_morethan32levels)

# Impute missing values
# http://trevorstephens.com/post/73770963794/titanic-getting-started-with-r-part-5-random
# Impute with mean/median

# Impute with a decision tree
# Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
#                 data=combi[!is.na(combi$Age),], method="anova")
# combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
#----------------------------------------------------------------------
# Random probability drawn from U(0, 1)
probs <- matrix(runif(nrow(test)*(ncol(ytrain) - 1)), nrow(test), ncol(ytrain) - 1)
write_submission(probs, paste0("unifseed", seed))
# Constant probability of 0.5
probs <- matrix(0.5, nrow(test), ncol(ytrain) - 1)
write_submission(probs, "constant0.5")
# Constant probability of proportion for each service
ytrain_props <- apply(ytrain[, -1], 2, mean)
probs <- matrix(rep(ytrain_props, each = nrow(test)), nrow(test), ncol(ytrain) - 1)
write_submission(probs, "constantprop")
#----------------------------------------------------------------------
# Random forest
library(randomForest)
probs <- matrix(NA, nrow(test), ncol(ytrain) - 1)
for (i in 1:ncol(ytrain)) {
  target <- ytrain[, i]
  model <- randomForest(target ~ ., data = train, 
                        ntree = 1000, importance = TRUE)
  save(model, file = file.path(results_dir, paste0("rf_ntrees1000", i)))
  probs[, i] <- predict(model, newdata = test, type = "prob")
}
write_submission(probs, paste0("rf_ntrees1000seed", seed))
if (get_notifications)
  pbPost(type = "note", 
         title = "stat222", 
         body = "RF done!",
         recipients = c(1, 2))

# GBM
library(gbm)
for (i in 1:ncol(ytrain)) {
  target <- ytrain[, i]
  model <- gbm(target ~ ., data = train, 
               distribution = "bernoulli", 
               n.trees = 1000, verbose = TRUE)
  save(model, file = file.path(results_dir, paste0("gbm_ntrees1000", i)))
  probs[, i] <- predict(object = model, newdata = test, type = "response", n.trees = 50)
}
write_submission(probs, paste0("gbm_ntrees1000seed", seed))
if (get_notifications)
  pbPost(type = "note", 
         title = "stat222", 
         body = "GBM done!",
         recipients = c(1, 2))
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
