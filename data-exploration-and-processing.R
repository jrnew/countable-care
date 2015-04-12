## @knitr data
rm(list = ls())
gc()
setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
data_dir <- "data"
fig_dir <- "fig"
results_dir <- "results"
dir.create(fig_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)
dir.create("submit", showWarnings = FALSE)
prop_missing_cutoff <- 0.95

# Read in data
train_readin <- read.csv(file.path(data_dir, "train_values.csv"), 
                         stringsAsFactors = FALSE, na.strings = "")
ytrain <- read.csv(file.path(data_dir, "train_labels.csv"))
test_readin <- read.csv(file.path(data_dir, "test_values.csv"), 
                        stringsAsFactors = FALSE, na.strings = "")

# Data exploration
# Column types
colnames_all <- names(train_readin)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type[-c(1, 2)])

# Check proportion of missing values in features
prop_missing <- sapply(train_readin, function(x) mean(is.na(x)))
par(mar = c(4.5, 4.5, 4.5, 2))
pdf(file.path(fig_dir, "prop-missing-in-columns.pdf"), width = 7, height = 5)
hist(prop_missing, main = "Proportion of missing values\nin features",
     xlab = "Proportion", ylab = "Number of features", col = "lightgrey")
dev.off()

# Data processing
# Check original number of features, not including id
ncol(train_readin[, -1]) # 1378

# Check for features with only constant values
cols_constant <- sapply(train_readin, function(x) length(unique(x)) == 1)
sum(cols_constant) # 20

# Feature engineering
# Column types
colnames_all <- names(train_readin)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type[-c(1, 2)])
cols_numeric <- colnames_type == "n"
cols_ordinal <- colnames_type == "o"
cols_categorical <- colnames_type == "c"

# Number of missing numeric/ordinal/categorical features
num_missing_numeric <- apply(train_readin[, cols_numeric], 1, function(x) sum(is.na(x)))
num_missing_ordinal <- apply(train_readin[, cols_ordinal], 1, function(x) sum(is.na(x)))
num_missing_categorical <- apply(train_readin[, cols_categorical], 1, function(x) sum(is.na(x)))
pdf(file.path(fig_dir, "dist-missing-values-across-women.pdf"), width = 12/1.2, 3.5/1.2)
par(mar = c(4.5, 4.5, 1, 1), mfrow = c(1, 3))
hist(num_missing_numeric, freq = FALSE, breaks = 50,
     main = "", col = "lightgrey",
     xlab = "Number of missing values for numeric features")
legend("topleft", legend = c(paste0("Mean = ", round(mean(num_missing_numeric), digits = 1), 
                                    "\nMedian = ", median(num_missing_numeric),
                                    "\nSD = ", round(sd(num_missing_numeric), digits = 1))),
       bty = "n")
hist(num_missing_ordinal, freq = FALSE, breaks = 50, 
     main = "", col = "lightgrey",
     xlab = "Number of missing values for ordinal features")
legend("topleft", legend = c(paste0("Mean = ", round(mean(num_missing_ordinal), digits = 1), 
                                    "\nMedian = ", median(num_missing_ordinal),
                                    "\nSD = ", round(sd(num_missing_ordinal), digits = 1))),
       bty = "n")
hist(num_missing_categorical, freq = FALSE, breaks = 50,
     main = "", col = "lightgrey",
     xlab = "Number of missing values for categorical features")
legend("topleft", legend = c(paste0("Mean = ", round(mean(num_missing_categorical), digits = 1), 
                                    "\nMedian = ", median(num_missing_categorical),
                                    "\nSD = ", round(sd(num_missing_categorical), digits = 1))),
       bty = "n")
dev.off()
num_missing_numeric_test <- apply(test_readin[, cols_numeric], 1, function(x) sum(is.na(x)))
num_missing_ordinal_test <- apply(test_readin[, cols_ordinal], 1, function(x) sum(is.na(x)))
num_missing_categorical_test <- apply(test_readin[, cols_categorical], 1, function(x) sum(is.na(x)))

# Check for features with proportion of missing values > prop_missing_cutoff
prop_missing <- sapply(train_readin, function(x) mean(is.na(x)))
cols_missing <- prop_missing > prop_missing_cutoff
sum(cols_missing)

# Remaining number of features, not including id
sum(!cols_constant & !cols_missing) - 1

# Remove above features and id
train <- train_readin[, names(train_readin) != "id" & !cols_constant & !cols_missing]
test <- test_readin[, names(train_readin) != "id" & !cols_constant & !cols_missing]

# Column types
colnames_all <- names(train)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type[-c(1, 2)])
cols_numeric <- colnames_type == "n"
cols_ordinal <- colnames_type == "o"
cols_categorical <- colnames_type == "c"

# Missing value imputation for remaining features
# Numeric features: Set as 0
for (i in 1:sum(cols_numeric)) {
  train[, cols_numeric][, i] <- ifelse(is.na(train[, cols_numeric][, i]), 
                                       0, train[, cols_numeric][, i])
  test[, cols_numeric][, i] <- ifelse(is.na(test[, cols_numeric][, i]), 
                                      0, test[, cols_numeric][, i])
}
# Ordinal features: Set as -1
# table(sapply(train[, cols_ordinal], min, na.rm = TRUE)) # min is 0 or 1
for (i in 1:sum(cols_ordinal)) {
  train[, cols_ordinal][, i] <- as.integer(ifelse(is.na(train[, cols_ordinal][, i]), 
                                              -1, train[, cols_ordinal][, i]))
  test[, cols_ordinal][, i] <- as.integer(ifelse(is.na(test[, cols_ordinal][, i]), 
                                             -1, test[, cols_ordinal][, i]))
}

# Categorical features
# Check for: i) features with categories in test but not train set.
# ii) features with missing values in test but not train set
cols_unknownlevels <- NULL
cols_nomissingintrain <- NULL
for (i in 1:sum(cols_categorical)) {
  if (any(is.na(test[, cols_categorical][, i])) & all(!is.na(train[, cols_categorical][, i]))) {
    print(i)
    cols_nomissingintrain <- c(cols_nomissingintrain, i)
  }
  if (any(!is.na(test[, cols_categorical][, i]) & 
            !(test[, cols_categorical][, i] %in% unique(train[, cols_categorical][, i])))) {
    cols_unknownlevels <- c(cols_unknownlevels, i)
    levels_train <- unique(train[, cols_categorical][, i])
    levels_train <- ifelse(is.na(levels_train), "missing", as.character(levels_train))
    levels_test <- unique(test[, cols_categorical][, i])
    levels_test <- ifelse(is.na(levels_test), "missing", as.character(levels_test))
    if (!("missing" %in% levels_train)) {
      print(i)
      print(levels_test[!(levels_test %in% levels_train)])
      print("---")
      cols_nomissingintrain <- c(cols_nomissingintrain, 
                                 rep(i, length(levels_test[!(levels_test %in% levels_train)])))
    }
  }
}

# Categorical features: Set as new category missing
for (i in 1:sum(cols_categorical)) {
  train[, cols_categorical][, i] <- as.factor(ifelse(is.na(train[, cols_categorical][, i]), 
                                                  "missing", train[, cols_categorical][, i]))
  test[, cols_categorical][, i] <- as.factor(ifelse(is.na(test[, cols_categorical][, i]),
                                                    "missing",
                                                    ifelse(!(test[, cols_categorical][, i] %in%
                                                      unique(train[, cols_categorical][, i])),
                                                      "missing", test[, cols_categorical][, i])))
}

# Set values in test set to most frequently-occurring category in train set for:
# i) features with categories in test but not train set.
# ii) features with missing values in test but not train set
for (c in seq_along(cols_nomissingintrain)) {
  i <- cols_nomissingintrain[c]
  value_new <- names(which.max(table(test[, cols_categorical][, i])))
  test[, cols_categorical][, i] <- as.factor(ifelse(as.character(test[, cols_categorical][, i]) ==
                                                      "missing", 
                                                    value_new, 
                                                    as.character(test[, cols_categorical][, i])))
}

# For random forest, ensure that categorical features have same
# levels in train and test sets
for (i in 1:sum(cols_categorical)) { 
  test[, cols_categorical][, i] <- factor(test[, cols_categorical][, i],
                                          levels = levels(train[, cols_categorical][, i]))
                                                    
}

# Add engineered features to data
train$num_missing_numeric <- num_missing_numeric
train$num_missing_ordinal <- num_missing_ordinal
train$num_missing_categorical <- num_missing_categorical
test$num_missing_numeric <- num_missing_numeric_test
test$num_missing_ordinal <- num_missing_ordinal_test
test$num_missing_categorical <- num_missing_categorical_test

# Convert release variable to factor, else it throws error
train$release <- as.factor(train$release)
test$release <- as.factor(test$release)

# Convert prediction label to alphabetical factor, 
# else it throws error in caret::predict
for (i in 1:ncol(ytrain))
  ytrain[, i] <- factor(ifelse(ytrain[, i] == 1, "yes", "no"))

# Save processed data to file
data <- list(train = train,
             ytrain = ytrain[, -1], # Drop id column 
             test = test, 
             prop_missing_cutoff = prop_missing_cutoff)
save(data, file = file.path(data_dir, paste0("data_cutoff", prop_missing_cutoff, ".rda")))
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Further data processing
# RF can only digest factors with up to 53 levels
cols_nlevels <- apply(train[, cols_categorical], 2, 
                      function(x) length(unique(x)))
cols_morethan53levels <- cols_nlevels > 53
sum(cols_morethan53levels) # none
names(train)[cols_morethan53levels]
# pdf(file.path(fig_dir, "features-with-more-than-53-levels.pdf"), width = 18, height = 15)
# par(mar = c(4.5, 4.5, 4.5, 2), mfrow = c(5, 6))
# for (i in 1:sum(cols_morethan32levels)) {
#   barplot(train[, cols_morethan32levels][, i])
#   print(range(train[, cols_morethan32levels][, i], na.rm = TRUE))
# }
# dev.off()
#----------------------------------------------------------------------
# Check number of observations per survey release
table(train_readin$release)

# Check number of combinations of ytrain
nrow(ytrain) # 14644
nrow(unique(ytrain[, -1])) # 932

# Check number of services used across all women
numy <- apply(ytrain[, -1], 1, sum)
summary(numy)
pdf(file.path(fig_dir, "number-of-svcs-used.pdf"), width = 7, height = 5)
par(mar = c(4.5, 4.5, 1, 1))
hist(numy, freq = FALSE, main = "",
     xlab = "Number of services used by each woman", col = "lightgrey")
legend("topright", legend = c(paste0("Mean = ", round(mean(numy), digits = 1), 
                                    "\nMedian = ", median(numy),
                                    "\nSD = ", round(sd(numy), digits = 1))),
       bty = "n")
dev.off()

# Check proportion of women using each service
pdf(file.path(fig_dir, "prop-women-using-each-service.pdf"), 
    width = 7, height = 5)
par(mar = c(4.5, 5.5, 4.5, 1))
barplot(apply(ytrain[, -1], 2, mean), ylim = c(0, 1),
        names.arg = gsub("service_", "", colnames(ytrain[, -1])),
        main = "All survey releases", xlab = "Health care service",
        ylab = "Proportion of women who\nused each service")
dev.off()

releases <- sort(unique(train_readin$release))
for (i in seq_along(releases)) {
  pdf(file.path(fig_dir, 
                paste0("prop-women-using-each-service-survey-release-", releases[i], ".pdf")), 
      width = 7, height = 5)
  par(mar = c(4.5, 5.5, 4.5, 1))
  barplot(apply(ytrain[train_readin$release == releases[i], -1], 2, mean), ylim = c(0, 1),
          names.arg = gsub("service_", "", colnames(ytrain[, -1])),
          main = paste0("Survey release ", releases[i]), xlab = "Health care service",
          ylab = "Proportion of women who\nused each service")
  dev.off()
}
