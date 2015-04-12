## @knitr data-processing

#======================================================================
# Data processing
#======================================================================
# rm(list = ls())
# gc()
# setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
# data_dir <- "data"
# prop_missing_cutoff <- 0.9

# Read in data
train_readin <- read.csv(file.path(data_dir, "train_values.csv"), 
                         stringsAsFactors = FALSE, na.strings = "")
ytrain <- read.csv(file.path(data_dir, "train_labels.csv"))
test_readin <- read.csv(file.path(data_dir, "test_values.csv"), 
                        stringsAsFactors = FALSE, na.strings = "")

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
