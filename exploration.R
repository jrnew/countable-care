rm(list = ls())
gc()
setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
fig_dir <- "fig"
results_dir <- "results"
dir.create(fig_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)
dir.create("submit", showWarnings = FALSE)
#----------------------------------------------------------------------
# Read in data
train_readin <- read.csv("data/train_values.csv", na.strings = "")
ytrain <- read.csv("data/train_labels.csv")
test_readin <- read.csv("data/test_values.csv", na.strings = "")
#----------------------------------------------------------------------
# Check number of observations per survey release
table(train_readin$release)

# Check number of combinations of ytrain
nrow(ytrain) # 14644
nrow(unique(ytrain[, -1])) # 932

# Check number of services used across all women
numy <- apply(ytrain[, -1], 1, sum)
summary(numy)
par(mar = c(4.5, 4.5, 4.5, 2))
pdf(file.path(fig_dir, "number-of-svcs-used.pdf"), width = 7, height = 5)
hist(numy, freq = FALSE, main = "Histogram of number of services used",
     xlab = "Number of services used", col = "lightgrey")
dev.off()

# Check proportion of women using each service
par(mar = c(4.5, 4.5, 4.5, 2))
pdf(file.path(fig_dir, "prop-women-using-each-service.pdf"), 
    width = 7, height = 5)
barplot(apply(ytrain[, -1], 2, mean), ylim = c(0, 1),
        names.arg = gsub("service_", "", colnames(ytrain[, -1])),
        main = "Proportion of women\nwho used each service",
        ylab = "Proportion")
dev.off()
#----------------------------------------------------------------------
# Data processing
# Column types
colnames_all <- names(train_readin)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type[-c(1, 2)])

# Check for columns with only missing values or constant values and drop them
cols_missingvalues <- sapply(train_readin, function(x) all(is.na(x)))
sum(cols_missingvalues) # 14
names(train_readin)[cols_missingvalues]
cols_constant <- sapply(train_readin, function(x) length(unique(x)) == 1)
sum(cols_constant) # 20
names(train_readin)[cols_constant]
dim(train_readin); dim(test_readin)
train <- train_readin[, !cols_missingvalues & !cols_constant]
test <- test_readin[, !cols_missingvalues & !cols_constant]
dim(train_readin); dim(test_readin)
dim(train); dim(test)

# Missing pattern plot
library(mi)
missing.pattern.plot(train)

# Check proportion of missing values in features
prop_missing <- sapply(train, function(x) mean(is.na(x)))
par(mar = c(4.5, 4.5, 4.5, 2))
pdf(file.path(fig_dir, "prop-missing-in-columns.pdf"), width = 7, height = 5)
hist(prop_missing, main = "Proportion of missing values\nin features",
     xlab = "Proportion", ylab = "Number of features", col = "lightgrey")
dev.off()

# Column types
colnames_all <- names(train)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type[-c(1, 2)])
cols_numeric <- colnames_type == "n"
cols_ordinal <- colnames_type == "o"
cols_categorical <- colnames_type == "c"
#----------------------------------------------------------------------
# Feature engineering
# Number of missing numeric/ordinal/categorical features
num_missing_numeric <- apply(train[, cols_numeric], 1, function(x) sum(is.na(x)))
num_missing_ordinal <- apply(train[, cols_ordinal], 1, function(x) sum(is.na(x)))
num_missing_categorical <- apply(train[, cols_categorical], 1, function(x) sum(is.na(x)))
hist(num_missing_numeric, freq = FALSE, 
     main = "Distribution of missing\nnumeric features across all women",
     xlab = "Number of missing numeric features")
hist(num_missing_ordinal, freq = FALSE, 
     main = "Distribution of missing\nordinal features across all women",
     xlab = "Number of missing ordinal features")
hist(num_missing_categorical, freq = FALSE, 
     main = "Distribution of missing\ncategorical features across all women",
     xlab = "Number of missing categorical features")

# Apply to test data
# test$num_missing_numeric <- apply(test[, cols_numeric], 1, function(x) sum(is.na(x)))
# test$num_missing_ordinal <- apply(test[, cols_ordinal], 1, function(x) sum(is.na(x)))
# test$num_missing_categorical <- apply(test[, cols_categorical], 1, function(x) sum(is.na(x)))

# What to do with missing values?
# Numeric features: Set as 0 or mean?
for (i in 1:sum(cols_numeric)) {
  train[, cols_numeric][, i] <- ifelse(is.na(train[, cols_numeric][, i]), 
                                       0, train[, cols_numeric][, i])
  test[, cols_numeric][, i] <- ifelse(is.na(test[, cols_numeric][, i]), 
                                       0, test[, cols_numeric][, i])
}
# Ordinal features: Set as -1
# table(sapply(train[, cols_ordinal], min, na.rm = TRUE)) # min is 0 or 1
for (i in 1:sum(cols_ordinal)) {
  train[, cols_ordinal][, i] <- ifelse(is.na(train[, cols_ordinal][, i]), 
                                       -1, train[, cols_ordinal][, i])
  test[, cols_ordinal][, i] <- ifelse(is.na(test[, cols_ordinal][, i]), 
                                      -1, test[, cols_ordinal][, i])
}
# Categorical features: Set as new category missing
for (i in 1:sum(cols_categorical)) {
  train[, cols_categorical][, i] <- ifelse(is.na(train[, cols_categorical][, i]), 
                                           "missing", train[, cols_categorical][, i])
  test[, cols_categorical][, i] <- ifelse(is.na(test[, cols_categorical][, i]), 
                                          "missing", test[, cols_categorical][, i])
}

# RF can only digest factors with up to 32 levels
cols_nlevels <- apply(train, 2, function(x) length(unique(x)))
cols_morethan32levels <- (cols_ordinal | cols_categorical) & cols_nlevels > 32
names(train)[cols_morethan32levels] # only ordinal features have this problem
sum(cols_morethan32levels)
pdf(file.path(fig_dir, "features-with-more-than-32-levels.pdf"), width = 18, height = 15)
par(mar = c(4.5, 4.5, 4.5, 2), mfrow = c(5, 6))
for (i in 1:sum(cols_morethan32levels)) {
  barplot(train[, cols_morethan32levels][, i])
  print(range(train[, cols_morethan32levels][, i], na.rm = TRUE))
}
dev.off()
