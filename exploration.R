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
train_readin <- read.csv("data/train_values.csv")
ytrain <- read.csv("data/train_labels.csv")
test_readin <- read.csv("data/test_values.csv")
#----------------------------------------------------------------------
# Check number of combinations of ytrain
dim(ytrain) # 14644    15
dim(unique(ytrain[, -1])) # 932  14

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
# Check for columns with only missing values or constant values and drop them
cols_missingvalues <- sapply(train_readin,
                             function(x) if (is.factor(x)) all(x == "") else all(is.na(x)))
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

# Column types
colnames_all <- names(train)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type)
cols_numeric <- colnames_type == "n"
cols_ordinal <- colnames_type == "o"
cols_categorical <- colnames_type == "c"



prop_missing <- sapply(train[, !cols_missingvalues & !cols_constant], function(x) 
  if (is.factor(x)) mean(x == "")
  else mean(is.na(x)))

par(mar = c(4.5, 4.5, 4.5, 2))
pdf(file.path(fig_dir, "prop-missing-in-columns.pdf"), width = 7, height = 5)
hist(prop_missing, main = "Proportion of missing values\nin features",
     xlab = "Proportion", ylab = "Number of features", col = "lightgrey")
dev.off()

sum(prop_missing > 0.5)



