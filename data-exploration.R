## @knitr data-exploration

#======================================================================
# Data exploration
#======================================================================
data_dir <- "data"
fig_dir <- "fig"
dir.create(fig_dir, showWarnings = FALSE)
prop_missing_cutoff <- 0.9

# Read in data
train_readin <- read.csv(file.path(data_dir, "train_values.csv"), 
                         stringsAsFactors = FALSE, na.strings = "")
ytrain <- read.csv(file.path(data_dir, "train_labels.csv"))
test_readin <- read.csv(file.path(data_dir, "test_values.csv"), 
                        stringsAsFactors = FALSE, na.strings = "")

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
