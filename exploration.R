rm(list = ls())
gc()
setwd("~/Copy/Berkeley/stat222-spring-2015/stat222sp15/projects/countable-care")
fig_dir <- "fig"
results_dir <- "results"
dir.create(fig_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)
dir.create("submit", showWarnings = FALSE)
#----------------------------------------------------------------------
train <- read.csv("data/train_values.csv")
ytrain <- read.csv("data/train_labels.csv")
test <- read.csv("data/test_values.csv")
#----------------------------------------------------------------------
# Check number of combinations of ytrain
dim(ytrain) # 14644    15
dim(unique(ytrain[, -1])) # 932  14
numy <- apply(ytrain[, -1], 1, sum)
summary(numy)
par(mar = c(4.5, 4.5, 4.5, 2))
pdf(file.path(fig_dir, "number-of-svcs-used.pdf"), width = 7, height = 5)
hist(numy, freq = FALSE, main = "Histogram of number of services used",
     xlab = "Number of services used", col = "lightgrey")
dev.off()

# # Check pairwise correlations in ytrain 
# # (does not really make sense for categorical response though...)
# comb <- combn(15, 2)
# correlation <- rep(NA, ncol(comb))
# for (i in 1:ncol(comb)) {
#   correlation[i] <- cor(ytrain[comb[1, i]], ytrain[comb[2, i]])
# }
# hist(correlation)

# Column types
colnames_all <- names(train)
colnames_type <- sapply(colnames_all, function(x) strsplit(x, "_")[[1]][1])
table(colnames_type)
cols_numeric <- colnames_type == "n"
cols_ordinal <- colnames_type == "o"
cols_categorical <- colnames_type == "c"

num_missing <- apply(train[, !cols_missingvalues & !cols_constant], 2, function(x) mean(is.na(x)))

