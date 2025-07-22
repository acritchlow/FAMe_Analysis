# IMPUTATION

# Load libraries
library(VIM)
library(skimr)
library(naniar)
library(dplyr)
library(readxl)

# Read the data
data <- read_excel("FAMe_Data.xlsx",
                   na = c("","NA", "?", "#VALUE!"),
                   trim_ws = TRUE)

# Select the relevant columns
dat_sub <- data %>%
  dplyr::select("bodyfat", "ALM", "ALMh", "thighmuscle", "intrafat", "subfat", "permuscle", 
                "onerep", "onerepllm", "onerepcsa", "mvc", "rtd", "pt_100", "pt_10", "pt_ratio", "protein", "mvpa", "bmi", "age", "id", "meno", "biopsyphase", "E2", "FEI", "TT", "FAI", "prog", "TEratio", "Ipernumber",  "IIApernumber", "Hybridpernumber", "Iperarea", "IIAperarea", "Hybridperarea", "Imeanarea", "IIAmeanarea", "Hybridmeanarea", "Tmeanarea", "pax7perfibre", "pax7perarea", "bodipy", "fibrosis")
# Overview of missing data in the dataset
Overview <- skimr::skim(dat_sub)

# Plot the distribution of missing data
hist(Overview$numeric.mean)
vis_miss(dat_sub)
gg_miss_var(dat_sub)

# Calculate missing percentage for each feature (column) and row
pMiss <- function(x) { sum(is.na(x)) / length(x) * 100 }
ID_miss <- as.data.frame(apply(dat_sub, 2, pMiss))  # Missing percentage for each feature
miR_miss <- as.data.frame(apply(dat_sub, 1, pMiss))  # Missing percentage for each row

# Visualize missing data pattern
aggr_plot <- aggr(dat_sub, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(dat_sub), 
                  cex.axis = 0.7, gap = 3, ylab = c("Histogram of missing data", "Pattern"))

# Check missing data before imputation
print("Missing data overview:")
print(ID_miss)
print(miR_miss)

# Apply KNN imputation to only "protein" and "mvpa"
dat_sub_imputed <- dat_sub %>%
  dplyr::select(protein, mvpa) %>%  # Only select protein and mvpa for imputation
  kNN(k = 10)  # Perform KNN imputation on square root of sample size

# Combine the imputed data back with the non-imputed columns
dat_imputed <- cbind(dat_sub %>% dplyr::select(-protein, -mvpa), dat_sub_imputed)

# Overview of the imputed data
Overview2 <- skimr::skim(dat_imputed)
hist(Overview2$numeric.mean)

# Save the imputed dataset
write.csv(dat_imputed, "FAMe_Imputed_Data.csv")

# Print the final dataset summary
summary(dat_imputed)
