
library(dplyr)



covid_data <- read.csv("owid-covid-data.csv", stringsAsFactors = FALSE)

cat("Column Names:\n")
print(colnames(covid_data))


cat("\nDataset Dimensions (Rows x Columns):\n")
print(dim(covid_data))

cat("\nDataset Structure:\n")
str(covid_data)


cat("\nDataset Summary:\n")
print(summary(covid_data))
