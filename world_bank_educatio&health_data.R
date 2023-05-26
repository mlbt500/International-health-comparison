list.files("data")
ed_spending1 <- read.csv(".\\data\\education_spending.csv") ## World Bank
health_spending1 <- read.csv(".\\data\\health_spending.csv") ## world Bank
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "SVN", "USA")

health_spending2 <- health_spending1[,c(2, 45:64)]
health_spending3 <- health_spending2[health_spending2$Country.Code %in% JBM_peers, ]
health_means <- colMeans(health_spending3[,2:21])
health_mean_row <- c("Mean", health_means)
health_spending4 <- rbind(health_spending3, health_mean_row)
# Remove the Mean row from health_spending4
health_spending4 <- health_spending4[-nrow(health_spending4), ]

# Transpose the health_spending4 dataframe
health_spending4 <- t(health_spending4)

# Convert the transposed dataframe to a new dataframe
health_spending4 <- as.data.frame(health_spending4, stringsAsFactors = FALSE)

# Set the first column name to "TIME"
colnames(health_spending4)[1] <- "TIME"

# Extract the country codes from the first row and set them as column names
colnames(health_spending4)[-1] <- unlist(health_spending4[1, -1])

# Remove the first row
health_spending4 <- health_spending4[-1, ]

# Convert the TIME column to Date format
health_spending4$TIME <- as.Date(health_spending4$TIME, format = "X%Y")


ed_spending3 <- split(ed_spending2, ed_spending2$LOCATION)
ed_spending4 <- lapply(ed_spending3, function(x) x[,6:7])
ed_spending5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), ed_spending4)
country_codes <- names(ed_spending3)
colnames(ed_spending5)[-1] <- country_codes
mean_column <- rowMeans(ed_spending5[,2:13], na.rm = TRUE)
ed_spending6 <- cbind(ed_spending5, "Mean" = mean_column)
ed_spending6

