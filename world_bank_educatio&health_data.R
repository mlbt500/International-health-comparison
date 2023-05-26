ed_spending1 <- read.csv(".\\data\\education_spending.csv") ## World Bank
health_spending1 <- read.csv(".\\data\\health_spending.csv") ## world Bank
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "SVN", "USA")

health_spending2 <- health_spending1[, c(2, 45:64)]
health_spending3 <- health_spending2[health_spending2$Country.Code %in% JBM_peers, ]
health_spending4 <- t(health_spending3[, -1])
colnames(health_spending4) <- health_spending3$Country.Code
rownames(health_spending4) <- 1:20
health_spending4 <- cbind(YEA = 2000:2019, health_spending4)
mean_column1 <- rowMeans(health_spending4[,2:13])
health_spending5 <- cbind(health_spending4, "Mean" = mean_column1)
health_df <- as.data.frame(health_spending5)

ed_spending2 <- ed_spending1[ed_spending1$LOCATION %in% JBM_peers & ed_spending1$SUBJECT == "PRY" & ed_spending1$MEASURE == "PC_GDP",]
ed_spending3 <- split(ed_spending2, ed_spending2$LOCATION)
ed_spending4 <- lapply(ed_spending3, function(x) x[,6:7])
ed_spending5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), ed_spending4)
country_codes <- names(ed_spending3)
colnames(ed_spending5)[-1] <- country_codes
mean_column2 <- rowMeans(ed_spending5[,2:13], na.rm = TRUE)
ed_spending6 <- cbind(ed_spending5, "Mean" = mean_column2)
ed_df <- as.data.frame(ed_spending6)
