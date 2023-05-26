list.files("data")
ed_spending1 <- read.csv(".\\data\\education_spending.csv") ## World Bank
health_spending1 <- read.csv(".\\data\\health_spending.csv") ## world Bank

health_spending2 <- health_spending1[,c(2, 45:64)]
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "SVN", "USA")
health_spending3 <- health_spending2[health_spending2$Country.Code %in% JBM_peers, ]
means <- colMeans(health_spending3[,2:21])
average_row <- c("AVE", means)
health_spending4 <- rbind(health_spending3, average_row)

unique(ed_spending1[,3])


ed_spending2 <- ed_spending1[ed_spending1$LOCATION %in% JBM_peers & ed_spending1$SUBJECT == "PRY" & ed_spending1$MEASURE == "PC_GDP",]
ed_spending3 <- split(ed_spending2, ed_spending2$LOCATION)
ed_spending4 <- lapply(ed_spending3, function(x) x[,6:7])
ed_spending5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), ed_spending4)