list.files("data")
ed_spending <- read.csv(".\\data\\education_spending.csv") ## World Bank
health_spending1 <- read.csv(".\\data\\health_spending.csv") ## world Bank

health_spending2 <- health_spending[,c(2, 45:64)]
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "SVN", "USA")
health_spending3 <- health_spending2[health_spending$Country.Code %in% JBM_peers, ]
means <- colMeans(health_spending3[,2:21])
average_row <- c("AVE", means)
health_spending4 <- rbind(health_spending3, average_row)

