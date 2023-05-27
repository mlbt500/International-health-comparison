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

health_df2 <- health_df
for (i in 2:ncol(health_df2)) {
  health_df2[, i] <- health_df2[, i] / health_df2[health_df2$YEA == 2010, i]
}

library(ggplot2)
library(dplyr)

summary_df <- health_df %>%
  group_by(YEA) %>%
  summarize(max_value = max(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, SVN, SWE),
            min_value = min(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, SVN, SWE))

ggplot(health_df, aes(x = YEA)) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
  geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
  geom_line(aes(y = AUT), color = "gray50", size = 0.8) +
  geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
  geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
  geom_line(aes(y = DNK), color = "gray50", size = 0.8) +
  geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
  geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
  geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
  geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
  geom_line(aes(y = SVN), color = "gray50", size = 0.8) +
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_ribbon(data = summary_df, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Health spending (%GDP)") +
  theme_bw()

  summary_df2 <- health_df2 %>%
    group_by(YEA) %>%
    summarize(max_value = max(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, SVN, SWE, USA),
              min_value = min(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, SVN, SWE, USA))
  
  ggplot(health_df2, aes(x = YEA)) +
    geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
    geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
    geom_line(aes(y = AUT), color = "gray50", size = 0.8) +
    geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
    geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
    geom_line(aes(y = DNK), color = "gray50", size = 0.8) +
    geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
    geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
    geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
    geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
    geom_line(aes(y = SVN), color = "gray50", size = 0.8) +
    geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
    geom_line(aes(y = USA), color = "gray50", size = 0.8) +
    geom_ribbon(data = summary_df2, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
    labs(x = NULL, y = NULL) +
    ggtitle("Health spending (/2010%GDP)") +
    theme_bw()
  

ed_spending2 <- ed_spending1[ed_spending1$LOCATION %in% JBM_peers & ed_spending1$SUBJECT == "PRY" & ed_spending1$MEASURE == "PC_GDP",]
ed_spending3 <- split(ed_spending2, ed_spending2$LOCATION)
ed_spending4 <- lapply(ed_spending3, function(x) x[,6:7])
ed_spending5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), ed_spending4)
country_codes <- names(ed_spending3)
colnames(ed_spending5)[-1] <- country_codes
mean_column2 <- rowMeans(ed_spending5[,2:13], na.rm = TRUE)
ed_spending6 <- cbind(ed_spending5, "Mean" = mean_column2)
ed_df <- as.data.frame(ed_spending6)
ed_df2 <- ed_df[6:15,c(1,3,4, 6:14)]
ed_df3 <- ed_df2
for (i in 2:ncol(ed_df3)) {
  ed_df3[, i] <- ed_df3[, i] / ed_df3[ed_df3$TIME == 2010, i]
}

summary_df_ed2 <- ed_df2 %>%
  group_by(TIME) %>%
  summarize(max_value = max(CAN, DEU, FIN, FRA, GBR, NLD, NOR, SVN, SWE, USA, Mean),
            min_value = min(CAN, DEU, FIN, FRA, GBR, NLD, NOR, SVN, SWE, USA, Mean))

ggplot(ed_df2, aes(x = TIME)) +
  geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
  geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
  geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
  geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
  geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
  geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
  geom_line(aes(y = SVN), color = "gray50", size = 0.8) +
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_line(aes(y = USA), color = "gray50", size = 0.8) +
  geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
  geom_ribbon(data = summary_df_ed2, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Primary education (%GDP)") +
  theme_bw()

summary_df_ed3 <- ed_df3 %>%
  group_by(TIME) %>%
  summarize(max_value = max(CAN, DEU, FIN, FRA, GBR, NLD, NOR, SVN, SWE, USA, Mean),
            min_value = min(CAN, DEU, FIN, FRA, GBR, NLD, NOR, SVN, SWE, USA, Mean))

ggplot(ed_df3, aes(x = TIME)) +
  geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
  geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
  geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
  geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
  geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
  geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
  geom_line(aes(y = SVN), color = "gray50", size = 0.8) +
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_line(aes(y = USA), color = "gray50", size = 0.8) +
  geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
  geom_ribbon(data = summary_df_ed3, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Primary education (/2010%GDP)") +
  theme_bw()