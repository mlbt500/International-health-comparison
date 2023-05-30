
ed_spending1 <- read.csv(".\\data\\education_spending.csv")  # World Bank
gov_ed_spending_per_pupil <- read.csv(".\\data\\gov_spending_education_per_pupil.csv")  # World Bank
health_spending1 <- read.csv(".\\data\\health_spending.csv")  # World Bank
gov_health <- read.csv(".\\data\\gov_health.csv") # World Bank

# Education spending (%GDP)
ed_spending2 <- ed_spending1[ed_spending1$LOCATION %in% JBM_peers & ed_spending1$SUBJECT == "PRY" & ed_spending1$MEASURE == "PC_GDP", ]
ed_spending3 <- split(ed_spending2, ed_spending2$LOCATION)
ed_spending4 <- lapply(ed_spending3, function(x) x[, 6:7])
ed_spending5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), ed_spending4)
country_codes <- names(ed_spending3)
colnames(ed_spending5)[-1] <- country_codes
mean_column2 <- rowMeans(ed_spending5[, 2:13], na.rm = TRUE)
ed_spending6 <- cbind(ed_spending5, "Mean" = mean_column2)
ed_spending6 <- as.data.frame(ed_spending6)
ed_spending6 <- ed_spending6[-1, ]
ed_df <- ed_spending6

ed_df2 <- ed_df
for (i in 2:ncol(ed_df2)) {
  ed_df2[, i] <- ed_df2[, i] / ed_df2[ed_df2$TIME == 2010, i]
}
ed_df2 <- ed_df2[, -ncol(ed_df2)]
mean_column3 <- rowMeans(ed_df2[, 2:13], na.rm = TRUE)
ed_df2 <- cbind(ed_df2, "Mean" = mean_column3)

# Plot: Primary education (%GDP)
summary_df <- OECD_health_df %>%
  group_by(YEA) %>%
  summarize(
    max_value = max(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE),  # I think JBM excludes the US
    min_value = min(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE)  
  )
ggplot(ed_df, aes(x = TIME)) +
  geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
  geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
  geom_line(aes(y = DNK), color = "gray50", size = 0.8) +
  geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
  geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
  geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
  geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
  geom_line(aes(y = CHE), color = "gray50", size = 0.8) +  # Replaced "SVN" with "CHE"
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_line(aes(y = USA), color = "gray50", size = 0.8) +
  geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
  geom_ribbon(data = summary_df_ed2, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Primary education (%GDP)") +
  theme_bw()

# Government expenditure per student, primary (% of GDP per capita)
primary_code <- unique(gov_ed_spending_per_pupil[, 1])[1]
gov_ed1 <- gov_ed_spending_per_pupil[, c(1, 4, 45:64)]
gov_ed2 <- gov_ed1[gov_ed1$Country.Code %in% JBM_peers & gov_ed1$Series.Name %in% primary_code, ]
gov_ed3 <- t(gov_ed2[, -1])
colnames(gov_ed3) <- gov_ed2$Country.Code
gov_ed3 <- gov_ed3[-1, ]
gov_ed3 <- cbind(YEA = 2000:2019, gov_ed3)
rownames(gov_ed3) <- 1:20
gov_ed3 <- as.data.frame(gov_ed3)
gov_ed3 <- as.data.frame(lapply(gov_ed3, as.numeric))
mean_column3 <- rowMeans(gov_ed3[, 2:13], na.rm = TRUE)
gov_ed4 <- cbind(gov_ed3, "Mean" = mean_column3)

gov_ed5 <- gov_ed4[, -14]
for (i in 2:ncol(gov_ed5)) {
  gov_ed5[, i] <- gov_ed5[, i] / gov_ed5[gov_ed5$YEA == 2010, i]
}
mean_column4 <- rowMeans(gov_ed5[, 2:13], na.rm = TRUE)
gov_ed5 <- cbind(gov_ed5, "Mean" = mean_column4)

summary_df_ed3 <- gov_ed5 %>%
  group_by(YEA) %>%
  summarize(
    max_value = max(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE, USA),  # Replaced "SVN" with "CHE"
    min_value = min(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE, USA)  # Replaced "SVN" with "CHE"
  )

# Plot: Government expenditure per student, primary (% of GDP per capita)
ggplot(gov_ed4, aes(x = YEA)) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
  geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
  geom_line(aes(y = AUT), color = "gray50", size = 0.8) +
  geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
  geom_line(aes(y = DNK), color = "gray50", size = 0.8) +
  geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
  geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
  geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
  geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
  geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
  geom_line(aes(y = CHE), color = "gray50", size = 0.8) +  # Replaced "SVN" with "CHE"
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_line(aes(y = USA), color = "gray50", size = 0.8) +
  geom_ribbon(data = summary_df_ed3, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Government expenditure per student, primary (% of GDP per capita)") +
  theme_bw()
