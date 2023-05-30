library(ggplot2)
library(dplyr)

# Read data
OECD_health <- read.csv(".\\data\\health_OECD.csv")
OECD_education <- read.csv(".\\data\\Education_OECD.csv")


# Define JBM peers
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "CHE", "USA", "AUS")  # I added AUS

# JBM compulsory health spending

OECD_health1 <- OECD_health[OECD_health$SUBJECT == "COMPULSORY" & OECD_health$MEASURE == "PC_GDP"  & OECD_health$LOCATION  %in% JBM_peers & OECD_health$TIME %in% 2000:2019,]
OECD_health2 <- OECD_health1[,c(1,6:7)]
OECD_health3 <- split(OECD_health2, OECD_health2$LOCATION)
OECD_health4 <- lapply(OECD_health3, function(x) x[,2:3])
OECD_health5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), OECD_health4)
colnames(OECD_health5) <- c("YEA", names(OECD_health4))
means <- rowMeans(OECD_health5[,2:13])
OECD_health_df <- cbind(OECD_health5, "Mean" = means)

# Total health
OECD_health1 <- OECD_health[OECD_health$SUBJECT == "TOT" & OECD_health$MEASURE == "PC_GDP"  & OECD_health$LOCATION  %in% JBM_peers & OECD_health$TIME %in% 2000:2019,]
OECD_health2 <- OECD_health1[,c(1,6:7)]
OECD_health3 <- split(OECD_health2, OECD_health2$LOCATION)
OECD_health4 <- lapply(OECD_health3, function(x) x[,2:3])
OECD_health5 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), OECD_health4)
colnames(OECD_health5) <- c("YEA", names(OECD_health4))
means <- rowMeans(OECD_health5[,2:13])
OECD_health_df2 <- cbind(OECD_health5, "Mean" = means)

#JMB plot

summary_df <- OECD_health_df %>%
  group_by(YEA) %>%
  summarize(
    max_value = max(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE),  # I think JBM excludes the US
    min_value = min(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE)  
  )

ggplot(OECD_health_df, aes(x = YEA)) +
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
  geom_line(aes(y = CHE), color = "gray50", size = 0.8) +  # Replaced "SVN" with "CHE"
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_ribbon(data = summary_df, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Government/Compulsory spending on health (%GDP)") +
  theme_bw()

# total health spend plot

summary_df <- OECD_health_df2 %>%
  group_by(YEA) %>%
  summarize(
    max_value = max(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE),  # I think JBM excludes the US
    min_value = min(GBR, Mean, AUT, CAN, DEU, DNK, FIN, FRA, NLD, NOR, CHE, SWE)  
  )

ggplot(OECD_health_df2, aes(x = YEA)) +
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
  geom_line(aes(y = CHE), color = "gray50", size = 0.8) +  # Replaced "SVN" with "CHE"
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  geom_ribbon(data = summary_df, aes(ymin = min_value, ymax = max_value), fill = "lightgray", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  ggtitle("Total health spend (%GDP)") +
  theme_bw()

# Kristian's graph
ggplot(OECD_health_df2, aes(x = YEA)) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) + # I'm 80 per cent sure he has got his numbers wrong!
  geom_line(aes(y = AUS), color = "black", size = 1.5) +
  ggtitle("Total health UK versus Australia (%GDP)") +
  labs(x = NULL, y = NULL) + 
  theme_bw()


# Primary education spending (%GDP)
primary_ed <- OECD_education[OECD_education$LOCATION %in% JBM_peers & OECD_education$SUBJECT == "PRY" & OECD_education$MEASURE == "PC_GDP",]
primary_ed <- primary_ed[,c(1, 6, 7)]
primary_ed <- split(primary_ed, primary_ed$LOCATION)
primary_ed <- lapply(primary_ed, function(x) x[,2:3])
primary_ed2 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), primary_ed)
colnames(primary_ed2) <- c("YEA", names(primary_ed))
means <- rowMeans(primary_ed2[,2:13], na = TRUE)
primary_ed2 <- cbind(primary_ed2, "Mean" = means)
primary_ed2 <- primary_ed2[-c(1, 2), -c(2, 3, 5)]

# Primary to post-secondary non-tertiary, % of GDP, 2000 – 2019

ed <- OECD_education[OECD_education$LOCATION %in% JBM_peers & OECD_education$SUBJECT == "PRY_NTRY" & OECD_education$MEASURE == "PC_GDP",]
ed <- ed[,c(1, 6, 7)]
ed <- split(ed, ed$LOCATION)
ed <- lapply(ed, function(x) x[,2:3])
ed2 <- Reduce(function(x, y) merge(x, y, by = "TIME", all = TRUE), ed)
colnames(ed2) <- c("YEA", names(ed))
means <- rowMeans(ed2[,2:13], na.rm = TRUE)
ed2 <- cbind(ed2, "Mean" = means)
ed2 <- ed2[-c(1, 2), -c(2, 3, 5)]

#Plot primary education

ggplot(primary_ed2, aes(x = YEA)) +
  geom_line(aes(y = GBR), color = "darkred", size = 1.5) +
  geom_line(aes(y = Mean), color = "black", size = 1.5, linetype = "dotted") +
  geom_line(aes(y = CAN), color = "gray50", size = 0.8) +
  geom_line(aes(y = DEU), color = "gray50", size = 0.8) +
  geom_line(aes(y = DNK), color = "gray50", size = 0.8) +
  geom_line(aes(y = FIN), color = "gray50", size = 0.8) +
  geom_line(aes(y = FRA), color = "gray50", size = 0.8) +
  geom_line(aes(y = NLD), color = "gray50", size = 0.8) +
  geom_line(aes(y = NOR), color = "gray50", size = 0.8) +
  geom_line(aes(y = USA), color = "gray50", size = 0.8) +  # Replaced "SVN" with "CHE"
  geom_line(aes(y = SWE), color = "gray50", size = 0.8) +
  labs(x = NULL, y = NULL) +
  ggtitle("Primary education (%GDP)") +
  theme_bw()