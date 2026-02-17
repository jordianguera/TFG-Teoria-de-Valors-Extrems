library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

losswide <- lossdf %>%
  pivot_wider(names_from = crypto, values_from = loss) %>%
  drop_na()
matriucor <- cor(losswide[,-1]) #per treure la columna amb la data

round(matriucor, 3)

matriucor <- cor(losswide[,-1], method = "spearman")
corlong <- melt(matriucor)
colnames(corlong) <- c("Criptomoneda1", "Criptomoneda2", "Correlacio")

ggplot(corlong, aes(x = Criptomoneda1, y = Criptomoneda2, fill = Correlacio)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4575b4",
                       mid = "white",
                       high = "#d73027",
                       midpoint = 0,
                       limits = c(-1,1)) +
  geom_text(aes(label = round(Correlacio, 2)),
            size = 4) +
  theme_minimal(base_size = 14) +
  labs(title = "Matriu de correlacions dels logretorns",
       fill = "Corr") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


