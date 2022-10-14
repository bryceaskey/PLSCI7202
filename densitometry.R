library(tidyverse)

# Read in data and set column types.
data <- read.csv("C:/Users/Bryce/Box/Bryce/Dual SSU/densitometry_data.csv")
data$gel <- factor(data$gel, levels=c("1BS-1BH", "2BH-1BS", "1BH-1AS", "2BH-1AS"))
data$band <- factor(data$band)

# Invert intensity by subtracting from 255 - more intuitive for plotting.
data$mean_intensity_inv <- 255 - data$mean_intensity

# Create column to distinguish his from strep bands.
data$col_type <- character(length=dim(data)[1])
data$col_type[grepl("H.", data$band)] <- "His"
data$col_type[grepl("S.", data$band)] <- "Strep"
data$col_type <- factor(data$col_type)

# Create column for band number.
data$band_num <- str_split(data$band, "H|S", simplify=TRUE)[,2]
data$band_num <- factor(data$band_num)

# Normalize for fraction size.
data$mean_intensity_inv[data$col_type=="His"] <- data$mean_intensity_inv[data$col_type=="His"]*8
data$mean_intensity_inv[data$col_type=="Strep"] <- data$mean_intensity_inv[data$col_type=="Strep"]*5

# Faceted plot - intensity of each band in gel is shown.
ggplot(data=data, mapping=aes(x=band, y=mean_intensity_inv, fill=col_type)) +
  geom_col(position="dodge") +
  facet_wrap(~gel, nrow=1) +
  labs(x="Band", y="Normalized intensity", fill="Column") +
  theme_bw() +
  theme(axis.text=element_text(color="black"))

ggsave(filename="C:/Users/bca47/Box/Bryce/Dual SSU/densitometry.pdf",
       device=pdf(), width=24, height=8, units="cm")
dev.off()

# Averaged plot - intensity of bands for each column type are averaged together.
data_mean <- data %>%
  group_by(gel, col_type) %>%
  summarise(sum=sum(mean_intensity_inv))

ggplot(data=data_mean, mapping=aes(x=gel, y=sum, fill=col_type)) +
  geom_col(position="dodge") +
  labs(x="SSU combo", y="Normalized intensity", fill="Column") +
  theme_bw() +
  theme(axis.text=element_text(color="black"))

ggsave(filename="C:/Users/bca47/Box/Bryce/Dual SSU/densitometry_avg.pdf",
       device=pdf(), width=12, height=8, units="cm")
dev.off()
