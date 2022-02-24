
use <- read.csv("missingness_data_ratings_merged_complete_file.csv")

names(use)[9] <- "Do not know"
names(use)[10] <- "No answer"
names(use)[11] <- "Skipped web"

library(tidyr)

use_long <- gather(use, reason, number, `Do not know`:`Skipped web`, factor_key=TRUE)

library(ggplot2)
ggplot(use_long) + geom_histogram(aes(x=number, fill=reason)) + facet_wrap(~reason, nrow=3) + xlim(0,80)

ggplot(use_long, aes(x = number, fill = reason)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2) + xlim(0,80)

ggplot(use_long, aes(x = number, fill = reason)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, add_density=TRUE) + xlim(0,40) 
