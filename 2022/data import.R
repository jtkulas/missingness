## rating data

together <- read.csv("2022\\missingness_data_ratings_merged_complete_file.csv")

together$alicia_rating[together$alicia_rating == "I don't mind sharing (1)"] <- 1
together$alicia_rating[together$alicia_rating == "mostly comfortable sharing (2)"] <- 2
together$alicia_rating[together$alicia_rating == "somewhat comfortable sharing (3)"] <- 3
together$alicia_rating[together$alicia_rating == "somewhat uncomfortable sharing (4)"] <- 4
together$alicia_rating[together$alicia_rating == "mostly uncomfortable sharing (5)"] <- 5
together$alicia_rating[together$alicia_rating == "none of your business (6)"] <- 6

together$john.o_rating[together$john.o_rating == "I don't mind sharing (1)"] <- 1
together$john.o_rating[together$john.o_rating == "mostly comfortable sharing (2)"] <- 2
together$john.o_rating[together$john.o_rating == "somewhat comfortable sharing (3)"] <- 3
together$john.o_rating[together$john.o_rating == "somewhat uncomfortable sharing (4)"] <- 4
together$john.o_rating[together$john.o_rating == "mostly uncomfortable sharing (5)"] <- 5
together$john.o_rating[together$john.o_rating == "none of your business (6)"] <- 6

together$alicia_rating <- as.numeric(as.character(together$alicia_rating))
together$john.o_rating <- as.numeric(as.character(together$john.o_rating))

#descr::freq(together$alicia_rating)
#descr::freq(together$john.o_rating)

icc <- psych::ICC(together[15:16])

round(icc$results$ICC[5],2) ## ICC 2k

together$intrusive <- rowMeans(together[15:16], na.rm=TRUE)

plot(together$intrusive, together$dontknow_rowcount1s)

descr::freq(together$validresponse_rowcount0s)

par(mfrow=c(2,2))
descr::freq(together$dontknow_rowcount1s)
descr::freq(together$noanswer_rowcount2s)
descr::freq(together$skippedweb_rowcount3s)
descr::freq(together$notapplicable_rowcount4s)

table(together$dontknow_rowcount1s, together$intrusive)
par(mfrow=c(1,1))
plot(together$dontknow_rowcount1s, together$intrusive)

a <- lm(intrusive ~ dontknow_rowcount1s + noanswer_rowcount2s + skippedweb_rowcount3s, data=together)
b <- lm(intrusive ~ poly(dontknow_rowcount1s, noanswer_rowcount2s, skippedweb_rowcount3s), data=together)
summary(b)

cor(together[c(18, 8:12, 4:7)])
