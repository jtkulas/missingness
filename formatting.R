data <- read.csv("AliciaANOVAs.csv")

data$rated <- as.factor(as.character(data$rated))

form1 <- data[ which(data$form==1), ]
form2 <- data[ which(data$form==2), ]
form3 <- data[ which(data$form==3), ]
form4 <- data[ which(data$form==4), ]

#########################################################
#########################################################
############################## Replicate below for once and twice - 9/30

newdata1_self <- form1[c(2,61:140)]
newdata2_self <- form2[c(2,61:140)]
newdata3_self <- form3[c(2,61:140)]
newdata4_self <- form4[c(2,61:140)]

library(tidyr)
form1_long_self <- gather(newdata1_self, item, response, item1:item80, factor_key=TRUE)
form1_long_self <- form1_long_self[order(form1_long_self$rated, form1_long_self$item), ]

form2_long_self <- gather(newdata2_self, item, response, item1:item80, factor_key=TRUE)
form2_long_self <- form2_long_self[order(form2_long_self$rated, form2_long_self$item), ]

form3_long_self <- gather(newdata3_self, item, response, item1:item80, factor_key=TRUE)
form3_long_self <- form3_long_self[order(form3_long_self$rated, form3_long_self$item), ]

form4_long_self <- gather(newdata4_self, item, response, item1:item80, factor_key=TRUE)
form4_long_self <- form4_long_self[order(form4_long_self$rated, form4_long_self$item), ]

form1_long_self$form <- 1
form2_long_self$form <- 2
form3_long_self$form <- 3
form4_long_self$form <- 4

form1_long_self$type <- "self"
form2_long_self$type <- "self"
form3_long_self$type <- "self"
form4_long_self$type <- "self"

# library(psych)
# temp <- describe(form1)
# write.csv(temp, "C:\\Kulas\\temp.csv")

#########################################################
#########################################################
############################## Replicate below for once and twice - 9/30

newdata1_once <- form1[c(2,141:220)]
newdata2_once <- form2[c(2,141:220)]
newdata3_once <- form3[c(2,141:220)]
newdata4_once <- form4[c(2,141:220)]

library(tidyr)
form1_long_once <- gather(newdata1_once, item, response, i1:i80, factor_key=TRUE)
form1_long_once <- form1_long_once[order(form1_long_once$rated, form1_long_once$item), ]

form2_long_once <- gather(newdata2_once, item, response, i1:i80, factor_key=TRUE)
form2_long_once <- form2_long_once[order(form2_long_once$rated, form2_long_once$item), ]

form3_long_once <- gather(newdata3_once, item, response, i1:i80, factor_key=TRUE)
form3_long_once <- form3_long_once[order(form3_long_once$rated, form3_long_once$item), ]

form4_long_once <- gather(newdata4_once, item, response, i1:i80, factor_key=TRUE)
form4_long_once <- form4_long_once[order(form4_long_once$rated, form4_long_once$item), ]

form1_long_once$form <- 1
form2_long_once$form <- 2
form3_long_once$form <- 3
form4_long_once$form <- 4

form1_long_once$type <- "once"
form2_long_once$type <- "once"
form3_long_once$type <- "once"
form4_long_once$type <- "once"

#########################################################
#########################################################
############################## Replicate below for twice and twice - 9/30

newdata1_twice <- form1[c(2,221:300)]
newdata2_twice <- form2[c(2,221:300)]
newdata3_twice <- form3[c(2,221:300)]
newdata4_twice <- form4[c(2,221:300)]

library(tidyr)
form1_long_twice <- gather(newdata1_twice, item, response, qx1:qx80, factor_key=TRUE)
form1_long_twice <- form1_long_twice[order(form1_long_twice$rated, form1_long_twice$item), ]

form2_long_twice <- gather(newdata2_twice, item, response, qx1:qx80, factor_key=TRUE)
form2_long_twice <- form2_long_twice[order(form2_long_twice$rated, form2_long_twice$item), ]

form3_long_twice <- gather(newdata3_twice, item, response, qx1:qx80, factor_key=TRUE)
form3_long_twice <- form3_long_twice[order(form3_long_twice$rated, form3_long_twice$item), ]

form4_long_twice <- gather(newdata4_twice, item, response, qx1:qx80, factor_key=TRUE)
form4_long_twice <- form4_long_twice[order(form4_long_twice$rated, form4_long_twice$item), ]

form1_long_twice$form <- 1
form2_long_twice$form <- 2
form3_long_twice$form <- 3
form4_long_twice$form <- 4

form1_long_twice$type <- "twice"
form2_long_twice$type <- "twice"
form3_long_twice$type <- "twice"
form4_long_twice$type <- "twice"

########################################################################
########################################################################
########################################################################
########################################################################

form1 <- rbind(form1_long_self, form1_long_once, form1_long_twice)
form2 <- rbind(form2_long_self, form2_long_once, form2_long_twice)
form3 <- rbind(form3_long_self, form3_long_once, form3_long_twice)
form4 <- rbind(form4_long_self, form4_long_once, form4_long_twice)

## Next step = add Quentada indices

library(quanteda)

test <- read.csv("Observer-Rated Personality_ Self1 (Fall 2019) - Copy - Copy_December 3, 2019_13.53.csv") 

temp <- as.data.frame(t(test))   ## Duplicate item
temp2 <- temp[-c(1:17),]
temp3 <- temp2[-c(81:85),]

data2 <- corpus(temp3, docid_field = "V1",
                text_field = "V1")

tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall", "ELF", "FOG.NRI"))

one <- cbind(form1,tab2)

###################################################

test <- read.csv("Observer-Rated Personality_ Self2 (Fall 2019) - Copy - Copy_December 8, 2019_09.37.csv") 

temp <- as.data.frame(t(test))   ## Duplicate item
temp2 <- temp[-c(1:17),]
temp3 <- temp2[-c(81:85),]

data2 <- corpus(temp3, docid_field = "V1",
                text_field = "V1")

tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall", "ELF", "FOG.NRI"))

two <- cbind(form2,tab2)

##################################################

test <- read.csv("Observer-Rated Personality_ Self3 (Fall 2019) - Copy - Copy_December 8, 2019_09.45.csv") 

temp <- as.data.frame(t(test))   ## Duplicate item
temp2 <- temp[-c(1:17),]
temp3 <- temp2[-c(81:85),]

data2 <- corpus(temp3, docid_field = "V1",
                text_field = "V1")

tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall", "ELF", "FOG.NRI"))

three <- cbind(form3,tab2)

#################################################

test <- read.csv("Observer-Rated Personality_ Self4 (Fall 2019) - Copy - Copy_December 8, 2019_09.54.csv") 

temp <- as.data.frame(t(test))   ## Duplicate item
temp2 <- temp[-c(1:17),]
temp3 <- temp2[-c(81:85),]

data2 <- corpus(temp3, docid_field = "V1",
                text_field = "V1")

tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall", "ELF", "FOG.NRI"))

four <- cbind(form4,tab2)

################################################
################################################
################################################
################################################ File to use (9/30/20):

USETHIS <- rbind(one,two,three,four)