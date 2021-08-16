#https://tutorials.methodsconsultants.com/posts/reading-sas-spss-or-stata-files-into-r-using-haven/

library(haven)
library(plyr)
data <- read_sas(data_file = "gss2020panel_r1.sas7bdat",
                 catalog_file = "formats.sas7bcat") 

as_factor(data)
#read the data in as factors

print_tagged_na(data$ABANY_2) #Ok, so the data are "tagged" - the code is under the NA
test_total <- is_tagged_na(data$ABANY_2); count(test_total)

test_d <- is_tagged_na(data$ABANY_2, "d") #view number of "don't know" codes
test_i <- is_tagged_na(data$ABANY_2, "i") #inapplicable codes (don't use this - it's for when a respondent didn't see a question given their survey form, or because it did not apply to their circumstance)
test_n <- is_tagged_na(data$ABANY_2, "n") #no answer codes
test_s <- is_tagged_na(data$ABANY_2, "s") #skips over a question on the web

count(test_d)
count(test_i)
count(test_n)
count(test_s)
