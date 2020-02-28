# one proportion testing

prop.test(x = 15, n = 43, alternative = "less")


# Two proportion testing
prop.test(x = c(7, 12), n = c(15, 28), alternative = "two.sided")

# Chi-Squares in R

library("gmodels")

CrossTable(SW_survey_renamed$Age, SW_survey_renamed$RankI, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

# Goodness of Fit Chi-Square

library("dplyr")

SW_survey_renamed %>% group_by(FanYN) %>% summarize(count=n())

observed = c(552, 284)
expected = c(0.90, 0.10)
chisq.test(x = observed, p = expected)

## Both x and p have to be vectors of the same length.

# McNemar Chi-Square

## Going to recode into morning and afternoon for the time stamp

str(bakery_sales)

bakery_sales$DateR <- as.Date(bakery_sales$Date, format="%m/%d/%Y")

library("tidyr")

bakery_sales1 <- separate(bakery_sales, DateR, c("Year", "Month", "Day"), sep="-")

bakery_sales1$DayR <- NA
bakery_sales1$DayR[bakery_sales1$Day <= 15] <- 0
bakery_sales1$DayR[bakery_sales1$Day > 15] <- 1

bakery_sales1$CoffeeSales <- NA
bakery_sales1$CoffeeSales[bakery_sales1$Item == "Coffee"] <- 1
bakery_sales1$CoffeeSales[bakery_sales1$Item != "Coffee"] <- 0

CrossTable(bakery_sales1$DayR, bakery_sales1$CoffeeSales, fisher=TRUE, chisq = TRUE, mcnemar = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

