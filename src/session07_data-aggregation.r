library(ggplot2)
library(plyr)

gDat <- read.delim("data//gapminderDataFiveYear.txt")
str(gDat)

tDat <- with(gDat,
             cbind(cambodia = lifeExp[country == "Cambodia"],
                   canada = lifeExp[country == "Canada"],
                   rwanda = lifeExp[country == "Rwanda"]))
rownames(tDat) <- with(gDat, year[country == "Canada"])
str(tDat)
tDat

apply(tDat, 1, mean)
apply(tDat, 1, median)
rowMeans(tDat)
which.min(tDat[1, ])
apply(tDat, 1, which.min)
colnames(tDat)[apply(tDat, 1, which.min)]

apply(tDat, 2, min)

apply(tDat, 2, summary)
apply(tDat, 2, quantile)
apply(tDat, 2, quantile, probs = c(0.25, 0.75))

## challenge:
## use apply to compute the range of lifeExp for each country
apply(tDat, 2, range)

apply(tDat, 2, function(x) c(min = min(x), max = max(x)))

str(aggregate(lifeExp ~ continent, gDat, FUN = mean))

lifeExpByYearAndContinent <-
  aggregate(lifeExp ~ year * continent, gDat, FUN = mean)

ggplot(lifeExpByYearAndContinent,
       aes(x = year, y = lifeExp, color = continent)) +
  geom_line()

aggregate(country ~ continent, gDat,
          function(x) length(unique(x)))

nlevels(gDat$country)

x <- aggregate(country ~ continent, gDat,
               function(x) length(unique(x)))
sum(x$country) == nlevels(gDat$country)

library(plyr)

ggplot(subset(gDat, country == "Zimbabwe"),
       aes(x = year, y = lifeExp)) + geom_point(size = 7) +
  geom_smooth(se = FALSE, method = "lm")

lm(lifeExp ~ year, gDat, subset = country == "Zimbabwe")

(yearMin <- min(gDat$year))

lm(lifeExp ~ I(year - yearMin), gDat,
   subset = country == "Zimbabwe")

jFit <- lm(lifeExp ~ I(year - yearMin), gDat,
           subset = country == "Zimbabwe")
coef(jFit)

jFun <- function(z) {
  jFit <- lm(lifeExp ~ I(year - yearMin), z)
  jCoef <- coef(jFit)
  names(jCoef) <- c("intercept", "slope")
  return(jCoef)
}
jFun(subset(gDat, country == "Zimbabwe"))
jFun(subset(gDat, country == "Canada"))

gCoef <- ddply(gDat, ~ country * continent, jFun)
str(gCoef)

write.table(gCoef, "gCoef.txt", quote = FALSE, sep = "\t",
            row.names = FALSE)
