###Diamonds Data Set

##price vs. x
ggplot(aes(x=x, y=price), data=diamonds) +
  geom_point() +
  xlim(3,9)

##Correlation between price and x, y, z
cor.test(diamonds$x, diamonds$price)
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

##Scatterplot Price vs. Depth
qplot(depth, price, data=diamonds)

##Adjustments - Price vs. Depth
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks=seq(55,67,2))

##Typical Depth Range
summary(diamonds$depth)

##Correlation - Depth vs. Price
cor.test(diamonds$depth, diamonds$price)

#Answer = -0.0106474 
# The correlation of depth vs. price is below .3 and very close to 0
# indicating no correlation therefore it would not be wise to use depth 
# as a variable to predict price of a diamond. 

###Scatterplot for Prices vs. Carat eliminating top 1% of price and carat values
ggplot(aes(x= carat, y=price), data=diamonds) +
  geom_point(alpha = 1/5) +
  xlim(0, quantile(diamonds$carat, prob = 0.99)) +
  ylim(0, quantile(diamonds$price, prob = 0.99))

###Scatterplot for Price vs. Volume = x*y*z
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
qplot(diamonds$volume, diamonds$price)

###Correlation of Price and Volume
diamonds_subsetted <- subset(diamonds, volume >0 & volume <=800)
cor.test(diamonds_subsetted$volume, diamonds_subsetted$price)
#answer= 0.9235455 

###Adjustments Price vs. Volume
ggplot(aes(x=volume, y=price), data=diamonds_subsetted) +
  geom_point(alpha=1/30) +
  geom_smooth(method='lm', color='red')

###Mean Price by Clarity
library(dplyr)
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarize(mean_price=as.numeric(mean(price)), 
            median_price=as.numeric(median(price)),
            min_price=as.numeric(min(price)),
            max_price=as.numeric(max(price)), 
            n=n())
head(diamondsByClarity)

###Bar Charts of Mean Price
data(diamonds)
library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

#Bar Plots
clarity <- barplot(diamonds_mp_by_clarity$mean_price, names.arg=diamonds_mp_by_clarity$clarity)
color <- barplot(diamonds_mp_by_color$mean_price, names.arg=diamonds_mp_by_color$color)

library(gridExtra)
grid.arrange(clarity, color, ncol = 1)