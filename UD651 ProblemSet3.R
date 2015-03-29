qplot(x=price, data=diamonds)

#Cheaper Diamonds
qplot(x=price, data=diamonds, xlim=c(0,5000), binwidth = 50)

#price by cut histograms
qplot(x=price, data=diamonds) + 
  facet_wrap(~cut, scales="free_y")

by(diamonds$price, diamonds$cut, summary)

#Price Per Carat by Cut
qplot(x=price/carat, data=diamonds, binwidth = .01) + 
  facet_wrap(~cut) + 
  scale_x_log10()

#Price Box Plots
qplot(x=color, y=price, data=diamonds,ylim=c(400,1000), geom='boxplot')
by(diamonds$price, diamonds$color, summary)

#Price Per Carat Box Plots by Color
diamonds$price_by_carat <- diamonds$price/diamonds$carat
qplot(x=color, y=price_by_carat, data=diamonds, geom='boxplot')

#Carat Frequency Polygon
qplot(carat, data = diamonds, geom = "freqpoly", xlim=c(0,5), binwidth = 0.01)

###Aid Recieved Per Person (Data from GAPMINDER)
library('ggplot2')
library('reshape2')
setwd('eda/problem_sets/ps3/gapminder')

aid <- melt(t(read.csv('aid_per_person.csv', header=TRUE, row.names = 1)), na.rm = T)
names(aid) <- c("Year", "Country", "value")
# Clean up the year column
aid$Year <- sub("X(.*)\\.0$", "\\1", aid$Year)
# Remove everything before 1969
aid <- aid[aid$Year > 1969,]

# First plot
qplot(data = aid, x = Year, y = value, geom = "boxplot") +
  coord_flip() +
  ylab("Aid Given Per Person (2007 USD)")

ggsave(filename="~/by_year.png")

# Flip country as it shows up reversed for some reason
aid$Country <- factor(aid$Country, levels=rev(levels(aid$Country)))

# Create a decade column so we can facet_wrap on it
aid$Decade <- sub("..(.).", "'\\10s", aid$Year)
# Make Decade a factor so the plot is ordered correctly
aid$Decade <- factor(aid$Decade, unique(aid$Decade))

# Second plot
qplot(data = aid, x = Country, y = value, geom = "boxplot") +
  facet_wrap(~Decade) +
  coord_flip() +
  ylab("Aid Given Per Person (2007 USD)")

ggsave(filename="~/by_country.png")