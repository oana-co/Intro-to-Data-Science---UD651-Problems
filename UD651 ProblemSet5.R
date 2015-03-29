##UD651 PS5
#Price Histograms with Facet and Color
qplot(x=price, data=diamonds, color=cut) + facet_wrap(~color) 

#Price vs. Table colored by Cut
ggplot(aes(x=table, y=price), data=diamonds) +
  geom_point(aes(color=cut)) +
  xlim(50,80)

#Price vs. Volume and Diamond Clarity
diamonds <- transform(diamonds, volume=x*y*z)
ggplot(aes(x=volume, y=price), data=diamonds)+
  geom_point(aes(color=clarity)) +
  scale_y_log10() +
  ylim(0, quantile(diamonds$price, prob=0.99)) + 
  xlim(0,350)

#Proportion of Friendships initiated
pf <- read.delim('pseudo_facebook.tsv', sep="\t")
pf <- transform(pf, prop_initiated = friendships_initiated/friend_count)

#prop_initiated vs Tenure
ggplot(aes(x=tenure, y=prop_initiated), data=pf) +
  geom_line(aes(color=year_joined.bucket), 
            stat = 'summary', fun.y=median)

#Smoothing Prop_initiated vs Tenure
ggplot(aes(x=(250*tenure)/250, y=prop_initiated), data=subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color=year_joined.bucket), 
            stat = 'summary', fun.y=median) + 
  geom_smooth()

#Largest Group Mean prop_initiated
aggregate(pf$prop_initiated~pf$year_joined.bucket, FUN=mean)

#Price/Carat Binned, Faceted, & Colored
ggplot(aes(x=cut, y=(price/carat)), data=diamonds)+
  geom_jitter(aes(color=color))+
  facet_wrap(~clarity)