rm(list=ls())
source('0_resources.R')

dt <- fread('dtimp.csv')
options(scipen=999)

# final cleaning, discovered by exploratory analysis
dt <- dt[car_kilometers < 1000000] # FILTER: Cars under 1 mil km
dt[,car_power := round(car_power,0)] # Round car_power becayse of imputation
dt[,car_cylinder := round(car_cylinder,0)] # dito
dt[,car_co2 := round(car_co2,0)] # dito
dt <- dt[car_year <= 2018] # FILTER: No ordered cars
dt <- dt[,car_age := 2018 - car_year] # new feature
dt <- dt[complete.cases(dt)] # FILTER

# What are the most popular car brands?
z <- dt[,.(count = .N),by=.(car_brand)]
z <- transform(z,car_brand=reorder(car_brand,-count))
z <- setorder(z,-count)
g1 <- ggplot(z,aes(x=car_brand, y=count)) + 
  geom_bar(stat='identity', fill=pal[5], col='#000000') +
  t +
  xlab('car brand') +
  ggtitle('Popularity of brands, by count of brands')
ggsave('1.png',g1,width=48.8,height=27.4, units='cm')
g1
rm(z, g1)

# What are the most expensive car brands on average
z <- dt[,.(price = median(offer_price),count = .N),by=.(car_brand)]
z <- z[count > 10]
z <- transform(z,car_brand=reorder(car_brand,-price))
z <- setorder(z,-price)
g2 <- ggplot(z,aes(x=car_brand, y=price)) + 
  geom_bar(stat='identity', fill=pal[5], col='#000000') +
  t +
  xlab('car brand') + ylab('median price') +
  ggtitle('Median price of an offer, by brand (n > 5)')
ggsave('2.png',g2,width=48.8,height=27.4, units='cm')
g2
rm(z, g2)

# What is the relationship between kilometers & price?
z <- dt[car_condition == 'used']
g3 <- ggplot(z,aes(x=car_kilometers,y=offer_price)) + 
  geom_point(col=pal[3],alpha=0.2) +
  ylim(0,50000) + xlim(0,400000) +
  xlab('kilometers traversed') + ylab('price') +
  ggtitle('Relationship between traversed kilometers & price') +
  t
ggsave('3.png',g3,width=48.8,height=27.4, units='cm')
g3
rm(z)

# What is the relationship between kilometers & price? (level-log)
z <- dt[car_condition == 'used']
g4 <- ggplot(z,aes(x=log(car_kilometers),y=offer_price)) + 
  geom_point(col=pal[3],alpha=0.2) +
  geom_smooth(method = 'lm', col=pal[2], se=F) +
  ylim(0,50000) + xlim(8,13) +
  xlab('kilometers traversed (log)') + ylab('price') +
  ggtitle('Relationship between traversed kilometers & price (2)') +
  t
ggsave('4.png',g4,width=48.8,height=27.4, units='cm')
g4
rm(z)

# What is the relationship between age & price?
z <- dt[car_condition == 'used']
g5 <- ggplot(z,aes(x=car_age,y=offer_price)) + 
  geom_jitter(col=pal[3],alpha=0.2) +
  ylim(0,50000) +
  xlab('car age') + ylab('price') +
  ggtitle('Relationship between age of car & price') +
  t
ggsave('5.png',g5,width=48.8,height=27.4, units='cm')
g5
rm(z)

# Is the relationship between age and kilometers strong? (alluding non-independence of variables)
z <- dt[car_condition == 'used'] # Filter for more linearity
g6 <- ggplot(z,aes(x=car_age,y=car_kilometers)) + 
  geom_jitter(col=pal[3],alpha=0.2) +
  ylim(0,300000) +
  xlab('car age') + ylab('kilometers traversed') +
  ggtitle('Relationship between car age & kilometers traversed') +
  t
ggsave('6.png',g6,width=48.8,height=27.4, units='cm')


grid.arrange(g3, g5, g6, nrow = 1)

rm(g3, g4, g5, g6, z)



lm1 <- with(z,lm(car_kilometers ~ car_age))
summary(lm1)

rm(z, lm1, g6)

# Normal relation if car is newer than 2005?
z <- dt[car_condition == 'used' & car_year > 2005] # Filter for more linearity
lm1 <- with(z,lm(car_kilometers ~ car_age))
g7 <- ggplot(data=z, aes(x=lm1$residuals)) + 
  geom_histogram(bins=100, fill=pal[2],col='#000000') +
  xlab('residuals') + ylab('count') +
  ggtitle('Distribution of residuals of used cars produced after 2005 - kilometers traversed regressed on car age') +
  t
ggsave('7.png',g7,width=48.8,height=27.4, units='cm')
g7
rm(z, g7)

# What is the relationship between seller ratings & price?
z <- dt[,.(price = median(offer_price),count = .N),by=.(seller_ratings)]
g8 <- ggplot(z,aes(x=as.integer(seller_ratings),y=price)) + 
  geom_bar(fill=pal[3],col='#000000', stat='identity') +
  ylim(0,12000) + xlim(0,20) +
  xlab('seller # ratings') + ylab('median price') +
  ggtitle('Relationship between') +
  t
ggsave('8.png',g8,width=48.8,height=27.4, units='cm')
g8
rm(z, g8)

# What is the relationship between seller ratings & price?
z <- dt[,.(price = median(offer_price),count = .N),by=.(seller_location)]
z <- transform(z,seller_location=reorder(seller_location,-price))
z <- setorder(z,-count)
g9 <- ggplot(z,aes(x=seller_location,y=price)) + 
  geom_bar(fill=pal[3], col='#000000', stat='identity') +
  ylim(0,15000) +
  xlab('province') + ylab('median price') +
  t
ggsave('9.png',g9,width=48.8,height=27.4, units='cm')
g9
rm(z, g9)