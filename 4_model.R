rm(list=ls())
dt <- fread('dtimp.csv',stringsAsFactors=T)
options(scipen=999)
dt <- dt[complete.cases(dt)] # FILTER

source('0_resources.R')

# Hypertuning of model(s)
tune_rf_model <- function(x_train,y_train,filename) {
  if (!file.exists(filename)) {
    tuned_random_forest <- tuneRF(x_train, y_train, 
                                  mtryStart=3, 
                                  stepFactor=1,
                                  nTreeTry=500,
                                  improve=0.005,
                                  plot=T,
                                  doBest=T,
                                  trace=T)
    saveRDS(tuned_random_forest,file=filename)
  } else {
    tuned_random_forest <- readRDS(filename)
  }
  tuned_random_forest
}

# Reduce cardinality for models
dt <- dt[,car_model := as.character(car_model)]
dt <- dt[,car_brand := as.character(car_brand)]

z <- dt[,.(count = .N),by=.(car_model)] # Select most popular car models and store in z
z <- setorder(z,-count) # reorder z descending
dt[!(car_model %in% head(z,49)$car_model)]$car_model <- 'other' # Select top 50 of car brands
rm(z)

# Reduce cardinality for brands
z <- dt[,.(count = .N),by=.(car_brand)] # Select most popular car models and store in z
z <- setorder(z,-count) # reorder z descending
dt[!(car_brand %in% head(z,49)$car_brand)]$car_brand <- 'other' # Select top 50 of car brands
rm(z)

dt <- dt[,car_model := as.factor(car_model)]
dt <- dt[,car_brand := as.factor(car_brand)]

y <- dt[['offer_price']]
x <- dt[,2:19]

# train test split
smp_size <- floor(0.80 * nrow(dt))
set.seed(19880303)
train_ind <- sample(seq_len(nrow(dt)), size = smp_size)
x_train <- x[train_ind, ]
x_test <- x[-train_ind, ]

y_train <- y[train_ind]
y_test <- y[-train_ind]
model <- tune_rf_model(x_train,y_train,'model.RDS')

y_pred <- predict(model,x_test)
test <- data.table(x_test,y_test,y_pred)

test[,diff := y_pred - y_test]
test <- test[order(-diff)]  

g10 <- ggplot(test,aes(x=y_pred,y=y_test)) + 
  geom_point(col=pal[3]) +
  geom_smooth(method='lm', col=pal[2], se=F) +
  xlim(0,30000) + 
  ylim(0,30000) +
  xlab('predicted ask price') + ylab('real ask price') +
  t
ggsave('10.png',g10,width=48.8,height=27.4, units='cm')
g10
rm(g10)

g11 <- ggplot(test,aes(x=y_pred,y=y_test)) + 
  geom_point(col=pal[3]) +
  geom_smooth(method='lm', col=pal[2], se=F) +
  geom_vline(xintercept=4000, col=pal[1], linetype='dotted', size=2) + 
  geom_vline(xintercept=8000, col=pal[1], linetype='dotted', size=2) +
  xlim(0,30000) + 
  ylim(0,30000) +
  xlab('predicted ask price') + ylab('real ask price') +
  t
ggsave('11.png',g11,width=48.8,height=27.4, units='cm')
g11
rm(g11)

g12 <- ggplot(test[y_test >= 4000 & y_test <= 8000],aes(x=y_pred,y=y_test)) + 
  geom_point(col=pal[3]) +
  geom_smooth(method='lm', col=pal[2], se=F) +
  geom_vline(xintercept=4000, col=pal[1], linetype='dotted', size=2) + 
  geom_vline(xintercept=8000, col=pal[1], linetype='dotted', size=2) +
  xlim(4000,8000) + 
  ylim(0,12000) +
  xlab('predicted ask price') + ylab('real ask price') +
  t
ggsave('12.png',g12,width=48.8,height=27.4, units='cm')
g12
rm(g12)

voor_maaike <- test[y_test >= 4000 & y_test <= 8000]
