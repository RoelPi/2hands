rm(list=ls())
source('0_resources.R')

dt <- fread('dt.csv')
dt <- dt[car_brand != 'overige_merken'] # FILTER

# Statistical imputation of transmission
distFill <- function(dt,cnames) {
  c <- cnames
  for (i in 1:length(c)) {
    dt_no_col <- dt[is.na(dt[[c[i]]])]
    dt_col <- dt[!is.na(dt[[c[i]]])]
    dist <- dt_col[,.(count = .N),by=get(c[i])]
    dist$p <- dist$count / sum(dist$count)
    dt_no_col[[c[i]]] <- sample(x = dist$get, nrow(dt_no_col), replace=T, prob=dist$p)
    dt <- rbind(dt_col,dt_no_col)
    dt <- dt[sample(nrow(dt)),]
  }
  dt
}
dt <- distFill(dt,'car_transmission')

# Reduce cardinality of brands
## unique(x$car_brand)
z <- dt[,.(count = .N),by=.(car_brand)]
z <- transform(z,car_brand=reorder(car_brand,-count))
## ggplot(z,aes(x=car_brand,y=count)) + geom_bar(stat='identity')
z <- setorder(z,-count)
dt[car_brand %in% z[count <= z[car_brand =='overige merken']$count]$car_brand]$car_brand <- 'overig' # All rows of a brand with a total count lower than 'Overige merken' = 'Overig'
rm(z)

# Change character columns to factor columns
x1 <- dt[,lapply(.SD,as.factor),.SDcols = c('seller_location','car_brand','car_model','car_fuel','car_doors','car_transmission','car_body','car_condition','car_norm')]
x2 <- dt[,.SD,.SDcols = !(colnames(dt) %in% c('seller_location','car_brand','car_model','car_fuel','car_doors','car_transmission','car_body','car_condition','car_norm'))]
dt <- cbind(x2,x1)
rm(x1, x2)

# Imputation per brand
brands <- as.character(unique(dt$car_brand))
x_brand <- list()
for (i in 1:length(brands)) {
  x_brand[i] <- list(dt[car_brand == brands[i]])
  names(x_brand)[i] <- brands[i]
}

x_brand_imputed <- list()
x_brand_OOB <- list()
z_brand <- list()

set.seed(19880303)
registerDoParallel(cores=3)

for (i in 1:length(x_brand)) {
  print(paste0('Imputing ',names(x_brand)[[i]]))
  x_brand[[i]]$car_model <- droplevels(x_brand[[i]]$car_model) # Relevel car models because missForest is a bitch on > 53 categories
  x_brand[[i]]$car_brand <- droplevels(x_brand[[i]]$car_brand) # Relevel car brands because missForest is a bitch on > 53 categories
  z_brand[[i]] <- x_brand[[i]][,.(count = .N),by=.(car_model)] # Select most popular car models and store in z
  z <- setorder(z_brand[[i]],-count) # reorder z descending
  if (length(unique(x_brand[[i]]$car_model)) > 49) {
    x_brand[[i]][!(car_model %in% head(z,49)$car_model)]$car_model <- 'other' # Select top 50 of car models when necessary
  }
  
  x_brand_imputed[[i]] <- missForest(x_brand[[i]][,2:19], verbose=T, parallelize='forests', maxiter=5, ntree=125, variablewise=T)
  x_brand_OOB[[i]] <- x_brand_imputed[[i]]$OOBerror
  names(x_brand_imputed)[i] <- names(x_brand)[[i]]
  names(x_brand_OOB)[i] <- names(x_brand)[i]
  x_brand_imputed[[i]] <- cbind(x_brand[[i]][,1],x_brand_imputed[[i]]$ximp)
}

dt <- rbindlist(x_brand_imputed,fill=T)

rm(x_brand,x_brand_imputed,x_brand_OOB,z,z_brand, brands, i, distFill)

write.csv(dt,'dtimp.csv',row.names=F)

# Reduce cardinality of car models
### unique(x$car_model)
# z <- x[,.(count = .N),by=.(car_model)]
# z <- transform(z,car_model=reorder(car_model,-count))
### ggplot(z,aes(x=car_model,y=count)) + geom_bar(stat='identity')
# z <- setorder(z,-count)
# x[!(car_model %in% head(z,100)$car_model)]$car_model <- 'other' # Arbitrarily select top-100
#rm(z)

# Dummify categorical variables
# x <- data.table(dummy.data.frame(x, c('seller_location','car_fuel','car_transmission','car_body','car_condition','car_brand','car_model'), sep='_'))
# x1 <- x[,lapply(.SD,as.logical),.SDcols = (colnames(x) %like% 'seller_location' | 
#                                colnames(x) %like% 'car_fuel' | 
#                                colnames(x) %like% 'car_transmission' | 
#                                colnames(x) %like% 'car_body' | 
#                                colnames(x) %like% 'car_condition' | 
#                                colnames(x) %like% 'car_brand' | 
#                                colnames(x) %like% 'car_model')]
# x2 <- x[,.SD,.SDcols = !(colnames(x) %like% 'seller_location' | 
#                                        colnames(x) %like% 'car_fuel' | 
#                                        colnames(x) %like% 'car_transmission' | 
#                                        colnames(x) %like% 'car_body' | 
#                                        colnames(x) %like% 'car_condition' | 
#                                        colnames(x) %like% 'car_brand' | 
#                                        colnames(x) %like% 'car_model')]
#x <- cbind(x2,x1)
#rm(x1, x2)

