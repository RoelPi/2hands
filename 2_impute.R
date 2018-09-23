dt <- fread('dt.csv')
x <- dt[,2:19]
y <- dt[,1]
rm(dt)

# Reduce cardinality of brands
## unique(x$car_brand)
z <- x[,.(count = .N),by=.(car_brand)]
z <- transform(z,car_brand=reorder(car_brand,-count))
## ggplot(z,aes(x=car_brand,y=count)) + geom_bar(stat='identity')
z <- setorder(z,-count)
x[car_brand %in% z[count <= z[car_brand =='overige merken']$count]$car_brand]$car_brand <- 'overig' # All rows of a brand with a total count lower than 'Overige merken' = 'Overig'
rm(z)

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
x <- distFill(x,'car_transmission')

# Reduce cardinality of models
## unique(x$car_model)
z <- x[,.(count = .N),by=.(car_model)]
z <- transform(z,car_model=reorder(car_model,-count))
## ggplot(z,aes(x=car_model,y=count)) + geom_bar(stat='identity')
z <- setorder(z,-count)
x[!(car_model %in% head(z,100)$car_model)]$car_model <- 'overig' # Arbitrarily select top-100
rm(z)

# Dummify categorical variables
x <- data.table(dummy.data.frame(x, c('seller_location','car_fuel','car_transmission','car_body','car_condition','car_brand','car_model'), sep='_'))
x1 <- x[,lapply(.SD,as.logical),.SDcols = (colnames(x) %like% 'seller_location' | 
                                colnames(x) %like% 'car_fuel' | 
                                colnames(x) %like% 'car_transmission' | 
                                colnames(x) %like% 'car_body' | 
                                colnames(x) %like% 'car_condition' | 
                                colnames(x) %like% 'car_brand' | 
                                colnames(x) %like% 'car_model')]
x2 <- x[,.SD,.SDcols = !(colnames(x) %like% 'seller_location' | 
                                        colnames(x) %like% 'car_fuel' | 
                                        colnames(x) %like% 'car_transmission' | 
                                        colnames(x) %like% 'car_body' | 
                                        colnames(x) %like% 'car_condition' | 
                                        colnames(x) %like% 'car_brand' | 
                                        colnames(x) %like% 'car_model')]
x <- cbind(x2,x1)
rm(x1, x2)

# Impute missing values (quick & dirty)
# x <- missForest(x)