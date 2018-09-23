library(data.table)
library(ggplot2)
library(lubridate)
library(magrittr)
library(stringr)
library(missForest)
library(dummies)

rm(list=ls())
offers <- fread('offers.csv', encoding='UTF-8')
# bids <- fread('bids.csv', encoding='UTF-8')
cars <- fread('cars.csv', encoding='UTF-8')

#####################################################################
# Clean offers ######################################################
#####################################################################

# ID integers to character
offers[,offer_id := as.character(offer_id)]
offers[,offer_seller_id := as.character(offer_seller_id)]

# Fix scrape date
offers[,scrape_date := gsub('..\\:..\\:..','',scrape_date)]
offers[,scrape_date := substr(scrape_date,5,length(scrape_date))]
offers$scrape_date %<>%
  gsub('Jan','01', .) %>%
  gsub('Feb','02', .) %>%
  gsub('Mar','03', .) %>%
  gsub('Apr','04', .) %>%
  gsub('May','05', .) %>%
  gsub('Jun','06', .) %>%
  gsub('Jul','07', .) %>%
  gsub('Aug','08', .) %>%
  gsub('Sep','09', .) %>%
  gsub('Oct','10', .) %>%
  gsub('Nov','11', .) %>%
  gsub('Dec','12', .)
offers[,scrape_date := gsub(' ','',scrape_date)]
offers <- offers[,scrape_date := mdy(scrape_date)]

# Clean price as euro sign is not recognized (UTF-8)
offers[,offer_price := as.double(gsub('\\,','.',gsub('\200 |\\.', '', offer_price)))]

# Characters to dates
offers$offer_date %<>%
  gsub(' januari ','-01-', .) %>%
  gsub(' februari ','-02-', .) %>%
  gsub(' maart ','-03-', .) %>%
  gsub(' april ','-04-', .) %>%
  gsub(' mei ','-05-', .) %>%
  gsub(' juni ','-06-', .) %>%
  gsub(' juli ','-07-', .) %>%
  gsub(' augustus ','-08-', .) %>%
  gsub(' september ','-09-', .) %>%
  gsub(' oktober ','-10-', .) %>%
  gsub(' november ','-11-', .) %>%
  gsub(' december ','-12-', .)
offers <- offers[,offer_date := dmy(offer_date)]

offers$offer_seller_started_on %<>%
  gsub(' januari ','-01-', .) %>%
  gsub(' februari ','-02-', .) %>%
  gsub(' maart ','-03-', .) %>%
  gsub(' april ','-04-', .) %>%
  gsub(' mei ','-05-', .) %>%
  gsub(' juni ','-06-', .) %>%
  gsub(' juli ','-07-', .) %>%
  gsub(' augustus ','-08-', .) %>%
  gsub(' september ','-09-', .) %>%
  gsub(' oktober ','-10-', .) %>%
  gsub(' november ','-11-', .) %>%
  gsub(' december ','-12-', .)
offers <- offers[,offer_seller_started_on := dmy(offer_seller_started_on)]

# Get postal code from location & turn in province
offers[,offer_seller_location := substr(offer_seller_location,regexpr('[0-9][0-9][0-9][0-9]', offer_seller_location),(regexpr('[0-9][0-9][0-9][0-9]', offer_seller_location)+3))]
zip <- fread('zip.csv')
zip[,Postcode := as.character(Postcode)]
zip <- zip[,.(Provincie = head(Provincie,1)), by=.(Postcode)]
offers <- merge(offers,zip,by.x='offer_seller_location',by.y='Postcode')
offers[,offer_seller_location := Provincie]
offers[,Provincie := NULL]
offers[,offer_seller_location := gsub('Brussel \\(19 gemeenten\\)','Brussel',offer_seller_location)]
offers[,offer_seller_location := tolower(offer_seller_location)]
rm(zip)

# Fix brand
offers[,offer_brand := tolower(offer_brand)]
offers[,offer_brand := gsub(' ','_',offer_brand)]

#####################################################################
# Clean cars ########################################################
#####################################################################

cars <- unique(cars,by=c('offer_id','dimension'))
cars <- dcast(cars,offer_id ~ dimension, value.var='value')
colnames(cars) <- c('offer_id','doors','year','fuel','co2','body','cylinder','condition','norm','guarantee','interior','kilometers','color','model','options','transmission','power','website')

# Fix ID
cars[,offer_id := as.character(offer_id)]

# Fix doors
cars <- cars[grepl('deur',doors) | is.na(doors)] # FILTER
cars[,doors := as.integer(substr(doors,0,1))]

# Fix build year
cars <- cars[,year := as.integer(substr(year,0,4))]

# Fix fuel
cars[fuel == 'Diesel,']$fuel <- 'Diesel'
cars[fuel == 'Benzine,']$fuel <- 'Diesel'
cars[fuel == 'Elektrisch,']$fuel <- 'Elektrisch'
cars[fuel == 'CNG,']$fuel <- 'Elektrisch'
cars[fuel == 'Super']$fuel <- 'Benzine'
cars[,fuel := tolower(fuel)]

# Fix co2
cars[,co2_e := co2]
cars[grepl(' cm',co2)]$co2 <- NA
cars[grepl(' gr',co2_e)]$co2_e <- NA
cars[,co2 := as.integer(gsub(' gr/km','',co2))]
cars[grepl('elektrisch',fuel)]$co2 <- 0

# Fix model
cars[,model := tolower(model)]
cars[,model := gsub(' ','_',model)]

# Fix body
cars[body == 'SUV of Terreinwagen']$body <- 'SUV'
cars[body == 'Berline 3/5-deurs']$body <- 'Berline 3_5'
cars[body == 'Berline 2/4-deurs']$body <- 'Berline 2_4'
cars[body == 'Schade-auto']$body <- 'Schade'
cars[,body := tolower(body)]

# Fix cylinders
cars[,cylinder := gsub(' cm³','',cylinder)]
cars[,cylinder := gsub('\\.','',cylinder)]
cars[grepl(cylinder,'km')]$cylinder <- NA
cars[,cylinder := as.integer(cylinder)]

# Fix condition
cars[condition == 'Gebruikt']$condition <- 'used'
cars[condition == 'Nieuw']$condition <- 'new'

# Fix Euro norm
cars[!grepl('Euro [0-8]',norm)]$norm <- NA
cars[,norm := as.integer(gsub('Euro ','',norm))]

# Fix kilometers
km_range <- data.table(str_split_fixed(cars$kilometers, ' - ', 2))
colnames(km_range) <- c('start','end')
km_range[,start := as.integer(gsub(' km|\\.', '', start))]
km_range[end == '']$end <- km_range[end == '']$start
km_range[,end := as.integer(gsub('\\.','',end))]
km_range[,kilometers := (start + end) / 2]
cars[,kilometers := km_range$kilometers]
rm(km_range)

# Fix transmission
cars[,transmission := gsub('Manueel','manual',transmission)]
cars[,transmission := gsub('Automatisch','automatic',transmission)]

# Fix power
cars <- cars[grepl(' PK',power) | is.na(power)] # FILTER
cars[,power := as.integer(gsub(' PK','',power))]

# Remove some columns
cars[,guarantee := NULL]
cars[,interior := NULL]
cars[,color := NULL]
cars[,options := NULL]
cars[,website := NULL]
cars[,co2_e := NULL]

# merge data
offers <- merge(x=offers, y=cars, by='offer_id',all.x=F, all.y=F) # FILTER
rm(cars)
new_names <- paste0('car_',c('doors','year','fuel','co2','body','cylinder','condition','norm','kilometers','model','transmission','power'))
setnames(offers, old=c('doors','year','fuel','co2','body','cylinder','condition','norm','kilometers','model','transmission','power'), new=new_names)
rm(new_names)

# Create 'days online' feature
offers[,offer_days_online := as.integer(scrape_date - offer_date)]

# Create 'seller days active' feature
offers[,offer_seller_days_active := as.integer(scrape_date - offer_seller_started_on)]

# Remove columns that are not features
dt <- copy(offers)
rm(offers)
dt[,offer_id := NULL]
dt[,offer_title := NULL]
dt[,scrape_date := NULL]
dt[,offer_url := NULL]
dt[,offer_date := NULL]
dt[,offer_seller := NULL]
dt[,offer_seller_id := NULL]
dt[,offer_seller_started_on:= NULL]

setnames(dt,old = c('offer_seller_ratings','offer_seller_location','offer_seller_days_active', 'offer_brand'),new=c('seller_ratings','seller_location','seller_days_active','car_brand'))
setcolorder(dt,c('offer_price','offer_views','offer_days_online','seller_ratings','seller_location','seller_days_active','car_brand','car_model','car_year','car_fuel','car_kilometers','car_doors','car_transmission','car_body','car_co2','car_norm','car_cylinder','car_power','car_condition'))
write.csv(dt,'dt.csv',row.names=F)
