rm(list = ls())
library(tidyverse)

#load datafiles
data2015 = read.csv(file='data/car_prices.csv')
data2021 = read.csv(file='data/vehicles.csv')
#check columns
sapply(data2015, class)
sapply(data2021, class)
#columns to keep:
#2015: year, manufacturer<-make, model, trim, body, transmission, state, condition, odometer, color, interior, price=sellingprice, seller?, mmr?, saledate?
#2021: year, manufacturer, model (includes trim info), body=type, transmission, state, condition, odometer, color=paint_color, [no interior], price

data_shortened_2015= data2015 %>% select(year, manufacturer=make, model, trim, body, transmission, state, condition, odometer, color, interior, price=sellingprice)
data_shortened_2021 = data2021 %>% select(year, manufacturer, model, body=type, transmission, state, condition, odometer, color=paint_color, price)

#select manufacturers and save to csv fiels for mapping
#manufacturers_2015=data_shortened_2015 %>% select(manufacturer) %>% unique() %>% arrange(manufacturer)
#manufacturers_2021=data_shortened_2021 %>% select(manufacturer) %>% unique() %>% arrange(manufacturer)
#write.csv(manufacturers_2015,'mappings/manufacturers_2015.csv')
#write.csv(manufacturers_2021,'mappings/manufacturers_2021.csv')

#load manufacturer mapping
manufacturer_mapping_2015=read.csv('mappings/manufacturers_2015.csv')
manufacturer_mapping_2021=read.csv('mappings/manufacturers_2021.csv')

#change old to new manufacturer name for both files
data_shortened_2015= left_join(data_shortened_2015,manufacturer_mapping_2015 %>% rename(manufacturer=`manufacturer.2015`),by="manufacturer") %>% mutate(manufacturer=Std.name) %>% select(!X)%>% select(!Std.name)
data_shortened_2021= left_join(data_shortened_2021,manufacturer_mapping_2021,by="manufacturer") %>% mutate(manufacturer=Std.name) %>% select(!X)%>% select(!Std.name)

#filter both files to only incldue top X manufacturers (smaller ones likely will not have sufficinet data for significant analyses)
manufacturer_list_final = union(data_shortened_2015 %>% select(manufacturer,price), data_shortened_2021 %>% select(manufacturer,price))%>% group_by(manufacturer) %>% summarize(cnt=n()) %>% arrange(desc(cnt)) %>% top_n(28) %>% filter(!is.na(manufacturer), manufacturer!='', manufacturer!='other', manufacturer!='infiniti') %>% select(manufacturer)
manufacturer_list_final=manufacturer_list_final[['manufacturer']]

data_shortened_2015=data_shortened_2015 %>% filter(manufacturer %in% manufacturer_list_final)
data_shortened_2021=data_shortened_2021 %>% filter(manufacturer %in% manufacturer_list_final)

#save both shortened files to new file
write.csv(data_shortened_2015,'data/data_shortened_2015.csv')
write.csv(data_shortened_2021,'data/data_shortened_2021.csv')

#-----------------------
#main data cleaning section

rm(list = ls())
library(tidyverse)

#read new files
data_shortened_2015=read.csv('data/data_shortened_2015.csv')
data_shortened_2021=read.csv('data/data_shortened_2021.csv')

data_shortened_2015=data_shortened_2015 %>% mutate(model=str_trim(tolower(str_replace(str_replace(str_replace(model,'/',''),' ',''),'-','')),side='both'))
data_shortened_2021=data_shortened_2021 %>% mutate(model=str_trim(tolower(str_replace(str_replace(str_replace(model,'/',''),' ',''),'-','')),side='both'))

data_grouped_2015 = data_shortened_2015 %>% group_by(manufacturer,model) %>% summarize(cnt=n()) %>% arrange(desc(cnt))
data_grouped_2021 = data_shortened_2021 %>% group_by(manufacturer,model) %>% summarize(cnt=n()) %>% arrange(desc(cnt))

#outer join by manufacturer, model, sort by model/manufacturer, then evaluate no-matches first to find patterns, then to manually extend matching)
#testsummary=full_join(data_grouped_2021,data_grouped_2015, by=c('manufacturer', 'model'))
#testsummary = testsummary %>% mutate(totalcnt=ifelse(is.na(`cnt.x`), `cnt.y`, ifelse(is.na(`cnt.y`), `cnt.x`, `cnt.x`+`cnt.y`))) %>% filter(totalcnt>=100) %>% arrange(model) %>% arrange(desc(totalcnt)) %>% arrange(manufacturer)
#write.csv(testsummary,'mappings/testsummary.csv')
model_naming=read.csv('mappings/model_naming_standardization.csv')
model_naming=model_naming %>% mutate(modelnew=ifelse(modelnew!='',modelnew,model)) %>% filter(modelnew!='')


data_cleaned_2015=left_join(data_shortened_2015, model_naming, by=c('manufacturer','model')) %>% filter(!is.na(modelnew))
data_cleaned_2021=left_join(data_shortened_2021, model_naming, by=c('manufacturer','model')) %>% filter(!is.na(modelnew))

#no transmission adjustments needed, filter for available data
data_cleaned_2015 %>% group_by(transmission) %>% summarize(cnt=n())
data_cleaned_2021 %>% group_by(transmission) %>% summarize(cnt=n())

data_cleaned_2015=data_cleaned_2015 %>% filter(transmission=='automatic' | transmission=='manual')
data_cleaned_2021=data_cleaned_2021 %>% filter(transmission=='automatic' | transmission=='manual')


#for color: 2015 data adjustments only:
#off-white = white; gray = grey
data_cleaned_2015=data_cleaned_2015 %>% mutate(color=ifelse(color=='off-white','white',color)) %>% mutate(color=ifelse(color=='gray','grey',color))
#no filtering of colors, cars with no/other colors can still be analyzed, as color may not necessarily be a deciding criterion

data_cleaned_2015 %>% group_by(color) %>% summarize(cnt=n()) %>% arrange(desc(cnt))
data_cleaned_2021 %>% group_by(color) %>% summarize(cnt=n()) %>% arrange(desc(cnt))


#state (already mostly clean, only excluding puerto rico(2015 data missing several states))
data_cleaned_2015=data_cleaned_2015 %>% filter(state!='pr')


#condition (cleaned for both datasets to reperesent bcukets 1 to 4 depending on condition)
data_cleaned_2015=data_cleaned_2015 %>% mutate(condition=as.numeric(condition))
data_cleaned_2015=data_cleaned_2015 %>% mutate(condition_bucket=case_when(
                                                      condition <=2 ~ 1,
                                                      condition >2 & condition <= 3 ~ 2,
                                                      condition >3 & condition <= 4 ~ 3,
                                                      condition >4 & condition <= 5 ~ 4,
))

data_cleaned_2021=data_cleaned_2021 %>% mutate(condition_bucket=case_when(
  condition=='salvage' | condition=='fair'  ~ 1,
  condition=='good' ~ 2,
  condition=='like new' | condition=='excellent' ~ 3,
  condition=='new' ~ 4,
))

#group odometer
#bucketing approach:
#1k - 10k
#10k - 30k
#30k - 60k
#60k - 100k
#100k - 150
#150+
data_cleaned_2021 %>% filter(odometer > 1000 & odometer < 400000) %>% ggplot(aes(x=odometer)) + geom_histogram()
data_cleaned_2015 %>% filter(odometer > 1000 & odometer < 400000) %>% ggplot(aes(x=odometer)) + geom_histogram()

data_cleaned_2015=data_cleaned_2015 %>%filter(odometer > 1000 & odometer < 400000) %>% mutate(odo_bucket=case_when(
  odometer <=10000 ~ 1,
  odometer >10000 & odometer <= 30000 ~ 2,
  odometer >30000 & odometer <= 60000 ~ 3,
  odometer >60000 & odometer <= 100000 ~ 4,
  odometer >100000 & odometer <= 150000 ~ 5,
  odometer >150000 ~ 6
))

data_cleaned_2021=data_cleaned_2021 %>%filter(odometer > 1000 & odometer < 400000) %>% mutate(odo_bucket=case_when(
  odometer <=10000 ~ 1,
  odometer >10000 & odometer <= 30000 ~ 2,
  odometer >30000 & odometer <= 60000 ~ 3,
  odometer >60000 & odometer <= 100000 ~ 4,
  odometer >100000 & odometer <= 150000 ~ 5,
  odometer >150000 ~ 6
))


#create and group age
data_cleaned_2015=data_cleaned_2015 %>% mutate(age=2015-year+1)
data_cleaned_2021=data_cleaned_2021 %>% mutate(age=2021-year+1)

#groups:
#1-2
#3-5
#6-8
#9-12
#13-20
#21+

data_cleaned_2015 %>% filter(age<30) %>% ggplot(aes(x=age)) + geom_histogram()
data_cleaned_2021 %>% filter(age<30) %>% ggplot(aes(x=age)) + geom_histogram()

data_cleaned_2015=data_cleaned_2015 %>%filter(age<30) %>% mutate(age_bucket=case_when(
  age <=2 ~ 1,
  age >2 & age <= 5 ~ 2,
  age >5 & age <= 8 ~ 3,
  age >8 & age <= 12 ~ 4,
  age >12 & age <= 20 ~ 5,
  age >20 ~ 6
))

data_cleaned_2021=data_cleaned_2021 %>%filter(age<30) %>% mutate(age_bucket=case_when(
  age <=2 ~ 1,
  age >2 & age <= 5 ~ 2,
  age >5 & age <= 8 ~ 3,
  age >8 & age <= 12 ~ 4,
  age >12 & age <= 20 ~ 5,
  age >20 ~ 6
))

#body (excluded for now, incomplete data, and should not be relevant since type will cover it sufficiently)
#data_cleaned_2015 %>% group_by(body) %>% summarize(cnt=n()) %>% arrange(desc(cnt))
#data_cleaned_2021 %>% group_by(body) %>% summarize(cnt=n()) %>% arrange(desc(cnt))

#remove outliers
data_cleaned_2015 = data_cleaned_2015 %>% filter(price>500 & price<100000)
data_cleaned_2021 = data_cleaned_2021 %>% filter(price>500 & price<100000)

#save cleaned datasets
write.csv(data_cleaned_2015,'data/data_cleaned_2015.csv')
write.csv(data_cleaned_2021,'data/data_cleaned_2021.csv')

#read cleaned datasets
data_cleaned_2015=read.csv('data/data_cleaned_2015.csv')
data_cleaned_2021=read.csv('data/data_cleaned_2021.csv')

#group data by all relevant metrics, add mean, sd, n, filter low n's (grouping by input vector for later selectability in Shiny)

group_data=function(grouping){
  grouped_prices_2015=data_cleaned_2015 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
  grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
  
  merged_detail=inner_join(grouped_prices_2021,grouped_prices_2015,by=grouping,suffix = c(".2021", ".2015"))
  merged_detail=merged_detail %>% mutate(newcol=avg_price.2021-avg_price.2015)
  return (merged_detail)
}


#sum data by defined grouping metric
summing_data=function(merged_detail,summing){
  return(merged_detail %>% group_by_at(summing) %>%  summarize(weighted_total=weighted.mean(newcol, cnt.2021+cnt.2015), count2021=sum(cnt.2021),count2015=sum(cnt.2015),avg2021=weighted.mean(avg_price.2021,cnt.2021),avg2015=weighted.mean(avg_price.2015,cnt.2015),sd2021=sum(sd_price.2021^2,na.rm=TRUE)^0.5,sd2015=sum(sd_price.2015^2,na.rm=TRUE)^0.5) %>% arrange(desc(weighted_total)))
}

#-------------------
#test for total counts depending on level of matching detail
grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
summing=c('manufacturer')

i=1
while(i<9){
  merged_detail=group_data(grouping[1:i])
  print(sum(merged_detail %>% group_by(manufacturer) %>%  summarize(cnt2015=sum(cnt.2015),cnt2021=sum(cnt.2021)) %>% arrange(desc(cnt2021)) %>% select(cnt2021)))
  i=i+1
}

#-------------------
#test for total results depending on matching and output detail

grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
summing=c('manufacturer')


merged_detail=group_data(grouping)
summing_data(merged_detail,summing)

view(summing_data(merged_detail,summing))

#-------------------
#2021 view

grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
summing=c('manufacturer', 'age_bucket')

grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 

tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg))

manufacturers=c('porsche','mercedes','cadillac')
#view(tmp %>% select(manufacturer) %>% unique())

tmp %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes(x=age_bucket, y=avg , color=manufacturer)) + geom_line()+geom_point()

tmp %>% select(!count) %>% select(!sd) %>% spread(age_bucket, avg) %>% mutate(percentagedrop=`6`/`1`) %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes(x=reorder(manufacturer, percentagedrop), y=percentagedrop)) + geom_bar(stat='identity') + coord_flip()


#generalized based on input
grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
inputsel='age_bucket'
summing=c('manufacturer', inputsel)
summing
grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 

tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg))

manufacturers=c('porsche','mercedes','cadillac')

inputsel='age_bucket'
tmp %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes_string(x=inputsel, y='avg' , color='manufacturer')) + geom_line()+geom_point()

tmp %>% select(!count) %>% select(!sd) %>% spread(inputsel, avg) %>% mutate(percentagedrop=`6`/`1`) %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes(x=reorder(manufacturer, percentagedrop), y=percentagedrop)) + geom_bar(stat='identity') + coord_flip()


#-------------
#garbage dump
