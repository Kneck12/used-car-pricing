rm(list = ls())
library(tidyverse)

#load datafiles
data2015 = read.csv(file='car_analysis/data/car_prices.csv')
data2021 = read.csv(file='car_analysis/data/vehicles.csv')
#check columns
sapply(data2015, class)
sapply(data2021, class)
view(data2021)
#columns to keep:
#2015: year, manufacturer<-make, model, trim, body, transmission, state, condition, odometer, color, interior, price=sellingprice, seller?, mmr?, saledate?
#2021: year, manufacturer, model (includes trim info), body=type, transmission, state, condition, odometer, color=paint_color, [no interior], price

data_shortened_2015= data2015 %>% select(year, manufacturer=make, model, trim, body, transmission, state, condition, odometer, color, interior, price=sellingprice)
data_shortened_2021 = data2021 %>% select(year, manufacturer, model, body=type, transmission, state, condition, odometer, color=paint_color, price)

#select manufacturers and save to csv fiels for mapping
#manufacturers_2015=data_shortened_2015 %>% select(manufacturer) %>% unique() %>% arrange(manufacturer)
#manufacturers_2021=data_shortened_2021 %>% select(manufacturer) %>% unique() %>% arrange(manufacturer)
#write.csv(manufacturers_2015,'car_analysis/manufacturers_2015.csv')
#write.csv(manufacturers_2021,'car_analysis/manufacturers_2021.csv')

#load manufacturer mapping
manufacturer_mapping_2015=read.csv('car_analysis/manufacturers_2015.csv')
manufacturer_mapping_2021=read.csv('car_analysis/manufacturers_2021.csv')

#change old to new manufacturer name for both files
data_shortened_2015= left_join(data_shortened_2015,manufacturer_mapping_2015 %>% rename(manufacturer=`manufacturer.2015`),by="manufacturer") %>% mutate(manufacturer=Std.name) %>% select(!X)%>% select(!Std.name)
data_shortened_2021= left_join(data_shortened_2021,manufacturer_mapping_2021,by="manufacturer") %>% mutate(manufacturer=Std.name) %>% select(!X)%>% select(!Std.name)

#filter both files to only incldue top X manufacturers (smaller ones likely will not have sufficinet data for significant analyses)
manufacturer_list_final = union(data_shortened_2015 %>% select(manufacturer,price), data_shortened_2021 %>% select(manufacturer,price))%>% group_by(manufacturer) %>% summarize(cnt=n()) %>% arrange(desc(cnt)) %>% top_n(28) %>% filter(!is.na(manufacturer), manufacturer!='', manufacturer!='other', manufacturer!='infiniti') %>% select(manufacturer)
manufacturer_list_final=manufacturer_list_final[['manufacturer']]

data_shortened_2015=data_shortened_2015 %>% filter(manufacturer %in% manufacturer_list_final)
data_shortened_2021=data_shortened_2021 %>% filter(manufacturer %in% manufacturer_list_final)

#save both shortened files to new file
write.csv(data_shortened_2015,'car_analysis/data_shortened_2015.csv')
write.csv(data_shortened_2021,'car_analysis/data_shortened_2021.csv')

rm(list = ls())
library(tidyverse)

#read new files
data_shortened_2015=read.csv('car_analysis/data_shortened_2015.csv')
data_shortened_2021=read.csv('car_analysis/data_shortened_2021.csv')

data_shortened_2015=data_shortened_2015 %>% mutate(model=str_trim(tolower(str_replace(str_replace(str_replace(model,'/',''),' ',''),'-','')),side='both'))
data_shortened_2021=data_shortened_2021 %>% mutate(model=str_trim(tolower(str_replace(str_replace(str_replace(model,'/',''),' ',''),'-','')),side='both'))

data_grouped_2015 = data_shortened_2015 %>% group_by(manufacturer,model) %>% summarize(cnt=n()) %>% arrange(desc(cnt))
data_grouped_2021 = data_shortened_2021 %>% group_by(manufacturer,model) %>% summarize(cnt=n()) %>% arrange(desc(cnt))

#outer join by manufacturer, model, sort by model/manufacturer, then evaluate no-matches first to find patterns, then to manually extend matching)
#testsummary=full_join(data_grouped_2021,data_grouped_2015, by=c('manufacturer', 'model'))
#testsummary = testsummary %>% mutate(totalcnt=ifelse(is.na(`cnt.x`), `cnt.y`, ifelse(is.na(`cnt.y`), `cnt.x`, `cnt.x`+`cnt.y`))) %>% filter(totalcnt>=100) %>% arrange(model) %>% arrange(desc(totalcnt)) %>% arrange(manufacturer)
#write.csv(testsummary,'car_analysis/testsummary.csv')
model_naming=read.csv('car_analysis/model_naming_standardization.csv')
model_naming=model_naming %>% mutate(modelnew=ifelse(modelnew!='',modelnew,model)) %>% filter(modelnew!='')


data_cleaned_2015=left_join(data_shortened_2015, model_naming, by=c('manufacturer','model')) %>% filter(!is.na(modelnew))
data_cleaned_2021=left_join(data_shortened_2021, model_naming, by=c('manufacturer','model')) %>% filter(!is.na(modelnew))


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
data_cleaned_2015=data_cleaned_2015 %>% mutate(age=2021-year)
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

#group data by all relevant metrics, add mean, sd, n, filter low n's, remove outliers
data_cleaned_2015 = data_cleaned_2015 %>% filter(price>500 & price<100000)
data_cleaned_2021 = data_cleaned_2021 %>% filter(price>500 & price<100000)

grouped_prices_2015=data_cleaned_2015 %>% group_by(manufacturer,modelnew,transmission,state,condition_bucket,odo_bucket,color,age_bucket) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
grouped_prices_2021=data_cleaned_2021 %>% group_by(manufacturer,modelnew,transmission,state,condition_bucket,odo_bucket,color,age_bucket) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))

merged_detail=inner_join(grouped_prices_2021,grouped_prices_2015,by=c('manufacturer','modelnew','transmission','state','condition_bucket','odo_bucket','color','age_bucket'))
merged_detail=merged_detail %>% mutate(newcol=avg_price.x-avg_price.y) 
merged_detail %>% group_by(manufacturer) %>%  summarize(total=mean(newcol)) %>% arrange(desc(total))


#inner join both time sets

-------------
#run analyses




-------------
#garbage dump


grouped_prices_2021 %>% group_by(condition_bucket) %>% summarize(mn=mean(avg_price))
  
sapply(data2015, class)
  
#view(data2015 %>% select(saledate) %>% unique() %>% arrange(desc(saledate)))
  
  
  
data_shortened_2021 %>% select(manufacturer,model) %>% unique

testing2015=data_shortened_2015 %>% select(manufacturer,model) %>% unique %>% mutate(model=str_trim(tolower(str_replace(str_replace(model,' ',''),'-','')),side='both'))
testing2015=testing2015[['model']]
data_shortened_2021 %>% mutate(model=str_trim(tolower(str_replace(str_replace(model,' ',''),'-','')),side='both')) %>% filter(model %in% testing2015)

colnames(data)

data %>% mutate(age=)

data %>% group_by(make, model, trim, body, condition, transmission) %>% summarize(mean=mean(sellingprice), sdev=sd(sellingprice), cnt=n()) %>% arrange(desc(cnt)) 

data2015 %>% select(trim) %>% unique()
head(data2021 %>% select(size, type),50)




head(data2,20)


head(data2 %>% filter(!is.na(year)),5)


dim(data2) %>% filter(50:51)
       

view(data %>% group_by(make) %>% summarize(cnt=n()) %>% arrange(desc(cnt)))


data2 %>% filter(manufacturer=='audi') %>% select(price) %>% summarize(mn=mean(price))
data %>% filter(make=='Audi') %>% select(sellingprice) %>% summarize(mn=mean(sellingprice))
