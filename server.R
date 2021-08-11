function(input,output){
  #beginning of content
  
  
  
  manufacturer_2021_dep <- reactive({
    
    grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
    summing=c('manufacturer', 'age')
    
    manufacturers=input$checkgroup_manufacturers2021
    
    grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 
    
    tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg)) %>% filter(manufacturer %in% manufacturers)
    return (tmp)
  })
  
  selector <- reactive({
    inputsel=input$selector   
    return(inputsel)
  })
  
  manufacturer_2021_analysis <- reactive({
    
    grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
    inputsel=input$selector   
    summing=c('manufacturer', inputsel)
    
    manufacturers=input$checkgroup_manufacturers2021_2
    
    grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 
    
    tmp2 = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg)) %>% filter(manufacturer %in% manufacturers)
    return (tmp2)
  })
  
  manufacturer_20212015 <- reactive({
    
    manufacturers=input$checkgroup_manufacturers2021_3
    
    grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
    summing=c('manufacturer')
    
    #group data by all relevant metrics, add mean, sd, n, filter low n's (grouping by input vector for later selectability in Shiny)
    group_data=function(grouping){
      grouped_prices_2015=data_cleaned_2015 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
      grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
      
      merged_detail=inner_join(grouped_prices_2021,grouped_prices_2015,by=grouping,suffix = c(".2021", ".2015"))
      merged_detail=merged_detail %>% mutate(newcol=avg_price.2021-avg_price.2015) %>% filter(manufacturer %in% manufacturers)
      return (merged_detail)
    }
    
    #sum data by defined grouping metric
    summing_data=function(merged_detail,summing){
      return(merged_detail %>% group_by_at(summing) %>%  summarize(weighted_total=weighted.mean(newcol, cnt.2021+cnt.2015), count2021=sum(cnt.2021),count2015=sum(cnt.2015),avg2021=weighted.mean(avg_price.2021,cnt.2021),avg2015=weighted.mean(avg_price.2015,cnt.2015),sd2021=sum(sd_price.2021^2,na.rm=TRUE)^0.5,sd2015=sum(sd_price.2015^2,na.rm=TRUE)^0.5) %>% arrange(desc(weighted_total)))
    }
    
    merged_detail=group_data(grouping)
    sum=summing_data(merged_detail,summing)
    
    return (sum)
  })
  
  
  selector2 <- reactive({
    inputsel2=input$selector2
    return(inputsel2)
  })
  
  manufacturer_detail_20212015 <- reactive({
    
    manufacturers=input$checkgroup_manufacturers2021_4
    
    grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
    inputsel2=input$selector2   
    summing=c('manufacturer', inputsel2)
    
    #group data by all relevant metrics, add mean, sd, n, filter low n's (grouping by input vector for later selectability in Shiny)
    group_data=function(grouping){
      grouped_prices_2015=data_cleaned_2015 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
      grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
      
      merged_detail=inner_join(grouped_prices_2021,grouped_prices_2015,by=grouping,suffix = c(".2021", ".2015"))
      merged_detail=merged_detail %>% mutate(newcol=avg_price.2021-avg_price.2015) %>% filter(manufacturer %in% manufacturers)
      return (merged_detail)
    }
    
    #sum data by defined grouping metric
    summing_data=function(merged_detail,summing){
      return(merged_detail %>% group_by_at(summing) %>%  summarize(weighted_total=weighted.mean(newcol, cnt.2021+cnt.2015), count2021=sum(cnt.2021),count2015=sum(cnt.2015),avg2021=weighted.mean(avg_price.2021,cnt.2021),avg2015=weighted.mean(avg_price.2015,cnt.2015),sd2021=sum(sd_price.2021^2,na.rm=TRUE)^0.5,sd2015=sum(sd_price.2015^2,na.rm=TRUE)^0.5) %>% arrange(desc(weighted_total)))
    }
    
    merged_detail=group_data(grouping)
    sum=summing_data(merged_detail,summing)
    sum[, 2] <- sapply(sum[, 2], as.character)
    return (sum)
  })  
  
  
  manufacturer_detail_20212015_data <- reactive({
    
    manufacturers=input$checkgroup_manufacturers2021_5
    
    grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
    summing=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
    
    #group data by all relevant metrics, add mean, sd, n, filter low n's (grouping by input vector for later selectability in Shiny)
    group_data=function(grouping){
      grouped_prices_2015=data_cleaned_2015 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
      grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
      
      merged_detail=inner_join(grouped_prices_2021,grouped_prices_2015,by=grouping,suffix = c(".2021", ".2015"))
      merged_detail=merged_detail %>% mutate(newcol=avg_price.2021-avg_price.2015) %>% filter(manufacturer %in% manufacturers)
      return (merged_detail)
    }
    
    #sum data by defined grouping metric
    summing_data=function(merged_detail,summing){
      return(merged_detail %>% group_by_at(summing) %>%  summarize(weighted_total=weighted.mean(newcol, cnt.2021+cnt.2015), count2021=sum(cnt.2021),count2015=sum(cnt.2015),avg2021=weighted.mean(avg_price.2021,cnt.2021),avg2015=weighted.mean(avg_price.2015,cnt.2015)) %>% arrange(desc(weighted_total)))
    }
    
    merged_detail=group_data(grouping)
    sum=summing_data(merged_detail,summing)
    return (sum)
  }) 
  
  
  #beginning of outputs
  
  output$depreciation <- renderPlot(
    manufacturer_2021_dep() %>% ggplot(aes(x=age, y=avg , color=manufacturer, group=manufacturer)) + geom_line()+geom_point() 
    + ggtitle('Average vehicle price depending on vehicle age per manufacturer')
    + ylab('Vehicle price in USD')
    + xlab('Vehicle Age')
    + theme(
      plot.title = element_text(color="black", size=18, face="bold.italic"),
      axis.title.x = element_text(color="black", size=18, face="bold"),
      axis.title.y = element_text(color="black", size=18, face="bold"),
      text = element_text(size = 18)
    )
  )
  
  
  output$dep_percent <- renderPlot(
    manufacturer_2021_dep() %>% select(!count) %>% select(!sd) %>% spread(age, avg) %>% mutate(percentagedrop=(1-`5. 13-20`/`1. <=2`)*100) %>% ggplot(aes(x=reorder(manufacturer, percentagedrop), y=percentagedrop)) + geom_bar(stat='identity') + coord_flip() 
    + ggtitle('Average depreciation 12-20 year old cars vs. 1-2 year old cars per manufacturer')
    + ylab('Vehicle price change in percent 1-2 yrs. vs 12-20 yrs age')
    + xlab('Manufacturer')
    + theme(
      plot.title = element_text(color="black", size=18, face="bold.italic"),
      axis.title.x = element_text(color="black", size=18, face="bold"),
      axis.title.y = element_text(color="black", size=18, face="bold"),
      text = element_text(size = 18)
    )
  )
  
  
  output$free_2021 <- renderPlot(
    manufacturer_2021_analysis() %>% ggplot(aes_string(x=selector(), y='avg' , color='manufacturer', group='manufacturer')) + geom_line()+geom_point() 
    + ggtitle('Price discrepancy by select x axis variable per manufacturer')
    + ylab('Vehicle price in USD')
    + xlab('See selector')
    + theme(
      plot.title = element_text(color="black", size=18, face="bold.italic"),
      axis.title.x = element_text(color="black", size=18, face="bold"),
      axis.title.y = element_text(color="black", size=18, face="bold"),
      text = element_text(size = 18)
    )
  )
  
  
  output$value_dev_1 <- renderPlot(
    manufacturer_20212015() %>% ggplot(aes(x=reorder(manufacturer, weighted_total), y=weighted_total)) + geom_bar(stat='identity') + coord_flip() 
    + ggtitle('Average price development of equal cars between 2015 and 2021 per manufacturer')
    + ylab('Price increase 2015 to 2021')
    + xlab('Manufacturer')
    + theme(
      plot.title = element_text(color="black", size=18, face="bold.italic"),
      axis.title.x = element_text(color="black", size=18, face="bold"),
      axis.title.y = element_text(color="black", size=18, face="bold"),
      text = element_text(size = 18)
    )
  )
  
  
  output$value_dev_2 <- renderPlot(
    manufacturer_detail_20212015() %>% filter(count2021 > 10 & count2015 > 10) %>% ggplot(aes_string(x='manufacturer', y='weighted_total', fill=selector2())) + geom_bar(stat='identity', position="dodge") + coord_flip() 
    + ggtitle('Average price development of equal cars between 2015 and 2021 per manufacturer and selected group')
    + ylab('Price increase 2015 to 2021')
    + xlab('Manufacturer')
    + theme(
      plot.title = element_text(color="black", size=18, face="bold.italic"),
      axis.title.x = element_text(color="black", size=18, face="bold"),
      axis.title.y = element_text(color="black", size=18, face="bold"),
      text = element_text(size = 18)
    )
    + scale_fill_grey()
  )
  
  
  output$value_dev_table <- renderDataTable(
    manufacturer_detail_20212015_data()
    
  ) 
  
  #end of content
}