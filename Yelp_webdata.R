#the function-yelp.rest return a dataframe with the information of restaurants
#input:town-starting with capitalized letter-name of the town,state-two letters of state of the town
#town-first letter of the name of town must be capitalized
#state-must be two capitalized letters representing the names of the states of US
#output:the dataframe containing the information of all the restaurants of the town
yelp.rest<-function(town,state){
  library(gdata)
  
  yelp.address<-paste('http://www.yelp.com/search?find_desc=',
                      'Restaurants','&find_loc=',town,',+',state,sep='')
  yelp.lines<-readLines(yelp.address,encoding="UTF-8")
  # number of pages
  page.ind <- grep('<div class="page-of-pages arrange_unit arrange_unit--fill">',yelp.lines)+1
  page.num <- as.numeric(unlist(strsplit(yelp.lines[page.ind],'of '))[2])
  
  # all the lines for the restaurants of the town in the state
  tot.yelp.lines=c()
  for (i in 0:page.num-1){
    j <- 10*i
    new.yelp.address <- paste(yelp.address,'&start=',j,sep='')
    tot.yelp.lines <- append(tot.yelp.lines,readLines(new.yelp.address,encoding="UTF-8"))
  }
  
  #the index of the lines we can identify for the restaurants from yelp webpages
  #yelp.ind and other variables are used to distinguish all the restaurants from each other
  yelp.ind<- '<h3 class="search-result-title">'
  yelp.name<-'<span class="indexed-biz-name">'
  yelp.name.ind <- grep('<span class="indexed-biz-name">',tot.yelp.lines)
  yelp.food.type<-'<a href=.*Restaurants.*cflt|<span class="category-str-list">'
  yelp.food.type.ind <- grep('<a href=.*Restaurants.*cflt|<span class="category-str-list">',tot.yelp.lines)
  yelp.rat<-'<i class="star-img stars_'
  yelp.rat.ind <- '<i class="star-img stars_'
  yelp.price.range<-'<span class="business-attribute price-range">'
  yelp.price.range.ind <- '<span class="business-attribute price-range">'
  yelp.rating.num.ind<-grep( '<span class="review-count rating-qualifier">',tot.yelp.lines)+1
  yelp.Ad.name<- '<a class=\"biz-name\" href=\"/adredir\\?ad_business'
  #conbined other information pattern for each restaurant
  pattern <- paste(yelp.ind,yelp.Ad.name,yelp.food.type,yelp.rat,yelp.price.range,sep='|')
  other.lines.ind <- grep(pattern, tot.yelp.lines)
  #combine the indeces of total information for each restaurant and sort them
  tot.lines.ind <- sort(c(other.lines.ind ,yelp.rating.num.ind,yelp.name.ind))
  #identify those restaurants with missing information and label with special sympbol-%
  yelp.temp0 <- unlist(lapply(tot.yelp.lines[tot.lines.ind], function(x) ifelse(trim(x)== "<h3 class=\"search-result-title\">",'%',x)))      
  
  #rename all the adverting restaurant names to Ad-name
  ad.name.temp1 <- gsub("<a class=\"biz-name\" href=\"/adredir\\?ad_business.*</span></a>","<span class=\"indexed-biz-name\"> <span >Ad-name</span></a>",yelp.temp0)
  #keep spliting information in renamed restaurants
  ad.name.temp2 <-unlist(strsplit(paste(ad.name.temp1,collapse=',,'),'\\%,,|,,\\%,,'))
  ad.name.temp3<- strsplit(ad.name.temp2,',,')
  #remove the first element of the vecto- ad.name.temp3
  ad.name.temp3<-ad.name.temp3[-1]
  
  # number of reviews in each restaurant 
  
  
  num.rev1<-lapply(ad.name.temp3, function(x) trim(x[grep('reviews',x)]))
  
  num.rev2<- as.numeric( unlist(lapply(num.rev1, function(x) 
    ifelse(length(x)==0,NA,unlist(strsplit(x,' '))[1]))))
  
  #identify all the restaurant names
  rest.names1 <-lapply(ad.name.temp3, function(x) trim(x[grep(yelp.name,x)])) 
  
  rest.names2 <- unlist(lapply(rest.names1,
                               function(x) ifelse(length(x)==0,NA,strsplit(strsplit(x,'<span >')[[1]][2],'</span>')[[1]][1])))
  
  # price range of all the restaurants
  price.range1<-lapply(ad.name.temp3,function(x) trim(x[grep('price-range\">',x)]))
  price.range2 <- unlist(lapply( price.range1,function(x) ifelse(length(x)==0,NA,strsplit(strsplit(x,'price-range\">')[[1]][2],'</span>')[[1]][1])))
  
  # average rating of all the restaurants
  avg.rating1<-lapply(ad.name.temp3,function(x) trim(x[grep('<i class=\"star-img stars',x)]))
  avg.rating2<-unlist(lapply(avg.rating1,function(x) ifelse(length(x)==0,NA,strsplit(strsplit(x,'title=\"')[[1]][2],' star')[[1]][1])))
  # food types of all the restaurants
  food.type1 <- lapply(ad.name.temp3,function(x) trim(x[grep(yelp.food.type,x)]))
  food.type.len <- sapply(food.type1,function(z) length(z))
  #if  food type information is missing, define it as %,NA 
  #and % is used to split data in different restaurants
  for (i in 1:length(food.type1)){
    if (food.type.len[i]==0){
      food.type1[i][[1]] <- '%,NA'
    }
  }
  food.type2 <- unlist(food.type1)
  #identify the food type from the lines 
  loc.food.type <- function(x){
    if(trim(x)== "<span class=\"category-str-list\">"){
      return('%')
    }
    else{
      if (x!='%,NA'){
        return(strsplit(strsplit(x,'</a>')[[1]][1],'>')[[1]][2])
      }
      else{
        return(x)
      }
    }
  }
  
  food.type3 <- unlist(lapply(food.type2, loc.food.type))
  
  ##use % to split data by different restaurants into different variables, and delete the first element
  food.type <- unlist(strsplit(paste(food.type3,collapse=','),'\\%,|,\\%,')[[1]])[-1]
  
  # the final dataframe of all the restaurants including the Advertized ones
  data.final <- as.data.frame(list(Restaurant_Name=rest.names2,Price_Range=price.range2,Averaging_Rating=avg.rating2,Number_of_Raters=num.rev2,Food_Type=food.type))
  #remove all the advertising restaurants with the name-Ad-name
  data.final <- data.final[!data.final$Restaurant_Name=='Ad-name',]
  # delete the row numbers after removeing some rows
  rownames(data.final)<-NULL
  return(data.final)
  
}

#an example for all the restaurants from Yelp of the town-Amherst in the state-MA
yelp.rest(town="Amherst",state="MA")
