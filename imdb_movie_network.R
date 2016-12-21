#read the HTML code of the website "http://www.imdb.com/chart/top?ref_=ft_250"
#each line of the HTML code is a member of the character vector-top250.page 
top250.page <- readLines("http://www.imdb.com/chart/top?ref_=ft_250")
#select the string lines containing "/title/tt" 
#from the string vector-top250.page
#form a new string vector-top250.codes containing tt codes
top250.codes <- top250.page[grep("/title/tt",top250.page)]
#split the 500 string vectors by the split character "//?"
#turn the list of 500 entries, each with 4 strings into the vector with 4X500 members
#choose each third string in the four strings of 500 entries of the list
#form a new vector with the size 500 containing tt codes
top250.codes <- unlist(strsplit(top250.codes,split="//?"))[seq(from=3,to=1999,by=4)]
#Remove the repeating tt codes by choosing the even entries of the vector of size 500
#form a new vector with size 250 containing the unique tt codes for 250 movies
top250.codes <- top250.codes[seq(from=2,to=500,by=2)]

address <- "http://www.imdb.com/title/tt0047396/"
page <- readLines(address)
# you may have to change the path below
write(page,file="~/example.main.txt")

address <- "http://www.imdb.com/title/tt0245429/business"
page <- readLines(address)
# you may have to change the path below
write(page,file="~/example.finance.txt")

get.stuff <- function(imdb.code)
{
  # Make the movie's main page address
  address <- paste("http://www.imdb.com/title/",imdb.code,sep="")
  # Read the .html for the page
  page <- readLines(address)
  
  # extract the line that has the movie title and the year
  title.year <- page[grep("<title>",page)]
  # SUGGESTION: use a series of strsplit() commands to remove
  # the extraneous characters until you can put the title into
  # a variable called Title and year into a variable called Year.
  title.year.1<-unlist(strsplit(title.year,split="<title>"))[2]
  title.year.2<-unlist(strsplit(title.year.1,split="\\s+\\-"))[1]
  title<-unlist(strsplit(title.year.2,split="\\s+\\("))[1]
  year.temp<-unlist(strsplit(title.year.2,split="\\s+\\("))[2]
  year<-as.numeric(unlist(strsplit(year.temp,split="\\)"))[1])  
  # get content rating
  Content.Rating <- page[grep("contentRating",page)[1]]
  # SUGGESTION: use strsplit() to get rid of extaneous text
  Content.Rating<-unlist(strsplit(Content.Rating ,split=">"))[2]
  
  # get user ratings and number of raters
  User.Rating <- page[grep("Users rated this",page)[1]]
  User.Rating <- strsplit(User.Rating,"/")[[1]][1]
  User.Rating <- as.numeric(strsplit(User.Rating,"this ")[[1]][2])
  # SUGGESTION: use similar commands to above to get Num.Raters
  Num.Raters.1<-unlist(strsplit(page[grep("Users rated this",page)[1]],"\\s+\\(")	)[2]
  Num.Raters<-as.numeric(gsub(",","",unlist(strsplit(Num.Raters.1,split="\\s+votes"))[1]))  
  # Get genre information
  inds <- grep("ref_=tt_stry_gnr",page)
  Genre <- unlist(strsplit(page[inds],"/genre/"))[seq(from=2,to=length(inds)*2,by=2)]
  Genre <- unlist(strsplit(Genre,"\\?"))[seq(from=1,to=length(inds)*2,by=2)]
  Genre <- paste(Genre,collapse=" ")
  
  # Make the movie's finance page address
  address.finance <- paste("http://www.imdb.com/title/",imdb.code,"/business",sep="")
  business.page <- readLines(address.finance)
  
  
  budget <- NA
  opening <- NA
  gross <- NA
  temp <- grep("Budget",business.page)
  if (length(temp)>0)
  {
    budget <- business.page[temp+1]
    budget <- strsplit(budget," \\(")[[1]][1]
    budget <- gsub("\\$","",budget)
    budget <- as.numeric(gsub("\\,","",budget))
  } 
  #gross-The US gross revenue
  temp.gross <- grep("Gross",business.page)
  if (length(temp.gross)>0)
  {
    gross <- business.page[temp.gross+1]
    gross <- strsplit(gross," \\(")[[1]][1]
    gross <- gsub("\\$","",gross)
    gross <- as.numeric(gsub("\\,","",gross))
  } 	
  
  
  #opening-The revenue for Opening
  temp.opening <- grep("Opening",business.page)
  if (length(temp.opening)>0)
  {
    opening <- business.page[temp.opening+1]
    opening <- strsplit(opening," \\(")[[1]][1]
    opening <- gsub("\\$","",opening)
    opening <- as.numeric(gsub("\\,","",opening))
  } 
  
  # SUGGESTION: repeat for the gross revenue and the opening 
  
  return(list(Title=title,Year=year,Content.Rating=Content.Rating, 
              User.Rating=User.Rating,Num.Raters=Num.Raters,Genre=Genre,
              Budget=budget,Opening=opening,Gross=gross))
}
#test the function on one movie with tt code-tt0407887 
as.vector(get.stuff("tt0407887")) 

# The next command takes a few minutes.
data <- sapply(top250.codes,get.stuff)  
# Please explain what the next line does.
#transpose the 9 rows and 250 columns of data
#to 250 observations(movies) and 9 columns(variables)
#transform data to the format of data frame 
data <- as.data.frame(t(data))
# sapply() leave each column of data in an odd format (a vector of lists!)
# The next line fixes that.
for (i in 1:dim(data)[2])
  data[,i] <- unlist(data[,i])

#the function movie.name.gross.max: list the names and revenues of 
#the x movies with the largest grosses
#input: x-the number of movies with the largest grosses
#output: the names and revenues of these movies
movie.name.gross.max<-function(x){
  names.x<-c()
  revenues.x<-c()
  ind.x<-which(data$Gross==max(data$Gross,na.rm=T))
  names.x<-c(names.x,data$Title[ind.x])
  revenues.x<-c(revenues.x,data$Gross[ind.x])
  i<-x-length(ind.x)
  data.temp<-data[-ind.x,]
  while (i>0){
    ind.temp<-which(data.temp$Gross==max(data.temp$Gross,na.rm=T))
    names.x<-c(names.x,data.temp$Title[ind.temp])
    revenues.x<-c(revenues.x,data.temp$Gross[ind.temp])
    i<-i-length(ind.temp)
    data.temp<-data.temp[-ind.temp,]
  }
  return(list(names.x,revenues.x))
  
}
x.5<-movie.name.gross.max(x=5)
#the names of x=5 movies with the largest gross revenues
top5.movie.names<-unlist(x.5)[1:5]
top5.movie.names
#the revenues of x=5 movies with the largest gross revenues
top5.movie.revenues<-as.numeric(unlist(x.5)[6:10])
top5.movie.revenues

mean.gross.by.rating<-tapply(data$Gross,data$Content.Rating,mean,na.rm=T)
count.movies.by.rating<-tapply(data$Title,data$Content.Rating,length)

n.rating<-length(sort(unique(data$Content.Rating)))
plot(1:n.rating,1:n.rating,xlim=c(.75,n.rating+0.25),
     ylim=range(mean.gross.by.rating),type="n",
     axes=F,
     xlab="Movie Content Rating Categories",
     ylab="Mean Gross Revenues")
axis(2)
axis(1,at=1:n.rating,labels=sort(unique(data$Content.Rating)))
for (i in 1:n.rating)
{
  points(i,mean.gross.by.rating[i],pch=16,cex=1.5)
  text(i,mean.gross.by.rating[i],pos=1,
       paste(count.movies.by.rating[i]),cex=.6)
  
  plot(x=data$User.Rating,y=data$Gross,
       xlab="Movie user ratings",
       ylab="Gross revenues of movies",
       main="Scatterplot of gross revenues versus user ratings",
       col="skyblue2")
  
  length(data$Num.Raters)
  plot(x=data$Num.Raters,y=data$Gross,
       xlab="Number of Raters",
       ylab="Gross revenues of movies",
       main="Scatterplot of gross revenues versus number of raters",
       col="skyblue2")
  
  ind.drama<-grep('Drama',data$Genre)
  ind.comedy<-grep('Comedy',data$Genre)
  t.test(x=data$Gross[ind.drama],y=data$Gross[ind.comedy]
         , alternative = "two.sided", conf.level = 0.95)
  
  t.test(x=data$Gross,y=data$Opening,alternative = "two.sided", conf.level = 0.95)
  plot(x=data$Opening,y=data$Gross,
       xlab="Movie opening revenues",
       ylab="Gross revenues of movies",
       main="Scatterplot of gross revenues versus opening revenues",
       col="skyblue2")
  #straight line with y=x
  #all the points should be above this straight line
  #Gross revenues should be larger than opening revenue
  lines(x=data$Opening,y=data$Opening,lwd=2,col="red")
  #identity the index of the movie in the plot above
  #where gross revenue is less than opening revenues
  gross.less.than.opening<-which(data$Opening>data$Gross)
  #the movie which has some problem
  data[gross.less.than.opening,]
  
  linked.movies <- function(imdb.code)
  {
    
    # read the movie page
    address <- paste("http://www.imdb.com/title/",imdb.code,sep="")
    page <- readLines(address)
    # find the lines with the recommendations and strip the unneeded stuff
    recs <- page[grep("rec_item",page)]
    recs<- unlist(strsplit(recs,"data-tconst="))[seq(from=2,to=24,by=2)]
    # return the codes
    #remove all the parts not numbers in the vector-recs
    recs <- paste("tt",gsub("[^0-9]","",recs),sep="")
    
    return(recs)
  }
  linked.movies("tt0245429")
  matrix.rec<-matrix(0,nrow=dim(data)[1],ncol=dim(data)[1])
  
  for (i in 1:dim(data)[1]){
    ttcode.rec<-linked.movies(rownames(data)[i])
    for (j in 1:length(ttcode.rec)){
      if (length(grep(ttcode.rec[j],x=rownames(data)))>0){
        matrix.rec[i,  which (rownames(data)==ttcode.rec[j])]<-1}}}
  rownames(matrix.rec)<-rownames(data)
  colnames(matrix.rec)<-rownames(data)
  
  #the function-matrix.rec.by.ttcode define the links between other movies
  #and the movie with the input tt code by define the row with the name
  #of the tt code of this movie
  #input:tt code of the movie
  #output: the vector defining the recommendations(0,1) of 250 movies of this input movie
  matrix.rec.by.ttcode<-function(idmb.code){
    vec.ttcode<-rep(0,times=length(rownames(data)))
    link<-as.matrix(linked.movies(idmb.code),nrow=length(linked.movies(idmb.code)),ncol=1)
    tt<-apply(link,1,grep,x=rownames(data))
    tt1<-as.vector(unlist(tt))
    vec.ttcode[tt1]<-1
    return(vec.ttcode)
  }
  
matrix.rec.by.ttcode<-apply(as.matrix(rownames(data),nrow=length(rownames(data)),ncol=1),1,matrix.rec.by.ttcode)
matrix.rec.by.ttcode<-t(matrix.rec.by.ttcode)
colnames(matrix.rec.by.ttcode)<-rownames(data)
rownames(matrix.rec.by.ttcode)<-rownames(data)
  
matrix.rec.x<-function(x){
  matrix.rec<-matrix(0,nrow=dim(data)[1],ncol=dim(data)[1])
    
    for (i in 1:dim(data)[1]){
      ttcode.rec<-linked.movies(rownames(data)[i])[x]
      if (length(grep(ttcode.rec,x=rownames(data)))>0){
        matrix.rec[i,  which (rownames(data)==ttcode.rec)]<-1}}
    return(matrix.rec)}
# only included if the movie is the top recommendation
matrix.rec.1<-matrix.rec.x(x=1)

#the name of the movie linked to the most other movies
#the maximum number of recommendations by other movies-35
data[which(apply(matrix.rec.by.ttcode,2,sum)==max(apply(matrix.rec.by.ttcode,2,sum))),]$Title

library(statnet)
# links is the matrix with the links in it.
matrix.rec1<-matrix.rec
colnames(matrix.rec1)<-data$Title
rownames(matrix.rec1)<-data$Title
g <- network(matrix.rec1)
#plot your movie relationship network based on imdb web data
plot(g,displaylabels = TRUE, label.cex=0.5)