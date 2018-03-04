library(RSelenium)
library(XML)
library(rvest)
pJS <- wdman::phantomjs(port = 4567L)
remDr <- remoteDriver(port=4567L, browserName = 'chrome')
remDr$open()
Sys.sleep(2)
list<-list()
for(j in 1:12){
  remDr$navigate(paste0('http://dmall.danawa.com/v3/?controller=sale&methods=index&parentCategoryCode=1&childCategoryCode=',j,'#1'))
  source<-remDr$getPageSource()[[1]]
  a<-read_html(source)%>%html_nodes(css='.normal_group')%>%html_text()
  a<-strsplit(gsub('\\t','',a),'\\n')
  a<-a[[1]][a[[1]]!='']

  name<-a[seq(1,length(a),8)]
  id<-a[seq(3,length(a),8)]
  new_old<-a[seq(4,length(a),8)]
  direct<-a[seq(5,length(a),8)]
  date<-gsub('\\.','-',a[seq(6,length(a),8)])
  library(RCurl)
  library(stringr)
  price<-(str_extract_all(a[seq(8,length(a),8)],'\\d'))
  for(i in 1:length(price))price[[i]]<-paste(price[[i]],collapse='')
  price<-as.numeric(unlist(price))
  data<-data.frame(name,id,new_old,direct,date,price,stringsAsFactors = F)
  rownames(data)<-NULL

  for( i in 2:23 ){
    script<-paste0('javascript:movePage(',i,');return false;')
    remDr$executeScript(script,arg=1:2)
    source<-remDr$getPageSource()[[1]]
    a<-read_html(source)%>%html_nodes(css='.normal_group')%>%html_text()
    a<-strsplit(gsub('\\t','',a),'\\n')
    a<-a[[1]][a[[1]]!='']
    if(length(a)>8){Sys.sleep(2)
      name<-a[seq(1,length(a),8)]
      id<-a[seq(3,length(a),8)]
      new_old<-a[seq(4,length(a),8)]
      direct<-a[seq(5,length(a),8)]
      date<-gsub('\\.','-',a[seq(6,length(a),8)])
      library(RCurl)
      library(stringr)
      price<-(str_extract_all(a[seq(8,length(a),8)],'\\d'))
      for(i in 1:length(price))price[[i]]<-paste(price[[i]],collapse='')
      price<-as.numeric(unlist(price))
      table<-data.frame(name,id,new_old,direct,date,price,stringsAsFactors = F)
      table<-table[table$direct!='판매완료',]
      rownames(table)<-NULL
      data<-rbind(data,table)}

  }
  list[[j]]<-data
}
remDr$close()
pJS$stop()

name<-c('cpu','ram','m/b','vga','hdd','ssd','odd','case','power','keyboard','mouse','soft')
data<-NULL
for(i in 1:length(name)){
  list[[i]]$part<-name[i]
  data<-rbind(data,list[[i]])}

