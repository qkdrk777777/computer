library(shiny)
if(!require(devtools))install.packages('devtools') else library(devtools)
if(!require('DUcj'))devtools::install_github('qkdrk777777/DUcj',force=T)
library(DUcj)
library(XML)
library(stringr)
library(RCurl)
library(rvest)

url<-'https://www.cpubenchmark.net/cpu_list.php'
url1<-getURL(url)
tables<-readHTMLTable(url1)
tables[[6]][,5][as.character(tables[[6]][,5])=="NA"]<-NA
tables[[6]][,5]<-as.numeric(gsub('[,$*]','',tables[[6]][,5]))
tempurl<-'https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=%ED%99%98%EC%9C%A8'
line<-read_html(tempurl,encoding='UTF-8')
per<-html_nodes(line,css='.input_box')[4]%>%html_nodes(css='.recite')%>%html_text()
per<-str_replace(per,'\\p{Hangul}','')
per<-as.numeric(gsub('[,]',"",per))
tables[[6]]<-tables[[6]][,-c(3,4)]
for(i in 1:2)tables[[6]][,i]<-unfactor(tables[[6]][,i])
tables[[6]][,2]<-gsub('[,]','',tables[[6]][,2])
tables[[6]][,2]<-as.numeric(tables[[6]][,2])
output<-data.frame(tables[[6]][,1:2],price=round(tables[[6]][,3]*per),stringsAsFactors = F)
output$per_price<-round(output[,3]/output[,2],2)
output<-output[order(output[,4],decreasing = T),]
colnames(output)<-c('name','benchmark','won','rank')

for(i in 3:4)
  output[is.na(output[,i]),i]<-0
out1<-output

#gpu
url<-'https://www.videocardbenchmark.net/gpu_list.php'
library(shiny)
if(!require(devtools))install.packages('devtools') else library(devtools)
if(!require('DUcj'))devtools::install_github('qkdrk777777/DUcj',force=T)
library(DUcj)
library(XML)
library(stringr)
library(RCurl)
library(rvest)

url1<-getURL(url)
tables<-readHTMLTable(url1)
tables[[5]][,5][as.character(tables[[5]][,5])=="NA"]<-NA
tables[[5]][,5]<-as.numeric(gsub('[,$*]','',tables[[5]][,5]))
tempurl<-'https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=%ED%99%98%EC%9C%A8'
line<-read_html(tempurl,encoding='UTF-8')
per<-html_nodes(line,css='.input_box')[4]%>%html_nodes(css='.recite')%>%html_text()
per<-str_replace(per,'\\p{Hangul}','')
per<-as.numeric(gsub('[,]',"",per))
tables[[5]]<-tables[[5]][,-c(3,4)]
for(i in 1:2)tables[[5]][,i]<-unfactor(tables[[5]][,i])
tables[[5]][,2]<-gsub('[,]','',tables[[5]][,2])
tables[[5]][,2]<-as.numeric(tables[[5]][,2])
output<-data.frame(tables[[5]][,1:2],price=round(tables[[5]][,3]*per),stringsAsFactors = F)
output$per_price<-round(output[,3]/output[,2],2)
output<-output[order(output[,4],decreasing = T),]
colnames(output)<-c('name','benchmark','won','rank')

for(i in 3:4)
  output[is.na(output[,i]),i]<-0
out2<-output
a<-out1

b<-out2

a1<-a[,-4]
a1[,2]<-round(a1[,2],-2)
a2<-a1
a2[,2]<-a2[,2]+100
a3<-a1
a3[,2]<-a3[,2]-100

b1<-b[,-4]
b1[,2]<-round(b1[,2],-2)
b2<-b1
b2[,2]<-b2[,2]+100
b3<-b1
b3[,2]<-b3[,2]-100

c1<-rbind(a1,a2,a3)
c1<-c1[order(c1[,1]),]

c2<-rbind(b1,b2,b3)
c2<-c2[order(c2[,1]),]

c<-merge(c1,c2,by='benchmark')
c[c[,3]==0,3]<-NA
c[c[,5]==0,5]<-NA
d<-cbind(c,c[,3]+c[,5])

colnames(d)<-c('benchmark','cpu_name','cpu_won','gpu_name','gpu_won','total')
d$rank<-d$total/d$benchmark
for(i in 1:7)
  d<-d[order(d[,i]),]
library(shiny)

ui<-fluidPage(
  sliderInput('total','Total',min=0,max=15000000,value=c(10000,50000),step=10000,sep=',',pre='&#8361;'),
  dataTableOutput('out1'),
  tableOutput('txt')
)
server<-function(input, output,session){
  output$txt<-renderText({paste(input$total)
  })
  output$out1<-renderDataTable({d[input$total[1]<d$total&d$total<input$total[2],]
  })
}


shinyApp(ui,server)

