
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
out<-output
out2<-out[out[,3]!=0,]


ui<-fluidPage(
  radioButtons('selected',label='0 price',choices=list('Yes','No')),
  uiOutput('a1'),
  uiOutput('b1')
)
server<-function(input,output,session){
  output$a<-renderDataTable({out})
  output$a1<-renderUI({if(input$selected=='Yes'){dataTableOutput('a')}})

  output$b<-renderDataTable({out2})
  output$b1<-renderUI({if(input$selected=='No'){dataTableOutput('b')}})

}
shinyApp(ui,server)
