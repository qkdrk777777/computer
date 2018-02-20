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
out<-output
not0<-out[out[,3]!=0,]
amd<-out[regexpr("^AMD",out[,1])!=-1,]
amd0<-not0[regexpr("^AMD",not0[,1])!=-1,]
intel<-out[regexpr("^Intel",out[,1])!=-1,]
intel0<-not0[regexpr('^Intel',not0[,1])!=-1,]

ui<-fluidPage(
  radioButtons('selected',label='0 price',choices=list('ALL','cost Not 0','AMD','AMD cost Not 0',
                                                       'Intel','Intel cost Not 0')),
  uiOutput('all1'),uiOutput('not01'),
  uiOutput('amd1'),uiOutput('amd01'),
  uiOutput('intel1'),uiOutput('intel01')
)
server<-function(input,output,session){
  output$a<-renderDataTable({out})
  output$all1<-renderUI({if(input$selected=='ALL'){dataTableOutput('a')}})

  output$b<-renderDataTable({not0})
  output$not01<-renderUI({if(input$selected=='cost Not 0'){dataTableOutput('b')}})

  output$c<-renderDataTable({amd})
  output$amd1<-renderUI({if(input$selected=='AMD'){dataTableOutput('c')}})

  output$d<-renderDataTable({amd0})
  output$amd01<-renderUI({if(input$selected=='AMD cost Not 0'){dataTableOutput('d')}})

  output$e<-renderDataTable({intel})
  output$intel1<-renderUI({if(input$selected=='Intel'){dataTableOutput('e')}})

  output$f<-renderDataTable({intel0})
  output$intel01<-renderUI({if(input$selected=='Intel cost Not 0'){dataTableOutput('f')}})
}
shinyApp(ui,server)
