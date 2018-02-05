#' arrows is overlay in plot
#'
#' @param cpu= T or F. T is output of cpu , F is output of gpu
#' @param cal= delete of comma
#' @param amd= if amd=T only output of AMD cpu.
#' @param intal= if intel=T only output of intel cpu.
#' @param money_keep if 'money_keep =T' then '1 banchmark point of price' is cbind
#' @param order sort number if money_keep=T then order=1~4 is range. else if money_keep=F then order=1~3.
#' @param up is 'up' grater than banchmark score output.
#' @return
#' @examples
#' benchmark(cpu=T,amd=T,intel=T,header=0,na_rm=T,money_keep = T,order=3,up=500)
#' @export
benchmark<-function(cpu=T,cal=F,na_rm=F,amd=F,intel=F,money_keep=F,order,header=0,up=0){
if(!require(devtools))install.packages('devtools') else library(devtools)
if(!require('DUcj'))devtools::install_github('qkdrk777777/DUcj',force=T)
library(DUcj)
package(XML)
package(stringr)
package(RCurl)
package(rvest)

if(cpu)url<-'https://www.cpubenchmark.net/high_end_cpus.html' else url<-"https://www.videocardbenchmark.net/high_end_gpus.html"
url1<-getURL(url)
tables<-readHTMLTable(url1)
  tables[[4]][,3][as.character(tables[[4]][,3])=="NA"]<-NA
  tables[[4]][,3]<-as.numeric(gsub('[,$*]','',tables[[4]][,3]))
tempurl<-'https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=%ED%99%98%EC%9C%A8'
line<-read_html(tempurl,encoding='UTF-8')
per<-html_nodes(line,css='.input_box')[4]%>%html_nodes(css='.recite')%>%html_text()
per<-str_replace(per,'\\p{Hangul}','')
per<-as.numeric(gsub('[, ??]',"",per))

for(i in 1:2)tables[[4]][,i]<-unfactor(tables[[4]][,i])
tables[[4]][,2]<-gsub('[,]','',tables[[4]][,2])
tables[[4]][,2]<-as.numeric(tables[[4]][,2])
tables[[4]]<-tables[[4]][-is.na(tables[[4]][,2]),]
output<-data.frame(tables[[4]][,1:2],price=round(tables[[4]][,3]*per),stringsAsFactors = F)
if(na_rm){output<-output[is.na(output$price)!=T,]}
if(up!=0) output<-output[output[,2]>up,]
if(money_keep){
  output$per_price<-round(output[,3]/output[,2],1)
  output[order(output[,4],decreasing = T),]}
if(cpu){
if(intel==T&amd==T) output=output else if(amd){output<-output[regexpr("^AMD",output[,1])!=-1,]
} else if(intel){output<-output[regexpr("^Intel",output[,1])!=-1,]}
}
if(money_keep){
  if(order==1) t=4:1 else if(order==2)t=c(1,4:2) else if(order==3)t=c(1,4,2,3) else if(order==4)t=c(1,3,2,4) else stop('order is out of range 1~4')
  }else{if(order==1)t=3:1 else if(order==2) t=c(1,3,2) else if(order==3)t=1:3 else stop('order is out of range 1~3')}
if(cal) {for(i in t)
{if(i%in%c(1,2))tt=T else if(i%in%c(3,4)) tt=F
    output<-output[order(output[,i],decreasing=tt),]}

  } else{
  for(i in t){
    if(i%in%c(1,2))tt=T else if(i%in%c(3,4)) tt=F
    output<-output[order(output[,i],decreasing=tt),]
    output[,i]<-formatC(output[,i],format='d',big.mark = ',')}}

if(header!=0)
  output<-head(output,header)
return(output)}



