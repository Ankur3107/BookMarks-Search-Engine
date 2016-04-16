df <- data.frame()
for(i in 1:length(links)){
  
  tmp <- tryCatch(read_html(links[i]),
                  error = function (e) NULL)
  if (is.null(tmp)) {
    # you might want to put some informative message here.
    next()}
 
#Read each links one by one
e<-read_html(links[i])
#Scrape a tag text
fa<-html_nodes(e,"a")
da<-html_text(fa)
#Scrape h1 tag text
fh1<- html_nodes(e,"h1")
dh1<-html_text(fh1)
#Scrape h2 tag text
fh2<- html_nodes(e,"h2")
dh2<-html_text(fh2)

#collapse a ,h1 ,h2 into single data
dda <- paste(da,collapse = " ")
ddh1 <- paste(dh1,collapse = " ")
ddh2 <- paste(dh2,collapse = " ")
d<- paste0(dda,ddh1,ddh2)

#Add to the data.frame 'df'
df<- rbind(df,data.frame(link=i,data=d))

}
