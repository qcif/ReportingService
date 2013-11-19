library(shiny)
library(stringr)

# Load base data
storagedf <- read.csv("storagedata.csv", header=TRUE, stringsAsFactors=FALSE)

# scrub data, by removing empty rows
storagedata <- storagedf[storagedf[1] != "",]

# Munge data, by extracting member information
storagedata$member <- "Other"
storagedata$member[!is.na(str_extract(storagedata$Institution, "UQ"))] <- "UQ"
storagedata$member[!is.na(str_extract(storagedata$Institution, "QUT"))] <- "QUT"
storagedata$member[!is.na(str_extract(storagedata$Institution, "GU"))] <- "GU"
storagedata$member[!is.na(str_extract(storagedata$Institution, "JCU"))] <- "JCU"
storagedata$member[!is.na(str_extract(storagedata$Institution, "CQU"))] <- "CQU"
storagedata$member[!is.na(str_extract(storagedata$Institution, "USQ"))] <- "USQ"
storagedata$member[!is.na(str_extract(storagedata$Institution, "UoSC"))] <- "UoSC"

# Munge data - by by extracting size in TB
storagedata$size.TB <-as.numeric(gsub(",","",as.character(storagedata$RAC.Allocated.Storage..GB.)))/1000

# Munge data - by by extracting size in TB
storagedata$Released.to.Client <- as.Date(storagedata$Released.to.Client, "%d.%m.%Y")


# Reduce data down to useful columns
storagedata <- storagedata[c("QCloud.No.","Name.of.Project","Institution","Description","size.TB","Access.Type","Notes","Released.to.Client","member")]
top10storage <- head(storagedata[order(storagedata$size.TB,decreasing=T),],10)[c("QCloud.No.","Name.of.Project","Institution","size.TB")]
newdata <- head(storagedata[order(storagedata$Released.to.Client,decreasing=T),],10)[c("QCloud.No.","Name.of.Project","Institution","size.TB","Released.to.Client")]

publicdata <- storagedata[storagedata$Access.Type == c("Un-restricted"),]
publicdata <- publicdata[c("QCloud.No.","Name.of.Project","Institution","size.TB","Notes")]

memberstorage <- aggregate(storagedata[,c("size.TB")], by=list(storagedata$member), "sum", na.rm=TRUE)
names(memberstorage)[names(memberstorage)=="Group.1"] <- "member"
names(memberstorage)[names(memberstorage)=="x"] <- "size.TB"
memberstorage <- memberstorage[order(memberstorage$size.TB, decreasing=FALSE),]

# Build growth data
t1 <- storagedata[c("Released.to.Client", "size.TB")]
t1 <- t1[rowSums(is.na(t1[,1:2]))==0,]
dgrowth <- t1[order(t1$Released.to.Client,decreasing=FALSE),]
dgrowth$cumulutive.size.TB <- cumsum(dgrowth$size.TB)


# Build summary data
sum1 <- c("Number of allocated collections")
val1 <- sprintf("%i", nrow(storagedata))
sum2 <- c("Total of allocated collections")
val2 <- sprintf("%.4f TB", sum(storagedata$size.TB, na.rm=TRUE))
sum3 <- c("Largest collection allocation")
val3 <- sprintf("%.4f TB", max(storagedata$size.TB, na.rm=TRUE))
sum4 <- c("Average collection allocation")
val4 <- sprintf("%.4f TB", mean(storagedata$size.TB, na.rm=TRUE))
sum5 <- c("Smallest collection allocation")
val5 <- sprintf("%.4f TB", min(storagedata$size.TB, na.rm=TRUE))
sum6 <- c("Publically accessible collections")
val6 <- sprintf("%.0f%%", 100 * nrow(publicdata) / nrow(storagedata))

summatrix <- matrix(c(sum1,val1,sum2,val2,sum3,val3,sum4,val4,sum5,val5,sum6,val6),byrow=TRUE,6,2)
colnames(summatrix) <- c("Item", "Value")
summatrix.table <- as.table(summatrix)

# Define server logic 
shinyServer(function(input, output) {

  # Generate views of the data
  output$summary <- renderTable(summatrix.table)
  output$datagrowth <- renderPlot({
	plot(cumulutive.size.TB~Released.to.Client, data=dgrowth, type="o")	
  })
  output$memberstats <- renderPlot({
	par(mfrow=c(2,1)) 
	barplot(sort(table(storagedata$member)),col="blue",main="No. of collections allocated",xlab="QCIF Member", ylab="Collections")
	barplot(memberstorage$size.TB, names.arg=memberstorage$member, col="blue",main="Total size of collection allocations (TB)",xlab="QCIF Member", ylab="Size (TB)")
  }) 
  output$topstorageusers <- renderTable(top10storage)
  output$newreleases <- renderDataTable(newdata)
  output$publiccollections <- renderTable(publicdata)
  output$storage <- renderDataTable({storagedata})
})
