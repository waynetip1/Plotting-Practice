library(data.table)
library(dplyr)
library(tidyr)

setwd("C:/Users/Wayne Office Laptop/Documents/GitHub/Plotting-Practice")
fileURL <-"https://d18ky98rnyall9.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1507507200&Signature=GBX1JK~xArO2x-bMrihV-uRmaTS3ryHW4G1ivIfTg6FvsVKTXzMaDZdf1mtLGzsIPb3Umyt156GqdO4Dn9hd1LfgcQOamwKnODQUGsJaQNnvWjrQXZWvQMS9EMLpuhSFg6CO9pkSXI8Tj9daGeLJEkw2oMUEECqmYaIFV7m3ipc_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file(fileURL,destfile = "./plotData.csv")
pathdata <- "./"

plotData<-fread("plotData.csv")
nyFilter <- filter(plotData,Provider.State == "NY")

#plot 1
pdf("plot1.pdf")

hist(nyFilter$Average.Covered.Charges,col= "blue",xlab = "US Dollars",
     main="New York State Medical Costs", 
     sub="Average Covered Charges vs Average Total Payments")

legend("topright",c("Ave Covered Charges","Ave Total Payments"),
       fill = c('blue','red'))

hist(nyFilter$Average.Total.Payments,col = "red",add=T)
box()
dev.off()

# create select data with only interested variables
Filter <- plotData %>%
        group_by(Provider.State,DRG.Definition)%>%
        select(Provider.State,DRG.Definition,Average.Covered.Charges,
               Average.Total.Payments)

#edit disease names to be more compact
Filter$DRG.Definition <- gsub(".*194.*","PNEUMONIA/PLEURISY",
                                 Filter$DRG.Definition,ignore.case = T)
Filter$DRG.Definition <- gsub(".*292.*","HEARTFAIL/SHOCK",
                                 Filter$DRG.Definition,ignore.case = T)
Filter$DRG.Definition <- gsub(".*392.*","ESOPHAGITIS",
                                 Filter$DRG.Definition,ignore.case = T)
Filter$DRG.Definition <- gsub(".*641.*","MISC NUTRITION",
                                 Filter$DRG.Definition,ignore.case = T)
Filter$DRG.Definition <- gsub(".*690.*","URINARY INFECT",
                                 Filter$DRG.Definition,ignore.case = T)
Filter$DRG.Definition <- gsub(".*871.*","SEPTICEMIA",
                                 Filter$DRG.Definition,ignore.case = T)

# create factor for DRG variable and provoder state
Filter$DRG.Definition <- as.factor(Filter$DRG.Definition)
Filter$Provider.State <- as.factor(Filter$Provider.State)


# Shape data for each state and summarise on median function
CA<- Filter %>%
        filter(Provider.State=="CA")%>%
        group_by(DRG.Definition)%>%
        summarise(MedianAverageCovered=median(Average.Covered.Charges),
                  MedianAveragePayment=median(Average.Total.Payments))
FL<- Filter %>%
        filter(Provider.State=="FL")%>%
        group_by(DRG.Definition)%>%
        summarise(MedianAverageCovered=median(Average.Covered.Charges),
                  MedianAveragePayment=median(Average.Total.Payments))
IL<- Filter %>%
        filter(Provider.State=="IL")%>%
        group_by(DRG.Definition)%>%
        summarise(MedianAverageCovered=median(Average.Covered.Charges),
                  MedianAveragePayment=median(Average.Total.Payments))
NY<- Filter %>%
        filter(Provider.State=="NY")%>%
        group_by(DRG.Definition)%>%
        summarise(MedianAverageCovered=median(Average.Covered.Charges),
                  MedianAveragePayment=median(Average.Total.Payments))
PA<- Filter %>%
        filter(Provider.State=="PA")%>%
        group_by(DRG.Definition)%>%
        summarise(MedianAverageCovered=median(Average.Covered.Charges),
                  MedianAveragePayment=median(Average.Total.Payments))
TX<- Filter %>%
        filter(Provider.State=="TX")%>%
        group_by(DRG.Definition)%>%
        summarise(MedianAverageCovered=median(Average.Covered.Charges),
                  MedianAveragePayment=median(Average.Total.Payments))

# create Plots
colPal<-c("red","orange","green","yellow","blue","violet") # colors
pdf("plot2.pdf")
par(mfrow=c(3,2)) # layout of plots
par(oma=c(2,1,2,1)) # outer margins
# california plot
barplot(as.matrix(CA[,2:3]), beside = T,col = colPal,
        ylim = c(0,80000),
        main="California Median Average Covererd Cost and Total Payment",
        ylab = "US Dollars", cex.names = .75,cex.axis = .75,cex.main=.75 )
legend("topright",levels(IL$DRG.Definition),fill = colPal, title = "Treated Illness",
       cex = .65)
box()
# Florida Plot
barplot(as.matrix(FL[,2:3]), beside = T,col = colPal,
        ylim = c(0,80000),
        main="Florida Median Average Covererd Cost and Total Payment",
        ylab = "US Dollars", cex.names = .75,cex.axis = .75,cex.main=.75 )
legend("topright",levels(IL$DRG.Definition),fill = colPal, title = "Treated Illness",
       cex = .65)
box()
# Illinois plot
barplot(as.matrix(IL[,2:3]), beside = T,col = colPal,
        ylim = c(0,80000),
        main="Illinois Median Average Covererd Cost and Total Payment",
        ylab = "US Dollars", cex.names = .75,cex.axis = .75,cex.main=.75 )
legend("topright",levels(IL$DRG.Definition),fill = colPal, title = "Treated Illness",
       cex = .65)
box()
# NY plot
barplot(as.matrix(NY[,2:3]), beside = T,col = colPal,
        ylim = c(0,80000),
        main="New York Median Average Covererd Cost and Total Payment",
        ylab = "US Dollars", cex.names = .75,cex.axis = .75,cex.main=.75 )
legend("topright",levels(IL$DRG.Definition),fill = colPal, title = "Treated Illness",
       cex = .65)
box()
#pennslvania Plot
barplot(as.matrix(PA[,2:3]), beside = T,col = colPal,
        ylim = c(0,80000),
        main="Pennslvania Median Average Covererd Cost and Total Payment",
        ylab = "US Dollars", cex.names = .75,cex.axis = .75,cex.main=.75 )
legend("topright",levels(IL$DRG.Definition),fill = colPal, title = "Treated Illness",
       cex = .65)
box()
# Texas Plot
barplot(as.matrix(TX[,2:3]), beside = T,col = colPal,
        ylim = c(0,80000),
        main="Texas Median Average Covererd Cost and Total Payment",
        ylab = "US Dollars", cex.names = .75,cex.axis = .75,cex.main=.75 )
legend("topright",levels(IL$DRG.Definition),fill = colPal, title = "Treated Illness",
       cex = .65)
box()
dev.off()


