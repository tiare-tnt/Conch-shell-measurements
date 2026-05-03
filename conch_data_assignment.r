rm(list=ls())
#setwd('/Users/####/Desktop/####')
conch.data<-read.csv("conch.csv")
head(conch.data)
#sapply(masticBay.df, class)
unique(conch.data$Sample.Location)
conch.data$Juv.or.Adult<-as.character(conch.data$Juv.or.Adult)

for(i in 1:nrow(conch.data)){
  if(conch.data$Thickness..cm.[i]<1.5){
    conch.data$Juv.or.Adult[i] <- "Juv"
  }else{
    conch.data$Juv.or.Adult[i] <- "Adult"
  }

}


angiesBeach<-conch.data[conch.data$Sample.Location == "Angie's Beach",]
blazerCay<-conch.data[conch.data$Sample.Location == "Blazer Cay",]
bowryBay<-conch.data[conch.data$Sample.Location == "Bowry Bay",]
conchSound<-conch.data[conch.data$Sample.Location == "Conch Sound",]
grassyCreek<-conch.data[conch.data$Sample.Location == "Grassy Creek",]
staffordCreek<-conch.data[conch.data$Sample.Location == "Stafford Creek",]
youngSound<-conch.data[conch.data$Sample.Location == "Young Sound",]
masticBay<-conch.data[conch.data$Sample.Location == "Mastic Bay",]

angiesBeach.df<-data.frame(table(factor(angiesBeach$Juv.or.Adult)))
blazerCay.df<-data.frame(table(factor(blazerCay$Juv.or.Adult)))
bowryBay.df<-data.frame(table(factor(bowryBay$Juv.or.Adult)))
conchSound.df<-data.frame(table(factor(conchSound$Juv.or.Adult)))
grassyCreek.df<-data.frame(table(factor(grassyCreek$Juv.or.Adult)))
staffordCreek.df<-data.frame(table(factor(staffordCreek$Juv.or.Adult)))
youngSound.df<-data.frame(table(factor(youngSound$Juv.or.Adult)))
masticBay.df<-data.frame(table(factor(masticBay$Juv.or.Adult)))

angiesBeach.df
blazerCay.df
youngSound.df
bowryBay.df
conchSound.df
grassyCreek.df
staffordCreek.df
youngSound.df

total<-data.frame(1,2)
total[1,1]<-masticBay.df[1,1]
dim(masticBay.df)
total[1,2]<-masticBay.df[2,1]
total<-as.data.frame(total)
colnames(total)<c("Adult", "Juv")
total
masticBay.df<-t(masticBay.df)
masticBay.df<-as.data.frame(masticBay.df)
masticBay.df
mydata<-data.frame()
mydata<-as.data.frame(do.call(masticBay.df$Freq))
mydata
masticBay.df$Freq
my_list<-list(AngiesBeach = angiesBeach.df$Freq,BlazerCay = blazerCay.df$Freq,
              BowryBay = bowryBay.df$Freq,ConchSound = conchSound.df$Freq,GrassyCreek = grassyCreek.df$Freq,
              MasticBay = masticBay.df$Freq, StaffordCreek = staffordCreek.df$Freq,YoungSound = youngSound.df$Freq)
my_list
total.juv.adult<-as.data.frame(do.call(rbind, my_list))
plot(total.juv.adult)
colnames(total.juv.adult)<-c("Adults","Juveniles")
install.packages("stargazer")   # Install & load gridExtra
library("stargazer")
stargazer(total.juv.adult,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "conch.txt")
# ave conch size per site & SE angiesBeach$Length..cm.,blazerCay,  bowryBay, conchSound.,grassyCreek, masticBay,
#staffordCreek,youngSound 
d1<-data.frame(angiesBeach$Length..cm.)
colnames(d1)<-c("AngiesBeach")
d2<-data.frame(blazerCay$Length..cm.)
colnames(d2)<-c("BlazerCay")
d3<-data.frame(bowryBay$Length..cm.)
colnames(d3)<-c("BowryBay")
d4<-data.frame(conchSound$Length..cm.)
colnames(d4)<-c("ConchSound")
d5<-data.frame(grassyCreek$Length..cm.)
colnames(d5)<-c("GrassyCreek")
d6<-data.frame(masticBay$Length..cm.)
colnames(d6)<-c("MasticBay")
d7<-data.frame(staffordCreek$Length..cm.)
colnames(d7)<-c("StaffordCreek")
d8<- data.frame(youngSound$Length..cm.)
colnames(d8)<-c("YoungSound")
average.length<-cbind(d1,d2,d3,d4,d5,d6,d7,d8)

mean.lengths<-colMeans(average.length)
mean.lengths<-data.frame(mean.lengths)
head(mean.lengths)
#SE



standard.error<-sapply(average.length,function(x)sd(x)/sqrt(length(x)))
standard.error<-data.frame(standard.error)
combinedSE.mean<-cbind(mean.lengths,standard.error)

combinedSE.mean
stargazer(combinedSE.mean,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "mean_SE.txt")

combinedSE.mean$location<-c('Angies Beach', 'Blazer Cay', 'Bowry Bay', 
                            'Conch Sound', 'Grassy Creek', 'Mastic Bay', 'Stafford Creek',
                            'Young Sound')

combinedSE.mean
library(ggplot2)

# create dummy data

dev.new()
pdf_file<-
  '/Users/tiarethompson/Desktop/Enst310/conchgraph.pdf'
pdf(file=pdf_file)
# Most basic error bar
my_title <- expression(paste(italic("Strombus gigas "), "average length per site in the Andros Islands"))
ggplot(combinedSE.mean) +
  geom_bar( aes(x=location, y=mean.lengths), stat="identity", fill="blue", alpha=0.7) +
  xlab("Location of Conch Survey") +
  ggtitle(my_title)+
  ylab("Mean lengths of Shells (cm)") +
  geom_errorbar( aes(x=location, ymin=mean.lengths-standard.error, ymax=mean.lengths+standard.error), width=0.4, colour="orange", alpha=0.9, linewidth=1.3)


dev.off()
system(paste('open', pdf_file))

angiesBeach
old_ab<-length(angiesBeach$Length..cm.[angiesBeach$Length..cm.>20])
#blazerCay$Length..cm.
old_bc<-length(blazerCay$Length..cm.[blazerCay$Length..cm.>20])
#bowryBay$Length..cm.
old_bb<-length(bowryBay$Length..cm.[bowryBay$Length..cm.>20])
#conchSound$Length..cm.
old_cs<-length(conchSound$Length..cm.[conchSound$Length..cm.>20])
#grassyCreek$Length..cm.
old_gc<-length(grassyCreek$Length..cm.[grassyCreek$Length..cm.>20])
#masticBay$Length..cm.
old_mb<-length(masticBay$Length..cm.[masticBay$Length..cm.>20])
#staffordCreek$Length..cm.
old_sc<-length(staffordCreek$Length..cm.[staffordCreek$Length..cm.>20])
#youngSound$Length..cm
old_ys<-length(youngSound$Length..cm[youngSound$Length..cm>20])
old_total<- data.frame(c(old_ab,old_bb,old_bc,old_cs,old_gc,old_mb,old_sc,old_ys))
head(old_total)

old_total$location<-c('Angies Beach',  'Bowry Bay', 'Blazer Cay',
                            'Conch Sound', 'Grassy Creek', 'Mastic Bay', 'Stafford Creek',
                            'Young Sound')


colnames(old_total)<-c('number', 'location')
dev.new()
pdf_file<-
  '/Users/tiarethompson/Desktop/Enst310/oldconch.pdf'
pdf(file=pdf_file)
# Most basic error bar
my_title <- expression(paste(italic("Strombus gigas "), "adults with shells larger then 20 cm"))
ggplot(old_total) +
  geom_bar( aes(x=location, y=number), stat="identity", fill="skyblue", alpha=1) +
  xlab("Location of Conch Survey") +
  ggtitle(my_title)+
  ylab("Number of Adults with shells >= 20 cm") +
   scale_y_continuous(breaks=seq(0,40,5))
 


dev.off()
system(paste('open', pdf_file))


