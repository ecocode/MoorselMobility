library(dplyr)
library(lubridate)
library(parsedate)
library(ggplot2)
library(purrr)

unlink("*.png")

# data quality filter by telraam. volgens telraam moet dit in klaarlichte dag > 0.5
min_uptime <- 0.1

dataLeeuwerikenlaan <- read.csv("leeuwerikenlaan-totaal.csv",                 header=TRUE, stringsAsFactors=FALSE)
dataLeeuwerikenlaan <- dataLeeuwerikenlaan %>% filter(uptime>=min_uptime) %>% select ( date, car_lft, car_rgt, heavy_lft, heavy_rgt, uptime )
dataLeeuwerikenlaan <- dataLeeuwerikenlaan %>% rename ( leeuw_car_lft=car_lft, leeuw_car_rgt=car_rgt, leeuw_heavy_lft=heavy_lft, leeuw_heavy_rgt=heavy_rgt )
dataLeeuwerikenlaan$date <- parse_iso_8601(dataLeeuwerikenlaan$date)

dataBoslaan <- read.csv("boslaan-totaal.csv",                 header=TRUE, stringsAsFactors=FALSE)
dataBoslaan <- dataBoslaan %>% filter(uptime>=min_uptime) %>% select ( date, car_lft, car_rgt, heavy_lft, heavy_rgt, uptime )
dataBoslaan <- dataBoslaan %>% rename ( bos_car_lft=car_lft, bos_car_rgt=car_rgt, bos_heavy_lft=heavy_lft, bos_heavy_rgt=heavy_rgt )
dataBoslaan$date <- parse_iso_8601(dataBoslaan$date)

dataGroenlaan <- read.csv("groenlaan-totaal.csv",             header=TRUE, stringsAsFactors=FALSE)
dataGroenlaan <- dataGroenlaan %>% filter(uptime>=min_uptime) %>% select ( date, car_lft, car_rgt, heavy_lft, heavy_rgt, uptime )
dataGroenlaan <- dataGroenlaan %>% rename ( groen_car_lft=car_lft, groen_car_rgt=car_rgt, groen_heavy_lft=heavy_lft, groen_heavy_rgt=heavy_rgt )
dataGroenlaan$date <- parse_iso_8601(dataGroenlaan$date)

dataMoorselstraat <- read.csv("moorselstraat-totaal.csv",     header=TRUE, stringsAsFactors=FALSE)
dataMoorselstraat <- dataMoorselstraat %>% filter(uptime>=min_uptime) %>% select ( date, car_lft, car_rgt, heavy_lft, heavy_rgt, uptime )
dataMoorselstraat <- dataMoorselstraat %>% rename ( moorsstr_car_lft=car_lft, moorsstr_car_rgt=car_rgt, moorsstr_heavy_lft=heavy_lft, moorsstr_heavy_rgt=heavy_rgt )
dataMoorselstraat$date <- parse_iso_8601(dataMoorselstraat$date)

dataNachtegalenlaan <- read.csv("nachtegalenlaan-totaal.csv", header=TRUE, stringsAsFactors=FALSE)
dataNachtegalenlaan <- dataNachtegalenlaan %>% filter(uptime>=min_uptime) %>% select ( date, car_lft, car_rgt, heavy_lft, heavy_rgt, uptime )
dataNachtegalenlaan <- dataNachtegalenlaan %>% rename ( nacht_car_lft=car_lft, nacht_car_rgt=car_rgt, nacht_heavy_lft=heavy_lft, nacht_heavy_rgt=heavy_rgt )
dataNachtegalenlaan$date <- parse_iso_8601(dataNachtegalenlaan$date)

dataVinkenlaan <- read.csv("vinkenlaan-totaal.csv",           header=TRUE, stringsAsFactors=FALSE)
dataVinkenlaan <- dataVinkenlaan %>% filter(uptime>=min_uptime) %>% select ( date, car_lft, car_rgt, heavy_lft, heavy_rgt, uptime )
dataVinkenlaan <- dataVinkenlaan %>% rename ( vink_car_lft=car_lft, vink_car_rgt=car_rgt, vink_heavy_lft=heavy_lft, vink_heavy_rgt=heavy_rgt )
dataVinkenlaan$date <- parse_iso_8601(dataVinkenlaan$date)

## str(dataMoorselstraat)
## spitsMorgen <- filter(theStraat, hour(parse_iso_8601(date))==8)
## str(spitsMorgen)
## spitsMorgen$date <- parse_iso_8601(spitsMorgen$date)
## str(spitsMorgen)

allData <- full_join(full_join(full_join(full_join(full_join(dataBoslaan,dataMoorselstraat,by=c("date")),dataGroenlaan,by=c("date")),dataNachtegalenlaan,by=c("date")),dataVinkenlaan,by=c("date")),dataLeeuwerikenlaan,by=c("date"))

allData$date <- with_tz(allData$date,tzone="Europe/Brussels")

## add column with FASE depending on date
# eerste data in Moorselstraat begint 13 oktober 2022
allData <- allData %>% mutate(fase=ifelse(between(date,as.Date('2022-01-01'),as.Date('2022-10-03')),1,
                                   ifelse(between(date,as.Date('2022-10-03'),as.Date('2022-11-01')),2,
                                   ifelse(between(date,as.Date('2022-11-01'),as.Date('2023-01-04')),3,
                                   ifelse(between(date,as.Date('2023-01-04'),as.Date('2023-02-12')),4,
                                   ifelse(between(date,as.Date('2023-02-12'),as.Date('2023-04-01')),5,
                                   ifelse(between(date,as.Date('2023-04-01'),as.Date('2023-05-01')),6,
                                          0)))))))

## remove all data before 2022-10-3
allData <- filter(allData, fase!=0)
## remove feestdagen en schoolvakanties
allData <- filter(allData, date(date) != date(as.Date('2022-05-26')))
allData <- filter(allData, date(date) != date(as.Date('2022-06-06')))
allData <- filter(allData, date(date) != date(as.Date('2022-11-01')))
allData <- filter(allData, date(date) != date(as.Date('2022-11-11')))
allData <- filter(allData, date(date) != date(as.Date('2023-05-01')))
allData <- filter(allData, ! between(date, as.Date('2022-01-03'), as.Date('2022-01-08')))
allData <- filter(allData, ! between(date, as.Date('2022-02-28'), as.Date('2022-03-05')))
allData <- filter(allData, ! between(date, as.Date('2022-04-04'), as.Date('2022-04-19')))
allData <- filter(allData, ! between(date, as.Date('2022-07-01'), as.Date('2022-09-01')))
allData <- filter(allData, ! between(date, as.Date('2022-12-26'), as.Date('2023-01-07')))
allData <- filter(allData, ! between(date, as.Date('2023-02-20'), as.Date('2023-02-25')))
allData <- filter(allData, ! between(date, as.Date('2023-04-03'), as.Date('2023-04-15')))

fase_colors <- c ("#787878","darkgreen","red","blue","brown","magenta","purple")
fases <- c ("2022-01-01 -> 2022-10-02 zonder opstelling", "2022-10-03 -> 2022-10-31 opstelling 1",           "2022-11-01 -> 2023-01-03 opstelling 1+2",       "2023-01-04 -> 2023-02-11 opstelling 2+3",        "2023-02-12 -> 2023-03-31 opstelling 3",         "2023-04-01 -> 2023-04-30 zonder opstelling")
names(fase_colors) <- fases
# colors <- c("2022-10-03 -> 2022-10-31 opstelling 1"="#00AF00", "2022-11-01 -> 2023-01-03 opstelling 1+2"="red", "2023-01-04 -> 2023-02-11 opstelling 2+3"="blue", "2023-02-12 -> 2023-03-31 opstelling 3"="brown", "2023-04-01 -> 2023-04-30 zonder opstelling"="magenta")

vrachtwagengewicht <- 2
allData <- allData %>% mutate(totaal_nacht = nacht_car_lft + nacht_car_rgt + vrachtwagengewicht*(nacht_heavy_lft + nacht_heavy_rgt))
allData <- allData %>% mutate(totaal_vink = vink_car_lft + vink_car_rgt + vrachtwagengewicht*(vink_heavy_lft + vink_heavy_rgt))
allData <- allData %>% mutate(totaal_leeuw = leeuw_car_lft + leeuw_car_rgt + vrachtwagengewicht*(leeuw_heavy_lft + leeuw_heavy_rgt))
allData <- allData %>% mutate(totaal_groen = groen_car_lft + groen_car_rgt + vrachtwagengewicht*(groen_heavy_lft + groen_heavy_rgt))
allData <- allData %>% mutate(totaal_bos = bos_car_lft + bos_car_rgt + vrachtwagengewicht*(bos_heavy_lft + bos_heavy_rgt))
allData <- allData %>% mutate(totaal_moorsstr = moorsstr_car_lft + moorsstr_car_rgt + vrachtwagengewicht*(moorsstr_heavy_lft + moorsstr_heavy_rgt))
allData <- allData %>% rowwise() %>% mutate(totaal_vink_nacht = sum(totaal_nacht, totaal_vink,na.rm=FALSE))
allData <- allData %>% rowwise() %>% mutate(totaal_vink_nacht_bos = sum(totaal_nacht, totaal_vink, totaal_bos,na.rm=FALSE))
allData <- allData %>% rowwise() %>% mutate(totaal_moorsstr_groen = sum(totaal_moorsstr, totaal_groen,na.rm=FALSE))
allData <- allData %>% rowwise() %>% mutate(totaal = sum(totaal_nacht, totaal_leeuw, totaal_vink, totaal_groen, totaal_bos, totaal_moorsstr,na.rm=FALSE))
allData <- allData %>% mutate(hour = hour(date))
allData <- allData %>% select(fase,date,hour,totaal_nacht,totaal_leeuw,totaal_vink,totaal_groen,totaal_bos,totaal_moorsstr,totaal_vink_nacht,totaal_moorsstr_groen,totaal_vink_nacht_bos,totaal)
############# UITFILTEREN ENKEL WEEKDAGEN NEMEN !!!!
weekData <- filter(allData, wday(date, week_start=1)<=5)

## str(weekData)
## select(weekData,hour,totaal)

testMorgen <- filter(weekData, between(hour,0,23))
testMorgen %>% filter(!is.na(totaal))
## totaalPerUurDag <- aggregate(. ~ hour, data=spitsMorgen, FUN=sum, na.rm=TRUE, na.action=NULL)
totaalPerUurDag <- testMorgen %>% group_by(hour) %>% summarise(sum_totaal_per_uur=mean(totaal,na.rm=T))
totaalPerUurDag
ggplot(data=totaalPerUurDag,aes(x=hour,y=sum_totaal_per_uur))+geom_bar(stat="identity") +
    labs(title="Gemiddelde verkeersdruk in Moorsel per uur",x="uur",y="verkeersdruk per wageneenheid")
ggsave(device=png,filename="spitsTikkingen.png",dpi=300)

spitsMorgen <- filter(weekData, between(hour,8,8))
aantalUren <- 1  # gebruikt om slechte data te filteren
## str(spitsMorgen)


## spitsMorgen <- filter(weekData, hour(parse_iso_8601(date))==8)

## spitsMorgen <- filter(spitsMorgen, moorsstr_car_lft>0 & moorsstr_car_rgt>0)
## spitsMorgen <- filter(spitsMorgen, nacht_car_lft>0 & nacht_car_rgt>0)
## spitsMorgen <- filter(spitsMorgen, totaal_moorsstr>0 & totaal_nacht>0)
## str(spitsMorgen)
## select(spitsMorgen,date,fase,totaal_moorsstr,totaal_nacht)


ggplot(weekData,aes(y=fase,x=date))+geom_point()
ggsave(device=png,filename="fases.png",dpi=300)

weekData <- weekData %>% mutate(week=paste(year(date)*100+week(date),sep=""))
# print(select(weekData,date,week,totaal),n=Inf)

plotData <- weekData %>% group_by(week) %>% summarise( ytotaal=sum(totaal,na.rm=TRUE),.groups="drop")
# print(select(plotData,week,ytotaal),n=Inf)

ggplot(plotData, aes(y=ytotaal,x=week ))+geom_point() +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12))
ggsave(device=png,filename="totaalPerDag.png",dpi=300)

plotit <- function (colA,colB,titelColA,titelColB) {
    plotData <- spitsMorgen %>% filter ( {{colA}}>0 & {{colB}}>0 ) %>% filter (fase!=0)
    ## groeperen per datum wanneer er voor elke kolom ook een waarde is per uur !!!
    plotData <- plotData %>% group_by(fase,day=date(date)) %>% summarise( sumA=sum({{colA}}/aantalUren,na.rm=FALSE),sumB=sum({{colB}}/aantalUren,na.rm=FALSE),countRows=n(),.groups='drop') %>% filter(countRows>=aantalUren)
    ## str(plotData)
    print(select(plotData,fase,day,sumA,sumB,countRows),quote=FALSE,n=Inf)
    existingFases=unique(select(plotData,fase))
    labelsForFases <- existingFases %>% map(~ fases[.x]) %>% unlist()
    colorsForFases <- existingFases %>% map(~ fase_colors[.x]) %>% unlist() %>% setNames(labelsForFases)
    str(labelsForFases)
    str(colorsForFases)

    xmin <- min(plotData$sumB)
    ymin <- min(plotData$sumA)
    xmin <- 0
    ymin <- 0
    xmax <- max(plotData$sumB)
    ymax <- max(plotData$sumA)
    xticks <- ifelse(xmax-xmin>900,200,100)

    ggplot(plotData,aes(y=sumA,x=sumB,color=factor(fases[fase]))) +
        #xx    ggplot(plotData,aes(y=sumA,x=sumB,color=factor(fase_colors[fase],labels=labelsForFases))) +
        # coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
        coord_equal(ratio = 1, xlim=c(xmin,xmax), ylim=c(ymin,ymax)) +
        scale_y_continuous(breaks=seq(0,3000,100)) +
        scale_x_continuous(breaks=seq(0,3000,xticks)) +
        geom_point(alpha=0.3,size=4,show.legend = TRUE) +
        # geom_smooth(method='lm',se=FALSE) +
        stat_ellipse(level = .7, lwd=1.2, type="t") +
        #xx   scale_color_manual(name="Fase",values=colorsForFases) +
        scale_color_manual(name="Periode",values=fase_colors) +
        # geom_smooth(method='loess',span=50,se=FALSE) +
        theme(legend.position="bottom",legend.direction = "vertical") +
        labs(color="Periode", title=paste(titelColA," vs ",titelColB,sep=""),
             subtitle="verkeersdruk tss 8u en 9u in wageneenheden per uur",
             caption="Data source: telraam.net",
             x=titelColB,y=titelColA)
    ggsave(device=png,filename=paste(paste(titelColA,titelColB,sep="_vs_"),".png",sep=""),dpi=300,height=10)
}

plotit(totaal_moorsstr,totaal_nacht,"Moorselstraat","Nachtegalenlaan")
plotit(totaal_moorsstr,totaal_bos,"Moorselstraat","Boslaan")
plotit(totaal_moorsstr,totaal_groen,"Moorselstraat","Groenlaan")
plotit(totaal_moorsstr,totaal_vink,"Moorselstraat","Vinkenlaan")
plotit(totaal_moorsstr,totaal_leeuw,"Moorselstraat","Leeuwerikenlaan")
plotit(totaal_groen,totaal_bos,"Groenlaan","Boslaan")
plotit(totaal_bos,totaal_nacht,"Boslaan","Nachtegalenlaan")
plotit(totaal_bos,totaal_vink,"Boslaan","Vinkenlaan")
plotit(totaal_groen,totaal_nacht,"Groenlaan","Nachtegalenlaan")
plotit(totaal_groen,totaal_vink,"Groenlaan","Vinkenlaan")
plotit(totaal_vink,totaal_nacht,"Vinkenlaan","Nachtegalenlaan")
plotit(totaal_nacht,totaal_leeuw,"Nachtegalenlaan","Leeuwerikenlaan")
plotit(totaal_bos,totaal_leeuw,"Boslaan","Leeuwerikenlaan")
plotit(totaal_groen,totaal_leeuw,"Groenlaan","Leeuwerikenlaan")
plotit(totaal_vink,totaal_leeuw,"Vinkenlaan","Leeuwerikenlaan")
# plotit(totaal_bos,totaal_vink_nacht,"Boslaan","Vinken-+Nachtegalenlaan")
# plotit(totaal_moorsstr_groen,totaal_moorsstr,"Moorselstraat+Groenlaan","Moorselstraat")
# plotit(totaal_moorsstr_groen,totaal_groen,"Moorselstraat+Groenlaan","Groenlaan")

plotit(totaal_moorsstr_groen,totaal_vink_nacht_bos,"Moorselstraat+Groenlaan","Bos-+Vinken-+Nachtegalenlaan")


### GEMIDDELDE PER UUR PER STRAAT PER FASE
plotitGemiddelde <- function () {
    for (xfase in 1:6) {

        print ("###############################################")
        print(select(weekData %>% filter(fase==xfase) %>% filter(between(hour,5,17)) %>% filter(totaal!=0),date,hour,fase,totaal_moorsstr,totaal_nacht))
        print ("###############################################")

        totaalPerUurDag <- weekData %>% filter(fase==xfase) %>% filter(between(hour,5,17)) %>% filter(totaal>=400) %>% group_by(uur=hour) %>%
            summarise(moorsstr=ifelse(mean(totaal_moorsstr,na.rm=T)>0,mean(totaal_moorsstr,na.rm=T),NA),
                      boslaan=ifelse(mean(totaal_bos,na.rm=T)>0,mean(totaal_bos,na.rm=T),NA),
                      groenlaan=ifelse(mean(totaal_groen,na.rm=T)>0,mean(totaal_groen,na.rm=T),NA),
                      nachtlaan=ifelse(mean(totaal_nacht,na.rm=T)>0,mean(totaal_nacht,na.rm=T),NA),
                      vinklaan=ifelse(mean(totaal_vink,na.rm=T)>0,mean(totaal_vink,na.rm=T),NA),
                      leeuwlaan=ifelse(mean(totaal_leeuw,na.rm=T)>0,mean(totaal_leeuw,na.rm=T),NA),
                      .groups='drop') %>% select(uur,moorsstr,boslaan,groenlaan,nachtlaan,vinklaan,leeuwlaan) %>% as.data.frame()
        print(select(totaalPerUurDag,uur,moorsstr,nachtlaan))
        str(totaalPerUurDag)
        ggplot(totaalPerUurDag,aes(x=uur)) +
            scale_x_continuous(breaks=seq(0,24,1)) +
            geom_line(aes(y=moorsstr,colour="Moorselstraat"),size=1) +
            geom_line(aes(y=boslaan,colour="Boslaan"),size=1) +
            geom_line(aes(y=groenlaan,colour="Groenlaan"),size=1) +
            geom_line(aes(y=vinklaan,colour="Vinkenlaan"),size=1) +
            geom_line(aes(y=leeuwlaan,colour="Leeuwerikenlaan"),size=1) +
            geom_line(aes(y=nachtlaan,colour="Nachtegalenlaan"),size=1) +
            xlab("uur") + ylab("wageneenheid") + ggtitle(paste("gemiddelde verkeersdruk periode ",xfase,sep=""),subtitle=fases[xfase]) +
            labs(caption="Data source: telraam.net") +
            scale_color_manual(name="Meetpunt",values = c("Moorselstraat"="red","Nachtegalenlaan"="magenta","Boslaan"="blue","Groenlaan"="darkgreen","Vinkenlaan"="brown","Leeuwerikenlaan"="purple")) +
            theme(legend.position="bottom",legend.direction = "vertical") +
            coord_cartesian(xlim=c(5,17), ylim=c(0,500))
            ## labs(title=paste("gemiddelde druk voor fase ",xfase,sep=""),x="uur",y="wagenseenheid")
        ggsave(device=png,filename=paste("periode",xfase,".png",sep=""),dpi=300,height=10)
    }
}

plotitGemiddelde()

## avondplot
## ggplot(spitsAvond,aes(y=totaal_moorsstr,x=totaal_nacht)) + geom_point(color=fase_colors[spitsAvond$fase],alpha=0.4,size=5) + geom_smooth(method='lm') # + xlim(0,400) + ylim(0,400)
## ggsave(device=png,filename="avond_nacht_moorsstr",dpi=300)

## avondplot
## ggplot(spitsAvond,aes(y=totaal_groen,x=totaal_nacht)) + geom_point(color=fase_colors[spitsAvond$fase],alpha=0.4,size=5) + geom_smooth(method='lm') # + xlim(0,400) + ylim(0,400)
## ggsave(device=png,filename="avond_nacht_groen",dpi=300)
