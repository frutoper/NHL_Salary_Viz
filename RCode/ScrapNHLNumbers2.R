library(XML)
library(RCurl)

setwd("E:\\Hockey\\Salary")

teamlist <- c("ANA", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", 
              "CLB", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", 
              "MTL", "NAS", "NJD", "NYI", "NYR", "OTT", "PHI", 
              "ARI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN",
              "WAS", "WPG")

Url <- NULL
doc <- NULL
doc2<- NULL
all15_16 <- NULL
for(i in 1:length(teamlist)){
        Url <- paste('http://stats.nhlnumbers.com/teams/', teamlist[i], '?year=2016', sep="")
        doc <- readHTMLTable(Url, which=1)
        doc2 <- doc[-1,c(1,4)]
        NAindex <- which(is.na(doc2$V4))
        doc2$group <- "O"
        doc2$group[NAindex[1]:NAindex[2]] <- "F"
        doc2$group[NAindex[2]:NAindex[3]] <- "D"
        doc2$group[NAindex[3]:NAindex[4]] <- "G"        
        doc2 <- doc2[-NAindex,]
        doc2 <- doc2[doc2$V1 != "Player",]
        doc2 <- doc2[doc2$V1 != "14/15 Cap Overage",]
        doc2$Team <- teamlist[i]
        doc2$Year <- "15_16"
        all15_16 <- rbind(doc2,all15_16)
}


Url <- NULL
doc <- NULL
doc2<- NULL
all14_15 <- NULL
for(i in 1:length(teamlist)){
        Url <- paste('http://stats.nhlnumbers.com/teams/', teamlist[i], '?year=2015', sep="")
        doc <- readHTMLTable(Url, which=1)
        doc2 <- doc[-1,c(1,4)]
        NAindex <- which(is.na(doc2$V4))
        doc2$group <- "O"
        doc2$group[NAindex[1]:NAindex[2]] <- "F"
        doc2$group[NAindex[2]:NAindex[3]] <- "D"
        doc2$group[NAindex[3]:NAindex[4]] <- "G"        
        doc2 <- doc2[-NAindex,]
        doc2 <- doc2[doc2$V1 != "Player",]
        doc2 <- doc2[doc2$V1 != "13/14 Cap Overage",]
        doc2$Team <- teamlist[i]
        doc2$Year <- "14_15"
        all14_15 <- rbind(doc2,all14_15)
}

all <- rbind(all14_15, all15_16)
all$V4 <- as.numeric(as.character(all$V4))


all$name <- gsub( ",.*$", "", all$V1)

all$TeamYear <- paste(all$Team, all$Year, sep="_")
all$value <- all[,2]

write.csv(all, "Data\\NhlNumbers2.csv", row.names = F)

allJSON <- all[,c(3, 4, 5, 6, 8)]

teamTotals <- aggregate(value ~ Team + Year, data = allJSON, FUN = sum)
teamTotals14 <- teamTotals[teamTotals$Year == "14_15",]
teamTotals15 <- teamTotals[teamTotals$Year == "15_16",]

teamTotals14$value <- 69 - teamTotals14$value
teamTotals15$value <- 71 - teamTotals15$value

teamTotals14$value <- ifelse(teamTotals14$value < 0, 0, teamTotals14$value)
teamTotals15$value <- ifelse(teamTotals15$value < 0, 0, teamTotals15$value)

teamTotals <- rbind(teamTotals14, teamTotals15)
teamTotals$group <- 'S'
teamTotals$name <- 'Cap Space'


allJSON2 <- rbind(allJSON, teamTotals)


write.csv(allJSON2, "Data\\NhlNumbers3.csv", row.names = F)


