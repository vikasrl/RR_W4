Url_data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FMain_data.csv.bz2"
File_data <- "StormData.csv.bz2"
if (!file.exists(File_data)) {
        download.file(Url_data, File_data, mode = "wb")
}
data <- read.csv(file = File_data, header=TRUE, sep=",")
imp_data <- data
imp_data$BGN_DATE <- strptime(data$BGN_DATE, "%m/%d/%Y %H:%M:%S")
imp_data <- subset(imp_data, BGN_DATE > "1995-12-31")
imp_data <- subset(imp_data, select = c(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
imp_data$EVTYPE <- toupper(imp_data$EVTYPE)
imp_data <- imp_data[imp_data$FATALITIES !=0 | 
                       imp_data$INJURIES !=0 | 
                       imp_data$PROPDMG !=0 | 
                       imp_data$CROPDMG !=0, ]
H_data <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data = imp_data, FUN=sum)
H_data$PEOPLE_LOSS <- H_data$FATALITIES + H_data$INJURIES
H_data <- H_data[order(H_data$PEOPLE_LOSS, decreasing = TRUE), ]
Top10_events_people <- H_data[1:10,]
print(Top10_events_people)
imp_data$PROPDMGEXP <- gsub("[Hh]", "2", imp_data$PROPDMGEXP)
imp_data$PROPDMGEXP <- gsub("[Kk]", "3", imp_data$PROPDMGEXP)
imp_data$PROPDMGEXP <- gsub("[Mm]", "6", imp_data$PROPDMGEXP)
imp_data$PROPDMGEXP <- gsub("[Bb]", "9", imp_data$PROPDMGEXP)
imp_data$PROPDMGEXP <- gsub("\\+", "1", imp_data$PROPDMGEXP)
imp_data$PROPDMGEXP <- gsub("\\?|\\-|\\ ", "0",  imp_data$PROPDMGEXP)
imp_data$PROPDMGEXP <- as.numeric(imp_data$PROPDMGEXP)
imp_data$CROPDMGEXP <- gsub("[Hh]", "2", imp_data$CROPDMGEXP)
imp_data$CROPDMGEXP <- gsub("[Kk]", "3", imp_data$CROPDMGEXP)
imp_data$CROPDMGEXP <- gsub("[Mm]", "6", imp_data$CROPDMGEXP)
imp_data$CROPDMGEXP <- gsub("[Bb]", "9", imp_data$CROPDMGEXP)
imp_data$CROPDMGEXP <- gsub("\\+", "1", imp_data$CROPDMGEXP)
imp_data$CROPDMGEXP <- gsub("\\-|\\?|\\ ", "0", imp_data$CROPDMGEXP)
imp_data$CROPDMGEXP <- as.numeric(imp_data$CROPDMGEXP)
imp_data$PROPDMGEXP[is.na(imp_data$PROPDMGEXP)] <- 0
imp_data$CROPDMGEXP[is.na(imp_data$CROPDMGEXP)] <- 0
library(dplyr)
imp_data <- mutate(imp_data, 
                    PROPDMGTOTAL = PROPDMG * (10 ^ PROPDMGEXP), 
                    CROPDMGTOTAL = CROPDMG * (10 ^ CROPDMGEXP))
E_data <- aggregate(cbind(PROPDMGTOTAL, CROPDMGTOTAL) ~ EVTYPE, data = imp_data, FUN=sum)
E_data$ECONOMIC_LOSS <- E_data$PROPDMGTOTAL + E_data$CROPDMGTOTAL
E_data <- E_data[order(E_data$ECONOMIC_LOSS, decreasing = TRUE), ]
Top10_events_economy <- E_data[1:10,]
print(Top10_events_economy)
library(ggplot2)
g <- ggplot(data = Top10_events_people, aes(x = reorder(EVTYPE, PEOPLE_LOSS), y = PEOPLE_LOSS))
g <- g + geom_bar(stat = "identity", colour = "black")
g <- g + labs(title = "Total people loss in USA by weather events in 1996-2011")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = "Number of fatalities and injuries", x = "Event Type")
g <- g + coord_flip()
print(g)
g <- ggplot(data = Top10_events_economy, aes(x = reorder(EVTYPE, ECONOMIC_LOSS), y = ECONOMIC_LOSS))
g <- g + geom_bar(stat = "identity", colour = "black")
g <- g + labs(title = "Total economic loss in USA by weather events in 1996-2011")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = "Size of property and crop loss", x = "Event Type")
g <- g + coord_flip()
print(g)
