# Impact of Storms in the United States

Set global parameters


```r
library(knitr)
library(data.table)
library(ggplot2)
library(plyr)
library(scales)
opts_chunk$set(cache = FALSE, fig.align = 'center', echo = TRUE)
```

### Synopsis

The purpose of this anaylsis is to determine which types of events are most harmful with respect to population health and which events have the greatest economic consequences. To answer these questions, we accessed and analyzed storm data from the National Oceanic and Atmospheric Administration. This document shows our analysis step-by-step so that you may reproduce it in its entirety.

### Analytical Approach

To answer the question, "Which events are most harmful to population health?", we can simply sum up the fatalities and injuries by type of event and sort in decreasing order of the number of fatalities and injuries. 

Similarly, to answer the question, "Which events have the greatest economic consequences?", we can simply sum the crop damage and property damage by the type of event and sort in decreasing order of total damage value. 

#### An Alternative Approach

From the perspective of the municipal manager seeking to allocate resources to minimize the impact of storms and emergencies, perhaps these data can be used to answer the question, "What is the priority list of events that we should be prepared for in the U.S. today?" Instead of summarizing which events have had the greatest impact, the analysis could propose a list of event types to prepare for based on both expected occurrences of that event type and expected impact for each occurrence in the next 12 months. The guiding philosophy behind this approach is that, event types that have the largest expected consequences should be prioritized over those that are expected to be less impactful.

### Data Processing

Download the data


```r
# data can be downloaded from this url
url <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

# set destination for downloaded file
destFile.path <- c("./data/stormdata.csv.bz2")

if (!file.exists(destFile.path)) {
    dir.create("./data/")
    download.file(url, destfile = "./data/stormdata.csv.bz2", method = "curl")
}
```

Load data into memory


```r
stormData <- read.table("./data/stormdata.csv.bz2", sep = ",", header = TRUE)
```

For easier processing, remove unnecessary columns and format data into a data.table


```r
stormData <- stormData[,c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG")]
stormData <- data.table(stormData)
```

Using the BGN_DATE variable, calculate the year of the event. 


```r
stormData <- stormData[, date:= as.Date(stormData$BGN_DATE, "%m/%d/%Y %H:%M:%S")]
stormData <- stormData[, year:= year(date)]
```

Create a new column for cleaned versions of event categories and group similar events together. Focus on the events with meaningful injury, fatality, and damage values.


```r
stormData <- stormData[, EventCategory:= tolower(EVTYPE)]

indicies <- grep("avalance|avalanche", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("avalanche")

indicies <- grep("blizzard", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("blizzard")

indicies <- grep("coastal.*flood", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("coastal flood")

indicies <- grep("dry.*microburst", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("dry microburst")

indicies <- grep("extreme.*cold|wind.*chill", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("wind chill")

indicies <- grep("flash.*flo{2,3}d|flood.*flash", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("flash flood")

indicies <- grep("^flood", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("flood")

indicies <- grep("^frost", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("frost")

indicies <- grep("^funnel", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("funnel cloud")

indicies <- grep("summary", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("summary")

indicies <- grep("wa(y?)ter.*spout", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("waterspout")

indicies <- grep("thunder|thuder|tstm|thuner|thunde|tunder", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("thunderstorm")

indicies <- grep("^(rip current)", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("rip current")

indicies <- grep("^tropical", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("tropical storm")

indicies <- grep("^tornado|torndao", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("tornado")

indicies <- grep("^high wind", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("high wind")

indicies <- grep("^hurricane", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("hurricane")

indicies <- grep("heavy.*snow", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("heavy snow")

indicies <- grep("hail", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("hail")

indicies <- grep("heavy.*rain|heavy.*shower", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("heavy rain")

indicies <- grep("lig(n|h)t(n?)ing", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("lightning")

indicies <- grep("strong.*wind", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("strong wind")

indicies <- grep("wild.*fire|forest.*fire", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("wild fire")

indicies <- grep("^wint(e?)r", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("winter storm")

indicies <- grep("^volcanic", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("volcanic ash")

indicies <- grep("(high|heavy) surf", stormData$EventCategory) 
stormData$EventCategory[indicies] <- c("high surf")

indicies <- grep("((excessive|extreme) heat)|drought|heat wave|heat", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("excessive heat")

indicies <- grep("urban|((sml|small) stream)", stormData$EventCategory)
stormData$EventCategory[indicies] <- c("urban flood")
```

To answer the two main questions, calculate the overall sums of each measure for each event category


```r
stormDataByEventCategory <- stormData[, 
    list(totalFatalities      = sum(FATALITIES, na.rm = TRUE),
         totalInjuries        = sum(INJURIES, na.rm = TRUE),
         totalCropDamage      = sum(CROPDMG, na.rm = TRUE),
         totalPropertyDamage  = sum(PROPDMG, na.rm = TRUE)),
    by = c("EventCategory")]

stormDataByEventCategory <- stormDataByEventCategory[, totalHumanHarm:= totalFatalities + totalInjuries]
stormDataByEventCategory <- stormDataByEventCategory[, totalEconomicDamage:= totalCropDamage + totalPropertyDamage]
```

To answer the alternative question from the perspective of the municipal manager, we need a bit more information:

1. Total overall number of fatalities for each event type _(included from above)_ 
2. Total overall number of injuries for each event type _(included from above)_ 
3. Total overall crop damage for each event type _(included from above)_ 
4. Total overall property damage for each event type _(included from above)_
5. Average number of annual event occurrences for each event type  
6. Total number of occurrences for each event type  
7. Average fatalities per event in event type  
8. Average injuries per event in event type  
9. Average crop damage per event in event type  
10. Average property damager per event in event type  
11. Estimated 12 month fatality for each event type _(avg # of events x avg fatalities per event)_  
12. Estimated 12 month injury count for each event type _(avg # of events x avg fatalities per event)_  
13. Estimated 12 month crop damage for each event type _(avg # of events x avg fatalities per event)_  
14. Estimated 12 month property damage for each event type _(avg # of events x avg fatalities per event)_

Calculate and merge average and total event counts


```r
eventCountsByYear <- data.table(count(stormData, c("EventCategory", "year")))

totalCountsByEventCategory <- eventCountsByYear[, list(totalEventCount = sum(freq)), by = c("EventCategory")]

avgCountsByEventCategory <- eventCountsByYear[, list(avgAnnualEventCount = mean(freq)), by = c("EventCategory")]

stormDataByEventCategory <- merge(x = stormDataByEventCategory, y = avgCountsByEventCategory, by = c("EventCategory"), all = FALSE)

stormDataByEventCategory <- merge(x = stormDataByEventCategory, y = totalCountsByEventCategory, by = c("EventCategory"), all = FALSE)
```

Calculate average measure per event in each event category


```r
stormDataByEventCategory <- stormDataByEventCategory[,avgFatalitiesPerEvent:=       totalFatalities / totalEventCount]
stormDataByEventCategory <- stormDataByEventCategory[,avgInjuriesPerEvent:=         totalInjuries / totalEventCount]
stormDataByEventCategory <- stormDataByEventCategory[,avgCropDamagePerEvent:=       totalCropDamage / totalEventCount]
stormDataByEventCategory <- stormDataByEventCategory[,avgPropertyDamagePerEvent:=   totalPropertyDamage / totalEventCount]
```

Estimates of next 12 months = Avg Measure Per Event * Avg Annual Event Count


```r
stormDataByEventCategory <- stormDataByEventCategory[,NTMEstFatalities:= avgFatalitiesPerEvent * avgAnnualEventCount]
stormDataByEventCategory <- stormDataByEventCategory[,NTMEstInjuries:= avgInjuriesPerEvent * avgAnnualEventCount]
stormDataByEventCategory <- stormDataByEventCategory[,NTMEstCropDamage:= avgCropDamagePerEvent * avgAnnualEventCount]
stormDataByEventCategory <- stormDataByEventCategory[,NTMEstPropertyDamage:= avgPropertyDamagePerEvent * avgAnnualEventCount]

stormDataByEventCategory <- stormDataByEventCategory[,NTMEstHumanHarm:= NTMEstFatalities + NTMEstInjuries]
stormDataByEventCategory <- stormDataByEventCategory[,NTMEstEconomicDamage:= NTMEstPropertyDamage + NTMEstCropDamage]
```

### Results

#### Human Harm

The results below show that tornadoes, excessive heat, floods, lightning, and wind from thunderstorms cause the greatest harm to population health. These events each appear at the top of the lists of the most deadly and most injurious types of events.


```r
plotData <- stormDataByEventCategory[order(totalHumanHarm, decreasing = TRUE), ]
plotData <- plotData[1:10, ]

# plot the graph
g <- ggplot(plotData, aes(EventCategory, totalHumanHarm))
g + geom_bar(color="black", fill="#006699", stat="identity") + 
    labs(x = "Type of Event", y = "Total Fatalities + Injuries", title = "Human Harm") +
    coord_flip() +
    scale_x_discrete(limits = rev(plotData$EventCategory), labels = rev(plotData$EventCategory))
```

<img src="storm_impact_analysis_files/figure-html/plot-population harm-1.png" title="" alt="" style="display: block; margin: auto;" />

#### Economic Damage

The results below show that tornadoes, hail, floods, and wind from thunderstorms cause the greatest amount of economic damage, with each appearing at the top of the lists of the type of events that cause the most property and crop damage.


```r
plotData <- stormDataByEventCategory[order(totalEconomicDamage, decreasing = TRUE), ]
plotData <- plotData[1:10, ]

# plot the graph
g <- ggplot(plotData, aes(EventCategory, totalEconomicDamage))
g + geom_bar(color="gray", fill="#003366", stat="identity") + 
    labs(x = "Type of Event", y = "Total Value of Property and Crop Damage", title = "Economic Damage") +
    coord_flip() +
    scale_x_discrete(limits = rev(plotData$EventCategory), labels = rev(plotData$EventCategory)) +
    scale_y_continuous(labels = comma)
```

<img src="storm_impact_analysis_files/figure-html/plot-economic damage-1.png" title="" alt="" style="display: block; margin: auto;" />

#### Preparedness Analysis

From the perspective of the municipal manager, the analysis below highlights a prioritization list for the next year based on the average number of events likely to occur in a given year and the average impact of each event in that event category. The product of these produce metrics for estimated human harm and estimated economic damage over the next 12 months.

These results show that tornados, floods, thunderstorms, and excessive heat are the event categories most likely to cause the greatest human or economic harm in a given year.


```r
# Focus on the the most impactful event categories
plotData <- stormDataByEventCategory[NTMEstHumanHarm >= 80 | NTMEstEconomicDamage >= 10000, ]

# plot the graph
g <- ggplot(plotData, aes(NTMEstHumanHarm, NTMEstEconomicDamage))
g + geom_point(size = 4, alpha = 0.6, aes(color = EventCategory)) + 
    labs(x = "Estimated Human Harm", y = "Estimated Economic Damage", title = "Most Impactful Event Categories To Prepare For")
```

<img src="storm_impact_analysis_files/figure-html/plot-ntm analysis-1.png" title="" alt="" style="display: block; margin: auto;" />
