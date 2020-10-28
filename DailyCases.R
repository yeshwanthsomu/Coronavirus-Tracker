## This is the R code used to generate the Coronavirus Visualizations shown in the website.
# https://rpubs.com/yeshwanth88/669991

library(RColorBrewer)
library(reshape2)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(plotly)
library(riingo)
library(quantmod)
require("quantmod")
library(writexl)
library(readxl)
library(scales)
library(plotly)
library(plyr)
library(htmltab)
library(tibble)
library(taRifx)


## Organizing the world data

confirmed_world <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                            stringsAsFactors = FALSE, check.names =  FALSE)
death_world <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                        stringsAsFactors = FALSE, check.names =  FALSE)
recovered_world <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                            stringsAsFactors = FALSE, check.names =  FALSE)

confirmed_world <- confirmed_world[,-c(5:(ncol(confirmed_world)-150))]
death_world <- death_world[,-c(5:(ncol(death_world)-120))]
recovered_world <- recovered_world[,-c(5:(ncol(recovered_world)-150))]

confirmed_world <- melt(confirmed_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"),
                        variable.name = "Date", value.name = "Confirmed")
death_world <- melt(death_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"),
                    variable.name = "Date", value.name = "Death")
recovered_world <- melt(recovered_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"),
                        variable.name = "Date", value.name = "Recovered")

combined <- sort(union(levels(confirmed_world$Date), levels(death_world$Date)))
combined <- sort(union(combined, levels(recovered_world$Date)))

confirmed_world <- mutate(confirmed_world, Date= factor(Date, levels= combined))
death_world <- mutate(death_world, Date= factor(Date, levels= combined))
recovered_world <- mutate(recovered_world, Date= factor(Date, levels= combined))

world_history_data <- left_join(confirmed_world, death_world,
                                by= c("Province/State", "Country/Region", "Lat", "Long", "Date"))
world_history_data <- left_join(world_history_data, recovered_world,
                                by= c("Province/State", "Country/Region", "Lat", "Long", "Date"))

world_history_data$Date <- as.Date(as.character(world_history_data$Date), format = c("%m/%d/%y"))
colnames(world_history_data) <- make.names(colnames(world_history_data))

coronavirus <- world_history_data
lastday <- max(coronavirus$Date)

cols <- matrix(c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3")),ncol=1)
world.summary.data <- ddply(world_history_data,.(Country.Region, Date),function(x){
  colSums(x[,c("Confirmed","Death","Recovered")])
})

world.summary.data <- world.summary.data[world.summary.data$Date<=lastday,]
yesterday.data <- world.summary.data[world.summary.data$Date==lastday,]
sort.index <- sort(yesterday.data$Confirmed,decreasing=TRUE,index.return=TRUE)$ix
yesterday.data.major  <- yesterday.data[sort.index[1:20],]
yesterday.data.major  <- data.frame(Country.Region=yesterday.data$Country.Region[sort.index[1:20]])
yesterday.data.major$Country.Region  <- as.character(yesterday.data.major$Country.Region )
major.summary.data <- dplyr::inner_join(world.summary.data,yesterday.data.major,by = "Country.Region")


rownames(cols) <- unique(major.summary.data$Country.Region)

major.summary.data$Country.Region <- factor(major.summary.data$Country.Region,
                                            levels = rev(yesterday.data.major$Country.Region))

attach(major.summary.data)
recentdate <- major.summary.data$Date[nrow(major.summary.data)]
recentdate_countries <- major.summary.data[(major.summary.data$Date == recentdate),]
topcountries <- as.character(recentdate_countries[order(-recentdate_countries$Confirmed),1])

######################################################################################
# FUNCTION FOR LOOKING AT DAILY NEW CASES OF A COUNTRY
######################################################################################

countries <- function(Country = ct){
  
  country <- Country
  
  major.summary.data_countries <- subset(major.summary.data, subset = Country.Region %in% country)
  major.summary.data_countries$Diff[2:nrow(major.summary.data_countries)] <- diff(major.summary.data_countries$Confirmed)
  major.summary.data_countries$Diff[1] <- 0
  
  a <- list(
    x = major.summary.data_countries$Date[1],
    y = max(major.summary.data_countries$Diff),
    text = major.summary.data_countries[(major.summary.data_countries$Date == recentdate),"Confirmed"],
    xref = "x",
    yref = "y",
    showarrow=FALSE
  )
  
  # Daily New Confirmed Cases plot
  #############################################
  plot_ly(major.summary.data_countries,
          x=~Date, y=~Diff, type = "bar") %>% 
    layout(xaxis=list(title="Date", tickfont = list(size = 9)), yaxis=list(title="Cases")) %>% 
    layout(title = list(text = paste("Daily New Confirmed Cases for", country, sep=" "))) %>%
    layout(yaxis = list(range = c(0, 1.1*(max(major.summary.data_countries$Diff)))))%>%
    config(displayModeBar = F) 
}



# Coronavirus in Top Countries
countries(topcountries[1])

countries(topcountries[2])
  
countries(topcountries[3])

countries(topcountries[4])





## Organizing the US States data

confirmed_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                         stringsAsFactors = FALSE, check.names =  FALSE)
confirmed_US <- confirmed_US[,c(-1,-2,-3,-4,-5,-6,-8,-9,-10,-11)]
death_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv",
                     stringsAsFactors = FALSE, check.names =  FALSE)
death_US <- death_US[,c(-1,-2,-3,-4,-5,-6,-8,-9,-10,-11,-12)]

## look at which dates you want to look at
## this removes all the dates prior to the date you want to start at
confirmed_US <- confirmed_US[,-c(2:(ncol(confirmed_US)-150))]
death_US <- death_US[,-c(2:(ncol(death_US)-150))]

library(plyr)
confirmed_US <- aggregate(. ~ Province_State, data=confirmed_US, FUN=sum)
death_US <- aggregate(. ~ Province_State, data=death_US, FUN=sum)

#########################################################################################################
# INCLUDING POPULATION NUMBERS FOR EACH STATE
#########################################################################################################

confirmed_US <- confirmed_US[!(confirmed_US$Province_State=="Grand Princess" | confirmed_US$Province_State=="Diamond Princess"),]
death_US <- death_US[!(death_US$Province_State=="Grand Princess" | death_US$Province_State=="Diamond Princess"),]

## REPLACE THIS WITH THE EXCEL FILE
library(rvest)

worldmeter <- data.frame(html_table(read_html("https://www.worldometers.info/coronavirus/country/us/"),
                                    fill=TRUE)[[1]])

worldmeter <- worldmeter[-c(1,57:64),c(2,3,5,7,13)]

worldmeter[(worldmeter$USAState=="Guam"),5] <- 165768 #Guam Population
worldmeter[(worldmeter$USAState=="United States Virgin Islands"),5] <- 106977 #Virgin Islands Population
worldmeter[(worldmeter$USAState=="United States Virgin Islands"),1] <- "Virgin Islands" #Virgin Islands Name replacement
worldmeter[(worldmeter$USAState=="Northern Mariana Islands"),5] <- 56882 #Mariana Islands Population
worldmeter[nrow(worldmeter)+1,]=c("American Samoa", 0, 0, 0, 55465, 0) ## Adding American Samoa population and values

#Converting characters to numerics 
worldmeter$TotalCases <- as.numeric(gsub('![[:alnum:]]*[[:space:]]|[[:punct:]]', '', worldmeter$TotalCases))
worldmeter$TotalDeaths <- as.numeric(gsub('![[:alnum:]]*[[:space:]]|[[:punct:]]', '', worldmeter$TotalDeaths))
worldmeter$TotalRecovered <- as.numeric(gsub('![[:alnum:]]*[[:space:]]|[[:punct:]]', '', worldmeter$TotalRecovered))
worldmeter$Population <- as.numeric(gsub('![[:alnum:]]*[[:space:]]|[[:punct:]]', '', worldmeter$Population))
worldmeter$RecoveredRatio <- worldmeter$TotalRecovered/worldmeter$TotalCases
worldmeter[is.na(worldmeter)] <- 0


confirmed_US <- confirmed_US[order(confirmed_US$Province_State),]
worldmeter <- worldmeter[order(worldmeter$USAState),]

confirmed_US <- add_column(confirmed_US, Population=worldmeter$Population, .after=1)
confirmed_US <- add_column(confirmed_US, RecoveredRatio=worldmeter$RecoveredRatio, .after=1)


######################################################################################
# FUNCTION FOR LOOKING AT DAILY NEW CASES
######################################################################################

daily <- function(State = st, scope = Scope){
  state <- State
  Type <- scope
  
  confirmed_US_Subset <- subset(confirmed_US, Province_State == state)
  confirmed_US_Subset <- melt(confirmed_US_Subset, id.vars = c("Province_State","Population", "RecoveredRatio"), 
                              variable.name = "Date", value.name = "Confirmed")
  death_US_Subset <- subset(death_US, Province_State == state)
  death_US_Subset <- melt(death_US_Subset, id.vars = c("Province_State"),
                          variable.name = "Date", value.name = "Death")
  
  confirmed_US_Subset$Diff[2:nrow(confirmed_US_Subset)] <- diff(confirmed_US_Subset$Confirmed)
  confirmed_US_Subset$Diff[1] <- 0
  
  confirmed_US_Subset$Date <- as.Date(as.character(confirmed_US_Subset$Date), format = c("%m/%d/%y"))
  
  if(Type == "Daily"){
    # Daily New Confirmed Cases plot
    #############################################
    plot_ly(confirmed_US_Subset,
            x=~Date, y=~Diff, type = "bar") %>%
      layout(xaxis=list(title="Date", tickfont = list(size = 9)), yaxis=list(title="Cases")) %>%
      layout(title = list(text = paste("Daily New Confirmed Cases for", state, sep=" "))) %>%
      layout(yaxis = list(range = c(0, 1.1*(max(confirmed_US_Subset$Diff))))) %>%
      config(displayModeBar = F)
  } else if(Type == "Active"){  
    ######################################################################################
    # FUNCTION FOR LOOKING AT INFECTION RATES AND ACTIVE CASES
    ######################################################################################    
    confirmed_US_Subset$Recovered <- ceiling(confirmed_US_Subset$Confirmed*confirmed_US_Subset$RecoveredRatio)
    confirmed_US_Subset$Death <- death_US_Subset$Death
    confirmed_US_Subset$Active <-confirmed_US_Subset$Confirmed-confirmed_US_Subset$Recovered-confirmed_US_Subset$Death
    confirmed_US_Subset$ActiveDiff[2:nrow(confirmed_US_Subset)] <- diff(confirmed_US_Subset$Active)
    confirmed_US_Subset$ActiveDiff[1] <- 0
    
    confirmed_US_Subset$InfectionChance <- (confirmed_US_Subset$Active/(confirmed_US_Subset$Population-confirmed_US_Subset$Confirmed))*100
    
    # Total Active Cases & Infection Rate plots
    ##################################################
    plot_ly(confirmed_US_Subset, x =~Date, y =~Active, type = "scatter", mode = "lines", name = "Cases") %>%
      add_trace(x =~confirmed_US_Subset$Date, y =~confirmed_US_Subset$InfectionChance, type = "scatter",
                mode="lines", yaxis = "y2", name = "Infection Chance(%)") %>%
      layout(title = paste("Total Active Cases and Chances of Infection(%) for", state, sep=" "),
             yaxis= list(title="Cases"),
             yaxis2 = list(overlaying = "y", side = "right", range=c(0,5), title="",
                           tickfont = list(color = "red")),
             showlegend = FALSE) %>%
      layout(yaxis = list(range = c(0, 1.1*(max(confirmed_US_Subset$Active))))) %>%
      config(displayModeBar = F)
  }
}

# Coronavirus in US States

### Virginia
daily(State = "Virginia", scope= "Daily")

daily(State = "Virginia", scope= "Active")

### Washington DC
daily(State = "District of Columbia", scope= "Daily")

daily(State = "District of Columbia", scope= "Active")