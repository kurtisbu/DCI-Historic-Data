library(tidyverse)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(GGally)
library(ggrepel)
setwd("C:/Users/Kurtis Bulock.000/Desktop/Learning R/DCI Scrape/DCI Historic Data")
DCI_Historic_Data <- read.csv("C:/Users/Kurtis Bulock.000/Desktop/Learning R/DCI Scrape/DCI Historic Data/Full DCI Score File.csv")

str(DCI_Historic_Data)

DCI_Historic_Data$Date <- as.Date(DCI_Historic_Data$Date, format = "%m/%d/%Y")


#Order the data by Date

DCI_Historic_Data <- DCI_Historic_Data[order(DCI_Historic_Data$Date),]

#Remove Phantom Regiment 0 score
DCI_Historic_Data <- DCI_Historic_Data %>% 
  filter( TOTAL != 0)

#Separate date into y/m/d Columns

DCI_Historic_Data <-DCI_Historic_Data %>% 
  mutate(year = lubridate:: year(Date),
         month = lubridate:: month(Date),
         day = lubridate:: day(Date))

##Filter to only World Class Corps

DCI_Historic_Data <- DCI_Historic_Data %>% 
  filter(Corps %in% c("The Academy", "Blue Devils", "Blue Knights", "Blue Stars", "Bluecoats",
                                         "The Cadets", "Carolina Crown", "The Cavaliers", "Colts", "Boston Crusaders", 
                                         "Crossmen", "Genesis", "Jersey Surf", "Madison Scouts", "Mandarins", "Oregon Crusaders", 
                                         "Pacific Crest", "Phantom Regiment", "Pioneer", "Santa Clara Vanguard", "Seattle Cascades", 
                                         "Spirit of Atlanta", "Troopers"))

#Find start date for each Corps for each year and append it to Main Dataframe
#add Column to each row determining the how many days since the start of the season



DCI_Historic_Data <-DCI_Historic_Data %>% 
  group_by(Corps, year) %>% 
  select(Corps, Date) %>% 
  summarise_each(funs( Start = min)) %>% 
  left_join(DCI_Historic_Data, ., by = c("Corps", "year")) %>% 
  mutate(Season_Day = Date - Start) 
  
  
  
        

##Plot Every Corps year over year by Season_Day

DCI_Historic_Data %>% 
  select(Show ,Corps, TOTAL, Season_Day, year) %>% 
  filter( TOTAL > 0) %>% 
  ggplot(aes(x = Season_Day, y = TOTAL)) +
  # geom_vline(xintercept = 8) +
  geom_point(aes(group = as.factor(year), color = as.factor(year))) +
  geom_line( aes(group = as.factor(year), color= as.factor(year))) +
  # geom_vline(xintercept = 8) +
  ggtitle("World Class Corps Score over 4 years") +
  labs( x = "Days since first show", y = "Total Score") +
  #geom_smooth(aes(group = year), method = "lm", se = T) +
  facet_wrap( ~Corps)

#plot score vs Show 
##figure out how to re-order show based on date

# DCI_Historic_Data %>% 
#   select(Show ,Corps, TOTAL, Season_Day, year, Date) %>% 
#   filter( TOTAL > 0, Corps == "Blue Devils") %>% 
#   arrange(Date) %>% 
#   ggplot(aes(x = Show, y = TOTAL)) +
#   geom_point(aes(group = as.factor(year), color = as.factor(year))) +
#   geom_line( aes(group = as.factor(year), color= as.factor(year))) +
#   ggtitle("World Class Corps Score over 4 years") +
#   labs( x = "Days since first show", y = "Total Score") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))



### Stacked bar plots for each year
options(max.print = 1000000)
DCI_Historic_Data %>% 
  filter(Corps == "Bluecoats") %>% 
  select(Corps, Ge_TOTAL, Vis_TOTAL, Mus_TOTAL, Date, TOTAL, year) %>% 
  melt(c("Corps", "Date", "TOTAL", "year")) %>%
  ggplot() +
  geom_bar(aes(x = Date, y = value, fill = variable), position = "stack", stat = "identity") +
  geom_line(aes(x = Date, y = TOTAL)) +
  geom_smooth(aes(x = Date, y = TOTAL), method = "lm", se = F) +
  facet_wrap(~year, scales = "free")


  #Add column to represent number of shows by group/year
DCI_Historic_Data<- DCI_Historic_Data %>% 
dplyr::group_by(Corps, year) %>% 
dplyr::mutate(Show_Count = 1:n()) %>% 
  ungroup()


#GGally?
DCI_Historic_Data %>% 
  ungroup() %>% 
  select( Ge_TOTAL, Vis_TOTAL, Mus_TOTAL, TOTAL, Show_Count) %>% 
  ggpairs( columns = 1:5, title = "DCI Data")


## Are DCI Scores going up?
# Find the average score for the top 12 for First/Regional/Finals

#Fist show average
DCI_Historic_Data %>% 
  filter(Show_Count == 1) %>% 
  select(Corps, Date,  Ge_TOTAL,Vis_TOTAL, Mus_TOTAL,  TOTAL, year) %>% 
  group_by(year) %>% 
  arrange(desc(TOTAL)) %>% 
  top_n(n = 12, wt = TOTAL) %>% 
  
  ### Average of top 12 scores for the first show of each saeason
  summarise_if(is.numeric, mean) %>% 
  select(Ge_TOTAL,Vis_TOTAL, Mus_TOTAL, year) %>%
  reshape2::melt(id.var = "year") %>% 
  ggplot(aes(x = year, y = value, col = variable)) +
  geom_line()


##Find final show average

DCI_Historic_Data  %>% 
  select(Corps, Date,  Ge_TOTAL,Vis_TOTAL, Mus_TOTAL,  TOTAL, year) %>% 
  group_by(Corps, year) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(desc(TOTAL)) %>%
  top_n(n = 12, wt = TOTAL) %>% 
  
  
  ##Average of Top 12 Scores for the Final show of the season
  summarise_if(is.numeric, mean) %>%
  select(Ge_TOTAL,Vis_TOTAL, Mus_TOTAL, year) %>%
  reshape2::melt(id.var = "year") %>% 
  ggplot(aes(x = year, y = value, col = variable)) +
  geom_line()

## Find San Antonio Average
DCI_Historic_Data  %>% 
  select(Corps, Date,  Ge_TOTAL,Vis_TOTAL, Mus_TOTAL,  TOTAL, year, Show) %>% 
  group_by(Corps, year) %>% 
  filter(Show == "DCI Southwestern Championship") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(desc(TOTAL)) %>%
  top_n(n = 12, wt = TOTAL) %>% 
  
  
  ##Average of Top 12 Scores for the San Antonio Regional
  summarise_if(is.numeric, mean) %>% 
  select(Ge_TOTAL,Vis_TOTAL, Mus_TOTAL, year) %>%
  reshape2::melt(id.var = "year") %>% 
  ggplot(aes(x = year, y = value, col = variable)) +
  geom_line()

## Find Atlanta Regional Average
DCI_Historic_Data  %>% 
  select(Corps, Date,  Ge_TOTAL,Vis_TOTAL, Mus_TOTAL,  TOTAL, year, Show) %>% 
  group_by(Corps, year) %>% 
  filter(Show == "DCI Southeastern Championship") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(desc(TOTAL)) %>%
  top_n(n = 12, wt = TOTAL) %>% 
  
  
  ##Average of Top 12 Scores for the Atlanta Regional
  summarise_if(is.numeric, mean) %>% 
  select(Ge_TOTAL,Vis_TOTAL, Mus_TOTAL, year) %>%
  reshape2::melt(id.var = "year") %>% 
  ggplot(aes(x = year, y = value, col = variable)) +
  geom_line()


## Find Allentown Regional Average
DCI_Historic_Data  %>% 
  select(Corps, Date,  Ge_TOTAL,Vis_TOTAL, Mus_TOTAL,  TOTAL, year, Show) %>% 
  group_by(Corps, year) %>% 
  filter(Show == "DCI Eastern Classic") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(desc(TOTAL)) %>%
  top_n(n = 12, wt = TOTAL) %>% 
  
  
  ##Average of Top 12 Scores for the Allentown Regional
  summarise_if(is.numeric, mean) %>% 
  select(Ge_TOTAL,Vis_TOTAL, Mus_TOTAL, year) %>%
  reshape2::melt(id.var = "year") %>% 
  ggplot(aes(x = year, y = value, col = variable)) +
  geom_line()
  

## Case study on Pacific Crest Rise in the last 5 years

PC_Data <- DCI_Historic_Data %>% 
  select(Show ,Corps, TOTAL, Season_Day, year, Date) %>% 
  filter( Corps == "Pacific Crest")

DCI_Historic_Data %>% 
  select(Show ,Corps, TOTAL, Season_Day, year) %>% 
  filter( Corps == "Pacific Crest") %>% 
  ggplot(aes(x = Season_Day, y = TOTAL)) +
  # geom_vline(xintercept = 8) +
  geom_point(aes(group = as.factor(year), color = as.factor(year))) +
  geom_line( aes(group = as.factor(year), color= as.factor(year))) +
  # geom_vline(xintercept = 8) +
  ggtitle("Pacific Crest Score over 5 years") +
  labs( x = "Days since first show", y = "Total Score")
  #geom_smooth(aes(group = year), method = "lm", se = T) +


## Find the Average of the 12th place finals corps and find their average for each year.
## find 12th place corps for each finals, then join based on those groups

DCI_Historic_Data %>% 
  filter(Show %in% c("DCI World Championship Finals" , "DCI World Championship World Class Finals"), Place == 12) %>% 
  select(Corps, year) %>% 
  left_join(DCI_Historic_Data, by = c("Corps" = "Corps", "year" = "year")) %>% 
  select(Corps, TOTAL, year) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(mean(TOTAL))
  
  
  
  ## create plot to compare Mandarins 2015-17 to PC 2017-2019]
DCI_Historic_Data %>% 
  dplyr::filter(Corps %in% c("Mandarins", "Pacific Crest")) %>% 
  dplyr::select(Corps, Date, year, TOTAL) %>% 
  dplyr::filter(case_when(Corps == "Mandarins" ~ year < 2018, T ~ year > 2016)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(Season_year = case_when(Corps == "Mandarins" ~ year - 2014, T ~ year - 2016))
  
  
  ### Plot comapring PC to Phantom Regiment 2016-2019
DCI_Historic_Data %>% 
  filter(Corps %in% c("Phantom Regiment", "Pacific Crest"), year >= 2016) %>% 
  select(Corps, year, TOTAL) %>%
  dplyr::group_by(Corps, year) %>% 
  dplyr::summarise(Average = mean(TOTAL)) %>%
  ggplot(aes(x = year, y = Average, color = Corps)) +
  geom_line() +
  labs(x = "Season", y = "Average Score over the Season")

### Plot comparing Mandarins to Madison Scouts 2015-2018
DCI_Historic_Data %>% 
  filter(Corps %in% c("Madison Scouts", "Mandarins"), year <= 2018) %>% 
  select(Corps, year, TOTAL) %>%
  dplyr::group_by(Corps, year) %>% 
  dplyr::summarise(Average = mean(TOTAL)) %>%
  ggplot(aes(x = year, y = Average, color = Corps)) +
  geom_line() +
  labs(x = "Season", y = "Average Score over the Season")

  
### Re-do the below plot
DCI_Historic_Data %>% 
  filter(ifelse(year >= 2016))

## Plot average score for PC, 12th place, DCI Overall

Avg_DCI_Score <- DCI_Historic_Data %>% 
  filter(year >= 2016) %>% 
  select(Corps, year, TOTAL) %>% 
  dplyr:: group_by(year) %>% 
  dplyr::summarise(mean= mean(TOTAL)) %>% 
  dplyr:: mutate(Classification = "Avg_DCI_Score")

Avg_PC_Score <- PC_Data %>%
  filter(year >= 2016) %>% 
  select(Corps, year, TOTAL) %>% 
  dplyr:: group_by(year) %>% 
  dplyr::summarise(mean = mean(TOTAL)) %>% 
  dplyr:: mutate(Classification = "Avg_PC_Score")

Twelfth_place_average_score <- DCI_Historic_Data %>% 
  filter(Show %in% c("DCI World Championship Finals" , "DCI World Championship World Class Finals"), Place == 12, year >= 2016) %>% 
  select(Corps, year) %>% 
  left_join(DCI_Historic_Data, by = c("Corps" = "Corps", "year" = "year")) %>% 
  select(Corps, TOTAL, year) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(mean = mean(TOTAL)) %>% 
  dplyr:: mutate(Classification = "12th_Avg_Score")

bind_rows(Twelfth_place_average_score, Avg_PC_Score) %>% 
  bind_rows(Avg_DCI_Score) %>% 
  ggplot(aes(x = year, y = mean, color = Classification)) +
  geom_line()


## DCI Average Score with OC and Pioneer Removed
Avg_DCI_Score_adjusted <- DCI_Historic_Data %>% 
  select(Corps, year, TOTAL) %>% 
  filter(Corps != "Pioneer", Corps != "Oregon Crusaders") %>%
  dplyr:: group_by(year) %>% 
  dplyr::summarise(mean = mean(TOTAL)) %>% 
  dplyr:: mutate(Corps = "Avg_DCI_Score_adj")


  bind_rows(Avg_DCI_Score, Avg_DCI_Score_adjusted) %>% 
  ggplot(aes(x = year, y = mean, color = Corps)) +
  geom_line()
  
  
  ## Plot PC score over the years
  DCI_Historic_Data %>% 
    select(Show ,Corps, TOTAL, Season_Day, year) %>% 
    filter( TOTAL > 0, Corps == "Pacific Crest") %>% 
    ggplot(aes(x = Season_Day, y = TOTAL)) +
    geom_point(aes(color = as.factor(year))) +
    geom_line( aes(color = as.factor(year))) +
    ggtitle("Pacific Crest's Score over 5 years") +
    labs( x = "Days since first show", y = "Total Score")
  

### Fun Fact #1 
  
  #lowest and Highest Score on opening performance
  Min.Max <- DCI_Historic_Data %>% 
    filter( Season_Day == 0, Corps != "Genesis") %>% 
    select(Ge_TOTAL, Vis_TOTAL, Mus_TOTAL, TOTAL) %>% 
    summarise( min_TOTAL = min(TOTAL), max_TOTAL = max(TOTAL),
               min_Ge = min(Ge_TOTAL), max_Ge = max(Ge_TOTAL),
               min_Vis = min(Vis_TOTAL), max_Vis = max(Vis_TOTAL),
               min_Mus = min(Mus_TOTAL), max_Mus = max(Mus_TOTAL))
DCI_Historic_Data %>% 
  filter(Mus_TOTAL == 8.8, Season_Day == 0) %>% 
  select(Corps, Date, Show, TOTAL)
  head()
  
  
  
  ###Pacific Crest 29th Show in 2016
  DCI_Historic_Data %>% 
    filter(Corps == "Pacific Crest", year == 2016, Season_Day == 29) %>% 
    select(Show) %>% 
    print()
  
  "DCI Southwestern Championship", "DCI Southeastern Championship", "DCI Eastern Classic", "DCI World Championship Semifinals"
  
  
  DCI_Historic_Data %>% 
    select(Show ,Corps, TOTAL, Season_Day, year) %>% 
    filter( TOTAL > 0, Corps == "Pacific Crest") %>% 
    ggplot(aes(x = Season_Day, y = TOTAL)) +
    geom_point(aes(color = as.factor(year))) +
    geom_line( aes(color = as.factor(year))) +
    ggtitle("Pacific Crest's Score over 5 years") +
    labs( x = "Days since first show", y = "Total Score") + 
    theme_bw() +
    theme(legend.title = element_blank()) +
    geom_text_repel(aes(x = Season_Day, y = TOTAL, label = City, color = factor(year)),
                    box.padding = unit(.1, "lines"),
                    size = 2.5,
                    point.padding = unit(.2, "lines"),
                    segment.colour = 'black',
                    data = subset(DCI_Historic_Data, Corps == "Pacific Crest" & Show %in% c("DCI Southwestern Championship", 
                                                                                            "DCI Southeastern Championship", 
                                                                                            "DCI Eastern Classic", 
                                                                                            "DCI World Championship Semifinals")))
  
  ### Find score unimprovemements is top corps
  DCI_Historic_Data %>% 
    select(Show ,Corps, TOTAL, Season_Day, year) %>% 
    filter( TOTAL > 0, Corps == "Mandarins") %>% 
    ggplot(aes(x = Season_Day, y = TOTAL)) +
    geom_point(aes(color = as.factor(year))) +
    geom_line( aes(color = as.factor(year))) +
    ggtitle("Pacific Crest's Score over 5 years") +
    labs( x = "Days since first show", y = "Total Score") + 
    theme_bw() +
    theme(legend.title = element_blank()) +
    geom_text_repel(aes(x = Season_Day, y = TOTAL, label = City, color = factor(year)),
                    box.padding = unit(.1, "lines"),
                    size = 2.5,
                    point.padding = unit(.2, "lines"),
                    segment.colour = 'black',
                    data = subset(DCI_Historic_Data, Corps == "Mandarins" & Show %in% c("DCI Southwestern Championship", 
                                                                                            "DCI Southeastern Championship", 
                                                                                            "DCI Eastern Classic", 
                                                                                            "DCI World Championship Semifinals")))
  
  
DCI_Historic_Data %>% 
  filter(Corps == "Blue Devils", year == 2016) %>% 
  select(Corps, Show, Date, year, TOTAL) %>% 
  View()


# plot 11-16th place for 2019

DCI_Historic_Data %>% 
  filter(Corps %in% c("Crossmen","Phantom Regiment", "Spirit of Atlanta", "Pacific Crest","The Academy","Colts"), year == 2019) %>% 
  select(Date, Corps, TOTAL, Show) %>% 
  ggplot(aes(x = Date, y = TOTAL)) +
  geom_point(aes(color = as.factor(Corps))) +
  geom_line( aes(color = as.factor(Corps))) +
  ggtitle("Pacific Crest's Score over 5 years") +
  labs( x = "Days since first show", y = "Total Score") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  geom_text_repel(aes(x = Date, y = TOTAL, label = City, color = factor(Corps)),
                  box.padding = unit(.1, "lines"),
                  size = 2.5,
                  point.padding = unit(.2, "lines"),
                  segment.colour = 'black',
                  data = subset(DCI_Historic_Data, Corps %in% c("Crossmen","Phantom Regiment", "Sprit of Atlanta", "Pacific Crest","The Academy","Colts") & Show %in% c("DCI Southwestern Championship",
                                                                                      "DCI Southeastern Championship",
                                                                                      "DCI Eastern Classic",
                                                                                      "DCI World Championship Semifinals") & year == 2019))


#View Data
DCI_Historic_Data %>% 
  filter(Corps %in% c("Crossmen","Phantom Regiment", "Spirit of Atlanta", "Pacific Crest","The Academy","Colts"), year == 2019) %>% 
  select(Date, Corps, TOTAL, Show, City) %>% 
  view()
###### Write Drawdown function?



### GGplot Fixing
DCI_Historic_Data %>% 
  filter(Corps %in% c("Phantom Regiment", "Pacific Crest")) %>% 
  select(Corps, year, TOTAL) %>%
  dplyr::group_by(Corps, year) %>% 
  dplyr::summarise(mean = round(mean(TOTAL),3)) %>%
  bind_rows(Avg_DCI_Score) %>%
  filter(year > 2016) %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean, color = Corps)) +
  geom_point(aes(x = year, y = mean, color = Corps)) + 
  labs(x = "Season", y = "Average Score", color = 'Corps') +
  theme(legend.position = c(.8 , .2)) + 
  scale_fill_manual( name = "Corps2", values = c("DCI Average Score", "Madison Scouts", "Mandarins")) +
  geom_text_repel(aes(x = year, y = mean, label = mean, color = Corps),
                  box.padding = unit(.1, "lines"),
                  size = 2.5,
                  nudge_x = -.01,
                  nudge_y = .01,
                  segment.colour = NA,
                  point.padding = unit(.7, "lines"))


## GGPlot Fixing 2
DCI_Historic_Data %>% 
  dplyr::filter(Corps %in% c("Mandarins", "Pacific Crest")) %>% 
  dplyr::select(Corps, Date, year, TOTAL, Season_Day) %>% 
  dplyr::filter(case_when(Corps == "Mandarins" ~ year < 2018, T ~ year > 2016)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(Season_year = case_when(Corps == "Mandarins" ~ year - 2014, T ~ year - 2016)) %>% 
  ggplot() +
  geom_line(aes(x = Season_Day , y = TOTAL, color = Corps)) +
  facet_wrap(~ Season_year)+
  theme_bw() +
  labs(x = "Days Since First Show", y = "Total Score", title = "Mandarins 2015 - 2017 Vs. Pacific Crest 2017 - 2019") +
  theme(legend.position = c(.9,.2))

              




















DCI_Historic_Data %>% 
  filter(Corps %in% c("Phantom Regiment", "Pacific Crest")) %>% 
  select(Corps, year, TOTAL) %>%
  dplyr::group_by(Corps, year) %>% 
  dplyr::summarise(mean = round(mean(TOTAL),3)) %>%
  bind_rows(Avg_DCI_Score) %>%
  filter(year >= 2017) %>% 
  ggplot(aes(x = year, y = mean, color = Corps)) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  labs(x = "Season", y = "Average Score", color = 'Corps') +
  theme(legend.position = c(.8,.2)) +
 
  geom_text_repel(aes(x = year, y = mean, label = mean, color = Corps),
                  box.padding = unit(.1, "lines"),
                  size = 2.5,
                  nudge_x = -.01,
                  nudge_y = .01,
                  segment.colour = NA,
                  point.padding = unit(.7, "lines")) +
  scale_color_discrete(name = "Dose", labels = c("A", "B", "C"))
  





DCI_Historic_Data %>% 
  filter(Corps %in% c("Madison Scouts", "Mandarins")) %>% 
  select(Corps, year, TOTAL) %>%
  dplyr::group_by(Corps, year) %>% 
  dplyr::summarise(mean = round(mean(TOTAL),3)) %>%
  bind_rows(Avg_DCI_Score) %>%
  filter(year <= 2018) %>% 
  ggplot(aes(x = year, y = mean, color = Corps)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(legend.position = c(.8,.2))+ 
  geom_text_repel(aes(x = year, y = mean, label = mean, color = Corps),
                  box.padding = unit(.1, "lines"),
                  size = 2.5,
                  nudge_x = -.01,
                  nudge_y = .01,
                  segment.colour = NA,
                  point.padding = unit(.7, "lines")) +
  labs(x = "Season", y = "Average Score over the Season") 

?write.csv
write.csv(DCI_Historic_Data, file = "Updated_Full_DCI_File_060620.csv")
