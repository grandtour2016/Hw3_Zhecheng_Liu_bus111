library(coronavirus) #import coronavirus package for question 1-a
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(tidyr)
library(scales) #install this package to enable adding comma format to continous numebrs on y-axis
library(extrafont) #import this package to download different font families to charts
font_import() #import the fonts and register the fonts, this might take a few minutes, please be patient
loadfonts(device = "win") #load the fonts into my computer system

options(scipen = 999) #to remove scientific notation

#b
head(coronavirus,100) #show the first 100 elements
# c
#the first column is date, second is location,third is latitude,
#forth is longitude point, firth is type, the last one is cases 

#Question 2
#a
summary_df <- coronavirus %>% 
  filter(type == "confirmed") %>% #pick only data labeled "confirmed"
  group_by(country) %>%           #group by country
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20) #show the top 20 cases by country

#b
top5country <- as.data.frame(summary_df %>% head(5)) 
#convert tibble into dataframe. this is just my personal style. I prefer dataframe. 

ggplot(top5country,aes(x = country, y = total_cases)) + 
  geom_bar(color = "black", fill=rgb(0.1,0.4,0.5,0.7), stat = "identity")
#plot the bar chart with country on the x-axis and total number of cases on the y-axis

#c
ggplot(top5country,aes(x = country, y = total_cases)) + 
  geom_bar(color = "black", fill=rgb(0.1,0.4,0.5,0.7), stat = "identity") + 
  coord_flip()  #flip into horizontal bar chart

#d
ggplot(top5country,aes(x = country, y = total_cases)) + 
  geom_bar(color = "black", fill=rgb(0.1,0.4,0.5,0.7), stat = "identity") + 
  coord_flip() + 
  ggtitle("Top 5 countries by total cases")


#Question 3
#a
recent_cases <- coronavirus %>% 
  filter(type == "confirmed") %>% 
  group_by(date) %>%    #group the confirmed cases by each day
  summarise(total_cases = sum(cases)) %>% #calculate daily new confirmed cases in the world
  arrange(date) %>%     #arrange date from first date to the last date
  mutate(total_confirmed = cumsum(total_cases)) %>% #create another column of cumulative daily confirmed cases in the world
  ungroup()

recent_cases <- as.data.frame(recent_cases) 
#the first column is date, the second column is the daily new confirmed cases, the third column is the cumulative confirmed cases

#b
#plot the daily new confirmed cases by date
ggplot(recent_cases,aes(x = date, y = total_cases)) + 
  geom_line(color = "Dark Blue", size = 1.5, alpha = 0.9, linetype = 1) + 
  theme(plot.title = element_text(color = "Black",                #credit 1: change title color
                                  size = 15,                      #credit 2: change title size to be 15
                                  face = "bold",                  #credit 3: change font style to be bold
                                  hjust = 0.5,                    #credit 4: move title to the center
                                  family = "Palatino Linotype"),  #credit 5: change font to Palatino
        plot.subtitle = element_text(color = "blue",
                                     size = 12,
                                     hjust = 0.5,
                                     family = "Palatino Linotype"),#you may have trouble loading this font if using mac, if so, please remove all the font settings and rerun. thank you. 
        plot.caption = element_text(color = "Black", 
                                    face = "italic",
                                    hjust = 0,
                                    family = "Palatino Linotype"),
        axis.title.x = element_text(color = "Black",
                                    size = 10, 
                                    hjust = 0.5,
                                    family = "Palatino Linotype"),
        axis.title.y = element_text(color = "Black",
                                    size = 10,
                                    hjust = 0.5, 
                                    family = "Palatino Linotype"),
        axis.text.x = element_text(angle = 45,
                                   size = 10,
                                   hjust = 1,
                                   family = "Palatino Linotype"),
        axis.text.y = element_text(color = "Black",
                                   size = 10,
                                   hjust = 1,
                                   family = "Palatino Linotype"),
        panel.background = element_rect(fill = "lightblue",        #Credit 6: add background color
                                        color = "lightblue"),
        panel.grid.major = element_line(linetype = "solid",        #Credit 7: add major grid color and style
                                        color = "grey"),
        panel.grid.minor = element_line(linetype = "solid",        #Credit 8: add minor grid color and style
                                        color = "grey")) +
  labs(title = "Daily New Confirmed Cases in Global",              #Credit 9: add subtitles, and data sources
       subtitle = "Unit: one case",
       caption = "Data source: Johns Hopkins University Center for Systems Science and Engineering",
       x = "Date",
       y = "Daily Confirmed Cases") +
  scale_x_date(date_labels = "%b / %d",                             #Credit 10: edit date expression as Month / Day
               date_breaks  ="1 month") +                           #Credit 11: label dates every one month
  scale_y_continuous(labels = comma)                                #Credit 12: add comma format to y axis


#plot the cumulative confirmed cases by date
ggplot(recent_cases,aes(x = date, y = total_confirmed)) + 
  geom_line(color = "Dark Blue", size = 1.5, alpha = 0.9, linetype = 1) + 
  theme(plot.title = element_text(color = "Black",                #credit 1: change title color
                                  size = 15,                      #credit 2: change title size to be 15
                                  face = "bold",                  #credit 3: change font style to be bold
                                  hjust = 0.5,                    #credit 4: move title to the center
                                  family = "Palatino Linotype"),  #credit 5: change font to Palatino
        plot.subtitle = element_text(color = "blue",
                                     size = 12,
                                     hjust = 0.5,
                                     family = "Palatino Linotype"),
        plot.caption = element_text(color = "Black", 
                                    face = "italic",
                                    hjust = 0,
                                    family = "Palatino Linotype"),
        axis.title.x = element_text(color = "Black",
                                    size = 10, 
                                    hjust = 0.5,
                                    family = "Palatino Linotype"),
        axis.title.y = element_text(color = "Black",
                                    size = 10,
                                    hjust = 0.5, 
                                    family = "Palatino Linotype"),
        axis.text.x = element_text(angle = 45,
                                   size = 10,
                                   hjust = 1,
                                   family = "Palatino Linotype"),
        axis.text.y = element_text(color = "Black",
                                   size = 10,
                                   hjust = 1,
                                   family = "Palatino Linotype"),
        panel.background = element_rect(fill = "lightblue",        #Credit 6: add background color
                                        color = "lightblue"),
        panel.grid.major = element_line(linetype = "solid",        #Credit 7: add major grid color and style
                                        color = "grey"),
        panel.grid.minor = element_line(linetype = "solid",        #Credit 8: add minor grid color and style
                                        color = "grey")) +
  labs(title = "Cumulative Confirmed Cases in Global",
       subtitle = "Unit: one case",
       caption = "Data source: Johns Hopkins University Center for Systems Science and Engineering",
       x = "Date",
       y = "Total Confirmed Cases") +
  scale_x_date(date_labels = "%b / %d",                             #Credit 9: edit date expression as Month / Day
               date_breaks  ="1 month") +                           #Credit 10: label dates every one month
  scale_y_continuous(labels = comma)                                #Credit 11: add comma format to y axis

#in case you can't load the font package. please run the codes below. 
#if it says "fail to load the font family, please run the codes below without the font family settings"
ggplot(recent_cases,aes(x = date, y = total_cases)) + 
  geom_line(color = "Dark Blue", size = 1.5, alpha = 0.9, linetype = 1) + 
  theme(plot.title = element_text(color = "Black",                #credit 1: change title color
                                  size = 15,                      #credit 2: change title size to be 15
                                  face = "bold",                  #credit 3: change font style to be bold
                                  hjust = 0.5),                    #credit 4: move title to the center)
        plot.subtitle = element_text(color = "blue",
                                     size = 12,
                                     hjust = 0.5),
        plot.caption = element_text(color = "Black", 
                                    face = "italic",
                                    hjust = 0),
        axis.title.x = element_text(color = "Black",
                                    size = 10, 
                                    hjust = 0.5),
        axis.title.y = element_text(color = "Black",
                                    size = 10,
                                    hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   size = 10,
                                   hjust = 1),
        axis.text.y = element_text(color = "Black",
                                   size = 10,
                                   hjust = 1),
        panel.background = element_rect(fill = "lightblue",        #Credit 5: add background color
                                        color = "lightblue"),
        panel.grid.major = element_line(linetype = "solid",        #Credit 6: add major grid color and style
                                        color = "grey"),
        panel.grid.minor = element_line(linetype = "solid",        #Credit 7: add minor grid color and style
                                        color = "grey")) +
  labs(title = "Daily New Confirmed Cases in Global",              #Credit 8: add subtitles, and data sources
       subtitle = "Unit: one case",
       caption = "Data source: Johns Hopkins University Center for Systems Science and Engineering",
       x = "Date",
       y = "Daily Confirmed Cases") +
  scale_x_date(date_labels = "%b / %d",                             #Credit 9: edit date expression as Month / Day
               date_breaks  ="1 month") +                           #Credit 10: label dates every one month
  scale_y_continuous(labels = comma)                                #Credit 11: add comma format to y axis
