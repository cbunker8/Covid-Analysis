
# clean memory ------------------------------------------------------------
rm(list = ls())

# read in data ------------------------------------------------------------
#set working directory

master2=read_csv("del1.csv")
master3=read_csv("del3.csv")
master1.5=read_csv("master1.5.csv")
master4 = read_csv("del4.csv")
economic_status = read_csv("ec")

# see data ----------------------------------------------------------





# see data types ----------------------------------------------------------



# deliverable 1 ----------------------------------------------------------
library(DescTools)
library(tidyverse)
library(plotfunctions)
library(tinytex)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(hexbin)
library(extrafont)
library(dplyr)
library(scales)


font_import()
str(master2,width = 70,strict.width='cut')
absoluteT=table(master2$GDP_Status,
                exclude = NA)

names(absoluteT)[4]
propT=prop.table(absoluteT)*100


(tableFreq=as.data.frame(absoluteT))

names(tableFreq)=c("GDP_status","Count")
tableFreq$Percent=as.vector(propT)
tableFreq

# original before reorderbase= ggplot(data = tableFreq, 
#aes(x = GDP_status,
#y = Percent))



custom_order <- c("Lowest", "Low", "Average", "High")


color_mapping <- c("Lowest" = "grey", "Low" = "grey", "Average" = "grey", "High" = "grey")


tableFreq$GDP_status <- factor(tableFreq$GDP_status, levels = custom_order)
master2$GDP_Status <- factor(master2$GDP_Status, levels = custom_order)


LABELS=paste0(round(tableFreq$Percent,2), '%')
base= ggplot(data = tableFreq, 
             aes(x = reorder(GDP_status,Percent),y = Percent, fill = GDP_status)) 



base= base + theme_classic()


plot1 = base + geom_bar(position = "stack", width = .75,
                        stat = 'identity',na.rm = TRUE) + scale_fill_manual(values = color_mapping) + theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title='GDP status of Countries affected by Covid-19',subtitle ='Per Capita GDP Dollars',caption='Source: ourworldindata.org')



plot3 = plot1 + geom_hline(yintercept = 0, 
                           linetype="dashed", 
                           linewidth=.0, 
                           alpha=0.0) 
plot3


plot4 = plot3 + scale_y_continuous(breaks=c(0,10,25,50),
                                   limits = c(0, 50), 
                                   labels= unit_format(suffix = '%')) 
                                     
                                    

plot5 = plot4 + theme(plot.caption = element_text(hjust = 0), 
                      plot.title = element_text(hjust = 0.5))


plot6 = plot5 + theme(legend.position = "none") + theme(plot.subtitle = element_text(hjust = .5))+
  theme(text = element_text(size=16, family="Georgia")) 

plot6 = plot6 + scale_x_discrete(limits = c("Lowest", "Low", "Average","High"))

#custom_labels <- c("20k+ USD",
                   # "35k+ USD",
                  # "10k+ USD",
                  # "Under 10k USD")





#plot7 = plot6 +  geom_text(data = subset(tableFreq, GDP_status %in% c("Lowest", "Low", "Average", "High")),
                          # aes(label = custom_labels), vjust = -0.5, color = "black", size = 4)

plot7 = plot6 +  geom_text(data = subset(tableFreq, GDP_status %in% c("Lowest", "Low", "Average", "High")),
                           aes(label = c("20k+ USD",
                                         "35k+ USD",
                                         "10k+ USD",
                                         "Under 10k USD")), vjust = -0.5, color = "black", size = 4)
plot8 = plot7 + labs(x= NULL, y= NULL)+theme(text = element_text(size=13, family="Times New Roman"))
  
 
 
  

del1Draft = plot8


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------


master1.5$death_category <- case_when(
  master1.5$total_deaths_per_million < 200 ~ "Under 200 Dead",
  master1.5$total_deaths_per_million >= 3000 ~ "3000+ Dead",
  TRUE ~ "200 - 1999 Dead"
)


summary(master1.5,total_deaths_per_million)
base= ggplot(master1.5,aes(y = total_deaths_per_million))  
base + geom_boxplot()

(statVals=summary(master1.5$total_deaths_per_million,digits = 3)[1:6])

# the summary values as vector
statVals=statVals%>%as.vector() 
base= ggplot(master1.5,aes(y = total_deaths_per_million))  
b1= base + geom_boxplot() 
b1=b1+ scale_y_continuous(breaks = statVals) 
b1

b1=b1 +coord_flip()
b1

(upperT=ggplot_build(b1)$data[[1]]$ymax)
(numOutliers=sum(master1.5$total_deaths_per_million>upperT,na.rm = T))

txtOutliers=paste0('#Outlying deaths: ',numOutliers)
txtUpper=paste0('Threshold:',upperT)


b1_vertical = b1 + geom_hline(yintercept = upperT,
                              color='red',
                              linetype="dotted",
                              size=2) 
b1_annot=b1_vertical + annotate(geom = 'text',
                                label=txtUpper,
                                y = upperT+5,
                                x=0.2,
                                angle=90)

b1_annot=b1_annot + annotate(geom = 'text',
                             label=txtOutliers,
                             y = upperT+60,
                             x=0.1,
                             angle=0)
b1_annot
b1_annot_noX = b1_annot + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
b1_annot_noX

b1_newGrid=b1_annot_noX +  theme_classic()
b1_newGrid

b1_better_axisText = b1_newGrid+ theme(axis.text.x = element_text(angle = 60,
                                                                  size = 7,
                                                                  vjust = 0.5))
b1_better_axisText
library(DescTools)


cv=CoefVar(master1.5$total_deaths_per_million,na.rm = T)
sd=SD(master1.5$total_deaths_per_million,na.rm = T)
md=Median(master1.5$total_deaths_per_million,na.rm = T)
mn=Mean(master1.5$total_deaths_per_million,na.rm = T)
master1.5.low=MeanCI(master1.5$total_deaths_per_million,
                     na.rm = T)[['lwr.ci']]
maste1.5.up=MeanCI(master1.5$total_deaths_per_million,
                   na.rm = T)[['upr.ci']]
sk=Skew(master1.5$total_deaths_per_million,
        na.rm = T)



barWIDTH=10
library(ggplot2)
base= ggplot(master1.5)  
h1= base + geom_histogram(aes(x = total_deaths_per_million),
                          binwidth = 30,
                          fill='gold') 
h1=h1 + labs(y="count")
h1


base=ggplot(master1.5) + theme_minimal() 


hist2 <- base + 
  geom_histogram(aes(x = total_deaths_per_million, fill = death_category),binwidth = 133,color = "white"
  ) + 
  scale_fill_manual(values = c("Under 200 Dead" = "blue", 
                               "2000+ Dead" = "red"))


hist2 = hist2 + scale_fill_discrete(name = "Amount Dead", labels = c("6k","Under 200","2000 to 6k"))
hist2



hist2 = hist2 + labs(x= 'Total Deaths',y='Number of Countries',
                     title ='Reported Covid-19 Deaths', subtitle = "As of April 25, 2022",
                     caption = 'Source: ourworldindata.org')
hist2

ggplot_build(hist2)$data[[1]]%>%head()
(fromHist=ggplot_build(hist2)$data[[1]][,c('count','x','xmin','xmax')])
(modeClassInfo=round(fromHist[which.max(fromHist$count),],2))




hist3ann = hist2 + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank())
hist4ann = hist3ann + theme(panel.border = element_blank())

hist5 = hist4ann + scale_fill_discrete(name = "Amount Dead", labels = c("6k","200 to 6k","Under 200"))

mean1 = 1169 

del2Draft = hist5 + geom_vline(xintercept = mean1, color = "black", linetype = "solid") +
  annotate("text", x = mean1 + 730, y = 16, label = "Mean Deaths\n1169",size = 3)+
  theme(text = element_text(size=13, family="Times New Roman"))

del2Draft



# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

country_names <- c("Norway", "Singapore", "Australia", "Germany", "Japan",
                   "United Arab Emirates", "South Korea", "Italy", "Spain",
                   "Brazil", "Mexico", "Turkey", "India",
                   "Indonesia", "Vietnam")

#subset_data <- master3[master3$Country %in% country_names, ]
#median_age <- aggregate(median_age ~ Country, data = subset_data, FUN = median, na.rm = TRUE)

#death_aggregate <- aggregate(total_cases_per_million ~ Country, data = subset_data, FUN = sum, na.rm = TRUE)
subset_data <- master3[master3$Country %in% country_names, ]


median_age <- tapply(subset_data$median_age, subset_data$Country, median, na.rm = TRUE)


death_aggregate <- tapply(subset_data$total_cases_per_million, subset_data$Country, sum, na.rm = TRUE)

median_age <- subset_data %>%
  group_by(Country) %>%
  summarise(median_age = median(median_age, na.rm = TRUE))

death_aggregate <- subset_data %>%
  group_by(Country) %>%
  summarise(total_cases_per_million = sum(total_cases_per_million, na.rm = TRUE))

Age_deaths <- merge(median_age, death_aggregate, by = "Country")
Age_deaths$total_cases_per_million <- round(Age_deaths$total_cases_per_million)
Age_deaths$median_age <- round(Age_deaths$median_age)

Age_deaths <- full_join(Age_deaths,economic_status, by = "Country")


options(scipen = 999)
numericalplot1 <- ggplot(Age_deaths, aes(x = median_age, y = total_cases_per_million)) +
  geom_point(aes(fill = `economic_status`), shape = 21, size = 4,stroke = 1, alpha = 0.7) + 
  labs(
    title = "Analysis of Covid-19 Median Age and Cases",
    subtitle = "Cumulative over Length of Pandemic",
    x = "Median Age",
    y = "Cases per Million",
    caption = 'Source: ourworldindata.org',
    fill = "Economic Status"
  ) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(face = "bold")
  ) +
  geom_text_repel(aes(label = Country), vjust = -0.5, size = 3)


del3Draft1 <- numericalplot1 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  


del3Draft2 <- del3Draft1 + theme(text = element_text(size=13, family="Times New Roman"))


#get better annotation than correlation
# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft2, file = "delDraft.rds")


# deliverable 4  ----------------------------------------------------------
library(sf) 

zipMap <- st_read("world-administrative-boundaries.shp")


base=ggplot(data = zipMap)
base + geom_sf()

total_deaths <- sum(master4$total_deaths_per_million, na.rm = T)
master4 <- master4 %>% mutate(prop_death = total_deaths_per_million/total_deaths)


joined_data <- merge(zipMap, master4,by.x= 'name', by.y='Country')

joined_data = rename(joined_data,"Country" = "name")

joined_data = filter(joined_data,!region %in% 	
                       "Caribbean")


joined_data = filter(joined_data,!Country %in% c("Canada","United States of America"))


chlo <- base +
  geom_sf(data = joined_data, aes(fill = total_deaths_per_million)) +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-120, -35), ylim = c(-60, 40)) +
  labs(title = "Covid Deaths in Central and South America",subtitle = "By April 25th 2022", 
       fill = "Death Count",caption = "Source: ourworldindata.org", x = NULL, y =NULL) +
  theme(text = element_text(size=13, family="Times New Roman"))+  annotate(
    "text",
    x = -83,
    y = -10,
    label = "Perú: \n6,247",
    color = "black",
    size = 3,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = -51,
    y = -10,
    label = "Brasil: \n 3,077",
    color = "black",
    size = 3,
    fontface = "bold") + annotate(
      "text",
      x = -60,
      y = 14,
      label = "Venezuela: \n 201",
      color = "black",
      size = 3,
      fontface = "bold") +
  theme(legend.position = c(.10,.32), panel.background = element_blank(),axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),axis.title.y = element_blank(),    
        panel.border = element_blank(),    
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
chlo



del4Draft= chlo
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")
