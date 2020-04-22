#Scrape Data off of PGA Website
library(rvest)
library(tidyverse)

###Career Earnings
career_earnings<-'https://www.pgatour.com/stats/stat.014.html'

# Read the html code of the webpage in to the variable webpage_code
careerearnings_code<-read_html(career_earnings)

career_money        <- html_nodes(careerearnings_code,'td , .col-stat')
career_money_text   <- html_text(career_money,trim=T)
career_money.df.prelim     <- as.data.frame(career_money_text)

##
career_money.df  <- career_money.df.prelim %>% 
  slice(5:nrow(career_money.df.prelim))
#To Dataframe
career_money.df    <- as.data.frame(split(career_money.df, 1:4))%>%
  mutate(ThisWeek=as.numeric(as.character(career_money_text)),
         LastWeek=as.numeric(as.character(career_money_text.1)),
         Name=as.character(career_money_text.2),
         MN=as.character(career_money_text.3),
         Earnings=as.numeric(gsub("[\\$,]","",MN)))%>%
  select(ThisWeek,LastWeek,Name,Earnings)

career_money.df %>% filter(ThisWeek<=75)%>%
  ggplot(aes(reorder(Name,Earnings),Earnings))+
  scale_y_continuous(labels = scales::dollar)+
  geom_col()+
  coord_flip()

###Year to Year Earnings
#'https://www.pgatour.com/content/pgatour/stats/stat.109.y2020.html'
y_y_base1 <- 'https://www.pgatour.com/content/pgatour/stats/stat.109'
years  <- seq(1970,2020)
years1 <- paste("y",years,sep = "")
years.df <- as.data.frame(years1)%>%
  mutate(p=paste(y_y_base1,years1,sep = "."),
    vec=paste(p,'.html', sep = ""))
yearVector <- as.vector(years.df$vec)

# for (i in yearVector){
#   print(paste("The year is", i))
# }
##
#what to go into function 
year_money_url    <- 'https://www.pgatour.com/content/pgatour/stats/stat.109.y2019.html'
#year_money_url    <- 'https://www.pgatour.com/content/pgatour/stats/stat.109.y2007.html'
year_money_code   <- read_html(year_money_url)
year_money        <- html_nodes(year_money_code,'td , .col-stat')
year_money_text   <- html_text(year_money,trim=T)
year_money.df.prelim     <- as.data.frame(year_money_text)


year_money.df  <- year_money.df.prelim %>% 
  slice(6:nrow(year_money.df.prelim))
#To Dataframe
year_money.df    <- as.data.frame(split(year_money.df, 1:6))%>%
  mutate(Rank=as.numeric(as.character(year_money_text)),
         Name=as.character(year_money_text.2),
         Events=as.numeric(as.character(year_money_text.3)),
         MN=as.character(year_money_text.4),
         Victories=as.numeric(as.character(year_money_text.5)),
         Earnings=as.numeric(gsub("[\\$,]","",MN)))%>%
  select(Rank,Name,Events,Victories,Earnings)%>%
  replace_na(list(Victories=0))
##
t <- year_money.df %>% mutate(EperStart=Earnings/Events)

t %>% filter(Rank<=75)%>%
  ggplot(aes(reorder(Name,EperStart),EperStart, colour=Events))+
  scale_y_continuous(labels = scales::dollar())
  geom_col()+
  coord_flip()


y_y_earnings<-

# Read the html code of the webpage in to the variable webpage_code
y_yearnings_code<-read_html(y_y_earnings)

career_money        <- html_nodes(careerearnings_code,'td , .col-stat')
career_money_text   <- html_text(career_money,trim=T)
career_money.df.prelim     <- as.data.frame(career_money_text)