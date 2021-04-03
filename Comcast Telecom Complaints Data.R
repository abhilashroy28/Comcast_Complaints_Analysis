#'* In this data set, factors like population, gender and age were not         *
#'* considered. This was a closed data project, and analysis had to done only  *
#'* with the data provided. Description of steps taken have also been provided *
#'* for easy understanding.                                                    *                              

#'* Load all libraries                                                         *

library(tidyverse)
library(lubridate)
library(ggthemes)
library(tm)
library(tidytext)
library(ggrepel)

#'* Check working directory                                                    *
getwd()
setwd("/users/abhilashroy")
#'* Change working directory                                                   *
setwd(
  "/Users/abhilashroy/desktop/Rstudio | github/Comcast_Complaints_Analysis"
  )

#'* Check if it worked                                                         *
getwd()

#'* Read csv file                                                              *
Complaints <- read.csv("Comcast Telecom Complaints Data.csv")

#'* Check structure of data                                                    *
str(Complaints)

#'*This data set contains 2224 rows & 10 columns.                              *

#'*Now, to check whether all columns contain null values.                      *

length(which(is.na(Complaints$Ticket..)))
length(which(is.na(Complaints$Customer.Complaint)))
length(which(is.na(Complaints$Date)))
length(which(is.na(Complaints$Time)))
length(which(is.na(Complaints$Received.Via)))
length(which(is.na(Complaints$City)))
length(which(is.na(Complaints$State)))
length(which(is.na(Complaints$Zip.code)))
length(which(is.na(Complaints$Status)))
length(which(is.na(Complaints$Filing.on.Behalf.of.Someone)))

#'* None of the columns contain any null values.                               *

#'* View dataset in another tab                                                *
View(Complaints)

#'* Change data type of State, City, Status and Received.Via from              *
#'* Character to Factor                                                        *

Complaints <- Complaints %>% mutate(City = factor(City))
Complaints <- Complaints %>% mutate(State = factor(State))
Complaints <- Complaints %>% mutate(Status = factor(Status))
Complaints <- Complaints %>% mutate(Received.Via = factor(Received.Via))

#'* As some dates contain the character "/"and some contain "-",               *
#'* So, using lubridate package to convert them into date type and             *
#'* similar format. Also convert time to time data type.                       *

Complaints <- Complaints %>% mutate(Date = dmy(Date))
Complaints <- Complaints %>% mutate(Time = hms(Time))

#'* Check the structure of data                                                *
str(Complaints)

levels(Complaints$State)

#'* From the levels of State column, it can be easily seen that "District of   *
#'* Columbia has been mentioned twice with different spellings.                *
#'* One as "District of Columbia" and other as "District Of Columbia"          *
#'* Will change that below                                                     *

Complaints <- Complaints %>% 
  mutate(State = gsub("District Of Columbia","District of Columbia",State)) %>%
  mutate(State = as.factor(State))
 
#'*Changed data type to factor again, as changing data coverts it to character *

#'*Check again.                                                                *

levels(Complaints$State) #'* All good then :D                                  *

#'* Complaints in this data set are from 42 different states.                  *

#'* Below given are the number of complaints in STatus column.                 *

#'*             Status  `length(Status)`    *
#'*             <fct>         <int>         *
#'*            Closed          734          *
#'*             Open           363          *
#'*            Pending         154          *
#'*            Solved          973          *

#'*   => Create a new categorical variable with value as Open and Closed.      *
#'*        Open & Pending is to be categorized as Open, and                    *
#'*        Closed & Solved is to be categorized as Closed.                     * 

Complaints <- Complaints %>%
  mutate(Status = ifelse((Status=='Open'|Status=='Pending'),"Open","Closed"))

Complaints %>% group_by(Status) %>% summarize(length(Status))

Complaints <- Complaints %>% mutate(Status = as.factor(Status))

str(Complaints)

#'*       Conversion done properly                                             *

#'* In the beginning, I will check the no. of complaints at daily              *
#'* granularity level.                                                         *

#'* Before that, will have to create a data frame for positioning of text      *
#'* on the plot. Also check date on which max no. of complaints were made.     *

Complaints %>% 
  group_by(Date) %>% mutate(as.numeric(Date)) %>% 
  summarise(No_of_Complaints = n()) %>% top_n(10)

max_complaints <- 
  data_frame(max_complaints = c("Max complaints -> 218", 
             "Done on 24th June 2015"), 
             x = as.Date(c("2015-04-20", "2015-04-20")), y = c(210,195) )

Complaints %>% 
  group_by(Date) %>% summarise(No_of_Complaints = n()) %>%
  ggplot(aes(Date, No_of_Complaints)) + 
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%d/%m") +
  geom_point(col = "#583d72", size = 0.8) + geom_jitter(alpha = 0.2) +
  geom_rug(color = "#9f5f80") + geom_smooth() +
  geom_text(data = max_complaints, aes(x,y, label = max_complaints), 
            size = 5, color = "#7d0633") + theme_economist() +
  xlab("Date <- dd/mm/yy") + ylab("Number of Complaints") +
  ggtitle("Complaints at daily granularity levels") + 
  theme(axis.text.x = element_text(hjust = 0.5, angle = 90)) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#583d72", size = 20)) +
  theme(axis.title.x = element_text(size = 15, color = "#583d72", 
                                    face = "bold.italic")) +
  theme(axis.title.y = element_text(size = 15, color = "#583d72",  
                                    face = "bold.italic")) +
  geom_hline(yintercept = 218, color = "#9f5f80", lty = 2, size = 1) +
  geom_vline(aes(xintercept=as.Date("2015-06-24")), 
             color = "#9f5f80", lty = 2, size = 1)
  
               
#'* Through the above plot, it can be seen that max no. of complaints on a     *
#'* single day were 218 and were made on 24th June 2015.                       *  

#'* Even though the above plot gives decent overview on daily granularity      *
#'* levels, but even then its not going to be easy to estimate monthly         *
#'* complaints from the same.                                                  *

#'* So, generating a plot for Complaints at monthly granularity levels.        *

Month_Count <- 
  Complaints %>% 
  group_by(MonthsName = as.integer(month(Date)))

Month_Count %>%  group_by(MonthsName) %>% 
  summarize(No_of_Complaints = length(MonthsName)) %>% 
  ggplot(aes(MonthsName, No_of_Complaints, label = No_of_Complaints)) + 
  geom_line(color = "#bb596b", size = 2, lty = 5) +  geom_point() +
  geom_text_repel(nudge_x = 1, size = 6) + geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = c(1:12), 
                  labels = c("January", "February", "March", "April", "May", 
                             "June", "July", "August", "September", "October",
                             "November", "December")) +
  theme_economist() + xlab("Months") + ylab("No. of Complaints") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Complaints at Monthly granularity levels") +
  theme(plot.title = element_text(hjust = 0.5, color = "#bb596b", face = "bold",
                                  size = 20)) +
  theme(axis.title.x = element_text(size = 15, color = "#bb596b", 
                                    face = "bold.italic")) +
  theme(axis.title.y = element_text(size = 15, color = "#bb596b", 
                                    face = "bold.italic")) +
  geom_vline(xintercept = c(1:12), lty = 2, color = "grey")
  
 
#'* Through this, it can be intercepted that highest number of complaints      *
#'* were done in June, whereas the lowest number of complaints were filed in   *
#'* November.                                                                  *

#'* As it can be seen from the data, there are in total 2224 customer          *
#'* complaints. We need to see, which types of words have most frequently      *
#'* been used.                                                                 *

Complaints %>% 
  select(Customer.Complaint) %>%
  mutate(Customer.Complaint = removePunctuation(Customer.Complaint)) %>%
  mutate(Customer.Complaint = tolower(Customer.Complaint)) %>% 
  mutate(Customer.Complaint = stripWhitespace(Customer.Complaint)) %>%
  unnest_tokens(word, Customer.Complaint) %>% 
  count(word) %>% arrange(desc(n)) %>%
  filter(!word %in% stop_words$word) %>%
  filter(word != "comcast") %>%
  head(20)

#'* It can be easily seen that internet has been the most used word for        *
#'* complaints followed by service, billing, data and speed.                   *
#'* Through this its safe to assume that lots of people have issue with        *
#'* Internet and its related aspects.                                          *


#'* To see the Status of Complaints in all the states. Below code/plot.        *

Complaints %>% 
  ggplot(aes(x = fct_infreq(State), fill = Status)) + 
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = c(50,100,150,200,250,300)) +
  xlab("States") +ylab("No. of Complaints") + theme_economist() +
  theme(axis.text.x = element_text(hjust = 0.5, angle = 90)) +
  ggtitle("Status of Complaints", subtitle = "(State Wise)") +
  theme(plot.title = element_text(hjust = 0.5, color = "navy", size = 20)) +
  theme(plot.subtitle = element_text(hjust = 1, color = "dodgerblue4", 
                                     size = 12)) +
  theme(axis.title.x = element_text(color = "royalblue4", size = 15)) +
  theme(axis.title.y = element_text(color = "royalblue4", size = 15)) +
  scale_fill_manual(values = c("#af0069","#09015f"))

#'*    This plot clearly shows that Georgia has the highest number of          *
#'*    combined complaints.                                                    *
#'*    But doesn't portray properly, on which states has the minimum no.       *
#'*    of complaints and how many complaints does top 10 in the list have.     *                                                           *

Complaints %>% 
  group_by(State) %>% 
  summarise(length(State)) %>%
  arrange(`length(State)`) %>%
  top_n(10)

Complaints %>% 
  group_by(State) %>% 
  summarise(length(State)) %>%
  arrange(`length(State)`) %>%
  top_n(-10)

#'* Through above codes, we can easily say that minimum number of complaints   *  
#'* made in any state is 1, and this has been achieved by 4 states.            *
#'* Which are Iowa, Montana, Nevada and Rhode Island.                          *

#'* And, maximum no. of complaints have been done in Georgia.                  *
#'* A total of 288 complaints.                                                 *

str(Complaints)
Complaints %>%
  filter(Status == "Open") %>%
  ggplot(aes(x = fct_infreq(State), fill = Received.Via)) +
  geom_bar() + theme_economist() +
  theme(axis.text.x = element_text(hjust = 0.5, angle = 90)) +
  xlab("States") + ylab("No. of complaints") +
  theme(axis.title.x = element_text(color = "firebrick4", size = 15)) +
  theme(axis.title.y = element_text(color = "firebrick4", size = 15)) +
  ggtitle("States with Unresolved complaints", 
          subtitle = "(Ascending Order)") +
  theme(plot.title = element_text(hjust = 0.5, color = "maroon")) +
  theme(plot.subtitle = element_text(hjust = 1, color = "maroon")) +
  scale_fill_manual(name = "Complaints received via",
                    values = c("#e2703a", "#9c3d54"))
  
 #'* Above plot shows States having unresolved complaints in descending order. *

#'* This shows that Georgia has the highest no. of unresolved complaints.      *

#'* Now, to see the states with resolved complaints.                           *

Complaints %>% 
  filter(Status == "Closed") %>%
  ggplot(aes(x = fct_infreq(State), fill = Received.Via)) +
  geom_bar() + theme_economist() +
  theme(axis.text.x = element_text(hjust = 0.5, angle = 90)) +
  xlab("States") + ylab("No. of complaints") +
  theme(axis.title.x = element_text(color = "olivedrab4", size = 15)) +
  theme(axis.title.y = element_text(color = "olivedrab4", size = 15)) +
  ggtitle("States with Resolved Complaints", 
          subtitle = "(Ascending Order)") +
  theme(plot.title = element_text(hjust = 0.5, color = "dark green")) +
  theme(plot.subtitle = element_text(hjust = 1, color = "dark green")) +
  scale_fill_manual(name = "Complaints recieved via",
                    values = c("#9ecca4","#1f441e" ))

#'* This shows that Georgia has the highest no. of Resolved complaints as well.*

#'* Now, to see the percentage of complaints resolved till date.               *

p <- Complaints %>% group_by(Status) %>% summarize(length(Status))

pie(p$`length(Status)`, 
    labels = paste0(c("Resolved Complaints", "Open Complaints"), 
                    " ", round(c((1707/2224)*100, (517/2224)*100)), "%"),
    col = c("#72C588", "#C8B35C"))

#'* Through this we can see, that in the given time frame,                     *
#'* 77% of the complaints were resolved, whereas                               *
#'* 23% of the complaints are still unresolved.                                *


Complaints_received_via <-
  Complaints %>% filter(Status == "Closed") %>%
  group_by(Received.Via) %>% 
  summarize(length(Received.Via))

pie(Complaints_received_via$`length(Received.Via)`,
      labels = paste0(c("Customer Care Call", "Internet"), 
                      "     ", round(c((864/1707)*100, (843/1707)*100)),
                      "%"), col = c("#72C588", "#C8B35C"))

#'* And, out of those resolved cases, 51% have been from customer care call,   *
#'* and 49% were from internet.                                                *