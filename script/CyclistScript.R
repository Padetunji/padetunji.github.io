#Import all CSVs from a folder.

Cyclist_all<- list.files(path = "C:\\Users\\PROGRESSIVE\\Documents\\Personal\\MOOC\\Google Data Analytics\\Project\\Case Study\\RD - CSV", 
           pattern = "*.csv", full.names = TRUE) %>% 
          lapply(read_csv) %>% 
          bind_rows
          Cyclist_all                         
          
View(Cyclist_all)

Cyclist_all$started_at<- mdy_hm(Cyclist_all$started_at, tz ="UTC") #Convert char to datetime (NB: Use 'UTC' to avoid daylight savings)

Cyclist_all$ended_at<- mdy_hm(Cyclist_all$ended_at, tz ="UTC")     #Convert char to datetime (NB: Use 'UTC' to avoid daylight savings)

Cyclist_all$day<- format(as.Date(Cyclist_all$started_at), "%d")    #Extract day from date

Cyclist_all$day_of_the_wk<- format(as.Date(Cyclist_all$started_at), "%A")     #Extract weekday from date

Cyclist_all$ride_length<- difftime(Cyclist_all$ended_at, Cyclist_all$started_at, units = c("secs")) #Calculate ride duration

Cyclist_all$ride_length<-as.numeric(as.character(Cyclist_all$ride_length)) # Convert ride_length to numeric for calculation purposes

Cyclist_all2<- Cyclist_all[!(Cyclist_all$ride_length<0),] #Remove "ride_"bad data".

View(Cyclist_all2)

#Conduct descriptive analysis

mean(Cyclist_all2$ride_length)          
median(Cyclist_all2$ride_length)        
min(Cyclist_all2$ride_length)           
max(Cyclist_all2$ride_length)           
summary(Cyclist_all2$ride_length)       #Summarise descriptive analysis on one row.

#Compare members and casual users

aggregate(Cyclist_all2$ride_length ~ Cyclist_all2$member_casual, FUN = mean)
aggregate(Cyclist_all2$ride_length ~ Cyclist_all2$member_casual, FUN = median)
aggregate(Cyclist_all2$ride_length ~ Cyclist_all2$member_casual, FUN = max)
aggregate(Cyclist_all2$ride_length ~ Cyclist_all2$member_casual, FUN = min)

# Compare average ride time by each day for members vs casual users
aggregate(Cyclist_all2$ride_length ~ Cyclist_all2$member_casual + Cyclist_all2$day_of_the_wk, FUN = mean)

#Analyse ridership data by type and weekday
Cyclist_all2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
                group_by(member_casual, weekday) %>% 
               dplyr::summarise(number_of_rides = n(), 
                average_duration = mean(ride_length)) %>% 
                arrange(member_casual, weekday) 
                


#Visualise ridership data by type and weekday
Cyclist_all2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  dplyr::summarise(number_of_rides = n(), 
                   average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides, fill= member_casual))+
  geom_col(position = "dodge")

#Visualise ridership data by average duration
Cyclist_all2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  dplyr::summarise(number_of_rides = n(), 
                   average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration, fill= member_casual))+
  geom_col(position = "dodge")

#Export summary for further analysis.
counts<- aggregate(Cyclist_all2$ride_length ~ Cyclist_all2$member_casual + Cyclist_all2$day_of_the_wk, FUN = mean)
write.csv(counts, file = "C:\\Users\\PROGRESSIVE\\Documents\\Personal\\MOOC\\Google Data Analytics\\Project\\Case Study\\CycleProject.csv")

