# Mappeoppgave 1

## i sammarbeid med Vemund Furuhovde og Sofia-Andrea M?en

###Laster inn pakker
library(tidyverse)
library(ggplot2)
library(readr)
library(zoo)
library(data.table)

#Oppgave 1 til Mappe 1

##lagre url og leser denne
data_url <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"

temp <- read_lines(data_url)

##fjerner de siste radene
temp <- head(temp, -12)

##lager datatable
df <- read_table(temp)

Lav_tropo <- df

##Plotter denne
Lav_tropo %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(Globe, 13,
                                   align="left",
                                   fill=0)) %>% 
  ggplot(aes(x=date)) + geom_point(aes(y=Globe), col="deepskyblue3") + 
  geom_line(aes(y=Globe), col="blue") +
  geom_line(aes(y=moving_average),
            color = "red",
            size = 0.8)+
  geom_hline(yintercept = 0) +
  labs(title = "Latest Global Temps",
       x ="Latest Global Average Tropospheric Temperatures", 
       y ="T Departurefrom '91-'20 Avg (deg.C)") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))


##Ble ikke det perfekte resultat i forhold til
##orginalbildet, men ikke veldig langt unna.





#Oppgave 2

##leser inn og lager plot til hver enkelt for og se paa de.

##datasett 1
data_url <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"
temp <- read_lines(data_url)
temp <- head(temp, -12)
df <- read_table(temp)
Lav_tropo <- df

p1 <-
  Lav_tropo %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))

p1 %>% ggplot(aes(x=date, y=moving_average)) +
  geom_line(col="dark green") +
  labs(x = " ",
       y = "") +
  theme_bw()




##datasett2
data_url2 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
Mid_tropo <- read_lines(data_url2)
Mid_tropo
Mid_tropo <- head(Mid_tropo, -12)
Mid_tropo <- df <- read_table(Mid_tropo)



p2 <-
  Mid_tropo %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))

p2 %>% ggplot(aes(x=date, y=moving_average)) +
  geom_line(col="blue") +
  labs(x = " ",
       y = "") +
  theme_bw()




##datasett3
data_url3 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
Tropopause <- read_lines(data_url3)
Tropopause
Tropopause <- head(Tropopause, -12)
Tropopause <- df <- read_table(Tropopause)


p3<-
  Tropopause %>% select("NoPol", "Year", "Mo") %>% 
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))

p3 %>% ggplot(aes(x=date, y=moving_average)) +
  geom_line(col="purple") +
  labs(x = " ",
       y = "") +
  theme_bw()



##datasett4
data_url4 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
Lav_stratos <- read_lines(data_url4)
Lav_stratos
Lav_stratos <- head(Lav_stratos, -12)
Lav_stratos <- df <- read_table(Lav_stratos)

p4<-
  Lav_stratos %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))

p4 %>% ggplot(aes(x=date, y=moving_average)) +
  geom_line( col="orange") +
  labs(x = " ",
       y = "") +
  theme_bw()



##leser alle datasett og binder disse sammen til ett datasett
##derretter ser på NoPol i plot for å se på den.
data_url <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"
temp <- read_lines(data_url)
temp <- head(temp, -12)
df <- read_table(temp)
Lav_tropo <- df

data_url2 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
Mid_tropo <- read_lines(data_url2)
Mid_tropo
Mid_tropo <- head(Mid_tropo, -12)
Mid_tropo <- df <- read_table(Mid_tropo)

data_url3 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
Tropopause <- read_lines(data_url3)
Tropopause
Tropopause <- head(Tropopause, -12)
Tropopause <- df <- read_table(Tropopause)

data_url4 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
Lav_stratos <- read_lines(data_url4)
Lav_stratos
Lav_stratos <- head(Lav_stratos, -12)
Lav_stratos <- df <- read_table(Lav_stratos)

alle <- rbind(Lav_tropo, Lav_stratos, Tropopause, Mid_tropo, deparse.level = 2)

p5 <- alle %>% 
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>%
  mutate(moving_average = rollmean(NoPol, 13, fill = NA, align = "left"))


#p5 <- alle %>% 
#  mutate(moving_average=zoo::rollmean(NoPol, 13, na.pad = TRUE)) %>%
#  mutate(paste(Year, Mo, 1, sep="-")) %>%
#  mutate(as.Date(paste(1979, 3, 1, sep="-")))

#p5 %>% ggplot(aes(x=date, y=moving_average)) +
#  geom_line( col="pink") +
#  labs(x = " ",
#       y = "") +
#  theme_bw() 

#lager plot for alle i en.
#alle %>% 
#  ggplot(aes(x=Year)) +
#  geom_line(aes(y=alle, color="Gjennomsnittalle")) +
#  geom_point(aes(y=p1, color="Lower Troposphere")) +
#  geom_point(aes(y=p2, color="Mid-Troposphere")) +
#  geom_point(aes(y=p3, color="Tropopause")) +
#  geom_point(aes(y=p4, color="Lower Stratosphere")) +
#  labs(title="Temprature between 60 - 90 degrees north", 
#       x = "Year", 
#       y = " Average Temprature") + 
#  theme_bw()

alle %>% ggplot() +
  geom_line(data = p1, aes(x = date, y = moving_average, color = "Lower Troposphere")) +
  geom_line(data = p2, aes(x = date, y = moving_average, color = "Mid-Troposphere")) +
  geom_line(data = p3, aes(x = date, y = moving_average, color = "Tropopause")) +
  geom_line(data = p4, aes(x = date, y = moving_average, color = "Lower Stratosphere")) +
  labs(title="Temperature between 60 - 90 degrees north",
       x = "Year",
       y = "Average Temperature") + 
  theme_bw() +
  scale_color_manual(values = c("Lower Troposphere" = "green",
                                "Mid-Troposphere" = "blue",
                                "Tropopause" = "purple",
                                "Lower Stratosphere" = "orange"))

