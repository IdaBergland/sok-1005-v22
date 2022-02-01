#Mappeoppgave 2

#samarbeid om koder med Vemund Furuhovde og Sofia-Andrea Møen

#laster ned pakker man trenger
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(dplyr)

#oppgave 1

#laste inn dataene og lagre den
jason1 <- "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json"

#lese dataene og gjøre de til en dataramme
df <- fromJSON(jason1)

#endre navn
covid19 <- df

#endrer for å få 0-100 på x aksen istedet for 0-1,
#forkorter navnene, samt endrer name til stater
covid19 <- covid19 %>%
  mutate(short_name = abbreviate(covid19$name, minlength=4)) %>%
  mutate(fully_vaccinated = fully_vaccinated_pct_of_pop*100) %>%
  rename(Stater=name)

#lager et enkelt og oversiktelig plot et uten og et med en tilpasset linje til.
covid19 %>% ggplot(aes(x= fully_vaccinated, y= deaths_per_100k)) +
  geom_point(aes(colour=Stater)) + 
  labs(title= "Covid-19 relaterte dødsfall i USA i forhold til andel vaksinerte",
       x= "Prosent av populasjonen fullvaksinert",
       y= "Dødsfall pr 100 000") +
  theme_gray()+ 
  geom_text(aes(label = short_name, adj= -0.2, cex=3.5))+
  annotate("text", x= 60, y =17, 
           label = "       🢄 Lavere andel vaksinerte, 
høyere antall døde",
           col = "black",
           size = 4) +
  annotate("text", x= 73, y =9, 
           label = "       🢆 Høyere andel vaksinerte, 
lavere antall døde",
           col = "black",
           size = 4)+
  scale_x_continuous(breaks = c(45,50,55,60,65,70,75,80),
                     labels = function(x) paste0(x, "%"))

#med linje med smooth(kun linje ekstra)
covid19 %>% ggplot(aes(x= fully_vaccinated, y= deaths_per_100k)) +
  geom_point(aes(colour=Stater)) + 
  geom_smooth(method="loess", se=F) +
  labs(title= "Covid-19 relaterte dødsfall i USA i forhold til andel vaksinerte",
       x= "Prosent av populasjonen fullvaksinert",
       y= "Dødsfall pr 100 000") +
  theme_gray()+ 
  geom_text(aes(label = short_name, adj= -0.2, cex=3.5))+
  annotate("text", x= 60, y =17, 
           label = "       🢄 Lavere andel vaksinerte, 
høyere antall dødsfall",
           col = "black",
           size = 4) +
  annotate("text", x= 73, y =9, 
           label = "       🢆 Høyere andel vaksinerte, 
lavere antall døde",
           col = "black",
           size = 4)+
  scale_x_continuous(breaks = c(45,50,55,60,65,70,75,80),
                     labels = function(x) paste0(x, "%"))

#Du kan se at de statene som har en høyere andel vaksinerte 
#har en lavere andel døde.




#Oppgave 2

#bruker lm() funksjonen
lm(deaths_per_100k ~ fully_vaccinated, data= covid19)

#lager et plott med smooth og lm.
covid19 %>% ggplot(aes(x= fully_vaccinated, y= deaths_per_100k)) +
  geom_point(aes(colour=Stater)) + 
  geom_smooth(method= lm) +
  labs(title= "Covid-19 relaterte dødsfall i USA i forhold til andel vaksinerte",
       x= "Prosent av populasjonen som er fullvaksinert", y= "Dødsfall pr 100 000 pr Mnd") +
  theme_gray() + 
  geom_text(aes(label = short_name, adj= -0.2))+
  annotate("text", x= 60, y =17, 
           label = "       🢄 Lavere antall vaksinerte, 
høyere antall døde",
           col = "black",
           size = 4) +
  annotate("text", x= 73, y =9, 
           label = "       🢆 Høyere antall vaksinerte, 
lavere antall døde",
           col = "black",
           size = 4)+
  scale_x_continuous(breaks = c(45,50,55,60,65,70,75,80),
                     labels = function(x) paste0(x, "%"))

#Den rette blå linja indikerer på at jo flere som vaksinerer seg jo 
#færre antall døde er det, man kan se at i de statene hvor andelen 
#vaksinerte er over 75% andel vaksinerte har lavest antall døde.
#den grå linjen visen en linær regresjonsmodell, altså den viser
#statistisk sett muligheten hvor den blå linjen kan være, den er
#litt mykere enn den harde linjen, jeg prøver å forstå den, men
#mangler litt verktøy matematisk for å forstå den fullt ut. Min antakelse er
#derfor at den tar med et bredere perspektiv på de dataene vi har enn en rett linje.
