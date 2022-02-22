# Oppgave 3


#Laste inn pakker man trenger

library(tidyverse)
library(rvest)
library(ggplot2)
library(stringr)

#"Oppgave 1"


#lagre nettsiden OG lese den
webside <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132/bil")

#lagre den som tabeller
tables <- webside %>% html_table(fill = TRUE)

#ser på hvor mange tables jeg har og jeg finner ut at jeg
#skal ha den første
length(tables)

#trekke ut rette tabell
lengdeelbil <- tables[[1]]
lengdeelbil <- lengdeelbil[-1,]

#gjor om til dataframe og fjerner to ufullstendige rader
lengdeelbil <- as.data.frame(lengdeelbil)
lengdeelbil
lengdeelbil <- lengdeelbil[-c(19, 26), ]
lengdeelbil

######
#deler kolonnene og får bort km fra verdiene
#lengdeelbil <- lengdeelbil %>% separate(X2, c('WLTP', 'km', 'kWh'))
#lengdeelbil <- lengdeelbil %>% separate(X3, c('Stopp', 'km'))
#sjekker hva tallene er og de er character og må endres til tallverdier
#typeof('WLTP')
#typeof('Stopp')
#######

#Da denne måten ikke fungerte på plottet googlet jeg på nette over tips og endte med
# å bruke mutate og as.numeric funskjonene istedet.
#siden jeg brukte for hvordan bruke as.numeric, da jeg var usikker på denne:
#https://www.rdocumentation.org/packages/sjlabelled/versions/1.1.8/topics/as_numeric,
#samt pensumboken.

#lage nye navn til kolonnene 
colnames(lengdeelbil) <- c("Modell (temp. varierte fra 0° til -10°)", "WLTP", "Stopp", "Avvik")
lengdeelbil

#her bruker jeg mutate og as.numeric for å endre fra "character" og for å ta bort 
#km og det jeg ikke skal bruke
lengdeelbil <- lengdeelbil %>% 
  mutate(WLTP = as.numeric(gsub("km.*", "", WLTP)))
lengdeelbil <- lengdeelbil %>% 
  mutate(Stopp = as.numeric(gsub("km.*", "", Stopp)))

#ser på tabellen
lengdeelbil

#Plot til oppgave 1
#for å definere litt så er WLTP en måleenhet for bruket på elbilen , inkudert co2 og rekkevidde den skal klare
# og stopp er lengden den har kjørt når den blir tom for energi og må lades.

lengdeelbil %>% ggplot(aes(x=WLTP, y= Stopp)) +
  geom_point() +
  geom_abline(col="blue", size = 1) +
  labs(title= "Rekkevidde på EL-bil", 
       x= "WLTP", 
       y= "Stopp") +
  theme_gray()

#plot 1 ble ikke helt bra så måtte endre litt på aksene.
lengdeelbil %>% ggplot(aes(x = WLTP, y = Stopp)) +
  geom_point() +
  labs(title = "Rekkevidde på elbil",
       x= "WLTP, Estimatet på kjørelengde", 
       y= "Stopp, Faktisk kjørelengde",
       subtitle = "Linjen viser hvor langt bilene egentlig skulle kjørt") +
  theme_gray() +
  scale_y_continuous(limits = c(200, 600)) +
  scale_x_continuous(limits = c(200, 600)) +
  geom_abline(col = "orange",
              size = 1)
  
#her ser man at den oransje linjen skal forestille hva elbilene
#egentlig skulle kjørt



#Oppgave 2

#bruker lm funskjonen
lm(Stopp ~ WLTP, data=lengdeelbil)
#her ser vi at koffedesienten er ca -26,64 og Wltpen er ca 0.87

#så plottet
lengdeelbil %>% ggplot(aes(x = WLTP, y = Stopp)) +
  geom_point() +
  geom_smooth(method= lm) +
  labs(title= "Kjørelengde på elbil", 
       x= "WLTP", 
       y= "Stopp",
       subtitle = "Linjen viser hvor langt bilene egentlig skulle kjørt") +
  theme_gray() +
  scale_y_continuous(limits = c(200, 600)) +
  scale_x_continuous(limits = c(200, 600)) +
  geom_abline(col = "red",
              size = 1)

# Coefficients : Intercept viser -26.6450 og er tallet på hvor krysningspunktet til den blå linjen er
#på y aksen når x er lik 0.
#WLTP forteller hvor mye denne linære linjen øker når x øker med 1.

#den tegnede linjen kan tolkes som at den ligger noenlunde jevnt med
#den røde lovede linjen og har en bra økning, den røde linjen viser 
#hvor bilene egentlig skulle stoppet. Alikevel så er ikke resultatene
#så langt unna. den blå viser et slags gjennomsnitt av resultatene.
#Alt i alt så er de ikke så langt unna det de lover..





##