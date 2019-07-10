# https://google.github.io/styleguide/Rguide.xml
# Open data analyseren van CBS Statline in 10 stappen
# open data protocol... odata
# Package 'cbsodataR' ontwikkeld door ... van het CBS
# https://www.cbs.nl/nl-nl/onze-diensten/open-data
# https://www.cbs.nl/nl-nl/onze-diensten/open-data/databank-cbs-statline-als-open-data
# https://www.cbs.nl/nl-nl/onze-diensten/open-data/hulpmiddel-voor-het-gebruik-van-odata-in-r-en-python
# https://github.com/edwindj/cbsodataR
# https://www.cbs.nl/nl-nl/onze-diensten/open-data/website-open-data
# meta data: https://opendata.cbs.nl/statline/portal.html?_la=nl&_catalog=CBS&tableId=83765NED&_theme=243
# https://opendata.cbs.nl/ODataFeed/odata/83765NED/UntypedDataSet?$format=json
# voor PDF met toelichting variabelen in kerncijfers, ga naar:
# https://www.cbs.nl/nl-nl/maatwerk/2018/30/kerncijfers-wijken-en-buurten-2018
# lijst met gewenste data: naam data frame, cbs id van de dataset
# Stap 1 wat?
require('cbsodataR')
require('tidyverse')
#cbs_toc <- cbs_get_toc("Language" = "nl") #%>% filter(grepl('buurt|wijk|gemeente',ShortDescription) & grepl('2017',Period))
#View(cbs_toc)
cbs.datasets <- list(
  "Gebieden" = "83553NED", # Gebieden in Nederland 2017, bijvoorbeeld 'Zaanstreek/Waterland'
  "Woonplaatsen" = "83689NED", # Woonplaatsen in Nederland 2017
  "Kerncijfers" = "83765NED" # Kerncijfers wijken en buurten 2017
) 
# Stap 2 downloaden
# downloaden kan even duren
for (item in cbs.datasets){
  cbs_id <- item
  path <- file.path(getwd(),cbs_id)
  cbs_download_table(id=cbs_id, dir = path, cache = FALSE, verbose = FALSE)
  print(item)
}
# Stap 4 data opschonen
DataCleansing <- function(x){
  # spaties verwijderen
  x <- str_trim(x) 
  # '.' staat voor 'het cijfer is onbekend, onvoldoende betrouwbaar of geheim', zie toelichting
  x <- if_else(x=='.','',x) 
  # niets (blanco), het cijfer kan op logische gronden niet voorkomen
  x <- if_else(x=='','0',x) 
  return(x)
}
# Stap 5 bestanden en metadata als dataframe laden in R
for (name in names(cbs.datasets)){
  # bestandslocatie, elk bestand heet data.csv
  path.csv <- file.path(getwd(),cbs.datasets[name],"data.csv")
  # bestanden inlezen, maakt dataframe met opgegeven naam
  assign(name, 
         read_csv(file = path.csv, col_types = cols(.default = "c")) # alle kolommen als tekst laden, daarna bewerken
         )
  # metadata inlezen, maakt dataframet met suffix meta
  path.csv <- file.path(getwd(),cbs.datasets[name],"DataProperties.csv")
  assign(paste(name,'meta',sep = '_'), 
         read_csv(file = path.csv, col_types = cols(.default = "c")) # alle kolommen als tekst laden, daarna bewerken
  )
  assign(name,
         get(name) %>% mutate_all(DataCleansing)) # spaties links en rechts verwijderen
  print(name)
}
# Stap 6 Speciefieke bewerking per bestand
Kerncijfers <- Kerncijfers %>% mutate_at(c(6:ncol(Kerncijfers)), as.numeric)
Gebieden <- Gebieden %>% mutate_at(c('Inwonertal_48'), as.numeric)

# Stap 7 GeoDetail, de geografische dimensie laden
# De kerncijfers dataset wordt aangeleverd met de bijbehorende wijken en buurten. Deze kunnen
# namelijk in de loop van de tijd van naam of samenstelling wijzigen. Bijvoorbeeld door het bijbouwen van
# een nieuwe wijk. 
path.csv <- file.path(getwd(),cbs.datasets["Kerncijfers"],"WijkenEnBuurten.csv")
GeoDetail <- read_csv(file = path.csv, col_types = cols(.default = "c"))

# CBS gebruikt de volgende geografische codes, hieronder de codes relevant binnen de dataset 
# 'CBS Kerncijfers Wijken en Buurten' (kolom: DetailRegionCode)

# Stap 8, GeoDetail bewerken
# GM Gemeente
# WK Wijk
# BU Buurt
# entiteitnummer (####), bijvoorbeeld prefix 0439 voor Purmerend
# daarna wijknummer (##), bijvoorbeeld 07 voor Weidevenne
# daarna buurtnummer (##), bijvoorbeeld 02 voor Azie

pattern.wijk.1 <- "^Wijk [0123456789][0123456789]|^Wijk [0123456789]"
# Wijk02 Kunrade, 	Wijk10 Fokkesteeg, 	Wijk16 Huygenhoek
pattern.wijk.2 <- "^Wijk[0123456789][0123456789]"



GeoDetail$DetailRegionCode[GeoDetail$Title=="Nederland"] <- '	NL00'

GeoDetail <- GeoDetail %>% 
  mutate(
    WijkCode   = if_else(str_sub(GeoDetail$DetailRegionCode,1,2)=='BU',
                         paste('WK',str_sub(GeoDetail$DetailRegionCode,3,8), sep = ""),
                         ""),
    Naam   = if_else(str_detect(Title, pattern.wijk.1)==TRUE, 
                     str_sub(Title,9,100), 
                     Title),
    Naam   = if_else(str_detect(Title, pattern.wijk.2)==TRUE, 
                     str_sub(Title,8,100), 
                     Title)
    )

GeoDetail <- GeoDetail %>%   
  select(DetailRegionCode, Municipality, WijkCode, Title, Naam) %>%
  left_join(select(GeoDetail, DetailRegionCode, GemeenteNaam = Naam),by=c("Municipality" = "DetailRegionCode")) %>%
  left_join(select(GeoDetail, DetailRegionCode, WijkNaam = Naam)    ,by=c("WijkCode" = "DetailRegionCode"))

View(GeoDetail)

# Stap 9, data selecteren en combineren met geografische invalshoek 
# "GemiddeldeWoningwaarde_35"
# "GemiddeldElektriciteitsverbruikTotaal_47"
# "VernielingMisdrijfTegenOpenbareOrde_107"

kolom.index <- c(1,3,4,37,49,109) # Kolomnummer in naam +2. De kolommen ID en WijkEnBuurten zijn namelijk niet genummerd.

DataAnalyse <- Kerncijfers %>%
  select(kolom.index) %>%
  left_join(select(GeoDetail,DetailRegionCode, Naam, WijkNaam),
            by=c("WijkenEnBuurten"="DetailRegionCode"))

View(DataAnalyse)

# Stap 10, datavisualisatie met ggplot2

names(Kerncijfers)




