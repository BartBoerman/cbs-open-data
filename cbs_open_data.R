# Stap 1: Kies één of meerdere datasets uit de datacatalogus
require('cbsodataR')
require('tidyverse')
Inhoudsopgave <- cbs_get_toc("Language" = "nl") %>% 
  filter(grepl('buurt|wijk|gemeente', ShortDescription))
View(Inhoudsopgave)
cbs.datasets <- list(
  # Gebieden in Nederland 2017, bijvoorbeeld 'Zaanstreek/Waterland'
  "Gebieden" = "83553NED", 
  # Woonplaatsen in Nederland 2017
  "Woonplaatsen" = "83689NED",
  # Kerncijfers wijken en buurten 2017 
  "Kerncijfers" = "83765NED"
)
# Stap 2: Download data
for (item in cbs.datasets){
  cbs.id <- item
  path <- file.path(getwd(),cbs.id)
  cbs_download_table(id=cbs.id, dir = path, typed = TRUE, cache = TRUE, verbose = FALSE)
  print(item)
}
# Stap 3: CSV bestanden importeren in R en metadata raadplegen
DataCleansing <- function(x){
  x <- str_trim(x)  
  return(x)
}
for (name in names(cbs.datasets)){
  # Bestandslocatie, elk bestand heet data.csv
  path.csv <- file.path(getwd(),cbs.datasets[name],"data.csv")
  # CSV bestanden inlezen, maakt dataframes met opgegeven naam.
  assign(name, 
         read_csv(file = path.csv, col_types = cols(.default = "c"), locale = readr::locale(encoding = "windows-1252")) # alle kolommen als tekst laden, daarna bewerken
  )
  # CSV bestanden met metadata importeren, maakt dataframes met suffix meta.
  path.csv <- file.path(getwd(),cbs.datasets[name],"DataProperties.csv")
  assign(paste(name,'meta',sep = '_'), 
         read_csv(file = path.csv, col_types = cols(.default = "c"), locale = readr::locale(encoding = "windows-1252")) # alle kolommen als tekst laden, daarna bewerken
  )
  assign(name,
         get(name) %>% mutate_all(DataCleansing)) # spaties links en rechts verwijderen
  print(name)
}
# Metadata bekijken.
View(Kerncijfers_meta)
# Stap 4: Kolommen met meetwaarden converteren naar numeriek
Kerncijfers <- Kerncijfers %>% mutate_at(c(6:ncol(Kerncijfers)), as.numeric)
Gebieden <- Gebieden %>% mutate_at(c('Inwonertal_48'), as.numeric)
# Stap 5: GeoDetail, de dimensie met geografische kenmerken
path.csv <- file.path(getwd(),cbs.datasets["Kerncijfers"],"WijkenEnBuurten.csv")
GeoDetail <- read_csv(file = path.csv, col_types = cols(.default = "c"), locale = readr::locale(encoding = "windows-1252"))
pattern.wijknaam.1 <- "^Wijk [0123456789] "
pattern.wijknaam.2 <- "^Wijk [0123456789][0123456789] "
pattern.wijknaam.3 <- "^Wijk[0123456789][0123456789] "

GeoDetail$DetailRegionCode[GeoDetail$Key=="NL00"] <- 'NL00'

GeoDetail <- GeoDetail %>% 
  mutate(
    SoortRegio = case_when(str_sub(GeoDetail$DetailRegionCode,1,2)=='BU' ~ "Buurt",
                          str_sub(GeoDetail$DetailRegionCode,1,2)=='WK' ~ "Wijk",
                          str_sub(GeoDetail$DetailRegionCode,1,2)=='GM' ~ "Gemeente",
                          str_sub(GeoDetail$DetailRegionCode,1,4)=='NL00' ~ "Nederland",
                          TRUE ~ "Anders"),
    RegioNaam = case_when(str_detect(Title, pattern.wijknaam.1)==TRUE ~ str_sub(Title,8,100),
                          str_detect(Title, pattern.wijknaam.2)==TRUE ~ str_sub(Title,9,100),
                          str_detect(Title, pattern.wijknaam.3)==TRUE ~ str_sub(Title,8,100),
                          TRUE ~ Title),
    WijkCode  = if_else(str_sub(GeoDetail$DetailRegionCode,1,2)=='BU',
                        paste('WK',str_sub(GeoDetail$DetailRegionCode,3,8), sep = ""),
                        "")
    )

GeoDetail <- GeoDetail %>%
  left_join(select(GeoDetail,DetailRegionCode, WijkNaam = RegioNaam),
                    by=c("WijkCode"="DetailRegionCode")) %>% 
  select(Municipality,DetailRegionCode,SoortRegio,RegioNaam,WijkCode,WijkNaam)
View(GeoDetail)
# Stap 6: Eindelijk, CBS data analyseren!
kolom.index <- c(1:4,37,49,109) 
DataSelectie <- Kerncijfers %>%
  select(kolom.index) %>%
  left_join(select(GeoDetail,DetailRegionCode, RegioNaam, WijkNaam),
            by=c("WijkenEnBuurten"="DetailRegionCode"))
View(DataSelectie)
