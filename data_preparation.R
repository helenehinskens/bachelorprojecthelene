library(tidyverse)

#  data inladen
data <- read.csv("awesomedata.csv", sep=";")

# eerst alleen de subset pakken van mensen met een Instagram account
#https://r4ds.had.co.nz/transform.html
colnames(data)

# voor nu hier excluderen wie aangeeft in INC01W13 "ik heb geen account",
# maar evt nog aanvullen met de missingness codes 1111, 5555, 9999
summary(data$INC01W13)


data2 <- filter(data, INC01W13 != 'Ik heb geen account')


# nu maken we een nieuwe variabele 'donated' waarbij 0 gelijk staat aan 0 packages gedeeld
# en 1 gelijk staat aan 1 of meer packages gedeeld


data2 <- data2 %>% 
  mutate(donated = case_when(NAccSD == 0 ~ 0, 
                         TRUE ~ 1)) 

# even controleren met tabel
table(data2$donated, data2$NAccSD)

# Nu is het een kwestie van een hoop schaaltjes maken
# voorbeeld: de self-regulation schaal

# eerst die missing codes veranderen in NAs 
sel <- grepl("var",names(df))

selfregvars <- c("SRT01W14", 
                 "SRT02W14",
                 "SRT03W14",
                 "SRT04W14",
                 "SRT05W14",
                 "SRT06W14",
                 "SRT07W14",
                 "SRT08W14")
data2[selfregvars] <- lapply(data2[selfregvars], 
                  function(x) replace(x,x %in% c("1111","5555","9999"), NA) )

# correlaties checken
cor(data2$SRT01W14,data2$SRT02W14, use="complete.obs")
# deze correlatie is negatief, dus waarschijnlijk moet 1 van deze 2 variablen nog omgeschaald worden
# klopt, zie hieronder de lijst van variabelen die ook een "R" hebben: 

# "SRT02W14R"    "SRT04W14R"    "SRT05W14R"    "SRT07W14R"   "SRT08W14R"

# Wat we dus moeten doen: deze variabelen ook toevoegen een het lijstje selfregvars hierboven
# om er de missingness uit te halen, en controleren of de correlatie dan wel positief is
# en dit voor al deze variabelen doen. 

selfregvars <- c("SRT02W14R", 
                 "SRT04W14R",
                 "SRT05W14R",
                 "SRT07W14R",
                 "SRT08W14R")
data2[selfregvars] <- lapply(data2[selfregvars], 
                             function(x) replace(x,x %in% c("1111","5555","9999"), NA) )

# correlaties checken
cor(data2$SRT01W14,data2$SRT02W14R, use="complete.obs")
# nu is deze correlatie wel positief


# Nu we alle missingness hebben aangepakt en de omschalingen hebben, kunnen we het schaaltje 
# maken als gemiddelde score over al deze variabelen heen: 


data2 <- data2 %>%
  mutate(selfregulation = rowMeans(x = select(.data = data2,
                                   c("SRT01W14", 
                                     "SRT02W14R",
                                     "SRT03W14",
                                     "SRT04W14R",
                                     "SRT05W14R",
                                     "SRT06W14",
                                     "SRT07W14R",
                                     "SRT08W14R"))))

