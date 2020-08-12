library(readr)
library(knitr)
library(dplyr)
library(readxl)
library(reshape2)



izracunaj_tabelo_za_nazaj <- function(prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, trenutna_placa, tabela_kolicnikov){
  izbrani_kolicniki <- tabela_kolicnikov[tabela_kolicnikov$leto >=  prva_zaposlitev_leto,]
  posodobljeni_kolicniki <- data.frame("leto" = izbrani_kolicniki$leto, "mesec" = izbrani_kolicniki$mesec, "tecaj" = izbrani_kolicniki$tecaj)
  revalorizacijski_faktor <- posodobljeni_kolicniki[posodobljeni_kolicniki$leto == prva_zaposlitev_leto,]
  revalorizacijski_faktor <- revalorizacijski_faktor[posodobljeni_kolicniki$mesec == prva_zaposlitev_mesec,3][1]
  revalorizirana_prva_placa <- prva_placa * revalorizacijski_faktor
  povprecna_mesecna_rast_place <- (trenutna_placa / revalorizirana_prva_placa) ** (1/(2020 * 12 - prva_zaposlitev_leto * 12 + 8 - prva_zaposlitev_mesec))
  
  leta_nazaj <- c(rep.int(prva_zaposlitev_leto, (12 - prva_zaposlitev_mesec + 1)), sort(rep((prva_zaposlitev_leto + 1):2019, 12)), rep.int(2020,8))
  meseci_nazaj <- c(prva_zaposlitev_mesec : 12, rep.int(1:12, 2019 - prva_zaposlitev_leto), 1:8)
  tabela_nazaj <- data.frame("leto" = leta_nazaj, "mesec" = meseci_nazaj)
  stevilo_mesecev <- (leta_nazaj - prva_zaposlitev_leto) * 12 + (meseci_nazaj - prva_zaposlitev_mesec)
  faktorji <- povprecna_mesecna_rast_place ** stevilo_mesecev
  tabela_nazaj$faktor <- faktorji
  tabela_nazaj$placa <- revalorizirana_prva_placa * tabela_nazaj$faktor
  tabela_nazaj$osnova <- tabela_nazaj$placa * (1 - 0.3537) #odštejemo poprečne stopnjo davka http://www.pisrs.si/Pis.web/pregledPredpisa?id=DRUG4701

  tabela_nazaj
}

izracunaj_tabelo_za_naprej <- function(starost_leto, starost_mesec, upokojitvena_starost_leto, upokojitvena_starost_mesec, pricakovana_rast_place, trenutna_placa){

  mesecna_rast_place <- pricakovana_rast_place ** (1/12)
  print(mesecna_rast_place)
  leta_naprej <- NULL
  meseci_naprej <- NULL
  stevilo_preostalih_mesecev <- (upokojitvena_starost_leto - starost_leto) * 12 + upokojitvena_starost_mesec - starost_mesec - 1
  
  if (stevilo_preostalih_mesecev <= 4){
      leta_naprej <- rep.int(2020, stevilo_preostalih_mesecev)
      meseci_naprej <- c(9:(9 + stevilo_preostalih_mesecev -1))
  } else {
    stevilo_celih_let <- (stevilo_preostalih_mesecev - 4) %/% 12
    if ((stevilo_preostalih_mesecev - 4) %% 12 == 0){
      leta_naprej <- c(rep.int(2020,4), sort(rep.int(2021:(2020 + stevilo_celih_let), 12)))
      meseci_naprej <- c(9:12, rep(1:12, stevilo_celih_let))
    } else{
      leta_naprej <- c(rep.int(2020,4), sort(rep.int(2021:(2020 + stevilo_celih_let), 12)), rep(2020 + stevilo_celih_let + 1, (stevilo_preostalih_mesecev - 4) %% 12))
      meseci_naprej <- c(9:12, rep(1:12, stevilo_celih_let), 1:((stevilo_preostalih_mesecev - 4) %% 12))
    }
  }

  tabela_naprej <- data.frame("leto" = leta_naprej, "mesec" = meseci_naprej)
  print(head(tabela_naprej))
  tabela_naprej$faktor <- mesecna_rast_place ** (seq.int(from = 1, to = (upokojitvena_starost_leto - starost_leto) * 12 + upokojitvena_starost_mesec - starost_mesec - 1, by = 1))
  tabela_naprej$placa <- tabela_naprej$faktor * trenutna_placa
  tabela_naprej$osnova <- tabela_naprej$placa * (1 - 0.3537)
  
  tabela_naprej
}



izracunaj_pokojninsko_osnovo <- function(tabela_kolicnikov, starost_leto, starost_mesec, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, trenutna_placa, pricakovana_rast_place, upokojitvena_starost_leto, upokojitvena_starost_mesec){

  tabela_nazaj <- izracunaj_tabelo_za_nazaj(prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, trenutna_placa, tabela_kolicnikov)
  tabela_naprej <- izracunaj_tabelo_za_naprej(starost_leto, starost_mesec, upokojitvena_starost_leto, upokojitvena_starost_mesec, pricakovana_rast_place, trenutna_placa)
  tabela_skupna <- rbind(tabela_nazaj, tabela_naprej)

    cx <- c(0,cumsum(tabela_skupna$osnova))
  if (length(tabela_skupna$osnova) >= 25){
    povprecja <- (cx[(24+1):length(cx)] - cx[1:(length(cx) - 24)]) / 24
  } else {
    povprecja <- mean(tabela_skupna$osnova)
  }
  
  pokojninska_osnova <- max(povprecja)
  print(paste0("osnova je ", pokojninska_osnova))
  pokojninska_osnova
}


#############################################################################
#############################################################################

odmerni_odstotki <- read_excel("odmerni odstotki.xlsx")
colnames(odmerni_odstotki) <- c("doba", "odstotek_moski", "odstotek_zenski")


izracunaj_odmerni_odstotek <- function(odstotki, delovna_doba, spol){
  tabela <- NULL
  koef <- 0
  if (spol == "m"){
    koef <- 1.26
    tabela <- odstotki[,1:2]
  } else {
    koef <- 1.36
    tabela <- odstotki[,c(1,3)]
  }

    if (delovna_doba <= 40){
    odstotek <- tabela[tabela$doba == floor(delovna_doba), 2][[1]] + floor(2 * (delovna_doba - floor(delovna_doba))) * koef/2
  } else if (delovna_doba <= 43) {
    odstotek <- tabela[tabela$doba == 40, 2][[1]] + floor((delovna_doba - 40)*2) * 1.5
  } else {
    odstotek <- tabela[tabela$doba == 40, 2][[1]] + 9 + floor((delovna_doba - 43)*2) * koef/2
  } 
  odstotek <- odstotek / 100 
  print(paste0("odstotek je ", odstotek))
  odstotek
}


############################################################################


izracunaj_pokojnino <- function(spol, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, trenutna_placa, pricakovana_rast_place, najnizja_pokojninska_osnova, najvisja_pokojninska_osnova, starost_leto, starost_mesec, upokojitvena_starost_leto, upokojitvena_starost_mesec){

  skupna_delovna_doba <- ((upokojitvena_starost_leto - starost_leto) * 12  + upokojitvena_starost_mesec  - starost_mesec  + (2020 - prva_zaposlitev_leto) * 12 + 8 - prva_zaposlitev_mesec ) /12

  if (upokojitvena_starost_leto < 58 || skupna_delovna_doba < 15 || (upokojitvena_starost_leto < 65 & skupna_delovna_doba < 40)){
    return("Pogoji za starostno pokojnino niso izpolnjeni.")
  } else{
    
    tabela_kolicnikov <- read_excel("revalorizacijski faktorji.xlsx", skip = 2)
    colnames(tabela_kolicnikov) <- c("datum", "mesec", "leto", "st_enot1", "valuta1", "st_enot2", "valuta2", "tecaj")

    pokojninska_osnova <- izracunaj_pokojninsko_osnovo(tabela_kolicnikov, starost_leto, starost_mesec, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, trenutna_placa, pricakovana_rast_place, upokojitvena_starost_leto, upokojitvena_starost_mesec)
    
    odstotek <- izracunaj_odmerni_odstotek(odmerni_odstotki, skupna_delovna_doba, spol)
    pokojnina_po_placi <- pokojninska_osnova * odstotek * 1.032
    pokojnina_najnizja_osnova <- najnizja_pokojninska_osnova * odstotek
    pokojnina_najvisja_osnova <- najvisja_pokojninska_osnova * odstotek
    print(paste0("pokojnina_po_placi", pokojnina_po_placi))
    print(paste0("pokojnina_najnizja_osnova", pokojnina_najnizja_osnova))
    print(paste0("pokojnina_najvisja_osnova", pokojnina_najvisja_osnova))
    
    
    zajamcena_pokojnina <- 0
    
    pokojnina <- pokojnina_po_placi
    if (skupna_delovna_doba >= 40 && starost_leto <= 60){
      zajamcena_pokojnina <- 555.76
    } 
    
    if (pokojnina_po_placi >= pokojnina_najvisja_osnova){
      pokojnina <- pokojnina_najvisja_osnova
    } else if (pokojnina_po_placi <= pokojnina_najnizja_osnova){
      pokojnina <- max(pokojnina_najnizja_osnova, zajamcena_pokojnina)
    }
    print(round(pokojnina,2))
    return(round(pokojnina,2))
  }
  
}

# tabela_kolicnikov <- read_excel("kolicniki.xlsx")
# 
 # prva_zaposlitev_leto <- 2006
 # prva_zaposlitev_mesec <- 1
 # prva_placa <- 2000
 # trenutna_placa <- 2000
 # starost_leto <- 40
 # starost_mesec <- 0
 # upokojitvena_starost_leto <- 65
 # upokojitvena_starost_mesec <- 0
 # pricakovana_rast_place <- 1.02
 # najnizja_pokojninska_osnova <- 900
 # najvisja_pokojninska_osnova <- 3600




izracunaj_tabelo_za_nazaj2 <- function(dosedanje_place,trenutna_placa, prva_placa, prva_zaposlitev_leto, prva_zaposlitev_mesec){
  dolzina <- length(dosedanje_place)
  tabela_plac <- NULL
  if (dolzina == 0){
    mesecna_rast <- (trenutna_placa/prva_placa) ** (1/((2020 - prva_zaposlitev_leta) * 12 + 8 - prva_zaposlitev_mesec))
    letna_rast <- mesecna_rast ** 12
    leta <- seq.int(from = prva_zaposlitev_leta, to = 2020, by = 1)
    place <- prva_placa * mesecna_rast ** seq (from = 0, to = 2020-prva_zaposlitev_leta)
    tabela_plac <- data.frame("leto" = leta, "placa" = place)
  
  } else if (dolzina == 1){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta, to = prva_zaposlitev_leta + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (trenutna_placa/dosedanje_place[1]) ** (1/(2020 - prva_zaposlitev_leta - 5))
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta + 5, to = 2020, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leta - 5))
    tabela_plac <- rbind(tabela1, tabela2)
    
  } else if (dolzina == 2){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta, to = prva_zaposlitev_leta + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta + 5, to = prva_zaposlitev_leta + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (trenutna_placa/dosedanje_place[2]) ** (1/(2020 - prva_zaposlitev_leta - 10))
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta + 10, to = 2020, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leta - 10))
    tabela_plac <- rbind(tabela1, tabela2,tabela3)
    
  }else if (dolzina == 3){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta, to = prva_zaposlitev_leta + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta + 5, to = prva_zaposlitev_leta + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (dosedanje_place[3]/dosedanje_place[2]) ** (1/5)
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leta + 10, to = prva_zaposlitev_leto + 14, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 4))
    rast4 <- (trenutna_placa/dosedanje_place[3]) ** (1/(2020 - prva_zaposlitev_leto-15))
    tabela4 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 15, to = 2020, by = 1), "placa" = dosedanje_place[3] * rast4 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leto - 15))
    tabela_plac <- rbind(tabela1, tabela2, tabela3, tabela4)
    
  }else if (dolzina == 4){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto, to = prva_zaposlitev_leto + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 5, to = prva_zaposlitev_leto + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (dosedanje_place[3]/dosedanje_place[2]) ** (1/5)
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 10, to = prva_zaposlitev_leto + 14, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 4))
    rast4 <- (dosedanje_place[4]/dosedanje_place[3]) ** (1/5)
    tabela4 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 15, to = prva_zaposlitev_leto + 19, by = 1), "placa" = dosedanje_place[3] * rast4 ** seq.int(from = 0, to = 4))
    rast5 <- (trenutna_placa/dosedanje_place[4]) ** (1/(2020 - prva_zaposlitev_leto - 20))
    tabela5 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 20, to = 2020, by = 1), "placa" = dosedanje_place[4] * rast5 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leto - 20))
    tabela_plac <- rbind(tabela1, tabela2, tabela3, tabela4, tabela5)
    
  }else if (dolzina == 5){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto, to = prva_zaposlitev_leto + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 5, to = prva_zaposlitev_leto + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (dosedanje_place[3]/dosedanje_place[2]) ** (1/5)
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 10, to = prva_zaposlitev_leto + 14, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 4))
    rast4 <- (dosedanje_place[4]/dosedanje_place[3]) ** (1/5)
    tabela4 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 15, to = prva_zaposlitev_leto + 19, by = 1), "placa" = dosedanje_place[3] * rast4 ** seq.int(from = 0, to = 4))
    rast5 <- (dosedanje_place[5]/dosedanje_place[4]) ** (1/5)
    tabela5 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 20, to = prva_zaposlitev_leto + 20, by = 1), "placa" = dosedanje_place[4] * rast5 ** seq.int(from = 0, to = 4))
    rast6 <- (trenutna_placa/dosedanje_place[5]) ** (1/(2020 - prva_zaposlitev_leto - 25))
    tabela6 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 25, to = 2020, by = 1), "placa" = dosedanje_place[5] * rast6 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leto - 25))
    tabela_plac <- rbind(tabela1, tabela2, tabela3, tabela4, tabela5, tabela6)
    
  }else if (dolzina == 6){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto, to = prva_zaposlitev_leto + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 5, to = prva_zaposlitev_leto + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (dosedanje_place[3]/dosedanje_place[2]) ** (1/5)
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 10, to = prva_zaposlitev_leto + 14, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 4))
    rast4 <- (dosedanje_place[4]/dosedanje_place[3]) ** (1/5)
    tabela4 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 15, to = prva_zaposlitev_leto + 19, by = 1), "placa" = dosedanje_place[3] * rast4 ** seq.int(from = 0, to = 4))
    rast5 <- (dosedanje_place[5]/dosedanje_place[4]) ** (1/5)
    tabela5 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 20, to = prva_zaposlitev_leto + 24, by = 1), "placa" = dosedanje_place[4] * rast5 ** seq.int(from = 0, to = 4))
    rast6 <- (dosedanje_place[6]/dosedanje_place[5]) ** (1/5)
    tabela6 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 25, to = prva_zaposlitev_leto + 29, by = 1), "placa" = dosedanje_place[5] * rast6 ** seq.int(from = 0, to = 4))
    rast7 <- (trenutna_placa/dosedanje_place[5]) ** (1/(2020 - prva_zaposlitev_leto - 30))
    tabela7 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 30, to = 2020, by = 1), "placa" = dosedanje_place[6] * rast7 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leto - 30))
    tabela_plac <- rbind(tabela1, tabela2, tabela3, tabela4, tabela5, tabela6, tabela7)
    
  } else if (dolzina == 7){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto, to = prva_zaposlitev_leto + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 5, to = prva_zaposlitev_leto + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (dosedanje_place[3]/dosedanje_place[2]) ** (1/5)
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 10, to = prva_zaposlitev_leto + 14, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 4))
    rast4 <- (dosedanje_place[4]/dosedanje_place[3]) ** (1/5)
    tabela4 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 15, to = prva_zaposlitev_leto + 19, by = 1), "placa" = dosedanje_place[3] * rast4 ** seq.int(from = 0, to = 4))
    rast5 <- (dosedanje_place[5]/dosedanje_place[4]) ** (1/5)
    tabela5 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 20, to = prva_zaposlitev_leto + 24, by = 1), "placa" = dosedanje_place[4] * rast5 ** seq.int(from = 0, to = 4))
    rast6 <- (dosedanje_place[6]/dosedanje_place[5]) ** (1/5)
    tabela6 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 25, to = prva_zaposlitev_leto + 29, by = 1), "placa" = dosedanje_place[5] * rast6 ** seq.int(from = 0, to = 4))
    rast7 <- (dosedanje_place[7]/dosedanje_place[6]) ** (1/5)
    tabela7 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 30, to = prva_zaposlitev_leto + 34, by = 1), "placa" = dosedanje_place[6] * rast7 ** seq.int(from = 0, to = 4))
    rast7 <- (trenutna_placa/dosedanje_place[5]) ** (1/(2020 - prva_zaposlitev_leto - 35))
    tabela7 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 35, to = 2020, by = 1), "placa" = dosedanje_place[6] * rast7 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leto - 35))
    tabela_plac <- rbind(tabela1, tabela2, tabela3, tabela4, tabela5, tabela6, tabela7, tabela8)
    
  } else if (dolzina == 8){
    rast1 <- (dosedanje_place[1]/prva_placa) ** (1/5)
    tabela1 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto, to = prva_zaposlitev_leto + 4, by = 1), "placa" = prva_placa * rast1 ** seq.int(from = 0, to = 4))
    rast2 <- (dosedanje_place[2]/dosedanje_place[1]) ** (1/5)
    tabela2 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 5, to = prva_zaposlitev_leto + 9, by = 1), "placa" = dosedanje_place[1] * rast2 ** seq.int(from = 0, to = 4))
    rast3 <- (dosedanje_place[3]/dosedanje_place[2]) ** (1/5)
    tabela3 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 10, to = prva_zaposlitev_leto + 14, by = 1), "placa" = dosedanje_place[2] * rast3 ** seq.int(from = 0, to = 4))
    rast4 <- (dosedanje_place[4]/dosedanje_place[3]) ** (1/5)
    tabela4 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 15, to = prva_zaposlitev_leto + 19, by = 1), "placa" = dosedanje_place[3] * rast4 ** seq.int(from = 0, to = 4))
    rast5 <- (dosedanje_place[5]/dosedanje_place[4]) ** (1/5)
    tabela5 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 20, to = prva_zaposlitev_leto + 24, by = 1), "placa" = dosedanje_place[4] * rast5 ** seq.int(from = 0, to = 4))
    rast6 <- (dosedanje_place[6]/dosedanje_place[5]) ** (1/5)
    tabela6 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 25, to = prva_zaposlitev_leto + 29, by = 1), "placa" = dosedanje_place[5] * rast6 ** seq.int(from = 0, to = 4))
    rast7 <- (dosedanje_place[7]/dosedanje_place[6]) ** (1/5)
    tabela7 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 30, to = prva_zaposlitev_leto + 34, by = 1), "placa" = dosedanje_place[6] * rast7 ** seq.int(from = 0, to = 4))
    rast8 <- (trenutna_placa/dosedanje_place[5]) ** (1/(2020 - prva_zaposlitev_leto - 35))
    tabela8 <- data.frame("leto" = seq.int(from = prva_zaposlitev_leto + 35, to = 2020, by = 1), "placa" = dosedanje_place[6] * rast7 ** seq.int(from = 0, to = 2020 - prva_zaposlitev_leto - 35))
    tabela_plac <- rbind(tabela1, tabela2, tabela3, tabela4, tabela5, tabela6, tabela7, tabela8)
    
  }
  
  tabela_plac$osnova <- tabela_plac$placa * (1 - 0.3537)
  tabela_preteklih_osnov <- tabela_plac[,c(1,3)]
  tabela_preteklih_osnov
}


izracunaj_tabelo_za_naprej2 <- function(starost, upokojitvena_starost, pricakovana_rast_place, trenutna_placa){
  
  pricakovana_rast_place = 1 + pricakovana_rast_place/100
  leta_naprej <- seq.int(from = 2021, to = 2020 + (upokojitvena_starost - starost - 1), by = 1)
  faktorji_naprej <- pricakovana_rast_place ** (1:(upokojitvena_starost - starost - 1))
  place_naprej <- faktorji_naprej * trenutna_placa
  tabela_prihodnjih_plac <- data.frame("leto" = leta_naprej, "placa" = place_naprej)
  tabela_prihodnjih_plac$osnova <- tabela_prihodnjih_plac$placa * (1 - 0.3537)
  tabela_prihodnjih_osnov <- tabela_prihodnjih_plac[,c(1,3)]
  tabela_prihodnjih_osnov
}


izracunaj_pokojninsko_osnovo2 <- function(tabela_kolicnikov, starost, prva_zaposlitev_leto, prva_placa, trenutna_placa, pricakovana_rast_place, upokojitvena_starost, dosedanje_place){
  tabela_nazaj <- izracunaj_tabelo_za_nazaj2(dosedanje_place,trenutna_placa, prva_placa, prva_zaposlitev_leto)
  tabela_naprej <- izracunaj_tabelo_za_naprej2(starost, upokojitvena_starost, pricakovana_rast_place, trenutna_placa)
  tabela_skupna <- rbind(tabela_nazaj, tabela_naprej)
  colnames(tabela_skupna) <- c("let", "osnove")
  
  cx <- c(0,cumsum(tabela_skupna$osnove))
  if (length(tabela_skupna$osnove) >= 25){
    povprecja <- (cx[(24+1):length(cx)] - cx[1:(length(cx) - 24)]) / 24
  } else {
    povprecja <- mean(tabela_skupna$osnove)
  }
  
  pokojninska_osnova <- max(povprecja)
  pokojninska_osnova
}



izracunaj_pokojnino2 <- function(prva_zaposlitev_leto, prva_placa, trenutna_placa, pricakovana_rast_place, najnizja_pokojninska_osnova, najvisja_pokojninska_osnova, starost, upokojitvena_starost, dosedanje_place){
  print(dosedanje_place)
  skupna_delovna_doba <- upokojitvena_starost - starost  + (2020 - prva_zaposlitev_leto)
  if (upokojitvena_starost < 58 || skupna_delovna_doba < 15 || (upokojitvena_starost < 65 & skupna_delovna_doba < 40)){
    return("Pogoji za starostno pokojnino niso izpolnjeni.")
  } else{
    
    tabela_kolicnikov <- read_excel("kolicniki.xlsx")
    colnames(tabela_kolicnikov) <- c("leto", "kolicnik")

    pokojninska_osnova <- izracunaj_pokojninsko_osnovo2(tabela_kolicnikov, starost, prva_zaposlitev_leto, prva_placa, trenutna_placa, pricakovana_rast_place, upokojitvena_starost, dosedanje_place)
    print(pokojninska_osnova)
    odstotek <- izracunaj_odmerni_odstotek(odmerni_odstotki, skupna_delovna_doba)/100

    pokojnina_po_placi <- pokojninska_osnova * odstotek * 1.032
    pokojnina_najnizja_osnova <- najnizja_pokojninska_osnova * odstotek
    pokojnina_najvisja_osnova <- najvisja_pokojninska_osnova * odstotek
    
    zajamcena_pokojnina <- 0
    
    pokojnina <- pokojnina_po_placi
    if (skupna_delovna_doba == 40 && starost <= 60){
      zajamcena_pokojnina <- 555.76
    } 
    if (pokojnina_po_placi >= pokojnina_najvisja_osnova){
      pokojnina <- pokojnina_najvisja_osnova
    } else if (pokojnina_po_placi <= pokojnina_najnizja_osnova){
      pokojnina <- max(pokojnina_najnizja_osnova, zajamcena_pokojnina)
    }
    pokojnina <- round(pokojnina,2)
    print(round(pokojnina,2))
    pokojnina
  }
  
}


#dosedanje_place <- c(input$`placa po 5 letih`, input$`placa po 10 letih`, input$`placa po 15 letih`, input$`placa po 20 letih`, input$`placa po 25 letih`, input$`placa po 30 letih`, input$`placa po 35 letih`)[1:floor(delovna_doba/5)] 

  
