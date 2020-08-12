library(readr)
library(knitr)
library(dplyr)
library(readxl)
library(reshape2)


# povprečni prispevki v letih 2016-2019
stopnje_prispevkov <- read_xlsx("povprecni prispevki.xlsx")
print(head(stopnje_prispevkov))


#funkcija, ki revalorizira plače
revaloriziraj <- function(placa, kolicniki, izbrano_leto, izbrani_mesec){
  posodobljeni_kolicniki <- data.frame("leto" = kolicniki$leto, "mesec" = kolicniki$mesec, "tecaj" = kolicniki$tecaj)
  aux_kolicniki <- posodobljeni_kolicniki[posodobljeni_kolicniki$leto == izbrano_leto,]
  revalorizacijski_faktor <- aux_kolicniki[posodobljeni_kolicniki$mesec == izbrani_mesec, 3][1]
  placa * revalorizacijski_faktor
}


#tabela za čas do vključno datuma izračuna
izracunaj_tabelo_za_nazaj <- function(prva_zaposlitev_leto, prva_zaposlitev_mesec, izracun_leto, izracun_mesec, prva_placa, aktualna_placa, tabela_kolicnikov){
  zadnji_delovni_mesec <- NULL
  zadnje_delovno_leto <- NULL
  
  if (izracun_mesec == 1){
    zadnji_delovni_mesec = 12
    zadnje_delovno_leto = izracun_leto - 1
  } else {
    zadnji_delovni_mesec <- izracun_mesec - 1
    zadnje_delovno_leto <- izracun_leto
  }
  revalorizirana_prva_placa <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec)
  print(revaloriziraj(1,tabela_kolicnikov, izracun_leto, izracun_mesec))
  revalorirana_na_izracun <- revalorizirana_prva_placa / revaloriziraj(1,tabela_kolicnikov, izracun_leto, izracun_mesec)
  print(revalorirana_na_izracun)
  povprecna_mesecna_rast_place <- (aktualna_placa / revalorirana_na_izracun) ** (1/((izracun_leto - prva_zaposlitev_leto) * 12 + izracun_mesec - prva_zaposlitev_mesec))

  leta_nazaj <- c(rep.int(prva_zaposlitev_leto, (12 - prva_zaposlitev_mesec + 1)), sort(rep((prva_zaposlitev_leto + 1) : (zadnje_delovno_leto-1), 12)), rep.int(zadnje_delovno_leto, zadnji_delovni_mesec))
  meseci_nazaj <- c(prva_zaposlitev_mesec : 12, rep.int(1:12, zadnje_delovno_leto - 1 - prva_zaposlitev_leto), 1:zadnji_delovni_mesec)
  tabela_nazaj <- data.frame("leto" = leta_nazaj, "mesec" = meseci_nazaj)
  
  faktorji <- povprecna_mesecna_rast_place ** seq.int(from = 0, to = length(meseci_nazaj) - 1)
  tabela_nazaj$placa <- revalorirana_na_izracun * faktorji
  print(head(tabela_nazaj))
  tabela_nazaj
}





#tabela za čas po datumu izračuna
izracunaj_tabelo_za_naprej <- function(izracun_leto, izracun_mesec, starost_leto, starost_mesec, upokojitev_leto, upokojitev_mesec, pricakovana_rast_place, aktualna_placa){
  
  mesecna_rast_place <- pricakovana_rast_place ** (1/12)
  leta_naprej <- NULL
  meseci_naprej <- NULL

  stevilo_preostalih_mesecev <- (upokojitev_leto - izracun_leto) * 12 + upokojitev_mesec - izracun_mesec

  if (stevilo_preostalih_mesecev <= (12 - izracun_mesec)){
      leta_naprej <- rep.int(izracun_leto, stevilo_preostalih_mesecev)
      meseci_naprej <- c((izracun_mesec + 1):((izracun_mesec + 1) + stevilo_preostalih_mesecev -1))
  } else {
    stevilo_celih_let <- (stevilo_preostalih_mesecev - (12 - izracun_mesec )) %/% 12

    if ((stevilo_preostalih_mesecev - (12 - izracun_mesec)) %% 12 == 0){
      leta_naprej <- c(rep.int(izracun_leto, (12 - izracun_mesec )), sort(rep.int((izracun_leto + 1):(izracun_leto + stevilo_celih_let), 12)))
      meseci_naprej <- c((izracun_mesec + 1):12, rep(1:12, stevilo_celih_let))
    } else if (izracun_mesec == 12){
      if (stevilo_celih_let >= 1){
        leta_naprej <- c(sort(rep.int((izracun_leto + 1):(izracun_leto + stevilo_celih_let), 12)), rep(izracun_leto + stevilo_celih_let + 1, stevilo_preostalih_mesecev %% 12))
        meseci_naprej <- c(rep(1:12, stevilo_celih_let), 1:(stevilo_preostalih_mesecev %% 12))
      } else {
        leta_naprej <- c(rep(izracun_leto + 1, stevilo_preostalih_mesecev %% 12))
        meseci_naprej <- c(rep(1:12, stevilo_celih_let), 1:(stevilo_preostalih_mesecev %% 12))
      }
    } else {
      if (stevilo_celih_let >= 1){
        leta_naprej <- c(rep.int(izracun_leto, (12 - izracun_mesec )), sort(rep.int((izracun_leto + 1):(izracun_leto + stevilo_celih_let), 12)), rep(izracun_leto + stevilo_celih_let + 1, (stevilo_preostalih_mesecev - (12 - izracun_mesec)) %% 12))
        meseci_naprej <- c((izracun_mesec + 1): 12, rep(1:12, stevilo_celih_let), 1:((stevilo_preostalih_mesecev -  (12 - izracun_mesec )) %% 12))
      } else {
        leta_naprej <- c(rep.int(izracun_leto, (12 - izracun_mesec )), rep(izracun_leto + 1, (stevilo_preostalih_mesecev - (12 - izracun_mesec)) %% 12))
        meseci_naprej <- c((izracun_mesec + 1): 12, rep(1:12, stevilo_celih_let), 1:((stevilo_preostalih_mesecev -  (12 - izracun_mesec )) %% 12))
      }
    }
  }
  
  tabela_naprej <- data.frame("leto" = leta_naprej, "mesec" = meseci_naprej)
  faktorji <- mesecna_rast_place ** (seq.int(from = 1, to = (upokojitev_leto - izracun_leto) * 12 + upokojitev_mesec - izracun_mesec, by = 1))
  
  tabela_naprej$placa <- aktualna_placa * faktorji
  
  tabela_naprej
}






#funkcija, ki izracuna pokojninsko osnovo
izracunaj_pokojninsko_osnovo <- function(tabela_kolicnikov, starost_leto, starost_mesec, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, pricakovana_rast_place, izracun_leto, izracun_mesec, upokojitev_leto, upokojitev_mesec){

  tabela_nazaj <- izracunaj_tabelo_za_nazaj(prva_zaposlitev_leto, prva_zaposlitev_mesec, izracun_leto, izracun_mesec, prva_placa, aktualna_placa, tabela_kolicnikov)
  tabela_naprej <- izracunaj_tabelo_za_naprej(izracun_leto, izracun_mesec, starost_leto, starost_mesec, upokojitev_leto, upokojitev_mesec, pricakovana_rast_place, aktualna_placa)
  tabela_skupna <- rbind(tabela_nazaj, tabela_naprej)
  print(paste("skupna"))
  print(head(tabela_skupna))
  tabela_letne_place <- head(tabela_skupna %>% group_by(leto) %>% summarise(letna_placa = sum(placa)), -1) 
  print(tabela_letne_place)
  
  prispevki <- NULL
  leta <- unique(tabela_letne_place$leto)
  zadnje_leto <- max(leta)
  if (zadnje_leto <= 2019){
    prispevki <- stopnje_prispevkov[stopnje_prispevkov$leto %in% leta, 2][[1]]
  } else {
    aux_prispevki <- stopnje_prispevkov[stopnje_prispevkov$leto %in% leta, ]
    prispevki1 <- aux_prispevki[aux_prispevki$leto <= 2019, 2][[1]]
    prispevki2 <- rep(0.3537, zadnje_leto - 2019)

    prispevki <- rev(c(prispevki2, prispevki1))
  }
  print(paste("prispevki"))
  print(prispevki)
  tabela_letne_place$letna_osnova <- tabela_letne_place$letna_placa * (1 - prispevki)
  tabela_letne_place$osnova <- tabela_letne_place$letna_osnova / 12
  print(head(tabela_letne_place))
  cx <- c(0,cumsum(tabela_letne_place$osnova))
  povprecja <- NULL
  
  if (length(tabela_letne_place$osnova) >= 25){
    povprecja <- (cx[25:length(cx)] - cx[1:(length(cx) - 24)]) / 24
  } else {
    povprecja <- mean(tabela_letne_place$osnova)
  }
  print(povprecja)
  pokojninska_osnova <- max(povprecja)
  print(paste0("osnova je ", pokojninska_osnova))
  pokojninska_osnova
}


#############################################################################
#############################################################################




izracunaj_odmerni_odstotek <- function(odstotki, delovna_doba, spol, upokojitev_leto){
  tabela <- NULL
  koef <- 0

  if(upokojitev_leto >= 2020){
    if (spol == "m"){
      koef <- 1.26
      tabela <- odstotki[,c(1,3)]
    } else {
      koef <- 1.36
      tabela <- odstotki[,1:2]
    }
  } else if (upokojitev_leto >= 2017){
    if (spol == "m"){
      koef <- 1.25
      tabela <- odstotki[,c(1,3)]
    } else {
      koef <- 1.38
      tabela <- odstotki[,1:2]
    }
  } else {
    if (spol == "m"){
      koef <- 1.25
      tabela <- odstotki[,c(1,3)]
    } else {
      koef <- 1.41
      tabela <- odstotki[,1:2]
    }
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


izracunaj_pokojnino <- function(spol, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, pricakovana_rast_place, starost_leto, starost_mesec, izracun_leto, izracun_mesec, upokojitev_leto, upokojitev_mesec){
  
  # pridobi podatke o pokojninah v posameznem letu
  tabela_visine_pokojnin <- read_xlsx("visine_pokojnin.xlsx")
  tabela_visine_pokojnin_aux <- tabela_visine_pokojnin[(tabela_visine_pokojnin$leto - upokojitev_leto) * 12 + tabela_visine_pokojnin$mesec - upokojitev_mesec <= 0,]
  pokojnine <- tabela_visine_pokojnin_aux[length(tabela_visine_pokojnin_aux$leto),3:6]

  # pridobitev ustrzne tabele odmernih odstotkov
  odmerni_odstotki <- NULL
  if (upokojitev_leto <= 2016){
    odmerni_odstotki <- read_excel("odstotki20132019.xlsx", sheet = 1)
    colnames(odmerni_odstotki) <- c("doba", "zenske", "moski")
  } else if (upokojitev_leto <= 2019){
    odmerni_odstotki <- read_excel("odstotki20132019.xlsx", sheet = 2)
    colnames(odmerni_odstotki) <- c("doba", "zenske", "moski")
  } else {
    odmerni_odstotki <- read_excel("odmerni odstotki.xlsx")
    colnames(odmerni_odstotki) <- c("doba", "zenske", "moski")
  }
  
  #izračun celotne delovne dobe in datum upokojitve
  skupna_delovna_doba <- ((upokojitev_leto - prva_zaposlitev_leto) * 12  + upokojitev_mesec  - prva_zaposlitev_mesec) / 12
  upokojitvena_starost_leto <- (starost_leto  * 12  + starost_mesec  + upokojitev_leto * 12 - 2020 * 12 + upokojitev_mesec - 8) %/% 12
  upokojitvena_starost_mesec <- (starost_leto  * 12  + starost_mesec  + upokojitev_leto * 12 - 2020 * 12 + upokojitev_mesec - 8) %% 12
  
  if (upokojitvena_starost_leto < 58 || skupna_delovna_doba < 15 || (upokojitvena_starost_leto < 65 & skupna_delovna_doba < 40)){
    return("Pogoji za starostno pokojnino niso izpolnjeni.")
  } else{
    
    #pridobi valorizacijske količnike
    tabela_kolicnikov <- read_xlsx("revalorizacijski faktorji.xlsx", skip = 2)
    colnames(tabela_kolicnikov) <- c("datum", "mesec", "leto", "st_enot1", "valuta1", "st_enot2", "valuta2", "tecaj")
    
    #izračun pokojninske osnove
    pokojninska_osnova <- izracunaj_pokojninsko_osnovo(tabela_kolicnikov, starost_leto, starost_mesec, prva_zaposlitev_leto, prva_zaposlitev_mesec, 
                                                       prva_placa, aktualna_placa, pricakovana_rast_place, izracun_leto, izracun_mesec, upokojitev_leto, upokojitev_mesec)
  
    #izračun odmernega odstotka
    odstotek <- izracunaj_odmerni_odstotek(odmerni_odstotki, skupna_delovna_doba, spol, upokojitev_leto)
    
    #izračun  uskladitev
    tabela_uskladitev <- tabela_visine_pokojnin[, c(1,2,7)]
    aux_tabela <- tabela_uskladitev[tabela_uskladitev$leto == upokojitev_leto,]
    aux_tabela <- tabela_uskladitev[tabela_uskladitev$mesec <= upokojitev_mesec,]
    uskladitveni_faktor = 1
    if (length(aux_tabela$leto) > 0){
      uskladitveni_faktor <- prod(aux_tabela[,3])
    }
    
    #Izračun pokojnin za vsako vrsto
    pokojnina_po_placi <- pokojninska_osnova * odstotek * uskladitveni_faktor
    pokojnina_najnizja_osnova <- pokojnine[3] * odstotek
    pokojnina_najvisja_osnova <- pokojnine[4] * odstotek
    najnizja_pokojnina <- pokojnine[2]
    
    print(paste0("Najnižja pokojnina znaša: ", najnizja_pokojnina))
    print(paste0("pokojnina_po_placi znaša: ", pokojnina_po_placi))
    print(paste0("pokojnina_najnizja_osnova znaša: ", pokojnina_najnizja_osnova))
    print(paste0("pokojnina_najvisja_osnova znaša: ", pokojnina_najvisja_osnova))
    
    zajamcena_pokojnina <- 0
    # preverimo, če je upravičen do zagotovljene pokojnine
    if (skupna_delovna_doba >= 40){
      zajamcena_pokojnina <- pokojnine[1]
    } 
    
    # preverimo, ali je njegova pokojnina znotraj dovoljenih
    pokojnina <- pokojnina_po_placi
    
    if (pokojnina_po_placi >= pokojnina_najvisja_osnova){
      pokojnina <- pokojnina_najvisja_osnova
    } else if (pokojnina_po_placi <= pokojnina_najnizja_osnova){
      pokojnina <- max(pokojnina_najnizja_osnova, zajamcena_pokojnina, najnizja_pokojnina)
    }
    print(round(pokojnina,2))
    return(round(pokojnina,2))
  }
  
}





















################ FUNKCIJE ZA NATANČNEJŠI IZRAČUN #################################




izracunaj_tabelo_za_nazaj2 <- function(dosedanje_place, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, izracun_leto, izracun_mesec){
  dolzina <- length(dosedanje_place)
  
  #pridobi valorizacijske količnike
  tabela_kolicnikov <- read_xlsx("revalorizacijski faktorji.xlsx", skip = 2)
  colnames(tabela_kolicnikov) <- c("datum", "mesec", "leto", "st_enot1", "valuta1", "st_enot2", "valuta2", "tecaj")

  #NAJPREJ NAREDIMO tabele za vsa končana 5-letna obdobja
  tabela <- data.frame("leto" = integer(), "mesec" = integer(), "placa" = numeric())
  if (dolzina == 0){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    
  } else if (dolzina == 1){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    rast <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor <- rast ** (0:59)
    
    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = revalorizirana0 * faktor)

    
  } else if (dolzina == 2){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2)
    
    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)
    
    
  }else if (dolzina == 3){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana3 <- revaloriziraj(dosedanje_place[3], tabela_kolicnikov, prva_zaposlitev_leto + 15, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    rast3 <- (revalorizirana3/revalorizirana2) ** (1/60)
    faktor3 <- rast3 ** (0:59)
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2, revalorizirana2 * faktor3)
    
    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)
    
    
  }else if (dolzina == 4){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana3 <- revaloriziraj(dosedanje_place[3], tabela_kolicnikov, prva_zaposlitev_leto + 15, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana4 <- revaloriziraj(dosedanje_place[4], tabela_kolicnikov, prva_zaposlitev_leto + 20, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    rast3 <- (revalorizirana3/revalorizirana2) ** (1/60)
    faktor3 <- rast3 ** (0:59)
    rast4 <- (revalorizirana4/revalorizirana3) ** (1/60)
    faktor4 <- rast4 ** (0:59)
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2, revalorizirana2 * faktor3, revalorizirana3 * faktor4)
    
    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)
    
    
  }else if (dolzina == 5){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana3 <- revaloriziraj(dosedanje_place[3], tabela_kolicnikov, prva_zaposlitev_leto + 15, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana4 <- revaloriziraj(dosedanje_place[4], tabela_kolicnikov, prva_zaposlitev_leto + 20, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana5 <- revaloriziraj(dosedanje_place[5], tabela_kolicnikov, prva_zaposlitev_leto + 25, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    rast3 <- (revalorizirana3/revalorizirana2) ** (1/60)
    faktor3 <- rast3 ** (0:59)
    rast4 <- (revalorizirana4/revalorizirana3) ** (1/60)
    faktor4 <- rast4 ** (0:59)
    rast5 <- (revalorizirana5/revalorizirana4) ** (1/60)
    faktor5 <- rast5 ** (0:59)
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2, revalorizirana2 * faktor3, revalorizirana3 * faktor4,
               revalorizirana4 * faktor5)
    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)
    
    
  }else if (dolzina == 6){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana3 <- revaloriziraj(dosedanje_place[3], tabela_kolicnikov, prva_zaposlitev_leto + 15, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana4 <- revaloriziraj(dosedanje_place[4], tabela_kolicnikov, prva_zaposlitev_leto + 20, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana5 <- revaloriziraj(dosedanje_place[5], tabela_kolicnikov, prva_zaposlitev_leto + 25, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana6 <- revaloriziraj(dosedanje_place[6], tabela_kolicnikov, prva_zaposlitev_leto + 30, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    
    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    rast3 <- (revalorizirana3/revalorizirana2) ** (1/60)
    faktor3 <- rast3 ** (0:59)
    rast4 <- (revalorizirana4/revalorizirana3) ** (1/60)
    faktor4 <- rast4 ** (0:59)
    rast5 <- (revalorizirana5/revalorizirana4) ** (1/60)
    faktor5 <- rast5 ** (0:59)
    rast6 <- (revalorizirana6/revalorizirana5) ** (1/60)
    faktor6 <- rast6 ** (0:59)
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2, revalorizirana2 * faktor3, revalorizirana3 * faktor4,
               revalorizirana4 * faktor5, revalorizirana5 * faktor6)
    
    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)
    
  } else if (dolzina == 7){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana3 <- revaloriziraj(dosedanje_place[3], tabela_kolicnikov, prva_zaposlitev_leto + 15, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana4 <- revaloriziraj(dosedanje_place[4], tabela_kolicnikov, prva_zaposlitev_leto + 20, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana5 <- revaloriziraj(dosedanje_place[5], tabela_kolicnikov, prva_zaposlitev_leto + 25, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana6 <- revaloriziraj(dosedanje_place[6], tabela_kolicnikov, prva_zaposlitev_leto + 30, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana7 <- revaloriziraj(dosedanje_place[7], tabela_kolicnikov, prva_zaposlitev_leto + 35, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    
    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    rast3 <- (revalorizirana3/revalorizirana2) ** (1/60)
    faktor3 <- rast3 ** (0:59)
    rast4 <- (revalorizirana4/revalorizirana3) ** (1/60)
    faktor4 <- rast4 ** (0:59)
    rast5 <- (revalorizirana5/revalorizirana4) ** (1/60)
    faktor5 <- rast5 ** (0:59)
    rast6 <- (revalorizirana6/revalorizirana5) ** (1/60)
    faktor6 <- rast6 ** (0:59)
    rast7 <- (revalorizirana7/revalorizirana6) ** (1/60)
    faktor7 <- rast7 ** (0:59)
    
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2, revalorizirana2 * faktor3, revalorizirana3 *  faktor4,
               revalorizirana4 * faktor5, revalorizirana5 * faktor6, revalorizirana6 * faktor7)

    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)

  } else if (dolzina == 8){
    revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
    revalorizirana0 <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana1 <- revaloriziraj(dosedanje_place[1], tabela_kolicnikov, prva_zaposlitev_leto + 5, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana2 <- revaloriziraj(dosedanje_place[2], tabela_kolicnikov, prva_zaposlitev_leto + 10, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana3 <- revaloriziraj(dosedanje_place[3], tabela_kolicnikov, prva_zaposlitev_leto + 15, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana4 <- revaloriziraj(dosedanje_place[4], tabela_kolicnikov, prva_zaposlitev_leto + 20, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana5 <- revaloriziraj(dosedanje_place[5], tabela_kolicnikov, prva_zaposlitev_leto + 25, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana6 <- revaloriziraj(dosedanje_place[6], tabela_kolicnikov, prva_zaposlitev_leto + 30, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana7 <- revaloriziraj(dosedanje_place[7], tabela_kolicnikov, prva_zaposlitev_leto + 35, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
    revalorizirana8 <- revaloriziraj(dosedanje_place[8], tabela_kolicnikov, prva_zaposlitev_leto + 40, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun

    rast1 <- (revalorizirana1/revalorizirana0) ** (1/60)
    faktor1 <- rast1 ** (0:59)
    rast2 <- (revalorizirana2/revalorizirana1) ** (1/60)
    faktor2 <- rast2 ** (0:59)
    rast3 <- (revalorizirana3/revalorizirana2) ** (1/60)
    faktor3 <- rast3 ** (0:59)
    rast4 <- (revalorizirana4/revalorizirana3) ** (1/60)
    faktor4 <- rast4 ** (0:59)
    rast5 <- (revalorizirana5/revalorizirana4) ** (1/60)
    faktor5 <- rast5 ** (0:59)
    rast6 <- (revalorizirana6/revalorizirana5) ** (1/60)
    faktor6 <- rast6 ** (0:59)
    rast7 <- (revalorizirana7/revalorizirana6) ** (1/60)
    faktor7 <- rast7 ** (0:59)
    rast8 <- (revalorizirana8/revalorizirana7) ** (1/60)
    faktor8 <- rast8 ** (0:59)
    
    place <- c(revalorizirana0 * faktor1, revalorizirana1 * faktor2, revalorizirana2 * faktor3, revalorizirana3 * faktor4,
               revalorizirana4 * faktor5, revalorizirana5 * faktor6, revalorizirana6 * faktor7, revalorizirana7 * faktor8)

    mesec <- NULL
    leta <- NULL
    if (prva_zaposlitev_mesec == 1){
      mesec <- c(rep(1:12, 5 * dolzina))
      leta <- c(sort(rep(prva_zaposlitev_leto : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)))
    }else{
      mesec <- c(prva_zaposlitev_mesec : 12, rep(1:12, 5 * dolzina - 1), 1:(prva_zaposlitev_mesec - 1))
      leta <- c(rep(prva_zaposlitev_leto, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 1) : (prva_zaposlitev_leto + 5 * dolzina - 1), 12)), rep(prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec - 1))
    }
    
    tabela <- data.frame("leto" = leta, "mesec" = mesec, "placa" = place)
  }
  

  # DODAJ ŠE ZA ZADNJE OBDOBJE
  zadnji_delovni_mesec <- NULL
  zadnje_delovno_leto <- NULL
  
  if (izracun_mesec == 1){
    zadnji_delovni_mesec = 12
    zadnje_delovno_leto = izracun_leto - 1
  } else {
    zadnji_delovni_mesec <- izracun_mesec - 1
    zadnje_delovno_leto <- izracun_leto
  }
  
  koncno_leto <- prva_zaposlitev_leto + 5 * dolzina
  stevilo_preostalih_mesecev <- izracun_leto * 12 + izracun_mesec - koncno_leto * 12 - prva_zaposlitev_mesec + 1
  stevilo_celih_let <- ifelse(prva_zaposlitev_mesec == 1, (stevilo_preostalih_mesecev - (12 - prva_zaposlitev_mesec + 1)) %/% 12, stevilo_preostalih_mesecev %/% 12)
  
  leta <- NULL
  meseci <- NULL
  


  if (stevilo_preostalih_mesecev <= (12 - prva_zaposlitev_mesec + 1)){
      leta <- c(rep(prva_zaposlitev_leto + 5 * dolzina, stevilo_preostalih_mesecev))
      meseci <- c(prva_zaposlitev_mesec : (prva_zaposlitev_mesec + stevilo_preostalih_mesecev - 1))
  } else if (stevilo_celih_let == 0){
      leta <- c(rep(prva_zaposlitev_leto + 5 * dolzina, 12 - prva_zaposlitev_mesec + 1), rep(prva_zaposlitev_leto + 5 * dolzina))
      meseci <- c(prva_zaposlitev_mesec : 12, 1: (stevilo_preostalih_mesecev - (12 - prva_zaposlitev_mesec  + 1)))
  } else {
    if(izracun_mesec == 1){
      leta <- c(rep(prva_zaposlitev_leto + 5 * dolzina, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 5 * dolzina + 1) : zadnje_delovno_leto, 12)))
      meseci <- c(prva_zaposlitev_mesec : 12, rep(1:12, stevilo_celih_let))
    } else {
      leta <- c(rep(prva_zaposlitev_leto + 5 * dolzina, 12 - prva_zaposlitev_mesec + 1), sort(rep((prva_zaposlitev_leto + 5 * dolzina + 1) : (izracun_leto - 1), 12)), rep(izracun_leto,izracun_mesec - 1))
      meseci <- c(prva_zaposlitev_mesec : 12, rep(1:12, stevilo_celih_let), 1 : (izracun_mesec - 1))
    }
  }  
                
  
  revalorizacijski_faktor_izracun <- revaloriziraj(1, tabela_kolicnikov, izracun_leto, izracun_mesec)
  revalorizirana_zadnja_petletna <- NULL
  if (dolzina == 0){
    revalorizirana_zadnja_petletna <- revaloriziraj(prva_placa, tabela_kolicnikov, prva_zaposlitev_leto, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
  } else {
    revalorizirana_zadnja_petletna <- revaloriziraj(dosedanje_place[dolzina],tabela_kolicnikov, prva_zaposlitev_leto + 5 * dolzina, prva_zaposlitev_mesec) * revalorizacijski_faktor_izracun
  }
  mesecna_rast <- (aktualna_placa/revalorizirana_zadnja_petletna) ** (1/((zadnje_delovno_leto - prva_zaposlitev_leto - 5 * dolzina) * 12 + zadnji_delovni_mesec - prva_zaposlitev_mesec))
  place <- revalorizirana_zadnja_petletna * mesecna_rast ** seq.int(from = 0, to = (zadnje_delovno_leto - prva_zaposlitev_leto - 5 * dolzina) * 12 + zadnji_delovni_mesec - prva_zaposlitev_mesec)
  tabela_zadnja <- data.frame("leto" = leta, "mesec" = meseci, "placa" = place)
  tabela_nazaj <- rbind(tabela, tabela_zadnja)
  tabela_nazaj
}



izracunaj_tabelo_za_naprej2 <- function(izracun_leto, izracun_mesec, starost_leto, starost_mesec, upokojitev_leto, upokojitev_mesec, pricakovana_rast_place, aktualna_placa){

  mesecna_rast_place <- pricakovana_rast_place ** (1/12)
  leta_naprej <- NULL
  meseci_naprej <- NULL
  
  stevilo_preostalih_mesecev <- (upokojitev_leto - izracun_leto) * 12 + upokojitev_mesec - izracun_mesec
  
  if (stevilo_preostalih_mesecev <= (12 - izracun_mesec)){
    leta_naprej <- rep.int(izracun_leto, stevilo_preostalih_mesecev)
    meseci_naprej <- c((izracun_mesec + 1):((izracun_mesec + 1) + stevilo_preostalih_mesecev - 1))
  } else {
    stevilo_celih_let <- (stevilo_preostalih_mesecev - (12 - izracun_mesec )) %/% 12
    
    if ((stevilo_preostalih_mesecev - (12 - izracun_mesec)) %% 12 == 0){
      leta_naprej <- c(rep.int(izracun_leto, (12 - izracun_mesec )), sort(rep.int((izracun_leto + 1):(izracun_leto + stevilo_celih_let), 12)))
      meseci_naprej <- c((izracun_mesec + 1):12, rep(1:12, stevilo_celih_let))
    } else if (izracun_mesec == 12){
      if (stevilo_celih_let >= 1){
        leta_naprej <- c(sort(rep.int((izracun_leto + 1):(izracun_leto + stevilo_celih_let), 12)), rep(izracun_leto + stevilo_celih_let + 1, stevilo_preostalih_mesecev %% 12))
        meseci_naprej <- c(rep(1:12, stevilo_celih_let), 1:(stevilo_preostalih_mesecev %% 12))
      } else {
        leta_naprej <- c(rep(izracun_leto + 1, stevilo_preostalih_mesecev %% 12))
        meseci_naprej <- c(rep(1:12, stevilo_celih_let), 1:(stevilo_preostalih_mesecev %% 12))
      }
    } else {
      if (stevilo_celih_let >= 1){
        leta_naprej <- c(rep.int(izracun_leto, (12 - izracun_mesec )), sort(rep.int((izracun_leto + 1):(izracun_leto + stevilo_celih_let), 12)), rep(izracun_leto + stevilo_celih_let + 1, (stevilo_preostalih_mesecev - (12 - izracun_mesec)) %% 12))
        meseci_naprej <- c((izracun_mesec + 1): 12, rep(1:12, stevilo_celih_let), 1:((stevilo_preostalih_mesecev -  (12 - izracun_mesec )) %% 12))
      } else {
        leta_naprej <- c(rep.int(izracun_leto, (12 - izracun_mesec )), rep(izracun_leto + 1, (stevilo_preostalih_mesecev - (12 - izracun_mesec)) %% 12))
        meseci_naprej <- c((izracun_mesec + 1): 12, rep(1:12, stevilo_celih_let), 1:((stevilo_preostalih_mesecev -  (12 - izracun_mesec )) %% 12))
      }
    }
  }
  
  tabela_naprej <- data.frame("leto" = leta_naprej, "mesec" = meseci_naprej)
  tabela_naprej$faktor <- mesecna_rast_place ** (seq.int(from = 1, to = (upokojitev_leto - izracun_leto) * 12 + upokojitev_mesec - izracun_mesec, by = 1))
  
  tabela_naprej$placa <- tabela_naprej$faktor * aktualna_placa
  tabela_naprej <- tabela_naprej[,c(1,2,4)]

  tabela_naprej
}







izracunaj_pokojninsko_osnovo2 <- function(tabela_kolicnikov, starost_leto, starost_mesec, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, pricakovana_rast_place, izracun_leto, izracun_mesec, upokojitev_leto, upokojitev_mesec, dosedanje_place){
  
  tabela_nazaj2 <- izracunaj_tabelo_za_nazaj2(dosedanje_place, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, izracun_leto, izracun_mesec)
  tabela_naprej2 <- izracunaj_tabelo_za_naprej2(izracun_leto, izracun_mesec, starost_leto, starost_mesec, upokojitev_leto, upokojitev_mesec, pricakovana_rast_place, aktualna_placa)
  tabela_skupna <- rbind(tabela_nazaj2, tabela_naprej2)

  tabela_letne_osnove <- head(tabela_skupna %>% group_by(leto) %>% summarise(letna_placa = sum(placa)),-1) 
  prispevki <- NULL
  leta <- unique(tabela_letne_osnove$leto)
  zadnje_leto <- max(unique(tabela_letne_osnove$leto))
  if (zadnje_leto <= 2019){
    prispevki <- stopnje_prispevkov[stopnje_prispevkov$leto %in% leta, 2][[1]]
  } else {
    aux_prispevki <- stopnje_prispevkov[stopnje_prispevkov$leto %in% leta, ]
    prispevki1 <- aux_prispevki[aux_prispevki$leto <= 2019, 2][[1]]
    prispevki2 <- rep(0.3537, zadnje_leto - 2019)
    
    prispevki <- rev(c(prispevki2, prispevki1))
  }
  tabela_letne_osnove$letna_osnova <- tabela_letne_osnove$letna_placa * (1 - prispevki)
  tabela_letne_osnove$osnova <- tabela_letne_osnove$letna_osnova / 12
  
  cx <- c(0,cumsum(tabela_letne_osnove$osnova))
  povprecja <- NULL
  
  if (length(tabela_letne_osnove$osnova) >= 25){
    povprecja <- (cx[25:length(cx)] - cx[1:(length(cx) - 24)]) / 24
  } else {
    povprecja <- mean(tabela_letne_osnove$osnova)
  }
  pokojninska_osnova <- max(povprecja)

  pokojninska_osnova
}



izracunaj_pokojnino2 <- function(spol, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, pricakovana_rast_place, starost_leto, starost_mesec, izracun_leto, izracun_mesec, upokojitev_leto, upokojitev_mesec, dosedanje_place){
  # pridobi podatke o pokojninah v posameznem letu
  tabela_visine_pokojnin <- read_xlsx("visine_pokojnin.xlsx")
  tabela_visine_pokojnin <- tabela_visine_pokojnin[(tabela_visine_pokojnin$leto - upokojitev_leto) * 12 + tabela_visine_pokojnin$mesec - upokojitev_mesec <= 0,]
  pokojnine <- tabela_visine_pokojnin[length(tabela_visine_pokojnin$leto),3:6]

  # pridobitev ustrzne tabele odmernih odstotkov
  odmerni_odstotki <- NULL
  if (upokojitev_leto <= 2016){
    odmerni_odstotki <- read_excel("odstotki20132019.xlsx", sheet = 1)
    colnames(odmerni_odstotki) <- c("doba", "zenske", "moski")
  } else if (upokojitev_leto <= 2019){
    odmerni_odstotki <- read_excel("odstotki20132019.xlsx", sheet = 2)
    colnames(odmerni_odstotki) <- c("doba", "zenske", "moski")
  } else {
    odmerni_odstotki <- read_excel("odmerni odstotki.xlsx")
    colnames(odmerni_odstotki) <- c("doba", "zenske", "moski")
  }
  
  #izračun celotne delovne dobe in datum upokojitve
  skupna_delovna_doba <- ((upokojitev_leto - prva_zaposlitev_leto) * 12  + upokojitev_mesec  - prva_zaposlitev_mesec) / 12
  upokojitvena_starost_leto <- (starost_leto  * 12  + starost_mesec  + upokojitev_leto * 12 - 2020 * 12 + upokojitev_mesec - 8) %/% 12
  upokojitvena_starost_mesec <- (starost_leto  * 12  + starost_mesec  + upokojitev_leto * 12 - 2020 * 12 + upokojitev_mesec - 8) %% 12
  
  if (upokojitvena_starost_leto < 58 || skupna_delovna_doba < 15 || (upokojitvena_starost_leto < 65 & skupna_delovna_doba < 40)){
    return("Pogoji za starostno pokojnino niso izpolnjeni.")
  } else{
    
    #pridobi valorizacijske količnike
    tabela_kolicnikov <- read_xlsx("revalorizacijski faktorji.xlsx", skip = 2)
    colnames(tabela_kolicnikov) <- c("datum", "mesec", "leto", "st_enot1", "valuta1", "st_enot2", "valuta2", "tecaj")
    
    #izračun pokojninske osnove

    pokojninska_osnova <- izracunaj_pokojninsko_osnovo2(tabela_kolicnikov, starost_leto, starost_mesec, prva_zaposlitev_leto, prva_zaposlitev_mesec, prva_placa, aktualna_placa, pricakovana_rast_place, izracun_leto, izracun_mesec, upokojitev_leto, upokojitev_mesec, dosedanje_place)
      
    #izračun odmernega odstotka
    odstotek <- izracunaj_odmerni_odstotek(odmerni_odstotki, skupna_delovna_doba, spol, upokojitev_leto)
    pokojnina_po_placi <- pokojninska_osnova * odstotek #* 1.032
    pokojnina_najnizja_osnova <- pokojnine[[3]] * odstotek
    pokojnina_najvisja_osnova <- pokojnine[[4]] * odstotek
    najnizja_pokojnina <- pokojnine[[2]]
    
    print(pokojnine[[3]])
    print(pokojnine[[4]])
    print(paste0("pokojnina_po_placi", pokojnina_po_placi))
    print(paste0("pokojnina_najnizja_osnova", pokojnina_najnizja_osnova))
    print(paste0("pokojnina_najvisja_osnova", pokojnina_najvisja_osnova))
    
    
    zajamcena_pokojnina <- 0
    # preverimo, če je upravičen do zagotovljene pokojnine
    if (skupna_delovna_doba >= 40){
      zajamcena_pokojnina <- pokojnine[[1]]
    } 
    
    # preverimo, ali je njegova pokojnina znotraj dovoljenih
    pokojnina <- pokojnina_po_placi
    if (pokojnina_po_placi >= pokojnina_najvisja_osnova){
      pokojnina <- pokojnina_najvisja_osnova
    } else if (pokojnina_po_placi <= pokojnina_najnizja_osnova){
      pokojnina <- max(pokojnina_najnizja_osnova, zajamcena_pokojnina, najnizja_pokojnina)
    }
    print(paste0("pokojnina je :"))
    print(round(pokojnina,2))
    return(round(pokojnina,2))
  }
}
