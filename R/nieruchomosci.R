library(dplyr)
library(tidyr)

adres_nieruchomosci <- paste0(
  "https://raw.githubusercontent.com/kflisikowsky/pandas_exercises/",
  "62863036107888c0c7273c40e8d309a43f4da3a9/data/models/real_estate.csv"
)
raw_nier <- read.csv(adres_nieruchomosci, check.names = FALSE)
stopifnot(nrow(raw_nier) == 414, ncol(raw_nier) == 7)

nier <- raw_nier
names(nier) <- c("id", "wiek_lat", "metro_m", "sklepy", "lat", "lon",
                 "cena_10k_twd_ping")
head(nier)

### Zadanie 1
  # Liczba rekordów i kolumn
    dim(nier)
    
  # Typy danych, liczba braków i podstawowe statystyki
    str(nier)
    colSums(is.na(nier))
    summary(nier)
    
  # Czy ID jest unikatowe / czy występują duplikaty
    anyDuplicated(nier$id)

  # zmienne liczbowe - wszystkie
    names(nier)[sapply(nier, is.numeric)]
    # ID oraz wiek -> nie są zwykłymi miarami ilościowymi

### Zadanie 2 — przelicz cenę i sprawdź jej rozkład
    
  # dodanie kolumny za pomocą
    nier <- mutate(nier, cena_twd_m2 = cena_10k_twd_ping * 10000 / 3.3)
    
    #statystyki ceny za m²
    summarise(nier,
              minimum = min(cena_twd_m2),
              mediana = median(cena_twd_m2),
              srednia = mean(cena_twd_m2),
              maksimum = max(cena_twd_m2)
              )
    # pięć nieruchomości o najwyższej cenie jednostkowej wraz z ich wiekiem, odległością od metra i liczbą sklepów.
    top5 <- arrange(nier, desc(cena_twd_m2))
    top5 <- slice_head(top5, n = 5)
    top5 <- select(top5, id, cena_twd_m2, wiek_lat, metro_m, sklepy)
    print(top5)
    

    # Pytanie: czy średnia jest wyższa od mediany?
    # Co to mówi o rozkładzie cen? Nie interpretuj tej ceny jako wartości całego lokalu
    
    mean(nier$cena_twd_m2) > median(nier$cena_twd_m2)
    
    # Średnia cena za m² jest wyższa od mediany,
    # Oznacza to, że wysokie ceny niektórych nieruchomości zawyżają średnią, 
    # a mediana lepiej opisuje typową cenę za m².
    

### Zadanie 3 — ile obserwacji jest blisko metra?

    # 1. Dodanie kolumny określającej bliskość metra
    nier <- mutate(nier, blisko_metra = metro_m <= 500)
    
    # 2. Liczba nieruchomości do 500 m od metra
    sum(nier$blisko_metra)
    
    # 3. Procent wszystkich nieruchomości do 500 m od metra
    mean(nier$blisko_metra) * 100
    
    # 4. Porównanie obu grup;  Policz, ile nieruchomości znajduje się w odległości do 500 m od stacji oraz
    #jaki to odsetek wszystkich rekordów. Następnie porównaj mediany ceny TWD/m² w dwóch grupach: **do 500 m** i **powyżej 500 m**.
    summarise(
      group_by(nier, blisko_metra),
      liczba = n(),
      mediana = median(cena_twd_m2)
    )
    
    # Spośród 414 nieruchomości 211 (51,0%) znajduje się do 500 m od stacji metra,
    # a mediana ceny w tej grupie wynosi około 135 758 TWD/m², wobec 85 152 TWD/m² dla nieruchomości oddalonych o ponad 500 m
    

### Zadanie 4 — wiek, odległość i sklepy

    nier <- nier %>%
      mutate(strefa = cut(metro_m, c(-Inf, 500, 1500, Inf),
                          labels = c("do 500 m", "500-1500 m", "ponad 1500 m")))
    # Statystyki dla wszystkich nieruchomości
    wszystkie <- nier %>%
      group_by(strefa) %>%
      summarise(
        liczba = n(),
        mediana_ceny = median(cena_twd_m2),
        mediana_wieku = median(wiek_lat),
        srednia_sklepow = mean(sklepy)
      )
    
    print(wszystkie)
    
    # Statystyki dla budynków w wieku maksymalnie 20 lat
    mlodsze <- nier %>%
      filter(wiek_lat <= 20) %>%
      group_by(strefa) %>%
      summarise(
        liczba = n(),
        mediana_ceny = median(cena_twd_m2),
        mediana_wieku = median(wiek_lat),
        srednia_sklepow = mean(sklepy)
      )
    
    print(mlodsze)
    
    # Porównanie rankingów median cen
    wszystkie %>%
      arrange(desc(mediana_ceny))
    
    mlodsze %>%
      arrange(desc(mediana_ceny))
    
    # Ranking median cen pozostał taki sam.
    # Najdroższe są nieruchomości do 500 m od metra, a najtańsze powyżej 1500 m.
    # Różnice w cenach mogą wynikać z wieku budynków, ponieważ młodsze nieruchomości mają wyższe mediany cen.
    