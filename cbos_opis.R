library(haven)
adres_cbos <- paste0("https://raw.githubusercontent.com/kflisikowsky/sad/",
                     "main/data/CBOS_201733_331_SPSS.sav")
plik_cbos <- tempfile(fileext = ".sav")
download.file(adres_cbos, plik_cbos, mode = "wb", quiet = TRUE)
cbos_raw <- read_sav(plik_cbos)
unlink(plik_cbos)

cbos <- cbos_raw %>% transmute(
  plec = as_factor(plec),
  miejscowosc = as_factor(klm6),
  kierunek = as_factor(q24),
  prognoza = as_factor(q25),
  wyksztalcenie = as_factor(q15)
)
dim(cbos)
head(cbos)

```{r cbos-zad-5}
# WASZE ROZWIĄZANIE
# count(), mutate(procent = 100 * n / sum(n)), filter(!is.na(...))
cbos %>%
  summarise(
    liczba_obserwacji = n(),
    braki_plec = sum(is.na(plec)),
    braki_miejscowosc = sum(is.na(miejscowosc)),
    braki_kierunek = sum(is.na(kierunek)),
    braki_prognoza = sum(is.na(prognoza)),
    braki_wyksztalcenie = sum(is.na(wyksztalcenie))
  )
bind_rows(
  cbos %>%
    filter(!is.na(plec)) %>%
    count(plec) %>%
    mutate(procent = round(100 * n / sum(n), 2)) %>%
    rename(kategoria = plec) %>%
    mutate(zmienna = "Płeć"),
  
  cbos %>%
    filter(!is.na(miejscowosc)) %>%
    count(miejscowosc) %>%
    mutate(procent = round(100 * n / sum(n), 2)) %>%
    rename(kategoria = miejscowosc) %>%
    mutate(zmienna = "Miejscowość")
) %>%
  select(zmienna, kategoria, n, procent)
cbos %>%
  filter(!is.na(miejscowosc)) %>%
  summarise(
    mianownik = n(),
    liczba_wies = sum(miejscowosc == "wieś"),
    liczba_duze_miasta = sum(miejscowosc == "miasto 500 000 i więcej"),
    procent_wies = round(100 * liczba_wies / mianownik, 2),
    procent_duze_miasta = round(100 * liczba_duze_miasta / mianownik, 2)
  )  
```
#Spośród 925 respondentów, którzy udzielili odpowiedzi na pytanie o miejsce zamieszkania, 405 osób (43,78%) mieszkało na wsi. Natomiast 83 osoby (8,97%) mieszkały w miastach liczących co najmniej 500 tys. mieszkańców. Odsetki obliczono względem liczby niebrakujących odpowiedzi na pytanie o miejscowość (N = 925).


```{r cbos-zad-6}
# WASZE ROZWIĄZANIE
# count(kierunek), filter(kierunek %in% c("W dobrym", "W złym"))
cbos %>%
  filter(!is.na(kierunek)) %>%
  count(kierunek) %>%
  mutate(procent = round(100 * n / sum(n), 2))
# Odpowiedź "w dobrym" była najliczniejsza (432 - 46.7%)

cbos %>%
  filter(!is.na(kierunek)) %>%
  summarise(
    mianownik_cala_proba = n(),
    liczba_dobrych = sum(kierunek == "W dobrym"),
    liczba_zlych = sum(kierunek == "W złym"),
    mianownik_zdecydowani = liczba_dobrych + liczba_zlych,
    procent_cala_proba = round(100 * liczba_dobrych / mianownik_cala_proba, 2),
    procent_zdecydowani = round(100 * liczba_dobrych / mianownik_zdecydowani, 2)
  )
#W całej próbie 46,70% respondentów oceniło kierunek zmian w kraju jako dobry (432 osoby na 925 respondentów). Natomiast wśród osób, które wyraziły jednoznaczną opinię, czyli wybrały odpowiedź „W dobrym” lub „W złym”, udział odpowiedzi „W dobrym” wyniósł 55,60% (432 osoby na 777 respondentów).Różnica wynika z zastosowania różnych mianowników. W pierwszym przypadku uwzględniono wszystkie niebrakujące odpowiedzi, natomiast w drugim pominięto osoby niezdecydowane oraz pozostałe kategorie odpowiedzi. W efekcie odsetek odpowiedzi pozytywnych wśród osób zdecydowanych jest wyższy.