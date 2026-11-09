library(dplyr)
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

#Zadanie 7
#Wybierz respondentów z kategorii `miejscowosc` równych `"wieś"` i `"miasto 500 000 i więcej"`. Dla każdej z tych dwóch grup policz **procent** odpowiedzi na pytanie `kierunek` (procent w obrębie grupy, nie w całej tabeli). Czy udział odpowiedzi „W złym” był taki sam?
#Sprwadźliczbę respondentów w obu grupach. Samo porównanie procentów bez liczebności może być mylące.


zad7_wynik <- cbos %>%
  filter(miejscowosc %in% c("wieś", "miasto 500 000 i więcej")) %>%
  count(miejscowosc, kierunek) %>%
  group_by(miejscowosc) %>%
  mutate(procent = 100 * n / sum(n))

print(zad7_wynik)

#Zadanie 8
#Zrób tabelę liczności odpowiedzi na `prognoza`. Następnie ogranicz dane do osób oceniających sytuację w kraju jako `"W złym"` i sprawdź, jak odpowiadały na pytanie o przyszłość. Która odpowiedź była najczęstsza w tej grupie? Czy na podstawie tej tabeli można wyciągać wnioski przyczynowe?
#dla wszystkich
cbos %>% 
  count(prognoza)

#tylko zły kierunek
zad8_wynik <- cbos %>%
  filter(kierunek == "W złym") %>%
  count(prognoza, sort = TRUE)

print(zad8_wynik)

#najczęstrza odpowiedź - Nie zmieni się 152
#Nie da się wyciągnąc wniosków przyczynowo skutkowych