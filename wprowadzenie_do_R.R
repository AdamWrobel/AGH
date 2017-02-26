
options(scipen = 5)


### definiowanie obiektu ###

# wektor
wektor <- c(1,2,3,4,5) 
wektor

wysokosc_pozyczki <- c(200000, 90000, 400000)
wartosc_hipoteki <- c(250000, 150000, 700000)

numer_klienta <- c('000245', '000349', '000953')

# ramka danych - data.frame
ramka_danych <- data.frame(numer_klienta, wartosc_hipoteki, wysokosc_pozyczki)


### odwolanie do obiektu ###

# drugi element wektora
wysokosc_pozyczki[2]

# wszystkie wiersze, druga i trzecia kolumna
ramka_danych[,2:3]

# pierwszy wiersz, wszystkie kolumny
ramka_danych[1,]

# zmienna o podanej nazawie
ramka_danych$wartosc_hipoteki

# wiersze spelniajace warunek: pozyczki wieksze niz 100000
ramka_danych[ramka_danych$wysokosc_pozyczki > 100000,]


### operacje na obiektach ###

x <- 1:200
y <- x^2
plot(x,y, type = 'l')

# stworzenie nowej zmiennej z zaktualizowana cena nieruchomosci
wsp_cen_nieruchomosci <- 1.03
ramka_danych$wartosc_hipoteki * wsp_cen_nieruchomosci 
  
ramka_danych$aktualna_wartosc_hipoteki <- ramka_danych$wartosc_hipoteki * wsp_cen_nieruchomosci 

# najprostrze funkcje
mean(ramka_danych$aktualna_wartosc_hipoteki)

sum(ramka_danych$aktualna_wartosc_hipoteki) 

summary(ramka_danych) 

head(ramka_danych)

### typy wektorow ###
str(ramka_danych)
ramka_danych$default <- c(FALSE, FALSE, TRUE)
ramka_danych$miasto <- c('Krakow', 'Poznan', 'Warszawa')
ramka_danych$wiek_klienta<- as.integer(c(25, 37, 42))
str(ramka_danych)


### wczytywanie danych ###

# zmiana folderu roboczego
setwd('E:/1TB_disk/Dane/Projekty/R_projects/AGH')

klienci <- read.csv('klienci_detaliczni.csv')
head(klienci)
str(klienci)

table(klienci$default)

# petle 
kwantyle <- list()
for(i in 1:dim(klienci)[2]){
  kwantyle[[i]] <- quantile(klienci[,i])
}


# cwiczenie #
head(iris)

# zdefiniuj nowa zmienna 

# policz srednia zmiennej Petal.Length dla kazdego poziomu zmiennej Species








# materialy dodatkowe #


### pakiet dplyr ###
install.packages('dplyr')
library(dplyr)

# operator %>%
# input %>% funkcja jest rownoznaczny z funkcja(input)

# funkcja select - wybieranie kolumn
klienci %>% select(income) %>% head

# funkcja filter - filtorwanie danych spelniajacych kryteria
klienci %>% filter(income > 10000) %>% head

# funkcja mutate - tworzenie nowej zmiennej
klienci <- klienci %>% mutate(income_to_loan = income/loan_size)
klienci %>% head

# funkcja group_by i summarize - tworzenie agregatow
klienci %>% group_by(default) %>% summarize(mean_income_to_loan = mean(income_to_loan))


# wizualizacje 
scenarios <- read.csv('FED_scenario_2016.csv')
plot(scenarios$Unemployment.rate, type = 'l')

# histogramy
hist(klienci$income)
hist(klienci$loan_size)

# rozklady - ggplot2
install.packages('ggplot2')
library(ggplot2)
ggplot(klienci) + geom_density(aes(income, group = default,fill = factor(default)), adjust=2, alpha = 0.5)
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length, group = Species, colour = Species))


