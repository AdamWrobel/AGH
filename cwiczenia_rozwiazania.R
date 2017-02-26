# cwiczenie 2 - zbior danych mtcars#
head(mtcars)
#[, 1]   mpg     Miles/(US) gallon
#[, 2]   cyl     Number of cylinders
#[, 3]   disp    Displacement (cu.in.)
#[, 4]   hp      Gross horsepower
#[, 5]   drat    Rear axle ratio
#[, 6]   wt      Weight (lb/1000)
#[, 7]   qsec    1/4 mile time
#[, 8]   vs      V/S
#[, 9]   am      Transmission (0 = automatic, 1 = manual)
#[,10]   gear    Number of forward gears
#[,11]   carb    Number of carburetors

# odpowiedz na pytania:
# ile samochodow z probki ma conajmniej 190 koni mechanicznych (zmienna hp)
A <- mtcars[mtcars$hp >= 190,]
dim(A)

# stworz histogram zmiennej mpg (licza mil jakie samochod jest w stanie przejachac na galonie paliwa)
hist(mtcars$mpg)

# zbuduj model regresji liniowej pomiedzy zmienna mpg a zmiennymi wt, qsec, am
# funkcja lm(zmienna_objasniana ~ zmienna_objasniajaca_1 + zmienna_objasniajaca_2, data = ramka_danych)
model <- lm(mpg ~ wt+qsec+am, data = mtcars)

# wykonaj funkcje summary na dopasowanym modelu
summary(model)

# wykonaj funkcje plot na dopasowanym modelu
plot(model)

model$coefficients


# cwiczenie 3 - petle #
# napisz petle, ktora bedzie :
# a) dopasowywac model jednowymiarowy regresji liniowej pomiedzy zmienna mpg i kazda ze pozostalych zmiennych
# b) przekazywac do konsoli wynik funkcji summary dla kazdego z modeli

for(i in 2:dim(mtcars)[2]){
  two_var <- mtcars[,c(1,i)]
  model <- lm(two_var)
  print(summary(model))
}
