#rm(list= ls())
#ls()


#clasa va tine minte functia de densitate si daca e bidimensionala sau nu
setClass("CRV", representation (
  densitate="function",
  is2Dimensional="logical"
))

#constructorul
CRV <- function(densitate,is2Dimensional= FALSE)
{
  #verificare daca a fost pasata o functie buna
  if(!is2Dimensional)
  {j <- integrate(densitate,-Inf,Inf)
  if(j$value-j$abs.error>1 ||j$value+j$abs.error<1  )
  {
    print(integrate(densitate,-Inf,Inf)$value)
    stop("Functia data nu e densitate de probabilitate")
  }
  }

  obj <- new("CRV", densitate = densitate,is2Dimensional=is2Dimensional)
  return (obj)
}


#functie care returneaza marginala la X pt o bidimensionala
margineX <- function(VA,c = -Inf, d = Inf) {
  tryCatch (
    {
      #evident 0 :)))
      if (c > d) 0
      #formula pentru gasirea marginii
      else {  function(x) (integrate(Vectorize(function(y) (VA@densitate(x, y))), c, d)$value)}
    },
    error = function(e)
    {
      stop("Functia de densitate comuna nu este valida!")
    })
}

#functie care returneaza marginala la Y pt o bidimensionala
margineY <- function(VA,c = -Inf, d = Inf) {
  tryCatch (
    {
      #evident 0 :)))
      if (c > d) 0
      #formula pentru gasirea marginii
      else function(y) (integrate(Vectorize(function(x) (VA@densitate(x, y))), c, d)$value)
    },
    error = function(e)
    {
      stop("Functia de densitate comuna nu este valida!")
    })
}


#5
#Formula pentru medie este integrala (inf to inf) xf(x)
E <- function(VA)
{
  return(integrate(Vectorize(function(x)VA@densitate(x)*x), -Inf, Inf)$value)
}

#Formula pentru varianta este integrala (inf to inf) (x-medie)^2f(x)
Var <- function(VA)
{
  return(integrate(Vectorize(function(x)VA@densitate(x)*(x-E(VA))^2), -Inf, Inf)$value )
}


#se schimba doar puterea din formula variantei
M3 <- function(VA)
{
  return(integrate(Vectorize(function(x)VA@densitate(x)*(x-E(VA))^3), -Inf, Inf)$value )
}

#se schimba doar puterea din formula variantei
M4 <- function(VA)
{
  return(integrate(Vectorize(function(x)VA@densitate(x)*(x-E(VA))^4), -Inf, Inf)$value )
}


#6
#aceleasi ca precedentele doar ca avem in loc de functia identica functia g
EApplyFunc<- function(VA,g)
{
  return(integrate(Vectorize(function(x)VA@densitate(x)*g(x)), -Inf, Inf)$value)
}



VarApplyFunc<- function(VA,g)
{
  return(integrate(Vectorize(function(x)VA@densitate(x)*(x-EApplyFunc(VA))^2), -Inf, Inf)$value )
}






#x <- function (y){
  #if(y<0)
    #  return(0)
  #  if(y>1)
    # return(0)
  #return(1)
  #}



#y <- function (h){
  #if(h<0)
    #  return(0)
  #if(h>1)
    #  return(0)
  #  return(3*h^2)
#}

#X <- CRV(Vectorize(y))

#print(Var(X))


#BIDI <- function(x,y) {

  #if(x<0)
  #  return(0)
  #if(y<0)
  #  return(0)
  #if(x>1)
  #  return(0)
  #if(y>1)
  #  return(0)
 # return(1)
  # return(6/7*(x+y)^2)
#}

#XY <- CRV(BIDI,is2Dimensional = TRUE)
#s <- margineX(XY)
#Y <- CRV(Vectorize(s))
#s <- margineY(XY)
#Y <- CRV(Vectorize(s))
#E(Y)

#print(generation_n_numbers(4,X))



#functie exemplu din seminar
#fx <- function(x){
#  f = 3*(x*x)
# f[x<0] = 0
# f[x>1] = 0
#  return(f)
#}

#functie exemplu din seminar
#fy <- function(x){
# f = (sin(x))/2
# f[x<0] = 0
#  f[x>pi] = 0
#  return(f)
#}



#x <- CRV(Vectorize(fx))
#Y <- CRV(Vectorize(fy))


#P(lowerbound <=VA<=upperbound|lowerCondition <=VA<=upperCondition)
P = function (VA,lowerbound,upperbound,lowerCondition,upperCondition)
{
  #Daca intevalul conditionalului si cel pe care vrem sa facem densitatea sunt disjuncte atunci returnam 0
  if (upperbound < lowerCondition || lowerbound > upperCondition)
    return (0)

  # Gasim intersectia intervalelor
  lb <- max(lowerbound, lowerCondition)
  ub <- min(upperbound, upperCondition)


  # Gasim probabilitatea avand in vedere conditiile puse
  i1 <- integrate (VA@densitate, lb, ub)$value

  # Calculam posibilitatea sa cadem in conditiile puse
  i2 <- integrate (VA@densitate, lowerCondition, upperCondition)$value

  if (i2 != 0)
    return (i1 / i2)

  # Daca nu avem cazuri in care se intampla conditia oprim
  stop("Conditia pusa este mult prea restrictiva")
}



hipergeometrica <- c("Definitie:Variabila aleatoare X urmeaza legea hipergeometrica cu parametrii a, b si n (a,b,n apartin lui N*, n <=a+b ) daca poate lua orice valoare ???ntreaga ???ntre max(0,n - b)si min(n,a) si P(X=k)=C(k,a)*C(n-k,b)/C(n,a+b) oricare ar fi k de la acel max la acel min.(C e notatia pentru combinari luate de la pana la)",
                     "Utilizare: Testul provenit din distributia hipergeometrica poate fi folosit pt determinarea daca o populatie e slab reprezentata",
                     "Notatie:",
                     "Media: E(X)=ap unde p=a/(a+b),q=1-p",
                     "Variatia: Var(X) = npq*(a+b-n)/(a+b-1)",
                     "Sursa: http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf")

uniforma <- c("Definitie: O v.a. U este repartizata uniform pe intervalul (a,b) daca admite densitatea de repartitie f(x)=1/(b-1), x in (a,b).",
              "Utilizare: Se foloseste atunci cand numarul cazurilor posibile tinde la infinit",
              "Notatie: U ~ U((a,b)), unde (a,b) este intervalul pe care este repartizata variabila aleatoare",
              "Media: E[U] = integrala de la -Inf la Inf din x*f(x) dx = (a+b)/2",
              "Variatia: Var(U) = E[U^2]-E[U]^2 = ((b-a)^2)/12",
              "Sursa: http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")

exponentiala <- c("Definitie: O v.a. X este repartizata exponential de parametru lambda > 0 daca densitatea de repartitie a lui X este de forma f(x) = lambda * e^(-lambda*x), x > 0",
                  "Utilizare: Modeleaza timpul de asteptare pana la aparitia unui eveniment de interes.",
                  "Notatie: X ~ Exp(lambda)",
                  "Media: E[X] = integrala de la -Inf la Inf din x*f(x) dx = 1/lambda",
                  "Variatia: Var(X) = E[X^2] - (E[x])^2 = 1/(lambda^2)",
                  "Sursa: Curs 10, Probabilitati si Statistica, Prof. Alexandru Amarioarei")


normala <- c("Definitie: Spunem ca o v.a. X este repartizata normal de parametri miu si sigma^2 daca admite densitatea f(x) = (1/(sqrt(2pi)*sigma))*e^(-(x-miu)^2/2*sigma^2), unde x in R)",
             "Notatie: X ~ N(miu,sigma^2)",
             "Media: E[X] = miu",
             "Variatia: Var(U) = sigma^2",
             "Sursa: Curs 10, Probabilitati si Statistica, Prof. Alexandru Amarioarei")




# Functie ajutatoare care afiseaza chestiile cerute din repartitia pe care e chemata
sinteza <- function(rep){
  for (i in rep)
  {
    print(i)
  }

}




#pt integrarea dubla
dubluintegrat <- function (f) {
  return (
    integrate(Vectorize(function (y) {
      integrate(Vectorize(function (x) { f(x, y) }), -Inf,Inf)$value
    }), -Inf,Inf)$value
  )
}


Covariance <- function(VA) {
  if (!VA@is2Dimensional) {
    stop("Variabila nu este bidimensionala")
  }
  else {
    EX <- E(CRV(Vectorize(margineX(VA))))
    EY <- E(CRV(Vectorize(margineY(VA))))
    #functia de integrat din care rezulta covarianta
    funcint <- function(x, y) { return ((x - EX) * (y - EY) * VA@densitate(x, y)) }
    return(dubluIntegrat(funcint))
  }
}

Corelation <- function(VA)
{
  if (!VA@is2Dimensional) {
    stop("Variabila nu este bidimensionala")
  }
  else {
    #Formula pentru covarianta
    X <- CRV(Vectorize(margineX(VA)))
    Y <- CRV(Vectorize(margineY(VA)))
    return (Covariance(VA) / sqrt(Var(X) * Var(Y)))

  }
}









# constanta de normalizare este insusi valoarea integralei functiei pe R

constNorm <- function (func, domeniu = list (c (-Inf, Inf)))
{
  # integram functia pe intervalele suport
  # daca este posibil, altfel aruncam eroare
  tryCatch (
    {

      suma <- 0

      # pentru fiecare interval pe care e definita functia
      # adaugam integrala corespunzatoare

      for (interval in domeniu)
      {

        # IMPORTANT: Vectorizam functia inainte de a o integra pe intervalul curent
        suma <- suma + integrate (Vectorize (func), lower = interval[1], upper = interval[2])$value
      }

      # constanta de normalizare
      return (1 / suma)
    },
    error = function (e)
    {
      # Daca am ajuns aici inseamna ca in blocul tryCatch a fost aruncata o eroare.
      message (paste("Eroare! ", e))
    })
}

#test1 <- function (x)
#{
#return (3*x*x + 2*x+7)
#}

#test2 <- function (x)
#{
  # ifelse este un operator ternar
  # daca se indeplineste conditia, intra pe prima ramura (7 * exp(7^x))
  # altfel pe a doua (0)
  #ifelse(x > 0, 7 * exp(-7*x), 0)
  #}

# eroare
#print (constNorm (test1, domeniu = list (c(0, 1))))
# 0.1111111
#print (constNorm (test1))
# 1
#print (constNorm (test2))




# Realizez graficul pentru o functie densitate de probabilitate
# func este functia PDF pe care o primim ca parametru si o vom plota
# left este capatul stang
# right este capatul drept
plotPDF <- function (func, left, right)
{

  # pace va fi vectorul cu ajutorul caruia facem plot
  # intre left si right (din cat in cat afisam punctele)
  pace <- seq (left, right, 0.001)

  #main etse folosit pentru a afisa titlul
  #xlab pentru a afisa axa-x
  #ylab pentru a afisa axa-y
  #type l pentru a afisa a rezulttaul sub forma de linie
  #col blue pentru culoarea albastra
  plot (pace, func (pace), main = 'Densitate de probabilitate',
        xlab = 'x', ylab = 'y', type="l", col="blue")
}

# exemplu de plot pentru PDF
plotPDF(function(x) {3*x*x}, 0, 1)


# Realizez graficul pentru care ploteaza CDF-ul unei variabile aleatoare discrete
# func este functia PDF pe care o primim ca parametru si o vom plota
# left este capatul stang
# right este capatul drept

plotCDF <- function (func, left, right)
{
  # aux este vectorul de puncte din grafic
  aux <- c()
  # pace va fi vectorul cu ajutorul caruia facem plot intre left si right
  # (din cat in cat afisam punctele)
  pace <- seq(left, right, 0.001)

  # parcurgem vectorul de pasi
  for (i in pace)
  {

    # tryCatch-ul este pentru a verifica daca functia este integrabila
    # in caz contrar, este returnata o eroare
    tryCatch (
      {
        # in vectorul de puncte aux este adaugat
        # punctul cu coordonatele i si integrala de la 0 la i de f
        # $value - valoarea finala a integralei
        # integrate returneaza un obiect de tipul "integrate"
        # al carui element "value" contine valoarea estimata
        # a integralei
        aux = append (aux, integrate (func, 0, i)$value)
      },
      error = function (e)
      {
        # afisam eroarea
        message (paste("Eroare! ", e))
      }
    )
  }

  #main este folosit pentru a afisa titlul
  #xlab pentru a afisa axa-x
  #ylab pentru a afisa axa-y
  #col red pentru culoarea albastra
  plot (pace, aux, main = 'Distributie cumulativa',
        xlab = 'x', ylab = 'y', col = "red")
}

# exemplu de plt cu CDF
#plotCDF(function(x) {3*x*x}, 0, 1)


# De cate ori verificam sa fie PDF pe fiecare interval al functiei date.
CONST_INTERVALS <- 10000

# O functie este densitate de probabilitate daca indeplineste 2 conditii:
# 1. este pozitiva
# 2. integrala ei pe R este 1
densProb <- function (func, left = -Inf, right = Inf)
{
  # verificam cele doua conditii
  # in caz de esec la integrare, este returnata valoarea FALSE
  tryCatch (
    {

      # integrala functiei pe intervalul primit
      # IMPORTANT: Vectorizam functia inainte de a o integra
      # pe intervalul curent
      integrala <- integrate (Vectorize (func), left, right)$value

      # verificam ca valoarea integralei sa fie egala cu 1
      if (integrala == 1)
      {
        # cream intervalul [L, R]
        L <- max(left, -2e9)
        R <- min(right, 2e9)
        dist = R / 10 - L / 10


        # functia runif() genereaza devieri aleatoare ale distributiei uniforme
        intervals <- runif (CONST_INTERVALS, L, R - dist)

        # Cautam pe intervale (CONST_INTERVALS)
        # pentru a afla daca functia este pozitiva
        for (i in intervals)
        {
          j = i + dist
          # in cazul in care integrala este negativa,
          # atunci functia nu este pozitiva
          # <=> nu este PDF
          # IMPORTANT: Vectorizam functia inainte de a o integra
          # pe intervalul curent
          if (integrate (Vectorize (f), i, j)$value < 0)
            return (FALSE)
        }
        return (TRUE)
      }
      else
        return (FALSE)
    },

    error = function (err)
    {
      message(paste("EROARE! ", err))
      return (FALSE)
    })
}

#FALSE
#densProb(function(x) (3*x*x))
#TRUE
#densProb(function(x) (3*x*x), 0, 1)




#9

#Generarea a n valori(unde n este precizat de utilizator!) dintr-o reparti??ie de variabile aleatoare continue

generation_n_numbers <- function(n, VA)
{
  #creem o lista vida
  result <- list()

  #F_1 functie inversa
  F_1 <- inverse(VA@densitate, -Inf, Inf)

  #cat timp nu avem n numere
  while(length(result) != n)
  {
    #se genereaza U variabila uniforma pe [0,1] - si 1-U e in [0,1]
    U <- runif(1, 0, 1)
    #se calculeaza functia inversa in punctul U
    X <- F_1(U)

    #adaugam pe U la rezultat
    result <- append(result, X)
  }

  #intoarcem rezultatul
  return(result)
}


#12

#Construirea sumei ??i diferen??ei a doua variabile aleatoare continue independente

sum_dif <- function(x, y, VA1, VA2)
{

  f1 <- VA1@densitate
  f2 <- VA2@densitate
  #calculam valoarea in care trebuie calculata integrala
  U <- x + y

  #integrala suma
  suma <- integrate(function(x){
    f1(x)*f2(U-x)
  }, -Inf, Inf)$value

  #calculam valoarea in care trebuie calculata integrala x2
  U <- x - y

  #integrala diferenta
  diferenta <- integrate(function(x){
    f1(x)*f2(x-U)
  }, -Inf, Inf)$value

  #punem valorile intr-o lista pentru a le returna
  result = list(suma, diferenta)
  return(result)
}
