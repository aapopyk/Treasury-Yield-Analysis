
# MAT 5740 Honors Credits Assignment

#2.)
D = read.csv("daily-treasury-rates.csv", header=TRUE)
D = subset(D, select = -Date)

n = c(52, 303, 554, 804, 1053, 1303, 1553, 1804, 2054, 2304, 2554, 2804, 3056, 3306, 3557, 3808, 4058, 4308, 4558, 4808)
n = rev(n)

x = c(1/12, 2/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)

plot(x, D[52,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2021")
plot(x, D[303,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2020")
plot(x, D[554,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2019")
plot(x, D[804,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2018")
plot(x, D[1053,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2017")
plot(x, D[1303,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2016")
plot(x, D[1553,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2015")
plot(x, D[1804,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2014")
plot(x, D[2054,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2013")
plot(x, D[2304,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2012")
plot(x, D[2554,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2011")
plot(x, D[2804,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2010")
plot(x, D[3056,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2009")
plot(x, D[3306,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2008")
plot(x, D[3557,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2007")
plot(x, D[3808,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2006")
plot(x, D[4058,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2005")
plot(x, D[4308,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2004")
plot(x, D[4558,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2003")
plot(x, D[4808,], xlab="Maturity (years)", ylab="Yield Rate (%)", type="b", main="2002")


#3.) y = ((x2-x)/(x2-x1))*%+((x-x1)/(x2-x1))*%, 4,6,8,9 years, where n[1] = 2002...n[20] = 2021

#4 years

Y4 = ((5-4)/(5-3))*D[n,7]+((4-3)/(5-3))*D[n,8]
for (i in 1:20) {
  Y4[i] = ((5-4)/(5-3))*D[n[i],7]+((4-3)/(5-3))*D[n[i],8]
}
Y4

#6 years 

Y6 = ((7-6)/(7-5))*D[n,8]+((6-5)/(7-5))*D[n,9]
for (i in 1:20) {
  Y6[i] = ((7-6)/(7-5))*D[n[i],8]+((6-5)/(7-5))*D[n[i],9]
}
Y6

#8 years

Y8 = ((10-8)/(10-7))*D[n,9]+((6-5)/(7-5))*D[n,10]
for (i in 1:20) {
  Y8[i] = ((10-8)/(10-7))*D[n[i],9]+((8-7)/(10-7))*D[n[i],10]
}
Y8

#9 years

Y9 = ((10-9)/(10-7))*D[n,10]+((9-7)/(10-7))*D[n,11]
for (i in 1:20) {
  Y9[i] = ((10-9)/(10-7))*D[n[i],10]+((9-7)/(10-7))*D[n[i],11]
}
Y9


#4.) 

A = matrix(nrow=10, ncol=20)

A[1,] = D[n,5]
A[2,] = D[n,6]
A[3,] = D[n,7]
A[4,] = Y4
A[5,] = D[n,8]
A[6,] = Y6
A[7,] = D[n,9]
A[8,] = Y8
A[9,] = Y9
A[10,] = D[n,10]
A

FR = matrix(nrow=10, ncol=20)
for (k in 1:20) { 
for (j in 1:10) {
  if (j < 2) {
    FR[j,k] = (A[j,k]**1/1)-1}
  else {
  FR[j,k] = ((A[j,k]**j)/(A[j-1,k]**j-1))-1}
}
}
FR

t = (1:10)
for (i in 1:20) {
  plot(t,FR[,i], xlab="time (years)", ylab="Forward Rate (%)", type="b")
}


#5.) ROR = (7 yr yield * 7) / (6 yr yield * 6) for all 20 years
ROR = c()
for (i in 1:20) {
  ROR[i] = (FR[7,i]*7)/(FR[6,i]*6)
}
ROR

#6.) Xt = Unemployment Rate, CPI (Less F&E) (2002-2021), Yt = 10 yr yield - 'h' yr yield 

UNRATE = read.csv("UNRATE.csv", header=TRUE)
UNRATE = UNRATE$UNRATE


r = c()
for (h in 1:9){
  r[h] = cor(UNRATE, A[10,]-A[h,])
}
max(r)
which.max(r)
plot(r, xlab="Index", ylab="Correlation", type="b")

# Use 10 yr - 5 yr yield because it has the strongest correlation

Yt = A[10,] - A[5,]

cor(UNRATE, Yt)
LM1 = lm(Yt~UNRATE)
summary(LM1)

# Relationship of Unemployment rate and 10 yr - 5 yr yield rate is highly significant with a strong positive correlation

CPI = read.csv("CPILFESL.csv")
CPI = CPI$CPILFESL_PC1

cor(CPI, Yt)
LM2 = lm(Yt~CPI)
summary(LM2)

# Relationship of Consumer Price Index and 10 yr - 5 yr yield rate is still significant but with a weaker negative correlation

