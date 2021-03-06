#' Make a plot
#' 
#' This function creates a plot.
#' 
#' @export
#' @param seqnumer of random values
IDP <- function(ss = ""){
library(graphics)
sequ <- toupper(ss)
sequ_ns <- gsub(" ", "", sequ, fixed = TRUE)
A <- 0.37
C <- 0.25
D <- 0.30
E <- 0.42
F <- 0.17
G <- 0.13
H <- 0.20
I <- 0.39
K <- 0.56
L <- 0.24
M <- 0.36
N <- 0.27
P <- 1.00
Q <- 0.53
R <- 0.38
S <- 0.24
T <- 0.32
V <- 0.39
W <- 0.25
Y <- 0.25
fppii <- 0
poss <- c("A","C","D","E","F","G","H","I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
net_neg_chg <- 0
net_pos_chg <- 0
indivx <- c(1)
indiv <- indivx[0]
indexppiix <- c()
indexppii <- indexppiix[0]
coli <- c()
colaa <- c()

for (i in c(1:nchar(sequ_ns))){

fppii <- fppii + eval(as.name(substr(sequ_ns,i,i)))
indiv[i] <- eval(as.name(substr(sequ_ns,i,i)))
indexppii[i] <- i

if (eval(as.name(substr(sequ_ns,i,i))) == 0.56){
	net_pos_chg <- net_pos_chg + 1}
else if (eval(as.name(substr(sequ_ns,i,i))) == 0.38){
	net_pos_chg <- net_pos_chg + 1}
else if (eval(as.name(substr(sequ_ns,i,i))) == 0.30){
	net_neg_chg <- net_neg_chg + 1}
else if (eval(as.name(substr(sequ_ns,i,i))) == 0.42){
	net_neg_chg <- net_neg_chg + 1}

}
seqv <- c()
for (i in c(1:nchar(sequ_ns))){
append(seqv, sequ_ns[i])}


fppii_avg <- fppii/(nchar(sequ_ns))
n <- nchar(sequ_ns)
netcharge <- abs(net_neg_chg - net_pos_chg)
rh <- (2.16 * n^((0.503-(0.11*log(1-fppii_avg)))) + (0.17158*netcharge) - ((0.07296) * 2.16 * n^((0.503-(0.11*log(1-0.012))))))
chg_con <- (0.17158*netcharge)
r_coil <- (2.16* n^((0.503-(0.11*log(1-0.12)))))
idpn = c(93,93,93,73,89,97,110,202,168,97,198,260,73,87,94,136,140,189,206,146,170,202)
idprh = c(32.4,30.4,27.4,23.8,28,25.7,33.7,44.3,35,28,45,49,24,24.8,24.52,28.1,28.2,32,39.7,32.9,38.3,39.7)
idpnc = c(15,15,15,14,19,29,43,29,16,15,19,28,6,4,4,9,9,5,14,4,10,1)
idpneg = c(17,17,17,17,21,31,53,38,23,23,15,51,14,9,10,14,24,30,43,10,26,27)
idppos = c(2,2,2,3,2,2,10,9,7,8,34,23,8,13,14,23,15,25,29,6,16,26)
idpfppii = c(0.489,0.458,0.283,0.45,0.328,0.335,0.363,0.402,0.378,0.353,0.399,0.37,0.363,0.412,0.356,0.403,0.374,0.364,0.351,0.376,0.39,0.413)

in.df = data.frame(indexppii,indiv)
idp.df = data.frame(idpn, idprh, idpnc, idpneg, idppos, idpfppii)


v <- c(chg_con, r_coil, rh)
barplot(v, horiz=TRUE, names.arg=c("Charge Rh", "Random Coil Rh", "Total Rh"), main="Composition of Rh")
invisible();
}
