IDP <- function(s = ""){
  if(s == ""){
    stop("Enter an Amino Acid Sequence.")
  }
sequ <- toupper(s)
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
for (i in c(1:nchar(sequ_ns))){
if (is.element(substr(sequ_ns,i,i), poss)){
	}
else{
		stop("Error. Please enter valid amino acid sequence.")}
fppii <- fppii + eval(as.name(substr(sequ_ns,i,i)))
if (as.name(substr(sequ_ns,i,i)) == K){
	net_neg_chg <- net_neg_chg + 1
	}
else if (as.name(substr(sequ_ns,i,i)) == R){
	net_neg_chg <- net_neg_chg + 1
	}
else if (as.name(substr(sequ_ns,i,i)) == D){
	net_pos_chg <- net_pos_chg + 1}
else if (as.name(substr(sequ_ns,i,i)) == E){
	net_pos_chg <- net_pos_chg + 1
	}

fppii_avg <- fppii/(nchar(sequ_ns))
n <- nchar(sequ_ns)
netcharge <- abs(net_neg_chg - net_pos_chg)
rh_uncorrected <- (2.16 * sequ_ns.length ^ ((0.503-(0.11*log(1-fppii_avg)))))
rh_chgcorrection <- (2.16 * sequ_ns.length ^((0.503-(0.11*log(1-fppii_avg)))) + 0.17158*netcharge - 0.07296 * 2.16 * sequ_ns.length^(0.503-(0.11*log(1-0.012))))

idpcsv <- read.csv("https://public.opencpu.org/ocpu/github/bjr67/IDP/data/idpdata.csv", sep=",")
print(plot(idpcsv$N,idpcsv$Rh, pch=19, xlab="N", ylab = "Rh")+
par(new=T)+
plot(n, rh_chgcorrection, col="red")+
par(new=F))}}
