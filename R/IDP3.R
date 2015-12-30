#' Make a plot
#' 
#' This function creates a plot.
#' 
#' @export
#' @param seqnumer of random values
IDP3 <- function(ssss = ""){
library(graphics)
sequ <- toupper(ssss)
sequ_ns <- gsub(" ", "", sequ, fixed = TRUE)
#pis
A <- 6.11
C <- 5.02
D <- 2.98
E <- 3.08
F <- 5.91
G <- 6.06
H <- 7.64
I <- 6.04
K <- 9.47
L <- 6.04
M <- 5.74
N <- 10.76
P <- 6.30
Q <- 5.65
R <- 10.76
S <- 5.68
T <- 5.60
V <- 6.02
W <- 5.88
Y <- 5.63
poss <- c("A","C","D","E","F","G","H","I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
seqv <- c()
seqv <- seqv[0]
name <- c()

for (i in c(1:nchar(sequ_ns))){
if (is.element(substr(sequ_ns,i,i), poss)){
seqv[i] <- eval(as.name(substr(sequ_ns,i,i)))
name[i] <- substr(sequ_ns,i,i)}}

seqdf <- data.frame(seqv)
for (i in seqdf$seqv){
if (is.element(substr(sequ_ns,i,i), poss)){
if (as.name(substr(sequ_ns,i,i)) == K){
	net_neg_chg <- net_neg_chg + 1
	coli[i] <- "red"}
else if (as.name(substr(sequ_ns,i,i)) == R){
	net_neg_chg <- net_neg_chg + 1
	coli[i] <- "red"}
else if (as.name(substr(sequ_ns,i,i)) == D){
	net_pos_chg <- net_pos_chg + 1
	coli[i]<- "blue"}
else if (as.name(substr(sequ_ns,i,i)) == E){
	net_pos_chg <- net_pos_chg + 1
	coli[i]<-"blue"}
else if (as.name(substr(sequ_ns,i,i)) == G){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == A){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == P){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == V){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == L){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == I){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == M){
	coli[i]="black"}
else if (as.name(substr(sequ_ns,i,i)) == S){
	coli[i]="yellow"}
else if (as.name(substr(sequ_ns,i,i)) == T){
	coli[i]="yellow"}
else if (as.name(substr(sequ_ns,i,i)) == C){
	coli[i]="yellow"}
else if (as.name(substr(sequ_ns,i,i)) == N){
	coli[i]="yellow"}
else if (as.name(substr(sequ_ns,i,i)) == Q){
	coli[i]="yellow"}
else if (as.name(substr(sequ_ns,i,i)) == H){
	coli[i]="red"}
else if (as.name(substr(sequ_ns,i,i)) == F){
	coli[i]="orange"}
else if (as.name(substr(sequ_ns,i,i)) == Y){
	coli[i]="orange"}
else if (as.name(substr(sequ_ns,i,i)) == W){
	coli[i]="orange"}
}}
#plothere
par(mfrow=c(2,1))
plot(seqv, ylab = "Isoelectric point", xlab = "Amino Acid Residue", pch=19)
plot(seqv, ylab = "Isoelectric point", xlab = "Amino Acid Residue", pch=19)
invisible();
}