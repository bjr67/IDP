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
pi <- c()
pi <- pi[0]
name <- c()
coli <- c()

for (i in c(1:nchar(sequ_ns))){
if (is.element(substr(sequ_ns,i,i), poss)){
pi[i] <- eval(as.name(substr(sequ_ns,i,i)))
name[i] <- substr(sequ_ns,i,i)}}

A <- "black"
C <- "yellow"
D <- "blue"
E <- "blue"
F <- "orange"
G <- "black"
H <- "red"
I <- "black"
K <- "red"
L <- "black"
M <- "black"
N <- "yellow"
P <- "black"
Q <- "yellow"
R <- "red"
S <- "yellow"
T <- "yellow"
V <- "black"
W <- "orange"
Y <- "orange"

for (i in c(1:nchar(sequ_ns))){
if (is.element(substr(sequ_ns,i,i), poss)){
coli[i] <- eval(as.name(substr(sequ_ns,i,i)))
}
}

pidf <- data.frame(pi, name, coli)

 
#plothere
plot(pidf$pi, col = pidf$coli, ylab = "Isoelectric point", xlab = "Amino Acid Residue", pch=19)
invisible();
}