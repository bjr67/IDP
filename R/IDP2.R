#' Make a plot
#' 
#' This function creates a plot.
#' 
#' @export
#' @param seqnumer of random values
IDP2 <- function(sss = ""){
library(graphics)
sequ <- toupper(sss)
sequ_ns <- gsub(" ", "", sequ, fixed = TRUE)
#ppii
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
poss <- c("A","C","D","E","F","G","H","I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
seqv <- c()
seqv <- seqv[0]

for (i in c(1:nchar(sequ_ns))){
if (is.element(substr(sequ_ns,i,i), poss)){
seqv[i] <- eval(as.name(substr(sequ_ns,i,i)))
}}

coli <- c()

A <- "black"
C <- "yellow"
D <- "blue"
E <- "blue"
F <- "purple"
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
W <- "purple"
Y <- "purple"

for (i in c(1:nchar(sequ_ns))){
if (is.element(substr(sequ_ns,i,i), poss)){
coli[i] <- eval(as.name(substr(sequ_ns,i,i)))
}
}

df <- data.frame(seqv, coli)

#plothere
plot(df$seqv, col = df$coli, ylab = "Isoelectric point", xlab = "Amino Acid Residue", pch=19)
invisible();
}