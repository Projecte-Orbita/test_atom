
# Inicialitzem algunes variables:

# les de compensació d'errors:

alpha=0.25;
beta=0.5;
gamma=0.2;
delta=0.2;
eta=0.2;
phi=0.33;
chi=0.33;
coef=c(alpha, beta, gamma, delta, eta, phi, chi);

# Funcions d'inicialització dels csv d'entrada i càlcul del percentil

initialize1 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  #prebarems=read.csv('proves_barems.csv', header = FALSE);
  bar = baremar_petits(prebarems);
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize2 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #prebarems=read.csv('proves_barems.csv', header = FALSE);
  bar = baremar_petits(prebarems);
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize3 <- function(punts, prebarems){
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #prebarems=read.csv('proves_barems_mit.csv', header = FALSE);
  bar=baremar_mitjans(prebarems);
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize4 <- function(punts, prebarems){
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  
  #prebarems=read.csv('proves_barems_mit.csv', header = FALSE);
  bar=baremar_mitjans(prebarems);
  
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize5 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  #prebarems=read.csv('proves_barems_sup.csv', header = FALSE);
  bar = baremar_grans(prebarems);
  #predreg = regressio_grans(prebarems);
  
  colnames(punts) <- c("Noms","L", "MT", "VP", "FM", "MLT", "R", "C");
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  punts$c <-  pnorm(punts$C, bar[7,1], bar[7,2]);
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$cg <- sapply(punts[["c"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize6 <- function(punts, prebarems){
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  
  #prebarems=read.csv('proves_barems_sup.csv', header = FALSE);
  bar = baremar_grans(prebarems);
  #predreg = regressio_grans(prebarems);
  
  colnames(punts) <- c("Noms","L", "MT", "VP", "FM", "MLT", "R", "C");
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  punts$c <-  pnorm(punts$C, bar[7,1], bar[7,2]);
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$cg <- sapply(punts[["c"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize_intra1 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));
  
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize_intra2 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));
  
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize_intra3 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize_intra4 <- function(punts, prebarems){
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));

  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize_intra5 <- function(punts, prebarems){

  #punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R","C");
  
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));
  punts$c <-  pnorm(punts$C, mean(punts$C, na.rm = TRUE), sd(punts$C, na.rm = TRUE));
  
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$cg <- sapply(punts[["c"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}

initialize_intra6 <- function(punts, prebarems){
  
 # punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R","C");
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));
  punts$c <-  pnorm(punts$C, mean(punts$C, na.rm = TRUE), sd(punts$C, na.rm = TRUE));
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$cg <- sapply(punts[["c"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  return(punts);
}
########
# Funcions per treure els errors, els apèndix diuen a quins cursos pertanyen.
######

add.errors.12 <- function(punts){
  
  #traiem els errors: 
  for (i in 1:6){
    punts[2*i] <- punts[2*i]-coef[i]*punts[2*i+1];
  }
  return(punts);
}

add.errors.34 <- function(punts){
  
  punts[4] <- punts[4]+punts[6];
  punts[5] <- punts[5]+punts[7];
  punts[12] <- punts[12]+punts[14];
  punts[13] <- punts[13]+punts[15];
  
  punts=punts[,-c(6,7,14,15)];
  
  #traiem els errors: 
  for (i in 1:6){
    punts[2*i] <- punts[2*i]-coef[i]*punts[2*i+1]
  }
  
  return(punts);
}

add.errors.56 <- function(punts){
  
  punts[4] <- punts[4]+punts[6]+punts[8];
  punts[5] <- punts[5]+punts[7]+punts[9];
  punts[14] <- punts[14]+punts[16]+punts[18];
  punts[15] <- punts[15]+punts[17]+punts[19];
  
  punts=punts[,-c(6:9,16:19)];
  
  #traiem els errors: 
  for (i in 1:7){
    punts[2*i] <- punts[2*i]-coef[i]*punts[2*i+1]
  }
  
  return(punts);
}

#initialize <- function(cicle, punts){
#  #punts[punts == 0] <- NA;
#  ifelse(cicle == 1, punts <- initialize1(punts), ifelse(cicle == 2, punts <- initialize2(punts), punts <- initialize3(punts)));
#  return(punts);
#}

#####
# Plots 


# Aquest és el plot mare per tal de fer els plots de barres

barplot.prova <- function(punts, curs, titol, tipus, nom_plot, escola, i){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
  
  if (tipus=='norm'| tipus == 'norm_intra'){
    colors <- myColors;
  } else {
    colors <- otherColors;
  }
  
  punts[['value.1']] <- factor(punts[['value.1']]);
  names(colors) <- levels(unique(punts[['value.1']]));


  ggplot(punts[punts$variable %in% levels(unique(punts$variable))[i],]) + 
    geom_bar(stat='identity', aes(x = Noms, y = value, fill = value.1)) + 
    labs(title = titol, 
         subtitle= ifelse(tipus=='norm','Comparació barem universal',
                          ifelse(tipus== 'norm_intra','Comparació intra-classe',
                                 ifelse(tipus == 'comp','Resultat predit',
                                        'Resultat predit intraclasse')))) +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size=16), 
          axis.text.y = element_text(size=14), 
          title=element_text(size = 20),
          #subtitle = element_text(size=15),
          #text = element_text(size=20), 
          plot.title = element_text(lineheight=.8, face="bold"), 
          legend.position="none") + 
    #ggtitle(titol) + 
    scale_fill_manual(name='value.1', values=colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) + 
    ggsave(file = paste("figures/", escola[2], "/", curs, "/", nom_plot, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

ggpbar12 <- function(nens, pos, punts, curs, tipus, escola){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
  if (tipus=='norm'|tipus == 'norm_intra'){
    colors <- myColors;
  } else {
    colors <- otherColors;
  }
  
  punts[['value.1']] <- factor(punts[['value.1']]);
  names(colors) <- levels(punts[['value.1']]);
  
  ggplot(punts[punts$Noms %in% levels(punts$Noms)[nens],]) + 
    geom_bar(stat='identity', 
             aes(x = variable, y = value, fill=value.1)) + 
    facet_wrap( ~ Noms, ncol=1, scales="free") +  
    labs(title = ifelse(tipus=='norm','Resultat experimental',
                        ifelse(tipus== 'norm_intra', 'Resultat experimental intraclasse',
                               ifelse(tipus == 'comp', 'Resultat predit',
                                      'Resultat predit intraclasse'))))  +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          title = element_text(size=20),
          legend.position="none", 
          strip.text.x = element_text(face = "bold")
    ) +
    scale_fill_manual(name = 'value.1', values = colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) +
    ggsave(file = paste("figures/", escola[2], "/", curs, "/",pos,"-", tipus, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 9, units = "in");
}

ggpbar3 <- function(nens, pos, punts, curs, tipus, escola){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
  
  if (tipus=='norm'|tipus == 'norm_intra'){
    colors <- myColors;
  } else {
    colors <- otherColors;
  }
  
  punts[['value.1']] <- factor(punts[['value.1']]);
  names(colors) <- levels(unique(punts[['value.1']]));
  
  ggplot(punts[punts$Noms %in% levels(unique(punts$Noms))[nens],]) + 
    geom_bar(stat='identity', aes(x = variable, y = value, fill = value.1)) + 
    facet_wrap( ~ Noms, ncol=2, scales="free") +  
    labs(title = ifelse(tipus=='norm','Resultat experimental',
                        ifelse(tipus== 'norm_intra', 'Resultat experimental intraclasse',
                               ifelse(tipus == 'comp', 'Resultat predit',
                                      'Resultat predit intraclasse'))))  +
    theme(axis.text.x=element_text(size = 20), 
          axis.text.y = element_text(size= 20), 
          axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          title = element_text(size=24),
          legend.position="none", 
          strip.text.x = element_text(face = "bold",size = 24)) + 
    scale_fill_manual(name = 'value.1', values=colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) + 
    ggsave(file = paste("figures/", escola[2], "/", curs, "/",pos,"-", tipus, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 9, units = "in");
}
## Barplots pels nens de primer a quart. 

barplot.nens12 <- function(punts, curs, tipus, escola){
  
  
  for(i in seq(1,length(levels(unique(punts$Noms))),1)){
  ggpbar12(i, i, punts, curs, tipus, escola);
    }
  
  
}

## Barplots pels nens de cinquè i sisè. 

barplot.nens3 <- function(punts, curs, tipus, escola){
  
  for(i in seq(1,length(levels(unique(punts$Noms))),1)){
    ggpbar3(i, i, punts, curs, tipus, escola);
  }
  
}
 

## plots de les proves conjuntes per tota la classe

plots1 <- function(punts, curs, tipus, escola, i){
  barplot.prova(punts = punts, curs = curs, titol = "Lectura",tipus, nom_plot = paste("lectura-", i,"-",tipus, sep = ""), escola, 1);
  barplot.prova(punts = punts, curs = curs,  titol = "Memòria de Treball",tipus, nom_plot = paste("mtp-", i,"-",tipus, sep =""),escola, 2); 
  #barplot.prova(punts = punts, curs =  curs, nom_prova = c("MT-I", "mtig"), titol = "Memòria de Treball: Imatges", nom_plot = 'mti-5',3) 
  #barplot.prova(punts = punts, curs =  curs, nom_prova = c("MT-N", "mtng"), titol = "Memòria de Treball: Números", nom_plot = 'mtn-5',4)  
  barplot.prova(punts = punts, curs = curs,  titol = "Velocitat de Processament",tipus, nom_plot = paste("vp-", i,"-",tipus, sep =""),escola,3);
  barplot.prova(punts = punts, curs = curs,  titol = "Fluïdesa Matemàtica",tipus, nom_plot = paste("fluidesa-", i,"-",tipus, sep =""),escola,4);
  barplot.prova(punts = punts, curs = curs, titol = "Memòria a Llarg Termini",tipus, nom_plot = paste("mltp-", i,"-",tipus, sep =""),escola,5);
  #barplot.prova(punts = punts, curs = curs, nom_prova = c("MLT-I", "mltig"), titol = "Memòria a Llarg Termini: Imatges", nom_plot = 'mlti-5',8)
  #barplot.prova(punts = punts, curs = curs, nom_prova = c("MLT-N", "mltng"), titol = "Memòria a Llarg Termini: Números", nom_plot = 'mltn-5',9)
  barplot.prova(punts = punts, curs = curs,  titol = "Raonament",tipus, nom_plot = paste("raonament-", i,"-",tipus, sep =""),escola,6);
  #barplot.prova(punts = punts, curs = curs,  titol = "Càlcul",tipus, nom_plot = paste("calcul-", i,"-", tipus, sep =""), 7);
}




plots2 <- function(punts, curs, tipus, escola, i){
  barplot.prova(punts = punts, curs = curs, titol = "Lectura",tipus, nom_plot = paste("lectura-", i,"-",tipus, sep = ""), escola,1);
  barplot.prova(punts = punts, curs = curs,  titol = "Memòria de Treball",tipus, nom_plot = paste("mtp-", i,"-",tipus, sep =""), escola,2); 
  #barplot.prova(punts = punts, curs =  curs, nom_prova = c("MT-I", "mtig"), titol = "Memòria de Treball: Imatges", nom_plot = 'mti-5',3) 
  #barplot.prova(punts = punts, curs =  curs, nom_prova = c("MT-N", "mtng"), titol = "Memòria de Treball: Números", nom_plot = 'mtn-5',4)  
  barplot.prova(punts = punts, curs = curs,  titol = "Velocitat de Processament",tipus, nom_plot = paste("vp-", i,"-",tipus, sep =""),escola,3);
  barplot.prova(punts = punts, curs = curs,  titol = "Fluïdesa Matemàtica",tipus, nom_plot = paste("fluidesa-", i,"-",tipus, sep =""),escola,4);
  barplot.prova(punts = punts, curs = curs, titol = "Memòria a Llarg Termini",tipus, nom_plot = paste("mltp-", i,"-",tipus, sep =""),escola,5);
  #barplot.prova(punts = punts, curs = curs, nom_prova = c("MLT-I", "mltig"), titol = "Memòria a Llarg Termini: Imatges", nom_plot = 'mlti-5',8)
  #barplot.prova(punts = punts, curs = curs, nom_prova = c("MLT-N", "mltng"), titol = "Memòria a Llarg Termini: Números", nom_plot = 'mltn-5',9)
  barplot.prova(punts = punts, curs = curs,  titol = "Raonament",tipus, nom_plot = paste("raonament-", i,"-",tipus, sep =""),escola,6);
  #barplot.prova(punts = punts, curs = curs,  titol = "Càlcul",tipus, nom_plot = paste("calcul-", i,"-", tipus, sep =""), 7);
}

plots3 <- function(punts, curs,tipus, escola, i){
  barplot.prova(punts = punts, curs = curs, titol = "Lectura",tipus, nom_plot = paste("lectura-", i,"-",tipus, sep = ""), escola,1);
  barplot.prova(punts = punts, curs = curs,  titol = "Memòria de Treball",tipus, nom_plot = paste("mtp-", i,"-",tipus, sep =""), escola,2); 
  #barplot.prova(punts = punts, curs =  curs, nom_prova = c("MT-I", "mtig"), titol = "Memòria de Treball: Imatges", nom_plot = 'mti-5',3) 
  #barplot.prova(punts = punts, curs =  curs, nom_prova = c("MT-N", "mtng"), titol = "Memòria de Treball: Números", nom_plot = 'mtn-5',4)  
  barplot.prova(punts = punts, curs = curs,  titol = "Velocitat de Processament",tipus, nom_plot = paste("vp-", i,"-",tipus, sep =""),escola,3);
  barplot.prova(punts = punts, curs = curs,  titol = "Fluïdesa Matemàtica",tipus, nom_plot = paste("fluidesa-", i,"-",tipus, sep =""),escola,4);
  barplot.prova(punts = punts, curs = curs, titol = "Memòria a Llarg Termini",tipus, nom_plot = paste("mltp-", i,"-",tipus, sep =""),escola,5);
  #barplot.prova(punts = punts, curs = curs, nom_prova = c("MLT-I", "mltig"), titol = "Memòria a Llarg Termini: Imatges", nom_plot = 'mlti-5',8)
  #barplot.prova(punts = punts, curs = curs, nom_prova = c("MLT-N", "mltng"), titol = "Memòria a Llarg Termini: Números", nom_plot = 'mltn-5',9)
  barplot.prova(punts = punts, curs = curs,  titol = "Raonament",tipus, nom_plot = paste("raonament-", i,"-",tipus, sep =""),escola,6);
  barplot.prova(punts = punts, curs = curs,  titol = "Càlcul",tipus, nom_plot = paste("calcul-", i,"-", tipus, sep =""), escola,7);
}


#####

# matrius que treuen els resultats dels colors:

matriu_petits <- function(colnorm, colpred){
  # colnorm i colexp es troben posant la línia   colnorm = punts[,-c(2:7)]; 
  # just després de la punts_exp = punts[,c(1:7)]; i la línia
  # colpred = difs[,-c(2:7)]; després de difs = colorejar_grans(difs);
  
  tot_nens = cbind(colnorm,colpred[,-c(1)]);
  tot_nens[is.na(tot_nens)==TRUE]=0;
  
  # inicialitzem la matriu: 
  
  matlist <- vector("list", length(rownames(tot_nens)));
  names(matlist) = tot_nens[,1];
  
  mat = matrix(0, nrow=6, ncol=6);
  colnames(mat) = c("L", "MT", "VP", "FM", "MLT", "R");
  rownames(mat) = c("L", "MT", "VP", "FM", "MLT", "R");
  
  # i la calculem
  
  for (i in 1:nrow(tot_nens)){
    for (j in 1:6){
      if (tot_nens[i,j+1]=='C'){
        mat[j,j]=1;
        if(tot_nens[i,j+7]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+7]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }
      } else if (tot_nens[i,j+1]=='B'){
        mat[j,j]=2;
        if(tot_nens[i,j+7]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+7]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }
      } else if (tot_nens[i,j+1]=='A'){
        mat[j,j]=3;
        if(tot_nens[i,j+7]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+7]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }}
      else{mat[j,j]=0}
    }
    matlist[[i]]=mat;
  }
  return(matlist);
}

matriu_grans <- function(colnorm, colpred){
  
  # colnorm i colexp es troben posant la línia   colnorm = punts[,-c(2:8)]; 
  # just després de la punts_exp = punts[,c(1:8)]; i la línia
  # colpred = difs[,-c(2:8)]; després de difs = colorejar_grans(difs);
  
  tot_nens = cbind(colnorm,colpred[,-c(1)]);
  
  tot_nens[is.na(tot_nens)==TRUE]=0;
  # inicialitzem la matriu: 
  
  matlist <- vector("list", length(rownames(tot_nens)));
  names(matlist) = tot_nens[,1];
  
  mat = matrix(0, nrow=7, ncol=7);
  colnames(mat) = c("L", "MT", "VP", "FM", "MLT", "R", "C");
  rownames(mat) = c("L", "MT", "VP", "FM", "MLT", "R", "C");
  
  # i la calculem
  
  for (i in 1:nrow(tot_nens)){
    for (j in 1:7){
      if (tot_nens[i,j+1]=='C'){
        mat[j,j]=1;
        if(tot_nens[i,j+8]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+8]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }
      } else if (na.omit(tot_nens[i,j+1])=='B'){
        mat[j,j]=2;
        if(tot_nens[i,j+8]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+8]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }
      } else if (tot_nens[i,j+1]=='A'){
        mat[j,j]=3;
        if(tot_nens[i,j+8]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+8]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }}
     else{mat[j,j]=0} 
    }
    matlist[[i]]=mat;
  }
  return(matlist);
}



#####
# Funcions que fan els informes
#####


informe1 <- function(puntso, curs, prebarems, escola){
  library(reshape2)
  library(ggplot2)
  
  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # gràfics normals globals
  
  punts <- initialize1(puntso, prebarems);
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:7));
  
  punts_exp = punts[,c(1:7)];
  colnorm = punts[,-c(2:7)];
  
  colnames(punts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R");
  punts1 <-  melt(punts[c("Noms", "L", "MT", "VP", "FM", "MLT",  "R")], id.var = "Noms");
  punts2 <- melt(punts[c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg")], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  plots1(punts, curs, 'norm', escola, 1);
  barplot.nens12(punts, curs, 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- initialize1_comp(puntso, prebarems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:7));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = 7);
  difs=data.frame(difs);
  
  colnames(difs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r");
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:7){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar_petits(difs, prebarems);
  colpred = difs[,-c(2:7)];
  
  difs[,c(2:7)]=cpunts[,-1];
  
  colnames(difs)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  cpunts1 <-  melt(difs[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  cpunts2 <- melt(difs[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  #plots1(cpunts, curs,'comp', 1);
  barplot.nens12(cpunts, curs, 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- initialize_intra1(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:7));
  
  punts_expi = ipunts[,c(1:7)];
  
  colnames(ipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  ipunts1 <-  melt(ipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  ipunts2 <- melt(ipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  plots1(ipunts, curs,'norm_intra', escola,1);
  #barplot.nens12(ipunts, curs, 'norm_intra');
  
  # # gràfics compensats intraclasse
  # 
  # cipunts <- initialize1_comp_intra(puntso, prebarems);
  # length <- length(cipunts[,1]);
  # cipunts <- subset(cipunts, select = -c(2:7));
  # 
  # idifs=matrix(nrow= nrow(punts_exp), ncol = 7);
  # idifs=data.frame(difs);
  # 
  # colnames(idifs) <- c("Noms", "lec", "mt", "vp", "fm", "MLT", "r");
  # 
  # idifs$Noms <- punts_expi$Noms;
  # 
  # for (i in 2:7){
  #   idifs[,i]=punts_expi[,i]-cipunts[,i];  
  # }
  # 
  # idifs = colorejar_petits(idifs);
  # colpred = difs[,-c(2:7)];
  # 
  # idifs[,c(2:7)]=cipunts[,-1];
  # 
  # colnames(cipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  # cipunts1 <-  melt(cipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  # cipunts2 <- melt(cipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  # cipunts <- data.frame(cipunts1, cipunts2[c("variable", "value")]);
  # plots1(cipunts, curs,'comp_intra', 1);
  # barplot.nens12(cipunts, curs, 'comp_intra');
  
  return(matriu_petits(colnorm, colpred));
}

informe2 <- function(puntso, curs, prebarems, escola){
  library(reshape2)
  library(ggplot2)
  
  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # gràfics normals globals
  
  punts <- initialize2(puntso, prebarems);
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:7));
  
  punts_exp = punts[,c(1:7)];
  colnorm = punts[,-c(2:7)];
  
  colnames(punts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R");
  punts1 <-  melt(punts[c("Noms", "L", "MT", "VP", "FM", "MLT",  "R")], id.var = "Noms");
  punts2 <- melt(punts[c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg")], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  plots1(punts, curs, 'norm',escola, 2);
  barplot.nens12(punts, curs, 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- initialize2_comp(puntso, prebarems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:7));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = 7);
  difs=data.frame(difs);
  
  colnames(difs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r");
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:7){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar_petits(difs, prebarems);
  colpred = difs[,-c(2:7)];
  
  difs[,c(2:7)]=cpunts[,-1];
  
  colnames(difs)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  cpunts1 <-  melt(difs[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  cpunts2 <- melt(difs[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  #plots1(cpunts, curs,'comp', 2);
  barplot.nens12(cpunts, curs, 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- initialize_intra2(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:7));
  
  punts_expi = ipunts[,c(1:7)];
  
  colnames(ipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  ipunts1 <-  melt(ipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  ipunts2 <- melt(ipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  plots1(ipunts, curs,'norm_intra',escola, 2);
  #barplot.nens12(ipunts, curs, 'norm_intra');
  
  # # gràfics compensats intraclasse
  # 
  # cipunts <- initialize2_comp_intra(puntso, prebarems);
  # length <- length(cipunts[,1]);
  # cipunts <- subset(cipunts, select = -c(2:7));
  # 
  # idifs=matrix(nrow= nrow(punts_exp), ncol = 7);
  # idifs=data.frame(difs);
  # 
  # colnames(idifs) <- c("Noms", "lec", "mt", "vp", "fm", "MLT", "r");
  # 
  # idifs$Noms <- punts_expi$Noms;
  # 
  # for (i in 2:7){
  #   idifs[,i]=punts_expi[,i]-cipunts[,i];  
  # }
  # 
  # idifs = colorejar_petits(idifs);
  # colpred = difs[,-c(2:7)];
  # 
  # idifs[,c(2:7)]=cipunts[,-1];
  # 
  # colnames(cipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  # cipunts1 <-  melt(cipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  # cipunts2 <- melt(cipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  # cipunts <- data.frame(cipunts1, cipunts2[c("variable", "value")]);
  # plots1(cipunts, curs,'comp_intra', 2);
  # barplot.nens12(cipunts, curs, 'comp_intra');
  # 
  return(matriu_petits(colnorm, colpred));
}

informe3 <- function(puntso, curs, prebarems, escola){
  library(reshape2)
  library(ggplot2)
  
  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # gràfics normals globals
  
  punts <- initialize3(puntso, prebarems);
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:7));
  
  punts_exp = punts[,c(1:7)];
  colnorm = punts[,-c(2:7)];
  
  colnames(punts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R");
  punts1 <-  melt(punts[c("Noms", "L", "MT", "VP", "FM", "MLT",  "R")], id.var = "Noms");
  punts2 <- melt(punts[c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg")], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  plots2(punts, curs, 'norm',escola, 3);
  barplot.nens12(punts, curs, 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- initialize3_comp(puntso, prebarems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:7));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = 7);
  difs=data.frame(difs);
  
  colnames(difs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r");
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:7){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar_petits(difs, prebarems);
  colpred = difs[,-c(2:7)];
  
  difs[,c(2:7)]=cpunts[,-1];
  
  colnames(difs)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  cpunts1 <-  melt(difs[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  cpunts2 <- melt(difs[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  #plots2(cpunts, curs,'comp', 3);
  barplot.nens12(cpunts, curs, 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- initialize_intra3(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:7));
  
  punts_expi = ipunts[,c(1:7)];
  
  colnames(ipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  ipunts1 <-  melt(ipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  ipunts2 <- melt(ipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  plots2(ipunts, curs,'norm_intra',escola, 3);
  #barplot.nens12(ipunts, curs, 'norm_intra');
  
  # # gràfics compensats intraclasse
  # 
  # cipunts <- initialize3_comp_intra(puntso, prebarems);
  # length <- length(cipunts[,1]);
  # cipunts <- subset(cipunts, select = -c(2:7));
  # 
  # idifs=matrix(nrow= nrow(punts_exp), ncol = 7);
  # idifs=data.frame(difs);
  # 
  # colnames(idifs) <- c("Noms", "lec", "mt", "vp", "fm", "MLT", "r");
  # 
  # idifs$Noms <- punts_expi$Noms;
  # 
  # for (i in 2:7){
  #   idifs[,i]=punts_expi[,i]-cipunts[,i];  
  # }
  # 
  # idifs = colorejar_petits(idifs);
  # colpred = difs[,-c(2:7)];
  # 
  # idifs[,c(2:7)]=cipunts[,-1];
  # 
  # colnames(cipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  # cipunts1 <-  melt(cipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  # cipunts2 <- melt(cipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  # cipunts <- data.frame(cipunts1, cipunts2[c("variable", "value")]);
  # plots1(cipunts, curs,'comp_intra', 3);
  # barplot.nens12(cipunts, curs, 'comp_intra');
  # 
  return(matriu_petits(colnorm, colpred));
}

informe4 <- function(puntso, curs, prebarems, escola){
  library(reshape2)
  library(ggplot2)
  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # gràfics normals globals
  
  # gràfics normals globals
  
  punts <- initialize4(puntso, prebarems);
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:7));
  
  punts_exp = punts[,c(1:7)];
  colnorm = punts[,-c(2:7)];
  
  colnames(punts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R");
  punts1 <-  melt(punts[c("Noms", "L", "MT", "VP", "FM", "MLT",  "R")], id.var = "Noms");
  punts2 <- melt(punts[c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg")], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  plots1(punts, curs, 'norm',escola, 4);
  barplot.nens12(punts, curs, 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- initialize4_comp(puntso, prebarems);

  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:7));
  
  difs=matrix(0, nrow = nrow(punts_exp), ncol = 7);
  difs=data.frame(difs);
  
  colnames(difs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r");
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:7){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar_petits(difs, prebarems);
  colpred = difs[,-c(2:7)];
  
  difs[,c(2:7)]=cpunts[,-1];
  
  colnames(difs)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  cpunts1 <-  melt(difs[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  cpunts2 <- melt(difs[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  #plots2(cpunts, curs,'comp', 4);
  barplot.nens12(cpunts, curs, 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- initialize_intra4(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:7));
  
  punts_expi = ipunts[,c(1:7)];
  
  colnames(ipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  ipunts1 <-  melt(ipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  ipunts2 <- melt(ipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  plots1(ipunts, curs,'norm_intra',escola, 4);
  #barplot.nens12(ipunts, curs, 'norm_intra');
  
  # gràfics compensats intraclasse
  # 
  # cipunts <- initialize4_comp_intra(puntso, prebarems);
  # length <- length(cipunts[,1]);
  # cipunts <- subset(cipunts, select = -c(2:7));
  # 
  # idifs=matrix(nrow= nrow(punts_exp), ncol = 7);
  # idifs=data.frame(difs);
  # 
  # colnames(idifs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r");
  # 
  # idifs$Noms <- punts_expi$Noms;
  # 
  # for (i in 2:7){
  #   idifs[,i]=punts_expi[,i]-cipunts[,i];  
  # }
  # 
  # idifs = colorejar_petits(idifs);
  # colpred = difs[,-c(2:7)];
  # 
  # idifs[,c(2:7)]=cipunts[,-1];
  # 
  # colnames(cipunts)[2:7] <- c("L", "MT", "VP", "FM", "MLT", "R")
  # cipunts1 <-  melt(cipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R")], id.var = "Noms");
  # cipunts2 <- melt(cipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg")], id.var = "Noms");
  # cipunts <- data.frame(cipunts1, cipunts2[c("variable", "value")]);
  # plots1(cipunts, curs,'comp_intra', 4);
  # barplot.nens12(cipunts, curs, 'comp_intra');
  # 
  return(matriu_petits(colnorm, colpred));
  
}

informe5 <- function(puntso, curs, prebarems, escola){
  library(reshape2)
  library(ggplot2)

  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # gràfics normals globals
  
  punts <- initialize5(puntso, prebarems);
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:8));
  
  punts_exp = punts[,c(1:8)];
  colnorm = punts[,-c(2:8)];
  
  colnames(punts)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R", "C");
  punts1 <-  melt(punts[c("Noms", "L", "MT", "VP", "FM", "MLT",  "R", "C")], id.var = "Noms");
  punts2 <- melt(punts[c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg", "cg")], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  plots3(punts, curs, 'norm',escola, 5);
  barplot.nens3(punts, curs, 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- initialize5_comp(puntso, prebarems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:8));
  
  difs=matrix(nrow = nrow(punts_exp), ncol = 8);
  difs=data.frame(difs);
  
  colnames(difs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r", "c");
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:8){
     difs[,i]=punts_exp[,i]-cpunts[,i];   
  }
  
  difs = colorejar_grans(difs, prebarems);
  colpred = difs[,-c(2:8)];
  
  difs[,c(2:8)]=cpunts[,-1];

  colnames(difs)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R",'C')
  cpunts1 <-  melt(difs[c("Noms","L", "MT", "VP", "FM", "MLT", "R",'C')], id.var = "Noms");
  cpunts2 <- melt(difs[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg",'cg')], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  #plots3(cpunts, curs,'comp', 5);
  barplot.nens3(cpunts, curs, 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- initialize_intra5(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:8));
  
  punts_expi = ipunts[,c(1:8)];
  
  colnames(ipunts)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R",'C')
  ipunts1 <-  melt(ipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R",'C')], id.var = "Noms");
  ipunts2 <- melt(ipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg",'cg')], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  plots3(ipunts, curs,'norm_intra', escola,5);
  #barplot.nens3(ipunts, curs, 'norm_intra');
  
  # gràfics compensats intraclasse
  
  # cipunts <- initialize5_comp_intra(puntso, prebarems);
  # length <- length(cipunts[,1]);
  # cipunts <- subset(cipunts, select = -c(2:8));
  # 
  # idifs=matrix(nrow= nrow(punts_exp), ncol = 8);
  # idifs=data.frame(difs);
  # 
  # colnames(idifs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r",'c');
  # 
  # idifs$Noms <- punts_expi$Noms;
  # 
  # for (i in 2:8){
  #   idifs[,i]=punts_expi[,i]-cipunts[,i];  
  # }
  # 
  # idifs = colorejar_grans(idifs);
  # 
  # idifs[,c(2:8)]=cipunts[,-1];
  # 
  # colnames(cipunts)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R",'C')
  # cipunts1 <-  melt(cipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R",'C')], id.var = "Noms");
  # cipunts2 <- melt(cipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg",'cg')], id.var = "Noms");
  # cipunts <- data.frame(cipunts1, cipunts2[c("variable", "value")]);
  # plots3(cipunts, curs,'comp_intra', 5);
  # barplot.nens3(cipunts, curs, 'comp_intra');
  
  return(matriu_grans(colnorm, colpred));
  
  
}

informe6 <- function(puntso, curs, prebarems, escola){
  library(reshape2)
  library(ggplot2)
  
  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # gràfics normals globals
  
  punts <- initialize6(puntso, prebarems);
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:8));
  
  punts_exp = punts[,c(1:8)];
  colnorm = punts[,-c(2:8)]
  
  colnames(punts)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R", "C");
  punts1 <-  melt(punts[c("Noms", "L", "MT", "VP", "FM", "MLT",  "R", "C")], id.var = "Noms");
  punts2 <- melt(punts[c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg", "cg")], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  plots3(punts, curs, 'norm',escola, 6);
  barplot.nens3(punts, curs, 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- initialize6_comp(puntso, prebarems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:8));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = 8);
  difs=data.frame(difs);
  
  colnames(difs) <- c("Noms", "lec", "mt", "vp", "fm", "mlt", "r",'c');
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:8){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar_grans(difs, prebarems);
  colpred = difs[,-c(2:8)]
  
  difs[,c(2:8)]=cpunts[,-1];
  
  colnames(difs)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R",'C')
  cpunts1 <-  melt(difs[c("Noms","L", "MT", "VP", "FM", "MLT", "R",'C')], id.var = "Noms");
  cpunts2 <- melt(difs[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg",'cg')], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
#  plots3(cpunts, curs,'comp', 6);
  barplot.nens3(cpunts, curs, 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- initialize_intra6(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:8));
  
  punts_expi = ipunts[,c(1:8)];
  
  colnames(ipunts)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R",'C')
  ipunts1 <-  melt(ipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R",'C')], id.var = "Noms");
  ipunts2 <- melt(ipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg",'cg')], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  plots3(ipunts, curs,'norm_intra', escola,6);
  #barplot.nens3(ipunts, curs, 'norm_intra');
  
  # gràfics compensats intraclasse
  
  # cipunts <- initialize6_comp_intra(puntso, prebarems);
  # length <- length(cipunts[,1]);
  # cipunts <- subset(cipunts, select = -c(2:8));
  # 
  # idifs=matrix(nrow= nrow(punts_exp), ncol = 8);
  # idifs=data.frame(difs);
  # 
  # colnames(idifs) <- c("Noms", "lec", "mt", "vp", "fm", "MLT", "r",'c');
  # 
  # idifs$Noms <- punts_expi$Noms;
  # 
  # for (i in 2:8){
  #   idifs[,i]=punts_expi[,i]-cipunts[,i];  
  # }
  # 
  # idifs = colorejar_grans(idifs);
  # colpred = difs[,-c(2:8)];
  # 
  # idifs[,c(2:8)]=cipunts[,-1];
  # 
  # colnames(cipunts)[2:8] <- c("L", "MT", "VP", "FM", "MLT", "R",'C')
  # cipunts1 <-  melt(cipunts[c("Noms","L", "MT", "VP", "FM", "MLT", "R",'C')], id.var = "Noms");
  # cipunts2 <- melt(cipunts[c("Noms","lecg","mtg","vpg","fmg","mltg", "rg",'cg')], id.var = "Noms");
  # cipunts <- data.frame(cipunts1, cipunts2[c("variable", "value")]);
  # plots3(cipunts, curs,'comp_intra', 6);
  # barplot.nens3(cipunts, curs, 'comp_intra');
  # 
  return(matriu_grans(colnorm, colpred));
}



