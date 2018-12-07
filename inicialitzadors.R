
# Funció per tenir en compte els errors

afegir_errors <- function(punts){
  
  if (ncol(punts) == 23){
    punts[4] <- punts[4]+punts[6]+punts[8];
    punts[5] <- punts[5]+punts[7]+punts[9];
    punts[14] <- punts[14]+punts[16]+punts[18];
    punts[15] <- punts[15]+punts[17]+punts[19];
    
    punts = punts[,-c(6:9,16:19)];
  }
  
  else if(ncol(punts) == 17){
    punts[4] <- punts[4]+punts[6];
    punts[5] <- punts[5]+punts[7];
    punts[12] <- punts[12]+punts[14];
    punts[13] <- punts[13]+punts[15];
    
    punts = punts[,-c(6,7,14,15)];
  }
  #traiem els errors:
  
  for (i in 1:((ncol(punts)-1)/2) ){
    punts[2*i+1][is.na(punts[2*i+1])]=0 # posem 0 als errors si la columna és buida
    punts[2*i] <- punts[2*i]-coef[i]*punts[2*i+1];
  }
  return(punts);
}

# Funcions d'inicialització dels csv d'entrada i càlcul del percentil

inicialitzar <- function(punts, barems){
  
  punts <- afegir_errors(punts);
  bar = baremar(barems);
  
  grans = FALSE;
  
  if (ncol(barems) == 8){
    grans = TRUE;
  }
  
  if (grans == TRUE){
    cols = c(1,2,4,6,8,10,12,14)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R","C");
  }
  
  else {
    cols = c(1,2,4,6,8,10,12)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  }
  
  punts <- data.frame(punts[cols]);
  colnames(punts) <- nomcols;
  
  punts$lec <- pnorm(punts$L, bar[1,1],bar[1,2]);
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- pnorm(punts$VP, bar[3,1],bar[3,2]);
  punts$fm <- pnorm(punts$FM, bar[4,1],bar[4,2]);
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- pnorm(punts$R, bar[6,1], bar[6,2]);
  
  if(grans == TRUE){
    punts$c <-  pnorm(punts$C, bar[7,1], bar[7,2]);
  }
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  
  if(grans == TRUE){
    punts$cg <- sapply(punts[["c"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  }
  
  return(punts);
}

inicialitzar_intra <- function(punts){
  
  punts <- afegir_errors(punts);
  grans = FALSE;
  
  if (ncol(punts) == 15){
    grans = TRUE;
  }
  
  if (grans == TRUE){
    cols = c(1,2,4,6,8,10,12,14)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R","C");
  }
  
  else {
    cols = c(1,2,4,6,8,10,12)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
    
  }
  
  punts <- data.frame(punts[cols]);
  colnames(punts) <- nomcols;
  
  punts$lec <- pnorm(punts$L, mean(punts$L, na.rm = TRUE),sd(punts$L, na.rm = TRUE));
  punts$mt <- pnorm(punts$MT, mean(punts$MT, na.rm = TRUE),sd(punts$MT, na.rm = TRUE));
  punts$vp <- pnorm(punts$VP, mean(punts$VP, na.rm = TRUE),sd(punts$VP, na.rm = TRUE));
  punts$fm <- pnorm(punts$FM, mean(punts$FM, na.rm = TRUE),sd(punts$FM, na.rm = TRUE));
  punts$mlt <- pnorm(punts$MLT, mean(punts$MLT, na.rm = TRUE),sd(punts$MLT, na.rm = TRUE));
  punts$r <- pnorm(punts$R, mean(punts$R, na.rm = TRUE), sd(punts$R, na.rm = TRUE));
  
  if(grans == TRUE){
    punts$c <-  pnorm(punts$C, mean(punts$C, na.rm = TRUE), sd(punts$C, na.rm = TRUE));
  }
  
  
  punts$lecg <- sapply(punts[["lec"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mtg <- sapply(punts[["mt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$vpg <- sapply(punts[["vp"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$fmg <- sapply(punts[["fm"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$mltg <- sapply(punts[["mlt"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  punts$rg <- sapply(punts[["r"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  if(grans == TRUE){
    punts$cg <- sapply(punts[["c"]], function(x) ifelse(x < 0.3, "C", ifelse(x > 0.85, "A", "B")));
  }
  
  return(punts);
}

inicialitzar_comp <- function(punts, barems){
  
  punts <- afegir_errors(punts);
  
  if (ncol(punts) == 13){
    cols = c(1,2,4,6,8,10,12)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  }
  else {
    cols = c(1,2,4,6,8,10,12,14)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R", "C");
  }
  
  punts <- data.frame(punts[cols]);
  colnames(punts) <- nomcols;
  
  #importem els barems
  bar = baremar(barems);
  predreg = regressions(barems);
  
  # demanem la funció de predir: 
  
  punts = predir(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar(punts, barems);
  
  return(punts);
  
}

# La següent no es fa servir:
inicialitzar_comp_intra <- function(punts, barems){
  # netegem una mica les dades
  
  punts[punts == '-'] <- NA;
  punts <- afegir_errors(punts);
  
  if (ncol(punts) == 13){
    cols = c(1,2,4,6,8,10,12)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  }
  else {
    cols = c(1,2,4,6,8,10,12,14)
    nomcols = c("Noms", "L", "MT", "VP", "FM", "MLT", "R", "C");
  }
  
  
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  bar = baremar(barems);
  predreg = regressions(barems);
  
  # demanem la funció de predir: 
  
  punts = predir_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar(punts);
  
  return(punts);
}