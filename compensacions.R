

predir <- function(punts, bar, predreg){

  grans = FALSE
  
  if (nrow(bar)==8){
    grans = TRUE;
  }
  punts$lec <- punts$L;
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- punts$VP;
  punts$fm <- punts$FM;
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- punts$R;
  
  if (grans == TRUE){
    punts$c <- punts$C;
  }
  
  for (i in 1:nrow(punts)){
    
    per_vp = pnorm(punts$VP[i], bar[3,1], bar[3,2]);
    per_fm = pnorm(punts$FM[i], bar[4,1], bar[4,2]);
    
    if (is.na(per_vp) && is.na(per_fm)) {
      punts$lec[i] <- 0;
      punts$vp[i] <- 0;
      punts$fm[i] <- 0;
      punts$r[i] <- 0;
      if (grans == TRUE){
        punts$c[i] <- 0;
      }
    }
    
    else if (!is.na(per_vp) && !is.na(per_fm) && per_vp  >= per_fm | is.na(per_fm)){
      
      new_vp <- data.frame(VP = punts$VP[i]);
      if (!is.na(punts$L[i])==TRUE){
        punts$lec[i] <- predict(predreg[[1]][[1]], newdata = new_vp,na.action = na.exclude);
        punts$lec[i] <- pnorm(punts$lec[i], bar[1,1], bar[1,2]);
      }
      if (!is.na(punts$VP[i])==TRUE){
        punts$vp[i] <- pnorm(punts$VP[i], bar[3,1], bar[3,2]);}
      if (!is.na(punts$FM[i])==TRUE){
        punts$fm[i] <- predict(predreg[[3]][[1]], newdata = new_vp);
        punts$fm[i] <- pnorm(punts$fm[i], bar[4,1], bar[4,2]);}
      if (!is.na(punts$R[i])==TRUE){
        punts$r[i] <- predict(predreg[[4]][[1]], newdata = new_vp);
        punts$r[i] <- pnorm(punts$r[i], bar[6,1], bar[6,2]);}
      if (grans == TRUE){
        if (!is.na(punts$C[i])==TRUE){
          punts$c[i] <- predict(predreg[[5]][[1]], newdata = new_vp);
          punts$c[i] <- pnorm(punts$c[i], bar[7,1], bar[7,2]);}
      }
      
    } else if (!is.na(per_vp) && !is.na(per_fm) && per_vp  < per_fm | is.na(per_vp)) {
      new_fm <- data.frame(FM = punts$FM[i]);
      if (!is.na(punts$L[i])==TRUE){
        punts$lec[i] <- predict(predreg[[1]][[2]], newdata = new_fm,na.action = na.exclude);
        punts$lec[i] <- pnorm(punts$lec[i], bar[1,1], bar[1,2]);}
      if (!is.na(punts$VP[i])==TRUE){
        punts$vp[i] <- predict(predreg[[2]][[2]], newdata = new_fm);
        punts$vp[i] <- pnorm(punts$vp[i], bar[3,1], bar[3,2]);}
      if (!is.na(punts$FM[i])==TRUE){
        punts$fm[i] <- pnorm(punts$FM[i], bar[4,1], bar[4,2]);}
      if (!is.na(punts$R[i])==TRUE){
        punts$r[i] <- predict(predreg[[4]][[2]], newdata = new_fm);
        punts$r[i] <- pnorm(punts$r[i], bar[6,1], bar[6,2]);}
      if (grans == TRUE){
        if (!is.na(punts$C[i])==TRUE){
          punts$c[i] <- predict(predreg[[5]][[2]], newdata = new_fm);
          punts$c[i] <- pnorm(punts$c[i], bar[7,1], bar[7,2]);}
      }
    } 
  }
  return(punts);
} 

predir_intra <- function(punts, bar, predreg){
  
  grans = FALSE;
  
  if (nrow(bar) == 8){
    grans = TRUE;
  }
  
  msd = matrix(0, nrow = nrow(bar) - 1, ncol=2);
  
  for (i in 1:(nrow(bar)-1)){
    msd[i,]=c(mean(punts[[i+1]]), sd(punts[[i+1]]))
  }
  
  punts$lec <- rep(0,nrow(punts));
  punts$mt <- pnorm(punts$MT, msd[2,1],msd[2,2]);
  punts$vp <- rep(0,nrow(punts));
  punts$fm <- rep(0,nrow(punts));
  punts$mlt <- pnorm(punts$MLT, msd[5,1],msd[5,2]);
  punts$r <- rep(0,nrow(punts));
  if (grans == TRUE){
    punts$c <- rep(0,nrow(punts));
  }
  
  for (i in 1:nrow(punts)){
    
    if (per_vp[i]>=per_fm[i]){
      new_vp <- data.frame(VP = punts$VP[i]);
      punts$lec[i] <- predict(predreg[[1]][[1]], newdata = new_vp);
      punts$lec[i] <- pnorm(punts$lec[i], msd[1,1], msd[1,2]);
      punts$vp[i] <- pnorm(punts$VP[i], msd[3,1], msd[3,2])
      punts$fm[i] <- predict(predreg[[3]][[1]], newdata = new_vp);
      punts$fm[i] <- pnorm(punts$fm[i], msd[4,1], msd[4,2]);
      punts$r[i] <- predict(predreg[[4]][[1]], newdata = new_vp);
      punts$r[i] <- pnorm(punts$r[i], msd[6,1], msd[6,2]);
      if (grans == TRUE){
        punts$c[i] <- predict(predreg[[5]][[1]], newdata = new_vp);
        punts$c[i] <- pnorm(punts$c[i], msd[7,1], msd[7,2]);
      }
      
    }
    
    else {
      new_fm <- data.frame(FM = punts$FM[i]);
      punts$lec[i] <- predict(predreg[[1]][[2]], newdata = new_fm);
      punts$lec[i] <- pnorm(punts$lec[i], msd[1,1], msd[1,2]);
      punts$vp[i] <- predict(predreg[[2]][[2]], newdata = new_fm);
      punts$vp[i] <- pnorm(punts$vp[i], msd[3,1], msd[3,2])
      punts$fm[i] <- pnorm(punts$FM[i], msd[4,1], msd[4,2]);
      punts$r[i] <- predict(predreg[[4]][[2]], newdata = new_fm);
      punts$r[i] <- pnorm(punts$r[i], msd[6,1], msd[6,2]);
      if (grans == TRUE){
        punts$c[i] <- predict(predreg[[5]][[2]], newdata = new_fm);
        punts$c[i] <- pnorm(punts$c[i], msd[7,1], msd[7,2]);
      }
    }
    
  }
  return(punts);
  
} 

colorejar <- function(punts, prebarems){
  bardif=bar_diferencies(prebarems);
  
  punts$lecg <- sapply(punts[["lec"]], 
                       function(x) ifelse(x < - bardif[1,2], "L", 
                                          ifelse(x > bardif[1,2], "N", "M")));
  
  punts$mtg <- sapply(punts[["mt"]], 
                      function(x) ifelse(x < - bardif[2,2], "L", 
                                         ifelse(x > bardif[2,2], "N", "M")));
  
  punts$vpg <- sapply(punts[["vp"]], 
                      function(x) ifelse(x < - bardif[3,2], "L", 
                                         ifelse(x > bardif[3,2], "N", "M")));
  punts$fmg <- sapply(punts[["fm"]], 
                      function(x) ifelse(x < - bardif[4,2], "L", 
                                         ifelse(x > bardif[4,2], "N", "M")));
  
  punts$mltg <- sapply(punts[["mlt"]], 
                       function(x) ifelse(x < - bardif[5,2], "L", 
                                          ifelse(x > bardif[5,2], "N", "M")));
  punts$rg <- sapply(punts[["r"]], 
                     function(x) ifelse(x < - bardif[6,2], "L", 
                                        ifelse(x > bardif[6,2], "N", "M")));
  if (nrow(bardif) == 7){
    punts$cg <- sapply(punts[["c"]], 
                       function(x) ifelse(x < - bardif[7,2], "L", 
                                          ifelse(x > bardif[7,2], "N", "M")))
  }
  
  return(punts);
  
}

inicialitzar_comp <- function(punts, prebarems){

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
  bar = baremar(prebarems);
  predreg = regressions(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar(punts, prebarems);
  
  return(punts);
  
}

# La següent no es fa servir:
inicialitzar_comp_intra <- function(punts, prebarems){
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
  
  bar = baremar_petits(prebarems);
  predreg = regressions(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar(punts);
  
  return(punts);
}

