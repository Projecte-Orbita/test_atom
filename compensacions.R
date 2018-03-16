

predir_petits <- function(punts, bar, predreg){
  #punts$VPT <- pmax(punts$VP, punts$FM/mean(punts$FM)*mean(punts$VP));
  
  
  punts$lec <- punts$L;
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- punts$VP;
  punts$fm <- punts$FM;
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- punts$R;
  
  for (i in 1:nrow(punts)){
    
    per_vp = pnorm(punts$VP[i], bar[3,1], bar[3,2]);
    per_fm = pnorm(punts$FM[i], bar[4,1], bar[4,2]);
    
    if (is.na(per_vp) && is.na(per_fm)) {
      punts$lec[i] <- 0;
      punts$vp[i] <- 0;
      punts$fm[i] <- 0;
      punts$r[i] <- 0;
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
    } 
  }
  return(punts);
} 
# aquesta funciona per petits i mitjans

predir_grans <- function(punts, bar, predreg){
  
  #punts$VPT <- pmax(punts$VP, punts$FM/mean(punts$FM)*mean(punts$VP));
  
  
  punts$lec <- punts$L;
  punts$mt <- pnorm(punts$MT, bar[2,1],bar[2,2]);
  punts$vp <- punts$VP;
  punts$fm <- punts$FM;
  punts$mlt <- pnorm(punts$MLT, bar[5,1],bar[5,2]);
  punts$r <- punts$R;
  punts$c <- punts$C;
  
  #  per_vp = pnorm(punts$VP, bar[3,1], bar[3,2]);
  #  per_fm = pnorm(punts$FM, bar[4,1], bar[4,2]);
  
  
  for (i in 1:nrow(punts)){
    
    per_vp = pnorm(punts$VP[i], bar[3,1], bar[3,2]);
    per_fm = pnorm(punts$FM[i], bar[4,1], bar[4,2]);
    
    if (is.na(per_vp) && is.na(per_fm)) {
      punts$lec[i] <- NA;
      punts$vp[i] <- NA;
      punts$fm[i] <- NA;
      punts$r[i] <- NA;
      punts$c[i] <- NA;
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
      if (!is.na(punts$C[i])==TRUE){
        punts$c[i] <- predict(predreg[[5]][[1]], newdata = new_vp);
        punts$c[i] <- pnorm(punts$c[i], bar[7,1], bar[7,2]);}
      
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
      if (!is.na(punts$C[i])==TRUE){
        punts$c[i] <- predict(predreg[[5]][[2]], newdata = new_fm);
        punts$c[i] <- pnorm(punts$c[i], bar[7,1], bar[7,2]);}
    } 
  }
  return(punts);
}


####### les dos funcions següents per ara no es fan servir: ####

predir_petits_intra <- function(punts, bar, predreg){
  
  msd = matrix(0,nrow=6, ncol=2);
  
  for (i in 1:6){
    msd[i,]=c(mean(punts[[i+1]]), sd(punts[[i+1]]))
  }
  
  
  #punts$VPT <- pmax(punts$VP, punts$FM/mean(punts$FM)*mean(punts$VP));
  punts$lec <- rep(0,nrow(punts));
  punts$mt <- pnorm(punts$MT, msd[2,1],msd[2,2]);
  punts$vp <- rep(0,nrow(punts));
  punts$fm <- rep(0,nrow(punts));
  punts$mlt <- pnorm(punts$MLT, msd[5,1],msd[5,2]);
  punts$r <- rep(0,nrow(punts));
  
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
    }
    
  }
  return(punts);
  
} 
# aquesta funciona per petits i mitjans

predir_grans_intra <- function(punts, bar, predreg){
  
  msd = matrix(0,nrow=7, ncol=2);
  
  for (i in 1:7){
    msd[i,]=c(mean(punts[[i+1]]), sd(punts[[i+1]]))
  }
  
  punts$lec <- rep(0,nrow(punts));
  punts$mt <- pnorm(punts$MT, msd[2,1],msd[2,2]);
  punts$vp <- rep(0,nrow(punts));
  punts$fm <- rep(0,nrow(punts));
  punts$mlt <- pnorm(punts$MLT, msd[5,1],msd[5,2]);
  punts$r <- rep(0,nrow(punts));
  punts$c <- rep(0,nrow(punts));
  
  #  per_vp = pnorm(punts$VP, bar[3,1], bar[3,2]);
  #  per_fm = pnorm(punts$FM, bar[4,1], bar[4,2]);
  
  
  for (i in 1:nrow(punts)){
    
    per_vp = pnorm(punts$VP[i], bar[3,1], bar[3,2]);
    per_fm = pnorm(punts$FM[i], bar[4,1], bar[4,2]);
    
    if (per_vp >= per_fm){
      new_vp <- data.frame(VP = punts$VP[i]);
      punts$lec[i] <- predict(predreg[[1]][[1]], newdata = new_vp);
      punts$lec[i] <- pnorm(punts$lec[i], msd[1,1], msd[1,2]);
      punts$vp[i] <- pnorm(punts$VP[i], msd[3,1], msd[3,2])
      punts$fm[i] <- predict(predreg[[3]][[1]], newdata = new_vp);
      punts$fm[i] <- pnorm(punts$fm[i], msd[4,1], msd[4,2]);
      punts$r[i] <- predict(predreg[[4]][[1]], newdata = new_vp);
      punts$r[i] <- pnorm(punts$r[i], msd[6,1], msd[6,2]);
      punts$c[i] <- predict(predreg[[5]][[1]], newdata = new_vp);
      punts$c[i] <- pnorm(punts$c[i], msd[7,1], msd[7,2]);
      
    } else {
      new_fm <- data.frame(FM = punts$FM[i]);
      punts$lec[i] <- predict(predreg[[1]][[2]], newdata = new_fm);
      punts$lec[i] <- pnorm(punts$lec[i], msd[1,1], msd[1,2]);
      punts$vp[i] <- predict(predreg[[2]][[2]], newdata = new_fm);
      punts$vp[i] <- pnorm(punts$vp[i], msd[3,1], msd[3,2])
      punts$fm[i] <- pnorm(punts$FM[i], msd[4,1], msd[4,2]);
      punts$r[i] <- predict(predreg[[4]][[2]], newdata = new_fm);
      punts$r[i] <- pnorm(punts$r[i], msd[6,1], msd[6,2]);
      punts$c[i] <- predict(predreg[[5]][[2]], newdata = new_fm);
      punts$c[i] <- pnorm(punts$c[i], msd[7,1], msd[7,2]);
    }
    
  }
  return(punts);
}

######

colorejar_petits <- function(punts, prebarems){
  
  bardif=bardif_petits(prebarems);
  
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
  return(punts);
} 
#aquesta funciona per petits i mitjans

colorejar_grans <- function(punts, prebarems){
  
  bardif=bardif_grans(prebarems);
  
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
  punts$cg <- sapply(punts[["c"]], 
                     function(x) ifelse(x < - bardif[7,2], "L", 
                                        ifelse(x > bardif[7,2], "N", "M")))
  
  return(punts);
}

initialize1_comp <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #importem els barems
  
  
  #  prebarems=read.csv('prebarems1.csv', header = FALSE);
  bar = baremar_petits(prebarems);
  predreg = regressio_petits(prebarems);
  
  # calculem la variable velocitat de processament total:
  
  # demanem la funció de predir: 
  
  punts = predir_petits(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize2_comp <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  # importem els barems
  #  prebarems=read.csv('prebarems2.csv', header = FALSE);
  bar = baremar_petits(prebarems);
  predreg = regressio_petits(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_petits(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize3_comp <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  # importem els barems
  
  #  prebarems=read.csv('prebarems3.csv', header = FALSE);
  bar = baremar_mitjans(prebarems);
  predreg = regressio_mitjans(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_petits(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize4_comp <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #importem els barems
  
  #  prebarems=read.csv('prebarems4.csv', header = FALSE);
  bar = baremar_mitjans(prebarems);
  predreg = regressio_mitjans(prebarems);
  
  
  # demanem la funció de predir: 
  
  punts = predir_petits(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize5_comp <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  
  #  punts=read.csv('punts-new-sup.csv', header = FALSE);
  #punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R",'C');
  
  #importem els barems
  
  #  prebarems=read.csv('prebarems5.csv', header = FALSE);
  bar = baremar_grans(prebarems);
  predreg = regressio_grans(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_grans(punts, bar, predreg);
  
  # colors dels grups
  
  # punts = colorejar_grans(punts);
  
  # punts <- punts[,-9]
  # això era per treure la columna de vpt, però ara ja no l'afegeixo (crec))
  
  return(punts);
}

initialize6_comp <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  #punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R", "C");
  
  #importem els barems
  
  #  prebarems=read.csv('prebarems6.csv', header = FALSE);
  bar = baremar_grans(prebarems);
  predreg = regressio_grans(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_grans(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_grans(punts, prebarems);
  
  return(punts);
}

initialize1_comp_intra <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #importem els barems
  
  
  #  prebarems=read.csv('prebarems1.csv', header = FALSE);
  bar = baremar_petits(prebarems);
  predreg = regressio_petits(prebarems);
  
  # calculem la variable velocitat de processament total:
  
  # demanem la funció de predir: 
  
  punts = predir_petits_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts);
  
  return(punts);
}

initialize2_comp_intra <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  punts[punts == '-'] <- NA;
  punts <- add.errors.12(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  # importem els barems
  #  prebarems=read.csv('prebarems2.csv', header = FALSE);
  bar = baremar_petits(prebarems);
  predreg = regressio_petits(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_petits_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize3_comp_intra <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  punts[punts == '-'] <- NA;
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  # importem els barems
  
  #  prebarems=read.csv('prebarems3.csv', header = FALSE);
  bar = baremar_mitjans(prebarems);
  predreg = regressio_mitjans(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_petits_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize4_comp_intra <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  punts[punts == '-'] <- NA;
  punts <- add.errors.34(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #importem els barems
  
  #  prebarems=read.csv('prebarems4.csv', header = FALSE);
  bar = baremar_mitjans(prebarems);
  predreg = regressio_mitjans(prebarems);
  
  
  # demanem la funció de predir: 
  
  punts = predir_petits_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_petits(punts, prebarems);
  
  return(punts);
}

initialize5_comp_intra <- function(punts, prebarems){
  
  # netegem una mica les dades
  for(i in 2:ncol(punts)){
    punts[,i]=as.numeric(as.character(punts[,i]));
  }
  
  #  punts=read.csv('punts-new-sup.csv', header = FALSE);
  punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12,14)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R",'C');
  
  #importem els barems
  
  #  prebarems=read.csv('prebarems5.csv', header = FALSE);
  bar = baremar_grans(prebarems);
  predreg = regressio_grans(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_grans_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_grans(punts);
  
  # punts <- punts[,-9]
  # això era per treure la columna de vpt, però ara ja no l'afegeixo (crec))
  
  return(punts);
}

initialize6_comp_intra <- function(punts, prebarems){
  
  # netegem una mica les dades
  
  punts[punts == '-'] <- NA;
  punts <- add.errors.56(punts);
  punts <- data.frame(punts[c(1,2,4,6,8,10,12)]);
  colnames(punts) <- c("Noms", "L", "MT", "VP", "FM", "MLT", "R");
  
  #importem els barems
  
  #  prebarems=read.csv('prebarems6.csv', header = FALSE);
  bar = baremar_grans(prebarems);
  predreg = regressio_grans(prebarems);
  
  # demanem la funció de predir: 
  
  punts = predir_grans_intra(punts, bar, predreg);
  
  # colors dels grups
  
  punts = colorejar_grans(punts);
  
  return(punts);
}
