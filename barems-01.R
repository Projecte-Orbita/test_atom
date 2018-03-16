
require(fitdistrplus)

#valors dels coeficients dels errors:


alpha=0.25;
beta=0.5;
gamma=0.2;
delta=0.2;
eta=0.2;
phi=0.33;
chi=0.33;
coef=c(alpha, beta, gamma, delta, eta, phi, chi);

# pel primer cicle barems es un csv amb 12 columnes, dues per cada prova (correctes 
# i incorrectes) i sense headings. 

#funció que neteja els barems i agafa només els casos complets
clean.barems <- function(prebarems){
  for (i in 1:ncol(prebarems)){
    prebarems[,i]=as.numeric(as.character(prebarems[,i]),options(warn=-1));
  }
  
  comp=complete.cases(prebarems);
  barems=prebarems[comp,];
  return(barems);
}

# funcions que preparem les matrius per calcular els barems i tal

prep_petits <- function(prebarems){
  
  barems=clean.barems(prebarems);
  
  #traiem els errors: 
  
  for (i in 1:6){
    barems[i] <- barems[2*i-1]-coef[i]*barems[2*i]
  }
  
  #agafem només les columnes bones
  barems=barems[,c(1:6)];
  
  colnames(barems)=c( "L", "MT", "VP", "FM", "MLT", "R");
  
  # afegim la velocitat de processament total
  
  barems$VPT=pmax(barems$VP, barems$FM/mean(barems$FM)*mean(barems$VP));
  
  return(barems);
}

prep_mitjans <- function(prebarems){
  
  barems=clean.barems(prebarems);
  
  #ajuntem les memòries de treball i llarg termini
  
  barems[3] <- barems[3]+barems[5];
  barems[4] <- barems[4]+barems[6];
  barems[11] <- barems[11]+barems[13];
  barems[12] <- barems[12]+barems[14];
  
  barems=barems[,-c(5,6,13,14)];
  
  #traiem els errors: 
  for (i in 1:6){
    barems[i] <- barems[2*i-1]-coef[i]*barems[2*i]
  }
  
  #agafem només les columnes bones
  barems=barems[,c(1:6)];
  
  colnames(barems)=c( "L", "MT", "VP", "FM", "MLT", "R");
  
  
  barems$VPT <- pmax(barems$VP, barems$FM/mean(barems$FM)*mean(barems$VP));
  
  return(barems);
}

prep_grans <- function(prebarems){
  barems=clean.barems(prebarems);
  
  #ajuntem les memòries de treball i llarg termini
  
  barems[3] <- barems[3]+barems[5]+barems[7];
  barems[4] <- barems[4]+barems[6]+barems[8];
  barems[13] <- barems[13]+barems[15]+barems[17];
  barems[14] <- barems[14]+barems[16]+barems[18];
  
  barems=barems[,-c(5:8,15:18)];
  
  #traiem els errors: 
  for (i in 1:7){
    barems[i] <- barems[2*i-1]-coef[i]*barems[2*i]
  }
  
  #agafem només les columnes bones
  barems=barems[,c(1:7)];
  
  colnames(barems)=c( "L", "MT", "VP", "FM", "MLT", "R","C");
  
  barems$VPT <- pmax(barems$VP, barems$FM/mean(barems$FM)*mean(barems$VP));
  
  return(barems);
}

##### PETITS #####

# funció de càlcul de barems i regressió per cicle inicial

baremar_petits<- function(prebarems){
  #calculem els barems i els posem a la matriu bar1:
  
  barems = prep_petits(prebarems);
  bar= matrix(nrow=7, ncol = 2);
  rownames(bar) <- c( "L", "MT", "VP", "FM", "MLT", "R","VPT");
  colnames(bar) <- c('mean', 'sd');
  
  for (i in 1:7){
    fit =fitdist(barems[,i],'norm')
    bar[i,] = c(fit$estimate[1],fit$estimate[2])
  }
  
  return(bar);
}

regressio_petits <- function(prebarems){
  
  barems = prep_petits(prebarems);
  predreg=list();
  vec=list();
  
  barems$MT <- NULL;
  barems$MLT <- NULL;
  
  for (i in 1:4){
    vec[[1]] = lm(barems[[i]]~VP, barems);
    vec[[2]] = lm(barems[[i]]~FM, barems);
    
    predreg[[i]] = vec;
    
  }
  
  return(predreg);
}

# funcions de càlcul de les diferències pre i post anàlisi estadístic petits

diferencia_petits <- function(prebarems){
  barems = prep_petits(prebarems);
  #  barems$MT <- NULL;
  #  barems$MLT <- NULL;
  
  bar=baremar_petits(prebarems);
  predreg = regressio_petits(prebarems);
  
  predit = predir_petits(barems, bar, predreg);
  
  predit$VPT = NULL;
  
  predit$L <- pnorm(predit$L, bar[1,1],bar[1,2]);
  predit$MT <- pnorm(predit$MT, bar[2,1],bar[2,2]);
  predit$VP <- pnorm(predit$VP, bar[3,1],bar[3,2]);
  predit$FM <- pnorm(predit$FM, bar[4,1],bar[4,2]);
  predit$MLT <- pnorm(predit$MLT, bar[5,1],bar[5,2]);
  predit$R <- pnorm(predit$R, bar[6,1], bar[6,2]);

  
  difs=matrix(nrow= nrow(predit), ncol = 6);
  
  for (i in 1:6){
    difs[,i]=predit[,i]-predit[,i+6]  
  }
  
  return(difs);
  
}

bardif_petits <- function(prebarems){
  
  difs = diferencia_petits(prebarems);
  
  bardif = matrix(0,nrow=6, ncol=2);
  
  for (i in c(1,3,4,6)){
    fit =fitdist(difs[,i],'norm');
    bardif[i,] = c(fit$estimate[1],fit$estimate[2]);
  }
  
  return(bardif);
  
}

##### MITJANS #####

# funció de càlcul de barems i regressió per cicle inicial

baremar_mitjans<- function(prebarems){
  #calculem els barems i els posem a la matriu bar1:
  
  barems = prep_mitjans(prebarems);
  bar= matrix(nrow=7, ncol = 2);
  rownames(bar) <- c( "L", "MT", "VP", "FM", "MLT", "R","VPT");
  colnames(bar) <- c('mean', 'sd');
  
  for (i in 1:7){
    fit =fitdist(barems[,i],'norm')
    bar[i,] = c(fit$estimate[1],fit$estimate[2])
  }
  
  return(bar);
}

regressio_mitjans <- function(prebarems){
  
  barems = prep_mitjans(prebarems);
  predreg=list();
  vec=list();
  
  barems$MT <- NULL;
  barems$MLT <- NULL;
  
  for (i in 1:4){
    vec[[1]] = lm(barems[[i]]~VP, barems);
    vec[[2]] = lm(barems[[i]]~FM, barems);
    
    predreg[[i]] = vec;
    
  }
  return(predreg);
}

# funcions de càlcul de les diferències pre i post anàlisi estadístic mitjans

diferencia_mitjans <- function(prebarems){
  barems = prep_mitjans(prebarems);
  #  barems$MT <- NULL;
  #  barems$MLT <- NULL;
  
  bar=baremar_mitjans(prebarems);
  predreg = regressio_mitjans(prebarems);
  
  predit = predir_petits(barems, bar, predreg);
  
  predit$VPT = NULL;
  
  predit$L <- pnorm(predit$L, bar[1,1],bar[1,2]);
  predit$MT <- pnorm(predit$MT, bar[2,1],bar[2,2]);
  predit$VP <- pnorm(predit$VP, bar[3,1],bar[3,2]);
  predit$FM <- pnorm(predit$FM, bar[4,1],bar[4,2]);
  predit$MLT <- pnorm(predit$MLT, bar[5,1],bar[5,2]);
  predit$R <- pnorm(predit$R, bar[6,1], bar[6,2]);

  
  difs=matrix(nrow= nrow(predit), ncol = 6);
  
  for (i in 1:6){
    difs[,i]=predit[,i]-predit[,i+6]  
  }
  
  return(difs);
  
}

bardif_mitjans <- function(prebarems){
  
  difs = diferencia_mitjans(prebarems);
  
  bardif = matrix(0,nrow=6, ncol=2);
  
  for (i in c(1,3,4,6)){
    fit =fitdist(difs[,i],'norm');
    bardif[i,] = c(fit$estimate[1],fit$estimate[2]);
  }
  
  return(bardif);
  
}

##### GRANS #####

baremar_grans <- function(prebarems){
  barems=prep_grans(prebarems);
  predreg=list();
  
  bar= matrix(nrow=8, ncol = 2);
  rownames(bar) <- c( "L", "MT", "VP", "FM", "MLT", "R","C","VPT");
  colnames(bar) <- c('mean', 'sd');
  
  #calculem els barems i els posem a la matriu bar:
  for (i in 1:8){
    fit =fitdist(barems[,i],'norm');
    bar[i,] = c(fit$estimate[1],fit$estimate[2]);
  }
  
  return(bar);
  
}

regressio_grans <- function(prebarems){
  
  barems = prep_grans(prebarems);
  predreg=list();
  vec=list();

  
  #bar5=baremar5(prebarems);
  
  barems$MT <- NULL;
  barems$MLT <- NULL;

 # lecpred_fm = lm(barems[[1]]~FM, barems);
 # lecpred_vp = lm(barems[[1]]~VP, barems);
  
  for (i in 1:5){
    vec[[1]] = lm(barems[[i]]~VP, barems);
    vec[[2]] = lm(barems[[i]]~FM, barems);
    predreg[[i]] = vec;
  }
  
  return(predreg);
}

# funcions de càlcul de les diferències pre i post anàlisi estadístic

diferencia_grans <- function(prebarems){
  barems = prep_grans(prebarems);
#  barems$MT <- NULL;
#  barems$MLT <- NULL;
  
  bar=baremar_grans(prebarems);
  predreg = regressio_grans(prebarems);
  
  predit = predir_grans(barems, bar, predreg);
  
  predit$VPT = NULL;
  
  predit$L <- pnorm(predit$L, bar[1,1],bar[1,2]);
  predit$MT <- pnorm(predit$MT, bar[2,1],bar[2,2]);
  predit$VP <- pnorm(predit$VP, bar[3,1],bar[3,2]);
  predit$FM <- pnorm(predit$FM, bar[4,1],bar[4,2]);
  predit$MLT <- pnorm(predit$MLT, bar[5,1],bar[5,2]);
  predit$R <- pnorm(predit$R, bar[6,1], bar[6,2]);
  predit$C <-  pnorm(predit$C, bar[7,1], bar[7,2]);
  
  difs=matrix(nrow= nrow(predit), ncol = 7);
  
  for (i in 1:7){
  difs[,i]=predit[,i]-predit[,i+7]  
  }
  
  return(difs);
  
}

bardif_grans <- function(prebarems){
  difs = diferencia_grans(prebarems);
  
  bardif = matrix(0,nrow=7, ncol=2);
  
  for (i in c(1,3,4,6,7)){
    fit =fitdist(difs[,i],'norm');
    bardif[i,] = c(fit$estimate[1],fit$estimate[2]);
  }
  
  return(bardif);
  
}

###### Funcions de baremació d'errors


bar_errors <- function(prebarems){
  prebarems[prebarems == '-'] <- NA;
  
  encerts = c();
  errors = c();
  indeximp = c();
  
  colencerts = seq(2,ncol(prebarems),2);
  colerrors = seq(3,ncol(prebarems),2);
  
  for (i in 1:nrow(prebarems)){
    encerts[i] = sum(as.numeric(prebarems[i,colencerts]));
    errors[i] = sum(as.numeric(prebarems[i, colerrors]));
    indeximp[i] = errors[i]/(errors[i]+encerts[i]);
  }
  
  fitindeximp =fitdist(indeximp,'norm')
  
  indextot = c(fitindeximp$estimate[1], fitindeximp$estimate[2]);
  return(indextot);
}


###### Regressió lineal de predicció de la velocitat de processament.

### per ara no la fem servir

regressio <- function(barems, curs){
  
  # calculem la variable velocitat de processament total:
  if (curs<5){
  colnames(barems) <- c("L", "MT", "VP", "FM", "MLT", "R")
  }
  else {
    
    colnames(barems) <- c("L", "MT", "VP", "FM", "MLT", "R","C")
  }
    barems$VPT <- pmax(barems$VP, barems$FM/mean(barems$FM)*mean(barems$VP));
    
    # calculem la regressió lineal:
    vpt_pred = lm(data=barems, VPT~., family=gaussian);
    return(vpt_pred);
    
}

