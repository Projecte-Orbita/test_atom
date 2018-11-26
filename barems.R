Sys.setlocale(category="LC_ALL", locale = "Catalan")

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


# funcions que preparem les matrius per calcular els barems i tal

preparar_barems <- function(prebarems){
  for (i in 1:ncol(prebarems)){
    prebarems[,i]=as.numeric(as.character(prebarems[,i]),options(warn=-1));
  }
  
  comp = complete.cases(prebarems);
  barems = prebarems[comp,];
  
  if (ncol(prebarems) == 22){
    
    barems[3] <- barems[3]+barems[5]+barems[7];
    barems[4] <- barems[4]+barems[6]+barems[8];
    barems[13] <- barems[13]+barems[15]+barems[17];
    barems[14] <- barems[14]+barems[16]+barems[18];
    
    barems=barems[,-c(5:8,15:18)];
  }
  else if (ncol(prebarems) == 16){
    
    barems[3] <- barems[3]+barems[5];
    barems[4] <- barems[4]+barems[6];
    barems[11] <- barems[11]+barems[13];
    barems[12] <- barems[12]+barems[14];
    
    barems=barems[,-c(5,6,13,14)];
  }
  
  
  
  clean_barems = barems;
  
  for (i in 1:(ncol(barems)/2)){
    clean_barems[i] <- barems[2*i-1]-coef[i]*barems[2*i]
  }
  
  
  if (ncol(barems) == 14){
    barems = clean_barems[-c(8:14)]
    colnames(barems) = c( "L", "MT", "VP", "FM", "MLT", "R", "C")
  }
  
  else {
    barems = clean_barems[-c(7:12)]
    colnames(barems) = c( "L", "MT", "VP", "FM", "MLT", "R")
  }
  
  barems$VPT = pmax(barems$VP, barems$FM/mean(barems$FM)*mean(barems$VP));
  return(barems)
}

baremar <- function(barems){
  #calculem els barems i els posem a la matriu bar1
  
  # petits i mitjans:
  if (ncol(barems) == 7){
    bar= matrix(nrow=7, ncol = 2);
    rownames(bar) <- c( "L", "MT", "VP", "FM", "MLT", "R","VPT");
    colnames(bar) <- c('mean', 'sd');
    
    for (i in 1:7){
      fit =fitdist(barems[,i],'norm')
      bar[i,] = c(fit$estimate[1],fit$estimate[2])
    }
    
    return(bar);
  }
  
  # grans:
  else {
    bar= matrix(nrow=8, ncol = 2);
    rownames(bar) <- c( "L", "MT", "VP", "FM", "MLT", "R", "C", "VPT");
    colnames(bar) <- c('mean', 'sd');
    
    #calculem els barems i els posem a la matriu bar:
    for (i in 1:8){
      fit =fitdist(barems[,i],'norm');
      bar[i,] = c(fit$estimate[1],fit$estimate[2]);
    }
    
    return(bar);
  }
}

regressions <- function(barems){
  
  predreg=list();
  vec=list();
  
  barems$MT <- NULL;
  barems$MLT <- NULL;
  
  for (i in 1:ncol(barems)){
    vec[[1]] = lm(barems[[i]]~VP, barems);
    vec[[2]] = lm(barems[[i]]~FM, barems);
    
    predreg[[i]] = vec;
    
  }
  
  return(predreg);
}

diferencies <- function(barems){
  
  bar = baremar(barems);
  predreg = regressions(barems);
  
  predit = predir(barems, bar, predreg);
  
  predit$VPT = NULL;
  
  predit$L <- pnorm(predit$L, bar[1,1],bar[1,2]);
  predit$MT <- pnorm(predit$MT, bar[2,1],bar[2,2]);
  predit$VP <- pnorm(predit$VP, bar[3,1],bar[3,2]);
  predit$FM <- pnorm(predit$FM, bar[4,1],bar[4,2]);
  predit$MLT <- pnorm(predit$MLT, bar[5,1],bar[5,2]);
  predit$R <- pnorm(predit$R, bar[6,1], bar[6,2]);
  if (nrow(bar) == 8){
    predit$C <-  pnorm(predit$C, bar[7,1], bar[7,2]);
  }
  
  difs=matrix(nrow= nrow(predit), ncol = nrow(bar)-1);
  
  for (i in 1:(nrow(bar)-1)){
    difs[,i]=predit[,i]-predit[,i+(nrow(bar)-1)]  
  }
  
  return(difs);
}

bar_diferencies <- function(barems){
  
  difs = diferencies(barems);
  bardif = matrix(0,nrow=ncol(difs), ncol=2);
  
  if (ncol(difs) == 6){
    cols = c(1, 3, 4, 6)
  }
  
  else{
    cols = c(1, 3, 4, 6, 7)
  }
  
  for (i in cols){
    fit =fitdist(difs[,i],'norm');
    bardif[i,] = c(fit$estimate[1],fit$estimate[2]);
  }
  
  return(bardif);
}


###### Funcions de baremació d'errors

# no s'està fent servir (crec)

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

