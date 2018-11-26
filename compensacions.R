Sys.setlocale(category="LC_ALL", locale = "Catalan")


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

# matrius que treuen els resultats dels colors:

matriu <- function(colnorm, colpred){
  # colnorm i colexp es troben posant la línia   colnorm = punts[,-c(2:7)]; 
  # just després de la punts_exp = punts[,c(1:7)]; i la línia
  # colpred = difs[,-c(2:7)]; després de difs = colorejar(difs);
  
  tot_nens = cbind(colnorm,colpred[,-c(1)]);
  tot_nens[is.na(tot_nens)==TRUE]=0;
  
  # inicialitzem la matriu: 
  
  matlist <- vector("list", length(rownames(tot_nens)));
  names(matlist) = tot_nens[,1];
  
  mat = matrix(0, nrow=(ncol(colnorm)-1), ncol=(ncol(colnorm)-1));
  
  if (ncol(mat)==6){
    colnames(mat) = c("L", "MT", "VP", "FM", "MLT", "R");
    rownames(mat) = c("L", "MT", "VP", "FM", "MLT", "R");
    num = 7;
  }
  else {
    colnames(mat) = c("L", "MT", "VP", "FM", "MLT", "R", "C");
    rownames(mat) = c("L", "MT", "VP", "FM", "MLT", "R", "C");
    num = 8;
  }
  
  # i la calculem
  
  for (i in 1:nrow(tot_nens)){
    for (j in 1:(num-1)){
      if (tot_nens[i,j+1]=='C'){
        mat[j,j]=1;
        if(tot_nens[i,j+num]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+num]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }
      } else if (tot_nens[i,j+1]=='B'){
        mat[j,j]=2;
        if(tot_nens[i,j+num]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+num]=='M'){
          mat[j,j]=mat[j,j]+0.1;
        } else {
          mat[j,j]=mat[j,j]+0.2;
        }
      } else if (tot_nens[i,j+1]=='A'){
        mat[j,j]=3;
        if(tot_nens[i,j+7]=='L'){
          mat[j,j]=mat[j,j]+0;
        } else if (tot_nens[i,j+num]=='M'){
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


colorejar <- function(punts, barems){
  bardif = bar_diferencies(barems);
  
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


