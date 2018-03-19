
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

inicialitzar <- function(punts, prebarems){
  
  punts <- afegir_errors(punts);
  
  grans = FALSE;
  
  if (ncol(prebarems) == 22){
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
  #prebarems=read.csv('proves_barems.csv', header = FALSE);
  
  bar = baremar(prebarems);
  
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

inicialitzar_intra <- function(punts, prebarems){
  
  punts <- afegir_errors(punts);
  
  grans = FALSE;
  
  if (ncol(prebarems) == 22){
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

########
# Funcions per treure els errors, els apèndix diuen a quins cursos pertanyen.
######

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
    punts[2*i] <- punts[2*i]-coef[i]*punts[2*i+1];
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

#####
# Funció que fa els informes
#####

informe <- function(puntso, curs, prebarems, escola){
  
  for(i in 2:ncol(puntso)){
    puntso[,i]=as.numeric(as.character(puntso[,i]));
  }
  
  # mirem si són els grans.
  
  num = 7;
  colnames_punts = c("L", "MT", "VP", "FM", "MLT", "R")
  melt_punts1 = c("Noms", "L", "MT", "VP", "FM", "MLT",  "R")
  melt_punts2 = c("Noms","lecg","mtg", "vpg","fmg","mltg", "rg")
  colnames_difs = c("Noms", "lec", "mt", "vp", "fm", "mlt", "r");
  
  if(curs[2] == 5 | curs[2] == 6){
    num = 8;
    colnames_punts = c(colnames_punts, "C")
    melt_punts1 = c(melt_punts1, "C" )
    melt_punts2 = c(melt_punts2, "cg")
    colnames_difs = c(colnames_difs, "c")
  }
  
  # gràfics normals globals
  
  punts <- inicialitzar(puntso, prebarems);
  
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:num));
  
  punts_exp = punts[,c(1:num)];
  
  colnorm = punts[,-c(2:num)];
  
  colnames(punts)[2:num] <- colnames_punts;
  punts1 <- melt(punts[melt_punts1], id.var = "Noms");
  punts2 <- melt(punts[melt_punts2], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  grafics_classe(punts, curs, 'norm', escola);
  grafics_nens(punts, curs[1], 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- inicialitzar_comp(puntso, prebarems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:num));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = num);
  difs=data.frame(difs);
  
  colnames(difs) <- colnames_difs;
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:num){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar(difs, prebarems);
  colpred = difs[,-c(2:num)];
  
  difs[,c(2:num)]=cpunts[,-1];
  
  colnames(difs)[2:num] <- colnames_punts;
  cpunts1 <- melt(difs[melt_punts1], id.var = "Noms");
  cpunts2 <- melt(difs[melt_punts2], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  grafics_nens(cpunts, curs[1], 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- inicialitzar_intra(puntso, prebarems);
  length <- length(ipunts[,1]);
  ipunts <- subset(ipunts, select = -c(2:num));
  
  punts_expi = ipunts[,c(1:num)];
  
  colnames(ipunts)[2:num] <- colnames_punts
  ipunts1 <- melt(ipunts[melt_punts1], id.var = "Noms");
  ipunts2 <- melt(ipunts[melt_punts2], id.var = "Noms");
  ipunts <- data.frame(ipunts1, ipunts2[c("variable", "value")]);
  grafics_classe(ipunts, curs,'norm_intra', escola);
  
  return(matriu(colnorm, colpred));
}

