Sys.setlocale(category="LC_ALL", locale = "Catalan")

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

#####
# Funció que fa els informes
#####

informe <- function(puntso, curs, barems, escola){
  
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
  
  punts <- inicialitzar(puntso, barems);
  
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:num));
  
  punts_exp = punts[,c(1:num)];
  colnorm = punts[,-c(2:num)];
  
  # Aquí hem de modificar els noms perquè quedin bé en els gràfics globals:
  # TODO: fer-ho en algun altre lloc...
  punts$Noms = formatejar_noms(punts$Noms)
  
  
  colnames(punts)[2:num] <- colnames_punts;
  punts1 <- melt(punts[melt_punts1], id.var = "Noms");
  punts2 <- melt(punts[melt_punts2], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
  grafics_classe(punts, curs, 'norm', escola);
  grafics_nens(punts, curs[1], 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- inicialitzar_comp(puntso, barems);
  
  cpunts$Noms = formatejar_noms(cpunts$Noms)
  
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:num));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = num);
  difs=data.frame(difs);
  
  colnames(difs) <- colnames_difs;
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:num){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar(difs, barems);
  colpred = difs[,-c(2:num)];
  
  cpunts$Noms = formatejar_noms(cpunts$Noms)
  
  difs[,c(2:num)]=cpunts[,-1];
  
  colnames(difs)[2:num] <- colnames_punts;
  cpunts1 <- melt(difs[melt_punts1], id.var = "Noms");
  cpunts2 <- melt(difs[melt_punts2], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  grafics_nens(cpunts, curs[1], 'comp', escola);
  
  #gràfics normals intraclasse:
  
  ipunts <- inicialitzar_intra(puntso);
  
  ipunts$Noms = formatejar_noms(ipunts$Noms)
  
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

informe_individual_intern <- function(puntso, curs, barems, escola){
  
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
  
  punts <- inicialitzar(puntso, barems);
  
  length <- length(punts[,1]);
  punts <- subset(punts, select = -c(2:num));
  
  punts_exp = punts[,c(1:num)];
  
  colnorm = punts[,-c(2:num)];
  
  colnames(punts)[2:num] <- colnames_punts;
  punts1 <- melt(punts[melt_punts1], id.var = "Noms");
  punts2 <- melt(punts[melt_punts2], id.var = "Noms");
  punts <- data.frame(punts1, punts2[c("variable", "value")]);
#  grafics_classe(punts, curs, 'norm', escola);
  grafics_nens_individual(punts, curs[1], 'norm', escola);
  
  # gràfics compensats globals
  
  cpunts <- inicialitzar_comp(puntso, barems);
  length <- length(cpunts[,1]);
  cpunts <- subset(cpunts, select = -c(2:num));
  
  difs=matrix(nrow= nrow(punts_exp), ncol = num);
  difs=data.frame(difs);
  
  colnames(difs) <- colnames_difs;
  
  difs$Noms <- punts_exp$Noms;
  
  for (i in 2:num){
    difs[,i]=punts_exp[,i]-cpunts[,i];  
  }
  
  difs = colorejar(difs, barems);
  colpred = difs[,-c(2:num)];
  
  difs[,c(2:num)]=cpunts[,-1];
  
  colnames(difs)[2:num] <- colnames_punts;
  cpunts1 <- melt(difs[melt_punts1], id.var = "Noms");
  cpunts2 <- melt(difs[melt_punts2], id.var = "Noms");
  cpunts <- data.frame(cpunts1, cpunts2[c("variable", "value")]);
  grafics_nens_individual(cpunts, curs[1], 'comp', escola);
  
  return(matriu(colnorm, colpred));
}

