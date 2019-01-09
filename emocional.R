Sys.setlocale(category="LC_ALL", locale = "Catalan")


# petits:

informe_emocional_petits <- function(index, emocional){  
  
  futur=FALSE;
  
  ambit = c("", "\\textbf{personal}", "\\textbf{escolar}", "\\textbf{social}", "\\textbf{familiar}", "\\textbf{familiar}");
  frase = c("", "normalment està", "a classe està", "al pati està", "a casa seva està", "amb la seva família està")
  
  for (i in 2:(ncol(emocional))){
    par = 0;
    if (!is.na(emocional[index,i]) && emocional[index,i]==1){
      cat("En/na ", as.character(emocional[index,1]), " mostra un \\emph{risc greu} de desadaptació en l'\\textbf{àmbit} ", ambit[i], " ja que ha indicat que ", frase[i], " molt trist.
          ", sep="")
      par=par+1;
      futur=TRUE;
    }
  }
  
  for (i in 2:(ncol(emocional))){
    if (par > 1){
      if (!is.na(emocional[index,i]) && emocional[index,i]==2){
        cat("A més, també mostra un \\emph{risc moderat} de desadaptació en l'\\textbf{àmbit} ", ambit[i], " ja que ha indicat que ", frase[i], " trist.", sep="");
        futur=TRUE;
      }
    }
    else {if (!is.na(emocional[index,i]) && emocional[index,i]==2){
      cat("En/na ", as.character(emocional[index,1]), " mostra un \\emph{risc moderat} de desadaptació en l'\\textbf{àmbit} ", ambit[i], " ja que ha indicat que ", frase[i], " trist.", sep="");
      futur=TRUE;
    }
    }
  }
  if(futur == FALSE){cat("No es constaten factors de risc de desadaptació emocional.")};
  return(futur);
}


informe_emocional <- function(index, emocional){  
  #
  futur=FALSE;
  lleus=0;
  
  emocional_nou <- as.character(emocional[index,1]);
  cc=c(1:5,5,6,6,7:15,15,16,16);
  
  for (i in cc){
    emocional_nou = c(emocional_nou, emocional[index,i+1])
  }
  
  
  personal <- list("\\textbf{personal}", c("li agrada com és", "està trist/a", "sap fer moltes coses", "s'enfada i es baralla"), list(c(1,2), c(4,3), c(NA,1), c(4,3)));
  
  escolar <- list("\\textbf{escolar}", c("li agrada anar a l'escola", "li agrada anar a l'escola", "s'avorreix a classe", "s'avorreix a classe", "treu bones notes", "estudia i s'esforça"), list(c(NA,1), c(NA,2), c(NA,4), c(NA,3), c(NA,1), c(NA,1)));
  
  social <- list("\\textbf{social}", c("li cauen bé els seus companys", "els altres el molesten", "li agrada jugar amb els altres", "li costa fer amics"), list(c(1,2), c(4,3), c(1,2), c(4,3)));
  
  familiar <- list("\\textbf{familiar}", c("està a gust a casa", "hi ha crits i discussions a casa", "està d'acord amb les normes de casa", "està d'acord amb les normes de casa", "estima a la seva família", "estima a la seva família"), list(c(1,2), c(4,3), c(NA,2), c(NA,1), c(1,5), c(2,NA)));
  
  #c(12,NA) c(12,3)
  
  afectat <- 0
  
  ambits <- list(personal, escolar, social, familiar);
  
  for(i in c(1,5,11,15)){
    
    greu <- NULL;
    lleu <- NULL;
    nb_pregs <- NULL;
    
    if(i == 1 | i == 11){nb_pregs <- seq(1,4,1)} else {nb_pregs <- seq(1,6,1)};
    
    for(j in nb_pregs){  
      
      if (!is.na(ambits[[(i-1)/4+1]][[3]][[j]][1]) && !is.na(emocional_nou[i+j]) 
          && emocional_nou[i+j] == ambits[[(i-1)/4+1]][[3]][[j]][1])
        {greu <- c(greu, list(paraula(ambits[[(i-1)/4+1]][[3]][[j]][1]), j));}
      
      else if (!is.na(ambits[[(i-1)/4+1]][[3]][[j]][2]) && !is.na(emocional_nou[i+j]) 
               && emocional_nou[i+j]== ambits[[(i-1)/4+1]][[3]][[j]][2])
        {lleu <- c(lleu, list(paraula(ambits[[(i-1)/4+1]][[3]][[j]][2]), j));}}
    
    if(length(greu) != 0 | length(lleu) != 0)
    {cat("
         
         \\textbf{\\`{A}mbit} ", ambits[[(i-1)/4+1]][[1]], ": 
         \\begin{itemize}
         ", sep = "");
      afectat <- afectat + 1}
    
    if(length(greu) != 0){cat("\\item Factors de \\emph{risc greu}: El/la ", 
                              as.character(emocional_nou[1]), " ha indicat que ", sep = "");               
      if(length(greu) != 2){sapply(1:(floor(length(greu)/2)-1), 
                                   function (x) cat(greu[[2*x-1]], 
                          ambits[[(i-1)/4+1]][[2]][as.numeric(greu[[2*x]])], ", ", sep = ""));}
      
      cat(if(length(greu) != 2)"i ", greu[[as.numeric(length(greu)-1)]], 
          ambits[[(i-1)/4+1]][[2]][as.numeric(greu[[length(greu)]])], ". ", sep = ""); 
      futur = TRUE;
    }
    
    
    if(length(lleu) != 0){cat("\\item Factors de \\emph{risc lleu}: El/la ", 
                              as.character(emocional_nou[1]), " ha indicat que ", sep = "");               
      if(length(lleu) != 2){sapply(1:(floor(length(lleu)/2)-1), 
                                   function (x) cat(lleu[[2*x-1]], 
                          ambits[[(i-1)/4+1]][[2]][as.numeric(lleu[[2*x]])], ", ", sep = ""));}
      
      cat(if(length(lleu) != 2)"i ", lleu[[as.numeric(length(lleu)-1)]], 
          ambits[[(i-1)/4+1]][[2]][as.numeric(lleu[[length(lleu)]])], ". ", sep = ""); 
      lleus <- lleus + 1;
    }
    
    if(length(greu) != 0 | length(lleu) != 0)
      cat("
          \\end{itemize}");
  }
  
  if(afectat == 0){cat("No es constaten factors de risc de desadaptació emocional.")}
  
  if(lleus>2){futur=TRUE};
  return(futur);
    }

paraula <- function(num){
  if(num == 1){return("mai ")};
  if(num == 2){return("poques vegades ")};
  if(num == 3){return("moltes vegades ")};
  if(num == 4){return("sempre ")}
  if(num == 5){return("moltes vegades (i no sempre) ")}}

creacio_grafics_emocional = function(punts, curs, escola){
  for(i in 1:length(punts[,1])){
    
    nom = as.character(punts[i,1])
    
    if(curs[2] == 1 | curs[2] == 2){
      dades = unname(unlist(punts[i, c(14:18)]))
      dades[is.na(dades)]=0
      valors = c(max(0,3-dades[1]), 
                 max(0,3-dades[2]),
                 max(0,3-dades[3]), 
                 ifelse(dades[4] == 1 | dades[5] == 1, 2, 
                        ifelse(dades[4] == 2 | dades[5] == 2, 1, 0))
      )
    }
    
    else if(curs[2] == 3 | curs[2] == 4){
      dades = unname(unlist(punts[i, c(18:33)]))
      dades[is.na(dades)]=0
      
      pre_valors = c(max(3-dades[1],0),        # m'agrada com sóc
                     max(dades[2]-2,0),        # estic trist
                     max(2-dades[3],0),        # crec que sé fer moltes coses
                     max(dades[4]-2,0),        # m'enfado i em barallo
                     min(max(3-dades[5],0),1), # m'agrada anar a l'escola
                     min(max(dades[6]-2,0),1), # m'avorreixo a classe
                     max(2-dades[7],0),        # trec bones notes
                     max(2-dades[8],0),        # estudio i m'esforço
                     max(3-dades[9],0),        # em cauen bé els meus companys
                     max(dades[10]-2,0),       # els altres em molesten
                     max(3-dades[11],0),       # m'agrada jugar amb els altres
                     max(dades[12]-2,0),       # em costa fer amics
                     max(3-dades[13],0),       # estic a gust a casa
                     max(dades[14]-2,0),       # hi ha crits i discussions a casa
                     min(max(3-dades[15],0),1), # estic d'acord amb les normes de casa
                     min(max(4-dades[16],0),2)  # estimo els meus pares
      )
      valors = c()
      
      for (j in 1:4){
        valors[j] = sum(pre_valors[((j-1)*4 + 1):(j*4)])
      }
      
    }
    else if(curs[2] == 5 | curs[2] == 6){
      dades = unname(unlist(punts[i, c(24:39)]))
      dades[is.na(dades)]=0
      pre_valors = c(if(dades[1]==1) 3 else {if(dades[1]==2) 1 else 0},        # m'agrada com sóc
                     if(dades[2]==4) 3 else {if(dades[2]==3) 1 else 0},        # estic trist
                     if(dades[3]==1) 1 else 0,        # crec que sé fer moltes coses
                     if(dades[4]==4) 3 else {if(dades[4]==3) 1 else 0},        # m'enfado i em barallo
                     if(dades[5]==1) 1 else {if(dades[5]==2) 1 else 0}, # m'agrada anar a l'escola
                     if(dades[6]==3) 1 else {if(dades[6]==4) 1 else 0}, # m'avorreixo a classe
                     if(dades[7]==1) 1 else 0,        # trec bones notes
                     if(dades[8]==1) 1 else 0,        # estudio i m'esforço
                     if(dades[9]==1) 3 else {if(dades[9]==2) 1 else 0},        # em cauen bé els meus companys
                     if(dades[10]==4) 3 else {if(dades[10]==3) 1 else 0},       # els altres em molesten
                     if(dades[11]==1) 3 else {if(dades[11]==2) 1 else 0},       # m'agrada jugar amb els altres
                     if(dades[12]==4) 3 else {if(dades[12]==3) 1 else 0},       # em costa fer amics
                     if(dades[13]==1) 3 else {if(dades[13]==2) 1 else 0},       # estic a gust a casa
                     if(dades[14]==4) 3 else {if(dades[14]==3) 1 else 0},       # hi ha crits i discussions a casa
                     if(dades[15]==1) 1 else {if(dades[15]==2) 1 else 0}, # estic d'acord amb les normes de casa
                     if(dades[16]==1 | dades[16]==2) 3 else {if(dades[16]==3) 1 else 0}  # estimo els meus pares
      )
      valors = c()
      
      for (j in 1:4){
        valors[j] = sum(pre_valors[((j-1)*4 + 1):(j*4)])
      }
    }
    
    arees = c("Personal", "Escolar", "Social", "Familiar")
    df_emocional = as.data.frame(cbind(arees, valors))
    
    valors_nets = valors
    valors_nets[is.na(valors)]=0 # Aquesta línia i l'anterior són per tractar missings
    # TODO: arreglar-ho més amunt i millor
    if (sum(valors_nets)==0) next
    
    grafic_emocional(i, curs, df_emocional, escola, nom)
    
  }
}