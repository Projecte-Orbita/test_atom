Sys.setlocale(category="LC_ALL", locale = "Catalan")


# petits:

informe_emocional_petits <- function(index, emocional){  
  
  futur=FALSE;
  
  ambit = c("", "\\textbf{personal}", "\\textbf{escolar}", "\\textbf{social}", "\\textbf{familiar}", "\\textbf{familiar}");
  frase = c("", "normalment està", "a classe està", "al pati està", "a casa seva està", "amb la seva família està")
  
  for (i in 2:(ncol(emocional))){
    par = 0;
    if (!is.na(emocional[index,i]) && emocional[index,i]==1){
      cat("En/na ", as.character(emocional[index,1]), " mostra un \\emph{risc greu} de desadaptació en l'\\textbf{àmbit} ", ambit[i], " ja que ha marcat que ", frase[i], " molt trist.
          ", sep="")
      par=par+1;
      futur=TRUE;
    }
  }
  
  for (i in 2:(ncol(emocional))){
    if (par > 1){
      if (!is.na(emocional[index,i]) && emocional[index,i]==2){
        cat("A més, també mostra un \\emph{risc moderat} de desadaptació en l'\\textbf{àmbit} ", ambit[i], " ja que ha marcat que ", frase[i], " trist.", sep="");
        futur=TRUE;
      }
    }
    else {if (!is.na(emocional[index,i]) && emocional[index,i]==2){
      cat("En/na ", as.character(emocional[index,1]), " mostra un \\emph{risc moderat} de desadaptació en l'\\textbf{àmbit} ", ambit[i], " ja que ha marcat que ", frase[i], " trist.", sep="");
      futur=TRUE;
    }
    }
  }
  if(futur == FALSE){cat("No es constaten factors de risc de desadaptació emocional.")};
  return(futur);
}








# an\`{a}lisi emocional cicles mitj\`{a} i superior

# assumim que estem rebent una matriu de N_nensx17 amb totes les respostes d'emocional

# emocional = read.csv('emocional.csv', headers=FALSE)

# creem una matriu per cada nen, amb les coses que diu greus i lleus i en quin 
# \`{a}mbit ho ha dit: 

# definim les coses greus i lleus

# per cada nen: 

#emocional <- read.csv('emocional.csv', header=FALSE, fileEncoding = "UTF-8");

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
      
      if (!is.na(ambits[[(i-1)/4+1]][[3]][[j]][1]) && !is.na(emocional_nou[i+j]) && emocional_nou[i+j] == ambits[[(i-1)/4+1]][[3]][[j]][1]){greu <- c(greu, list(paraula(ambits[[(i-1)/4+1]][[3]][[j]][1]), j));}
      
      else if (!is.na(ambits[[(i-1)/4+1]][[3]][[j]][2]) && !is.na(emocional_nou[i+j]) && emocional_nou[i+j]== ambits[[(i-1)/4+1]][[3]][[j]][2]){lleu <- c(lleu, list(paraula(ambits[[(i-1)/4+1]][[3]][[j]][2]), j));}}
    
    
    
    if(length(greu) != 0 | length(lleu) != 0)
    {cat("
         
         \\textbf{\\`{A}mbit} ", ambits[[(i-1)/4+1]][[1]], ": 
         \\begin{itemize}
         ", sep = "");
      afectat <- afectat + 1}
    
    if(length(greu) != 0){cat("\\item Factors de \\emph{risc greu}: El/la ", as.character(emocional_nou[1]), " ha marcat que ", sep = "");               
      if(length(greu) != 2){sapply(1:(floor(length(greu)/2)-1), function (x) cat(greu[[2*x-1]], ambits[[(i-1)/4+1]][[2]][as.numeric(greu[[2*x]])], ", ", sep = ""));}
      
      cat(if(length(greu) != 2)"i ", greu[[as.numeric(length(greu)-1)]], ambits[[(i-1)/4+1]][[2]][as.numeric(greu[[length(greu)]])], ". ", sep = ""); futur = TRUE;
    }
    
    
    if(length(lleu) != 0){cat("\\item Factors de \\emph{risc lleu}: El/la ", as.character(emocional_nou[1]), " ha marcat que ", sep = "");               
      if(length(lleu) != 2){sapply(1:(floor(length(lleu)/2)-1), function (x) cat(lleu[[2*x-1]], ambits[[(i-1)/4+1]][[2]][as.numeric(lleu[[2*x]])], ", ", sep = ""));}
      
      cat(if(length(lleu) != 2)"i ", lleu[[as.numeric(length(lleu)-1)]], ambits[[(i-1)/4+1]][[2]][as.numeric(lleu[[length(lleu)]])], ". ", sep = ""); lleus <- lleus + 1;
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
