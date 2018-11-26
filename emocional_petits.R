Sys.setlocale(category="LC_ALL", locale = "Catalan")


# an\`{a}lisi emocional cicles mitj\`{a} i superior

# assumim que estem rebent una matriu de N_nensx17 amb totes les respostes d'emocional

# emocional = read.csv('emocional.csv', headers=FALSE)

# creem una matriu per cada nen, amb les coses que diu greus i lleus i en quin 
# \`{a}mbit ho ha dit: 

# definim les coses greus i lleus

# per cada nen: 

#emocional <- read.csv('emocional.csv', header=FALSE, fileEncoding = "UTF-8");

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
 
