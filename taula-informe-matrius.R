Sys.setlocale(category="LC_ALL", locale = "Catalan")


taula_prova <- function(matriu, proves){

cat("\\begin{center}
\\begin{tabular}{|l|c|c|}
\\hline
Habilitat & Barem universal & Comparativa amb resultat esperat \\\\
 \\hline
 \\hline
 ")

for (j in 1:nrow(matriu)){
cat("\\textbf{", proves[j], "} & ");   
        if (round(matriu[j,j])==0){cat("no l'ha fet & no l'ha fet \\\\")}
        if (round(matriu[j,j]) == 1){cat("\\textbf{valors baixos} & ")};

        if (matriu[j,j] == 1.0){cat("\\textbf{valors baixos} \\\\ ")};
        if (matriu[j,j] == 1.1){cat("valors esperats \\\\")};
        if (matriu[j,j] == 1.2){cat("\\textbf{valors alts} \\\\ ")};

     if (round(matriu[j,j]) == 2){cat("valors esperats & ")};

        if (matriu[j,j] == 2.0){cat("\\textbf{valors baixos} \\\\ ")};
        if (matriu[j,j] == 2.1){cat("valors esperats \\\\")};
        if (matriu[j,j] == 2.2){cat("\\textbf{valors alts} \\\\ ")};


     if (round(matriu[j,j]) == 3){cat("\\textbf{valors alts} & ")};

        if (matriu[j,j] == 3.0){cat("\\textbf{valors baixos} \\\\ ")};
        if (matriu[j,j] == 3.1){cat("valors esperats \\\\")};
        if (matriu[j,j] == 3.2){cat("\\textbf{valors alts} \\\\ ")};
cat(" \\hline
");}
cat("\\end{tabular}
\\end{center}
");   
}
