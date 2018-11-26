# pdflatex /informes/Escola_Tecnos/informe_*.tex
# pdflatex /informes/Escola_Tecnos/informe_*.tex

cd ./informes/Pia_Sarrià/
for i in *.tex; do pdflatex $i;done
#for i in *.pdf; do open $i;