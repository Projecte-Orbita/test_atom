# Informes del Test Àtom
Aquest codi crea els informes automàtics del Test Àtom

Aquest repositori s'ha fet públic en concordança amb la política de **transparència** d'Òrbita. 

Aquesta pàgina conté tot el codi utilitzat a l'hora de fer els informes del test àtom, tan col·lectius (a nivell de classe) com els individuals (a nivell individual). Està pensat perquè escoles, gabinets de psicologia i particulars puguin conèixer tots els càlculs i **anàlisis estadístics** que duem a terme. 

En concret, els fitxers que implementen l'anàlisi estadístic són els anomenats "barems.R" i "compensacions.R". Els fitxers que implementen l'anàlisi de la dimensió emocional del test són els anomenats "emocional.R" i "emocional_petits.R". La resta de fitxers són complementaris i s'encarreguen de la redacció del text dels informes i de l'elaboració dels gràfics. 

Si trobeu errors en aquest codi o necessiteu més informació sobre els informes si us plau contacteu-nos a info\@projecteorbita.cat.

## Utilització

El fitxer principal és informe_excel, que crea els .tex dels informes directament des de l'excel de correccions. Té l'argument "path", que indica la ruta de l'excel, "nom_escola", que és el nom que s'imprimirà en els infromes i l'argument "individual" i "col·lectiu" per triar si volem els informes per nen o per classe.

## TODOs:

- Separar codi i text: s'ha de crear un diccionari amb les diferents parts del text lligades amb el que s'ha de dir per cada alumne
- Ordernar les funcions
- Automatitzar l'actualització dels barems