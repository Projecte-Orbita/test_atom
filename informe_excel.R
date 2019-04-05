Sys.setlocale(category="LC_ALL", locale = "Catalan")
source('manipulacions_dades.R', encoding = "UTF-8")
source('R2latex_general.R', encoding = "UTF-8")
  

informe_des_dexcel = function(path_excel, nom_escola, tipus = "classe"){
  source('R2latex_general.R', encoding = "UTF-8")
  source('manipulacions_dades.R', encoding = "UTF-8")
  
  unlink('figures/temp', recursive = T)
  unlink('dades/temp', recursive = T)
  unlink('informes/temp', recursive = T)
  
  pretractar_excels(path_excel, "temp")
  
  informe_general("temp", tipus, nom_escola)
  
}

informe_des_dexcel('dades/Correccions Tecnos 2018-19.xlsx', 'Escola Tecnos', 'classe')
# informe_des_dexcel('dades/Correccions Tecnos 2018-19.xlsx', 'Escola Tecnos', 'individual')

