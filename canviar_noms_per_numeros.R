# Funció que canvia els noms dels fitxers pels números de llista, tal com demana Tecnos
# Tal com està ara i comptant que això és molt provisional, cal assegurar-se que els números de matrícules
# que trobarem a l'excel i els fitxers han d'estar en el mateix ordre. Això és bastant greu i ens som conscients.
Sys.setlocale(category="LC_ALL", locale = "Catalan")

noms_per_nums = function(excel_nums, carpetes_noms){
  # Importem els números de matrícula, comptant que tenim un excel amb n columnes ben ordenades
  nums = as.data.frame(read_xlsx(excel_nums, sheet = 1))
  dirs = list.dirs(carpetes_noms)
  dirs = dirs[2:length(dirs)]
  
  i=1
  for (di in dirs){
    numeros = as.vector(nums[complete.cases(nums[,i]), i])
    files = list.files(di)
    j=1
    for (fi in files){
      aa = file.rename(file.path(di, fi), file.path(di,paste0(numeros[j], '.pdf')))
      if (!aa){
        print("Something wrong. Aborting")
        stop()
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
}

noms_per_nums('dades/numeros_llista_tecnos.xlsx', 'informes/finals/')
