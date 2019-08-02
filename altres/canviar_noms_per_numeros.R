# Funció que canvia els noms dels fitxers pels números de llista, tal com demana Tecnos
# Tal com està ara i comptant que això és molt provisional, cal assegurar-se que els números de matrícules
# que trobarem a l'excel i els fitxers han d'estar en el mateix ordre. Això és bastant greu i ens som conscients.
Sys.setlocale(category="LC_ALL", locale = "Catalan")

noms_per_nums = function(excel_nums, carpetes_noms){
  require(stringr)
  # Importem els números de matrícula, comptant que tenim un excel amb n columnes ben ordenades
  nums = as.data.frame(read_xlsx(excel_nums, sheet = 1))
  dirs = list.dirs(carpetes_noms)
  dirs = dirs[2:length(dirs)]
  
  i=1
  for (di in dirs){
    current_nums = as.vector(t(nums[,i]))
    numeros = current_nums[complete.cases(current_nums)]
    files = list.files(di)
    # Hem d'ordenar els fitxers per cognom, igual que les matrícules a l'excel.
    noms = str_split_fixed(files, "_", 3)
    noms = paste0(noms[,2], "_", noms[,3])
    noms_ordenats = sort(noms)
    index_dels_noms = c()
    for (nom in noms_ordenats){
      index_dels_noms = c(index_dels_noms, match(nom, noms))
    }
    files = files[index_dels_noms]
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
