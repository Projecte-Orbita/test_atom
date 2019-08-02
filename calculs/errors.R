errors <- function(punts){
  
  for(i in 2:ncol(punts)){
    punts[,i]=as.numeric(as.character(punts[,i]));
  }
  
  #punts[punts == '-'] <- NA;
  
  encerts = c();
  errors = c();
  indeximp = c();
  
  colencerts = seq(2,ncol(punts),2);
  colerrors = seq(3,ncol(punts),2);
  
  for (i in 1:nrow(punts)){
    encerts[i] = sum(as.numeric(punts[i,colencerts]), na.rm = TRUE);
    errors[i] = sum(as.numeric(punts[i, colerrors]), na.rm = TRUE);
    indeximp[i] = errors[i]/(errors[i]+encerts[i]);
    }
  
  #   # els límits són: 0.09 per baix i 0.18 per dalt.
  
  #indexper <- pnorm(indeximp, indextot[1],indextot[2]);
  
  return(indeximp);
  
}
