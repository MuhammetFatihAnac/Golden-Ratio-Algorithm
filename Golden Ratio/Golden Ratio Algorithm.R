altınoran<-function(x_üst,x_alt){
  
  i=1
  
  sabit<-(sqrt(5)-1)/2
  uzunluk<-x_üst-x_alt
  
  x1<-x_üst-(x_üst-x_alt)*sabit
  x2<-x_alt+(x_üst-x_alt)*sabit
  
  cat("\t","\t","\t","\t","\t","BAŞLANGIÇ DEĞERLERİ","\n","X1:",x1,"X2:",x2,"L:",uzunluk,"x_alt:",x_alt,"x_üst:",x_üst,"\n","\n")
  
  while(TRUE){
    
    f1<-fonksiyon(x1)
    f2<-fonksiyon(x2)
    
    if(f1<f2){
      x_alt<-x1
      uzunluk<-x_üst-x_alt
      
      cat("\t","\t","\t","\t","\t",i,"-inci İTERASYON SONUÇLARI","\n")
      data<-data.frame(x1=c(x1),x2=c(x2),fx1=c(f1),fx2=c(f2),L=c(uzunluk), x_alt=c(x_alt),x_üst=c(x_üst))
      rownames(data)=" "
      print(data)
      cat("\n")
      cat("\n")
      
      x1<-x2
      x2<-x_alt+(x_üst-x_alt)*sabit
      
      i=i+1
      
      
    }else{
      x_üst<-x2
      uzunluk<-x_üst-x_alt
      
      cat("\t","\t","\t","\t","\t",i,"-inci İTERASYON SONUÇLARI","\n")
      data<-data.frame(x1=c(x1),x2=c(x2),fx1=c(f1),fx2=c(f2),L=c(uzunluk), x_alt=c(x_alt),x_üst=c(x_üst))
      rownames(data)=" "
      print(data)
      cat("\n")
      cat("\n")
      
      x2<-x1
      x1<-x_üst-(x_üst-x_alt)*sabit
      
      i=i+1
      
      
      
    }
    if(uzunluk<0.25){
      cat("L=",uzunluk,"< 0.25 olduğu için iterasyonlar sonlandırılır.","\n","Modelin çözümü","[",x_alt,",",x_üst,"]","aralığındadır.")
      break
    }
    
  }
  
}