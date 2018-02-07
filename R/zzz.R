#OOP mit S4

#definiere Klasse
methods::setClass("Silvermantest",representation = representation(data="numeric",p_value="numeric",saved_seed="numeric",k="numeric"))

#definiere show, bzw. printfunktion um f?r Objekte dieser Klasse
methods::setMethod("show",signature(object="Silvermantest"),
  function(object){
    cat("Silvermantest: Testing the hypothesis if the number of modes is <= ", object@k,"\n")
    cat("The P-Value is ",object@p_value,"\n")
  }
)



