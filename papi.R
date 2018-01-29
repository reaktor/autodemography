# plumber.R

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' add +1 to the number 
#' @param number number to add to 
#' @get /number
function(number=0){
  list(number = paste0
}