## Esta función crea las diferentes funcionalidades para trabajar con la matriz y con su inversa.
makeCacheMatrix <- function(x = matrix()) {
  minver <- NULL #Se inicializa la variable para que cada vez que se calcule un makeCacheMatrix le de el valor NULL (limpie la variable).
  setM <- function(y) { #setM asigna un nuevo valor a la matriz en el entorno padre.
    x <<- y
    minver <<- NULL
  }
  getM <- function() x #getM obtiene el valor de la matriz.
  setminver <- function(minverse) minver <<- minverse #setminver guarda la matriz inversa en el entorno padre.
  getminver <- function() minver #getminver obtiene la matriz inversa
  list(setM = setM, getM = getM, #Lista por pantalla los valores de cada función anterior.
       setminver = setminver,
       getminver = getminver)
}


## La siguiente función es la principal, la cual llama a las funciones que se crearon en makeCacheMatrix
## para obtener el valor en cada punto que se desea de la matriz.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minver <- x$getminver() #Obtiene el valor de la matriz y lo guarda en una variable.
  if(!is.null(minver)) { #Se comprueba que el valor no es nulo para evitar hacer cálculos ya realizados.
    message("getting cached data")
    return(minver) #Retorna el resultado de la matriz inversa de x.
  }
  matriz <- x$getM() #getM llama a la función para guardar en una variable el valor de la matriz.
  minver <- solve(matriz,...) #Con la función solve() se calcula la inversa de una matriz.
  x$setminver(minver) #Llama a la función setminver pasándole el valor de la matriz inversa.
  minver #Muestra por pantalla el resultado de la matriz inversa de x.
}
