lista.xyz <- function(f, xlim = c(0.1,1), ylim = c(0.1,1), n = 100, ...){
  # Funcion que devuleve una lista con componentes x, y, z para ser
  # introducidos en el argumento 'x' del comando 'persp'
  if(!is.function(f)) {
    f1 <- function(x,y) x+y
    body(f1) <- parse(text = f)
    f <- f1
  }
  x <- seq(xlim[1], xlim[2], length.out = n)
  y <- seq(ylim[1], ylim[2], length.out = n)
  z <- outer(x, y, FUN = f, ...)
  list(x = x, y = y, z = z)
  
  # Ejemplo de uso:
  # Si f es una funcion:
  # f <- function(x,y, b) x^2 + b*y^2
  # d <- lista.xyz(f = f, b = 3)
  # persp(x = d, theta = 30, xlab = 'x', ylab = 'y')
  
  # Si f es el texto de una funcion:
  # d <- lista.xyz(f = 'x^2 + y^2')
  # persp(x = d, theta = 30, xlab = 'x', ylab = 'y')
}