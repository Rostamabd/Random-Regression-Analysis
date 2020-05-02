## Return coefficient matrix of 6-th order Legendre polynomials 

## Author: Rostam Abdollahi-Arpanahi <rostam7474 at gmail dot com>
## Create: 1-May-2020

Legdre = function(x) {
     y = rep(0,7)
       y[1] = sqrt(.5)
       y[2] = sqrt(1.5)*x
       x2 = x*x; y[3] = sqrt(2.5)*(1.5*x2 - 0.5)
       x3 = x2*x; y[4] = sqrt(3.5)*((15*x3/6)-(9*x/6))
       x4 = x3*x; y[5]=sqrt(4.5)*((105*x4/24)-(90*x2/24)+(3/8))
       x5 = x4*x; y[6]=sqrt(5.5)*((945*x5/120)-(105*x3/12)+(225*x/120))
       x6 = x5*x
       y[7]=sqrt(6.5)*((10395*x6/720)-(2835*x4/144)+(4725*x2/720)-(5/16))
       y  }
