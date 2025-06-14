#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : qua 04 jun 2025 15:29:34
# License :
# Updated :
#-------------------------------------------
order_magnitude = function(x)
{
  n1 = 10^(floor(log10(x)))
  n2 = 10^(ceiling(log10(x)))
  c(n1=n1, n2=n2)
}
