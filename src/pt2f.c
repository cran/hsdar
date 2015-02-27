#include <R.h>
#include <Rmath.h>

double F77_SUB(pt2f)(double* xx, double* nn)
{
  double x ;
  double n ;
  n = nn[0];
  x = xx[0];
  
  return pt(x, n, 0, 0);
}
