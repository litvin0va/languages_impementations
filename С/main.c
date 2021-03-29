/*
 * The program for finding the root of the equation x (x) = 0
 * in a given interval ab with an accuracy of eps by
 * the method of quadratic interpolation.
 * If successful, the function displays the root and
 * the number of iterations spent looking for it.
 * Otherwise the error code.
 *
 * Compile : gcc main.c - lm
 * Run: ./a.out
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>

#define MAXIT 12345
#define E 1e-10

static int COUNT = 0;

int solve (double a, double b, double eps, double *x0);
int get_count ();
int sign (double x);
double f (double x);
double inter (double x1, double x2, double x3, double y1, double y2, double y3, int *res);
bool compare_abs (double a, double b);

double f (double x)
{
    COUNT++;
    return x * x - 4;
}

int main(void)
{
  double a, b, eps, x;
  int h;
  printf ("Input a: ");
  scanf ("%lf", &a);
  printf ("Input b: ");
  scanf ("%lf", &b);
  printf ("Input eps: ");
  scanf ("%lf", &eps);

  h = solve (a, b, eps, &x);
  if (h >= 0)
    {
      printf ("\nIt: %d\n", h);
      printf ("\nAns: %le\n", x);
    }
  else
    {
      printf ("\nRes: %d\n", h);
    }

  //  printf ("\nCount= \n", get_count());
  return 0;
}

bool compare_abs (double a, double b)
{
  return (fabs (a) >= fabs (b));
}

int solve (double a, double b, double eps, double *x)
{
  double x1 = a,
      x2 = b,
      x3 = (a + b) / 2.,
      y1 = f (x1),
      y2 = f (x2),
      y3 = f (x3);
  int it = 0,
      ret = 1;

  if(fabs (y1) < eps)
    {
      *x = x1;
      return it;
    }
  if (fabs (y2) < eps)
    {
      *x = x2;
      return it;
    }
  if (fabs (y3) < eps)
    {
      *x = x3;
      return it;
    }

  for (it = 1; it < MAXIT; ++it)
    {
      double xn = inter (y1, y2, y3, x1, x2, x3, &ret);
      double yn = f (xn);
      if (!ret)
        return -1;

      if (fabs (yn) < eps)
        {
          *x = xn;
          return it;
        }
      else
        {
          if (compare_abs (yn, y1))
            {
              if (compare_abs (yn, y2))
                {
                  if (compare_abs (yn, y3))
                    return -2;
                  else
                    {
                      x3 = xn;
                      y3 = yn;
                    }
                }
              else
                {
                  if (compare_abs (y2, y3))
                    {
                      x2 = xn;
                      y2 = yn;
                    }
                  else
                    {
                      x3 = xn;
                      y3 = yn;
                    }
                }
            }
          else
            {
              if (compare_abs (y1, y2))
                {
                  if (compare_abs (y1, y3))
                    {
                      x1 = xn;
                      y1 = yn;
                    }
                  else
                    {
                      x3 = xn;
                      y3 = yn;
                    }
                }
              else
                {
                  if (compare_abs (y2, y3))
                    {
                      x2 = xn;
                      y2 = yn;
                    }
                    else
                    {
                      x3 = xn;
                      y3 = yn;
                    }
                }
            }
        }
    }
  return -3;
}


double inter (double x1, double x2, double x3, double y1, double y2, double y3, int *res)
{
  y1 = -y1 * x2 + y2 * x1;
  if (fabs (x1 - x2) < E)
    {
      *res = 0;
      return 0;
    }
  y1 /= x1 - x2;

  y2 = -y2 * x3 + y3 * x2;
  if (fabs (x3 - x2) < E)
    {
      *res = 0;
      return 0;
    }
  y2 /= x2 - x3;

  y1 = -y1 * x3 + y2 * x1;
  if (fabs (x1 - x3) < E)
    {
      *res = 0;
      return 0;
    }
  y1 /= x1 - x3;

  return y1;
}

int get_count ()
{
  return COUNT;
}
