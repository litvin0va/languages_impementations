! The program for finding the root of the equation x (x) = 0
! in a given interval ab with an accuracy of eps by
! the method of quadratic interpolation.
! If successful, the function displays the root and
! the number of iterations spent looking for it.
! Otherwise the error code.
!
! Compile : gfortran -o a main.f90
! Run: ./a


pure real function f(x)
  real, intent(in) :: x
  f = x * x - 4
end function

program main
   implicit none

   real     :: a, b, c, eps, x
   integer  :: h
   write(*,*) "Input a: "
   read(*,*) a
   write(*,*) "Input b: "
   read(*,*) b
   write(*,*) "Input eps: "
   read(*,*) eps
   CALL solve (a, b, eps, x, h)
   if (h >= 0) then
     write(*,*) "It: ", h
     write(*,*) "Ans: ", x
   else
     write(*,*) "Res: ", h
   endif
end program main


subroutine solve(a, b, eps, x, h)
  real,    intent(in)  :: a, b, eps ! input
  real,    intent(out) :: x         ! output
  integer, intent(out) :: h         ! output
  real                 :: x1, x2, x3, y1, y2, y3, xn, yn
  integer              :: it, ret, MAXIT
  MAXIT = 12345
  x1 = a
  x2 = b
  x3 = (a + b) / 2.
  y1 = f (x1)
  y2 = f (x2)
  y3 = f (x3)
  it = 0
  ret = 1

  if (abs (y1) < eps) then
    x = x1
    h = it
    return
  endif

  if (abs (y2) < eps) then
    x = x2
    h = it
    return
  endif

  if (abs (y3) < eps) then
    x = x3
    h = it
    return
  endif

  do it = 1, MAXIT
    call inter (y1, y2, y3, x1, x2, x3, ret, xn)
    yn = f (xn)
    if (ret == 0) then
      h = -1
      return
    endif
    if (abs (yn) < eps) then
      x = xn
      h = it
      return
    else
      if (abs (yn) >= abs (y1)) then
        if (abs (yn) >= abs (y2)) then
          if (abs (yn) >= abs (y3)) then
            h = -2
            return
          else
            x3 = xn
            y3 = yn
          endif
        else
          if (abs (y2) >= abs (y3)) then
            x2 = xn
            y2 = yn
          else
            x3 = xn
            y3 = yn
          endif
        endif
      else
        if (abs (y1) >= abs (y2)) then
          if (abs (y1) >= abs (y3)) then
            x1 = xn
            y1 = yn
          else
            x3 = xn
            y3 = yn
          endif
        else
          if (abs (y2) >= abs (y3)) then
            x2 = xn
            y2 = yn
          else
            x3 = xn
            y3 = yn
          endif
        endif
      endif
    endif
  end do
  h = -3
end subroutine

subroutine inter(x1, x2, x3, y1, y2, y3, res, ans)
  real,    intent(in)  :: x1, x2, x3, y1, y2, y3
  real,    intent(out) :: ans
  integer, intent(out) :: res
  real     :: tmp1, tmp2, masheps
  masheps = 1e-15
  tmp1 = -y1 * x2 + y2 * x1
  if (abs (x1 - x2) < masheps) then
      res = 0
      ans = 0
      return
    endif
  tmp1 = tmp1 / (x1 - x2);

  tmp2 = -y2 * x3 + y3 * x2
  if (abs (x3 - x2) < masheps) then
      res = 0;
      ans = 0
      return
    endif
  tmp2 = tmp2 / (x2 - x3);

  tmp1 = -tmp1 * x3 + tmp2 * x1
  if (abs (x1 - x3) < masheps) then
      res = 0
      ans = 0
      return
    endif
  ans = tmp1 / (x1 - x3)
  res = 1
end subroutine
