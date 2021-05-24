-- The program for finding the root of the equation f (x) := 0
-- in a given interval ab with an accuracy of eps by
-- the method of quadratic interpolation.
-- If successful, the function displays the root and
-- the number of iterations spent looking for it.
-- Otherwise the error code.
--
-- Compile: gnatmake main.adb
-- Run: ./main

with Ada.Text_IO, Ada.Integer_Text_IO, Ada.float_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.float_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;
procedure main is

  function f (x : float) return float is
  begin
    return x * x - 4.0;
  end f;

  function inter(x1, x2, x3, y1, y2, y3 : float;
                 res : in out Integer) return float is
    tmp1, tmp2, masheps : float;
  begin
    masheps := 0.000000000000001;
    tmp1 := -y1 * x2 + y2 * x1;
    if (abs (x1 - x2) < masheps) then
        res := 0;
        return 0.0;
      end if;
    tmp1 := tmp1 / (x1 - x2);

    tmp2 := -y2 * x3 + y3 * x2;
    if (abs (x3 - x2) < masheps) then
        res := 0;
        return 0.0;
      end if;
    tmp2 := tmp2 / (x2 - x3);

    tmp1 := -tmp1 * x3 + tmp2 * x1;
    if (abs (x1 - x3) < masheps) then
        res := 0;
        return 0.0;
      end if;
    return tmp1 / (x1 - x3);
  end inter;



  function solve (a, b, eps : float;
                  x : in out float) return Integer is
    x1, x2, x3, y1, y2, y3, xn, yn : float;
    it, ret, MAXIT : Integer;
  begin
    MAXIT := 12345;
    x1 := a;
    x2 := b;
    x3 := (a + b) / 2.0;
    y1 := f (x1);
    y2 := f (x2);
    y3 := f (x3);
    it := 0;
    ret := 1;

    if abs (y1) < eps then
      x := x1;
      return it;
    end if;

    if abs (y2) < eps then
      x := x2;
      return it;
    end if;

    if abs (y3) < eps then
      x := x3;
      return it;
    end if;

    for it in 1..MAXIT loop
      xn := inter (y1, y2, y3, x1, x2, x3, ret);
      yn := f (xn);

      if ret = 0 then
        return -1;
      end if;

      if abs (yn) < eps then
        x := xn;
        return it;
      else
        if abs (yn) >= abs (y1) then
          if abs (yn) >= abs (y2) then
            if abs (yn) >= abs (y3) then
              return -2;
            else
              x3 := xn;
              y3 := yn;
            end if;
          else
            if abs (y2) >= abs (y3) then
              x2 := xn;
              y2 := yn;
            else
              x3 := xn;
              y3 := yn;
            end if;
          end if;
        else
          if abs (y1) >= abs (y2) then
            if abs (y1) >= abs (y3) then
              x1 := xn;
              y1 := yn;
            else
              x3 := xn;
              y3 := yn;
            end if;
          else
            if abs (y2) >= abs (y3) then
              x2 := xn;
              y2 := yn;
            else
              x3 := xn;
              y3 := yn;
            end if;
          end if;
        end if;
      end if;
    end loop;
    return -3;
  end solve;


  a : float := 0.0;
  b : float := 0.0;
  eps : float := 0.0;
  h : Integer := 0;
  x : float := 0.0;
begin
  Put ("Input a: ");
  Get (a);
  Put ("Input b: ");
  Get (b);
  Put ("Input eps: ");
  Get (eps);
  h := solve (a, b, eps, x);
  if h >= 0 then
    Put ("It:");
    Put (h);
    new_line;
    Put ("Ans:");
    Put (x);
  else
    Put ("Res: ");
    Put (h);
  end if;
end main;
