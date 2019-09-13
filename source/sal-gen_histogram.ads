--  Abstract :
--
--  Support for histogram plots
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

generic
   Bin_Min : in Integer;
   Bin_Max : in Integer;

   type Value is private;
   with function To_Bin (Item : in Value) return Integer;

package SAL.Gen_Histogram is

   type Object is tagged private;

   Null_Histogram : constant Object;

   procedure Accumulate (Histogram : in out Object; Item : in Value);

private
   subtype Bin_Index is Integer range Bin_Min .. Bin_Max;
   type Bins is array (Bin_Index) of Integer;
   type Object is tagged record
      Bins        : SAL.Gen_Histogram.Bins := (others => 0);
      Max_Count   : Integer                := 0;
      Total_Count : Integer                := 0;
   end record;

   Null_Histogram : constant Object :=
     (Bins        => (others => 0),
      Max_Count   => 0,
      Total_Count => 0);

end SAL.Gen_Histogram;
