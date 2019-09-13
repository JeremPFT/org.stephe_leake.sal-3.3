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

package body SAL.Gen_Histogram is

   procedure Inc (Item : in out Integer)
   is begin
      Item := Item + 1;
   end Inc;

   procedure Accumulate (Histogram : in out Object; Item : in Value)
   is
      Bin : constant Integer := Integer'Max (Bin_Min, Integer'Min (Bin_Max, To_Bin (Item)));
   begin
      Inc (Histogram.Bins (Bin));
      Inc (Histogram.Total_Count);

      if Histogram.Bins (Bin) > Histogram.Max_Count then
         Histogram.Max_Count := Histogram.Bins (Bin);
      end if;
   end Accumulate;

end SAL.Gen_Histogram;
