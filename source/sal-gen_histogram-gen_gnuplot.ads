--  Abstract :
--
--  Output a histogram as a gnuplot dat commands files
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
   Units_Per_Bin : in Float;
package SAL.Gen_Histogram.Gen_Gnuplot is

   procedure Put_Plot
     (Histogram : in Object;
      Title     : in String;
      X_Label   : in String);
   --  Create a file (deleting if exists) containing gnuplot commands
   --  to create plot of Histogram data.

end SAL.Gen_Histogram.Gen_Gnuplot;
