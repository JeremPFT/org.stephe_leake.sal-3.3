--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2016, 2018 Stephen Leake All Rights Reserved.
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

with AUnit.Checks;
package body SAL.Calendar_More.AUnit is

   procedure Check
     (Label     : in String;
      Computed  : in Ada.Calendar.Time;
      Expected  : in Ada.Calendar.Time;
      Tolerance : in Duration)
   is
      use Ada.Calendar;
      use Standard.AUnit.Checks;

      Computed_Year    : Year_Number;
      Computed_Month   : Month_Number;
      Computed_Day     : Day_Number;
      Computed_Seconds : Day_Duration;

      Expected_Year    : Year_Number;
      Expected_Month   : Month_Number;
      Expected_Day     : Day_Number;
      Expected_Seconds : Day_Duration;
   begin
      Split (Computed, Computed_Year, Computed_Month, Computed_Day, Computed_Seconds);
      Split (Expected, Expected_Year, Expected_Month, Expected_Day, Expected_Seconds);

      Check (Label & ".year", Computed_Year, Expected_Year);
      Check (Label & ".month", Computed_Month, Expected_Month);
      Check (Label & ".day", Computed_Day, Expected_Day);
      Check (Label & ".seconds", Computed_Seconds, Expected_Seconds, Tolerance);
   end Check;

end SAL.Calendar_More.AUnit;
