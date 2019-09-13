--  Abstract :
--
--  AUnit checks for types in Ada.Calendar
--
--  Copyright (C) 2016 Stephen Leake All Rights Reserved.
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

with Ada.Calendar;
package SAL.Calendar_More.AUnit is

   procedure Check
     (Label     : in String;
      Computed  : in Ada.Calendar.Time;
      Expected  : in Ada.Calendar.Time;
      Tolerance : in Duration);

end SAL.Calendar_More.AUnit;
