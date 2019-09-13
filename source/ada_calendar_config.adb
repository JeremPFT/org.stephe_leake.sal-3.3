--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2005, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with SAL.Config_Files.Duration; use SAL.Config_Files.Duration;
with SAL.Config_Files.Integer;  use SAL.Config_Files.Integer;
package body Ada_Calendar_Config is

   use Ada.Calendar;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Key         : in String;
      Default     : in Ada.Calendar.Time;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Ignore)
     return Ada.Calendar.Time
   is
      Default_Year    : Year_Number;
      Default_Month   : Month_Number;
      Default_Day     : Day_Number;
      Default_Seconds : Day_Duration;

      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Default, Default_Year, Default_Month, Default_Day, Default_Seconds);

      Year    := Read (Config, Key & ".Year", Default_Year, Missing_Key);
      Month   := Read (Config, Key & ".Month", Default_Month, Missing_Key);
      Day     := Read (Config, Key & ".Day", Default_Day, Missing_Key);
      Seconds := Read (Config, Key & ".Seconds", Default_Seconds, Missing_Key);

      return Time_Of (Year, Month, Day, Seconds);
   end Read;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Ada.Calendar.Time;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Ada.Calendar.Time
   is
      Default_Year    : Year_Number;
      Default_Month   : Month_Number;
      Default_Day     : Day_Number;
      Default_Seconds : Day_Duration;

      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Default, Default_Year, Default_Month, Default_Day, Default_Seconds);

      Year    := Read (Config, Iterator, Leaf & ".Year", Default_Year, Missing_Key);
      Month   := Read (Config, Iterator, Leaf & ".Month", Default_Month, Missing_Key);
      Day     := Read (Config, Iterator, Leaf & ".Day", Default_Day, Missing_Key);
      Seconds := Read (Config, Iterator, Leaf & ".Seconds", Default_Seconds, Missing_Key);

      return Time_Of (Year, Month, Day, Seconds);
   end Read;

   procedure Write
     (Config      : in out SAL.Config_Files.Configuration_Type;
      Key         : in     String;
      Value       : in     Ada.Calendar.Time;
      Missing_Key : in     SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Ignore)
   is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Value, Year, Month, Day, Seconds);

      Write (Config, Key & ".Year", Year, Missing_Key);
      Write (Config, Key & ".Month", Month, Missing_Key);
      Write (Config, Key & ".Day", Day, Missing_Key);
      Write (Config, Key & ".Seconds", Seconds, Missing_Key);
   end Write;

end Ada_Calendar_Config;
