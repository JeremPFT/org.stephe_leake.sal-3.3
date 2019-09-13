--  Abstract :
--
--  Conversions between several time representations.
--
--  All values are expressed relative to a TAI clock.
--
--  References:
--
--  [1] http://aa.usno.navy.mil/faq/docs/TT.html
--  [2] http://hpiers.obspm.fr/eoppc/bul/bulc/UTC-TAI.history
--
--  Copyright (C) 2001, 2004 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

with Ada.Calendar;
with Ada.Real_Time;
with Interfaces;
with SAL.Generic_Decimal_Image;
with SAL.Generic_Fixed_Image;
with SAL.Interfaces_More;
package SAL.Time_Conversions is
   pragma Elaborate_Body; --  Body depends on Ada.Exceptions

   type Time_System_Type is (UTC, TAI, Relative);
   subtype Internal_Time_System_Type is Time_System_Type range TAI .. Relative;
   --  UTC is only for user input or display

   Small : constant := 10.0**(-9);
   type Time_Type is delta Small range -2**63 * Small .. (2**63 - 1) * Small;
   for Time_Type'Small use Small;
   for Time_Type'Size use 64;
   --  This gives a range of +- 292 years:
   --
   --  10.0**-9 seconds/small * 2**63 smalls = 9.2234e9 seconds
   --  60 seconds/min * 60 min/hr * 24 hrs/day * 365 days/year = 3.153e7 seconds/year

   --  Auto_Text_IO: ignore
   type Leap_Second_Table_Type (<>) is private;

   --  Auto_Text_IO: ignore
   type Leap_Second_Table_Access_Type is access constant Leap_Second_Table_Type;

   function Leap_Seconds
     (TAI_Time            : in Time_Type;
      Leap_Seconds_Table : in Leap_Second_Table_Type)
     return Integer;

   function TAI_To_UTC
     (TAI_Input_Time     : in Time_Type;
      Leap_Seconds_Table : in Leap_Second_Table_Type)
     return Time_Type;
   --  Return a UTC converted from TAI.

   function UTC_To_TAI
     (UTC_Input_Time     : in Time_Type;
      Leap_Seconds_Table : in Leap_Second_Table_Type)
     return Time_Type;
   --  Return a TAI converted from UTC.

   function Create (File_Name : in String) return Leap_Second_Table_Type;
   --  Create the leap_second table from a standard input file.
   --
   --  The latest input file is at [2].

   type Month_Type is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

   function Day_Of_Year
     (In_Year  : in Integer;
      In_Month : in Month_Type)
     return Integer;
   --  Return the day of year for the first day of each month; Jan 1
   --  is day 1.

   function Floor (Item : in Time_Type) return Time_Type;
   function Floor (Item : in Time_Type) return Integer;
   --  Same as Time_Type'Floor would be.

   function Modulo (Dividend, Divisor : in Time_Type) return Time_Type;
   function "mod" (Dividend, Divisor : in Time_Type) return Time_Type renames Modulo;

   ----------
   --  Useful constants.

   Seconds_Per_Minute    : constant := 60.0;
   Seconds_Per_Hour      : constant := 3600.0;
   Seconds_Per_Day       : constant := 86_400.0;
   Seconds_Per_Week      : constant := 604_800.0;
   Seconds_Per_Year      : constant := 31_536_000.0;
   Seconds_Per_Leap_Year : constant := 31_622_400.0;
   Days_Per_Week         : constant := 7;
   Days_Per_Year         : constant := 365;
   Days_Per_Leap_Year    : constant := 366;

   Days_Per_Julian_Year        : constant := 365.25;
   Days_Per_Julian_Century     : constant := 100.0 * Days_Per_Julian_Year;
   Julian_Days_Per_Second      : constant := 1.0 / (Seconds_Per_Day);
   Julian_Centuries_Per_Second : constant := 1.0 / (Seconds_Per_Day * Days_Per_Julian_Century);

   --  See [1] for a definition of TT, and the J2000 epoch.

   TT_Offset : constant := 32.184;
   --  time in TT system = time in TAI system + TT_Offset

   Julian_Days_1958_TAI : constant := 2_436_204.5;
   --  [1] 0 Hour 1 January 1958, TAI system; TAI origin

   TT_Offset_Days        : constant := TT_Offset / Seconds_Per_Day;
   Julian_Days_J2000_TAI : constant := 2_451_545.0 - TT_Offset_Days;
   --  [1] 12 Hour 1 January 2000 TT, converted to TAI system

   Julian_Centuries_1958_TAI : constant := Julian_Days_1958_TAI / Days_Per_Julian_Century;

   --  Note that there are not an integer number of days per year, so
   --  going forward a Julian year can mean shifting time of day by 6
   --  hours.

   ----------
   --  Operations

   function Leap_Year (Year : in Integer) return Boolean;
   --  Return TRUE if Year is a leap year.

   procedure To_Year_Day_Seconds
     (TAI_Time       : in     Time_Type;
      Year           :    out Integer;
      Day_In_Year    :    out Integer;
      Seconds_In_Day :    out Time_Type);
   --  TAI_Time is seconds since TAI origin. Day is in range 1 .. 364

   procedure To_Days_Seconds
     (TAI_Time       : in     Time_Type;
      Days           :    out Integer;
      Seconds_In_Day :    out Time_Type);
   --  TAI_Time is relative. Day is in range 1 .. 364

   function To_Julian_Day (TAI_Time : in Time_Type) return Long_Float;
   function To_Julian_Century (TAI_Time : in Time_Type) return Long_Float;
   --  Convert an absolute time; TAI_Time is seconds since TAI origin.

   function Seconds_To_Julian_Centuries (TAI_Time : in Time_Type) return Long_Float;
   --  Convert a relative time

   function To_J2000_Julian_Centuries (TAI_Time : in Time_Type) return Long_Float;
   --  TAI_Time is seconds since TAI origin; return Julian centuries since J2000 epoch.

   function Julian_Days_TT_To_Seconds_TAI (Julian_Days : in Long_Float) return Time_Type;
   function Julian_Days_TAI_To_Seconds_TAI (Julian_Days : in Long_Float) return Time_Type;

   function To_TAI_Time
     (Year           : in Integer;
      Day_In_Year    : in Integer;
      Seconds_In_Day : in Time_Type;
      Absolute       : in Boolean)
     return Time_Type;
   --  If absolute, Year must be > 1958, and result is seconds since TAI origin.
   --
   --  If not Absolute, leap days are ignored, and result is relative.

   function To_TAI_Time
     (Days           : in Integer;
      Seconds_Of_Day : in Time_Type)
     return Time_Type;
   --  Result is relative.
   --
   --  Nominally, Seconds_Of_Day should be < Seconds_Per_Day, but in
   --  fact it can be any Time_Type value.

   function To_TAI_Time
     (Hours             : in Integer;
      Minutes           : in Integer;
      Seconds_In_Minute : in Time_Type)
     return Time_Type;
   --  Result is relative.

   function To_Time (Time : in Ada.Real_Time.Time) return Time_Type;

   procedure To_Hour_Minute_Seconds
     (Seconds           : in     Time_Type;
      Hour              :    out Integer;
      Minute            :    out Integer;
      Seconds_In_Minute :    out Time_Type);

   ----------
   --  Conversions for counter/timers

   function Floor_Unsigned_16 (Item : in Time_Type) return Interfaces.Unsigned_16;
   --  Return smallest integral value not less than Item.
   --
   --  Raises SAL.Range_Error (with no message) if Item is outside
   --  range of Unsigned_16.

   function Floor_Unsigned_32 (Item : in Time_Type) return Interfaces.Unsigned_32;
   --  Return smallest integral value not less than Item.
   --
   --  Raises SAL.Range_Error (with no message) if Item is outside
   --  range of Unsigned_32.

   function To_Time (Microseconds : in Interfaces.Unsigned_16) return Time_Type;
   function To_Time (Microseconds : in Interfaces.Unsigned_32) return Time_Type;

   function To_Microseconds (Time : in Time_Type) return SAL.Interfaces_More.Unsigned_15;
   function To_Microseconds (Time : in Time_Type) return Interfaces.Unsigned_16;
   function To_Microseconds (Time : in Time_Type) return Interfaces.Unsigned_32;

   function Checked_Unsigned_16 (Label : in String; Item : in Time_Type) return Interfaces.Unsigned_16;
   --  If Item is in range of Unsigned_16, return Unsigned_16 (Item).
   --  Else raise Range_Error with Label, value of Item in message.

   ----------
   --  The NASA Goddard ASIST package defines a string time representation:
   --
   --  YY-DDD-HH:MM:SS.LLL
   --  1234567890123456789
   --
   --  For absolute times, if YY < 70, year = 2000 + YY, else year = 1900 + YY.
   --
   --  DDD is in range 001 .. 364 (365 for leap years)
   --
   --  No fields of the ASIST time string may be omitted.
   --
   --  To allow dates outside the range 1970 .. 2069, we extend the
   --  ASIST representation to permit 4 digit years.
   --
   --  The to_* (Time : in String) functions raise SAL.Invalid_Format
   --  if the string is not a valid format.

   subtype ASIST_Time_String_Type is String (1 .. 19);
   subtype Extended_ASIST_Time_String_Type is String (1 .. 21);

   TAI_Epoch_String : constant Extended_ASIST_Time_String_Type := "1958-001-00:00:00.000";
   J2000_TAI_String : constant Extended_ASIST_Time_String_Type := "2000-001-11:59:27.816";

   function To_TAI_Time
     (Time     : in String;
      Absolute : in Boolean)
     return Time_Type;
   --  Time must be ASIST format or extended ASIST format. If
   --  absolute, result is seconds since TAI origin.

   function Image is new SAL.Generic_Decimal_Image (Integer);
   pragma Warnings (Off, "instance does not use primitive operation ""mod"" at line 97"); -- gnat 2017
   pragma Warnings (Off, "instance uses predefined operation, not primitive operation ""mod"" at line 97"); -- gnat 2018
   function Image is new SAL.Generic_Fixed_Image (Time_Type);
   pragma Warnings (On);

   function To_ASIST_String (Time_TAI : in Time_Type) return ASIST_Time_String_Type;

   function To_Extended_ASIST_String (Time_TAI : in Time_Type) return Extended_ASIST_Time_String_Type;

   ----------
   --  GPS times

   type GPS_Time_Type is record
      --  GPS only has 11 bits in the week, but we allow more to
      --  accomodate a larger range of years.
      Weeks           : Interfaces.Unsigned_16;
      Seconds_Of_Week : Time_Type;
   end record;

   GPS_Epoch_TAI_String : constant Extended_ASIST_Time_String_Type := "1980-006-00:00:19.000";

   GPS_Epoch_TAI : constant Time_Type := 694656019.0; --  See unit test

   function To_GPS_Time (TAI : in Time_Type) return GPS_Time_Type;

   function To_TAI_Time (GPS : in GPS_Time_Type) return Time_Type;

   ----------
   --  to/from Ada.Calendar

   function To_Month_Type (Month : in Ada.Calendar.Month_Number) return Month_Type;

   function To_Month_Number (Month : in Month_Type) return Ada.Calendar.Month_Number;

   function To_TAI_Time (Cal : in Ada.Calendar.Time) return Time_Type;

   function To_Calendar_Time (TAI : in Time_Type) return Ada.Calendar.Time;

private

   --  Auto_Text_IO: ignore
   type Leap_Second_Table_Entry_Type is record
      Start_TAI_Time : Time_Type;
      Start_UTC_Time : Time_Type;
      Leap_Second    : Integer;
   end record;

   --  Auto_Text_IO: ignore
   type Leap_Second_Table_Type is array (Integer range <>) of Leap_Second_Table_Entry_Type;

end SAL.Time_Conversions;
