--  Abstract :
--
--  See spec
--
--  Copyright (C) 2001, 2004 - 2009, 2011 - 2013, 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Calendar;
with Ada.Exceptions;
with GNAT.Source_Info;
with Interfaces;
with SAL.Interfaces_More.AUnit;
with SAL.Time_Conversions.AUnit;
package body SAL.Time_Conversions.Test is

   --  File_Name is relative to build directory.
   Leap_Table_File_Name     : constant String := "../test/test_time_conversions-history.txt";
   Bad_Leap_Table_File_Name : constant String := "../test/bad_history.txt";

   ----------
   --  Test procedures

   procedure Test_Leap_Year (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Standard.AUnit.Checks;
   begin
      Check ("Leap_Year (1)", Leap_Year (1), False);
      Check ("Leap_Year (2)", Leap_Year (2), False);
      Check ("Leap_Year (3)", Leap_Year (3), False);
      Check ("Leap_Year (4)", Leap_Year (4), True);
      Check ("Leap_Year (5)", Leap_Year (5), False);
      Check ("Leap_Year (6)", Leap_Year (6), False);
      Check ("Leap_Year (7)", Leap_Year (7), False);
      Check ("Leap_Year (8)", Leap_Year (8), True);
      Check ("Leap_Year (9)", Leap_Year (9), False);
   end Test_Leap_Year;

   procedure Test_Year_Day_Seconds (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Message      : in String;
         Year         : in Integer;
         Day          : in Integer;
         Seconds      : in Time_Type;
         Expected_TAI : in Time_Type)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
         TAI_Time    : constant Time_Type := To_TAI_Time (Year, Day, Seconds, Absolute => True);
         Out_Year    : Integer;
         Out_Day     : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         To_Year_Day_Seconds (TAI_Time, Out_Year, Out_Day, Out_Seconds);

         Check (Message & ".year", Out_Year, Year);
         Check (Message & ".day", Out_Day, Day);
         Check (Message & ".Seconds", Out_Seconds, Seconds);

      end Check;
   begin

      Check
        ("1",
         Year         => 1958 + 0,
         Day          => 1,
         Seconds      => 0.0,
         Expected_TAI => 0.00);

      Check
        ("2",
         Year         => 1958 + 0,
         Day          => 20,
         Seconds      => 10.01,
         Expected_TAI => 1641610.01);

      Check
        ("3",
         Year         => 1958 + 5,
         Day          => 120,
         Seconds      => 20.01,
         Expected_TAI => 168048020.01);

   end Test_Year_Day_Seconds;

   procedure Test_Days_Seconds (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Exp_Days    : Integer;
      Exp_Seconds : Time_Type;

      procedure Check
        (Message      : in String;
         Days         : in Integer;
         Seconds      : in Time_Type;
         Expected_TAI : in Time_Type)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
         TAI_Time    : constant Time_Type := To_TAI_Time (Days, Seconds);
         Out_Days    : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         To_Days_Seconds (TAI_Time, Out_Days, Out_Seconds);

         Check (Message & ".day", Out_Days, Days);
         Check (Message & ".Seconds", Out_Seconds, Seconds);

      end Check;
   begin

      Check
        ("1",
         Days         => 1,
         Seconds      => 0.0,
         Expected_TAI => 86400.00);

      Check
        ("2",
         Days         => 20,
         Seconds      => 10.01,
         Expected_TAI => 1728010.01);

      Check
        ("3",
         Days         => 1946,
         Seconds      => 20.01,
         Expected_TAI => 168134420.01);

      Check
        ("4",
         Days         => 106_000,
         Seconds      => 0.01,
         Expected_TAI => 9158400000.01);

      To_Days_Seconds (Time_Type'Last, Exp_Days, Exp_Seconds);

      Check
        ("5",
         Days         => Exp_Days,
         Seconds      => Exp_Seconds,
         Expected_TAI => Time_Type'Last);

      Check
        ("6",
         Days         => 0,
         Seconds      => 839.0,
         Expected_TAI => 839.0);

   end Test_Days_Seconds;

   procedure Test_Hour_Minute_Seconds (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Message          : in String;
         Hour             : in Integer;
         Minute           : in Integer;
         Seconds          : in Time_Type;
         Expected_TAI     : in Time_Type;
         Expected_Hour    : in Integer;
         Expected_Minute  : in Integer;
         Expected_Seconds : in Time_Type)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
         TAI_Time    : constant Time_Type := To_TAI_Time (Hour, Minute, Seconds);
         Out_Hour    : Integer;
         Out_Minute  : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         To_Hour_Minute_Seconds (TAI_Time, Out_Hour, Out_Minute, Out_Seconds);

         Check (Message & ".Hour", Out_Hour, Expected_Hour);
         Check (Message & ".Minute", Out_Minute, Expected_Minute);
         Check (Message & ".Seconds", Out_Seconds, Expected_Seconds);

      end Check;
   begin

      Check
        ("1",
         Hour             => 0,
         Minute           => 0,
         Seconds          => 0.0,
         Expected_TAI     => 0.0,
         Expected_Hour    => 0,
         Expected_Minute  => 0,
         Expected_Seconds => 0.0);

      Check
        ("2",
         Hour             => 0,
         Minute           => 20,
         Seconds          => 10.01,
         Expected_TAI     => 1210.01,
         Expected_Hour    => 0,
         Expected_Minute  => 20,
         Expected_Seconds => 10.01);

      Check
        ("3",
         Hour             => 5,
         Minute           => 120,
         Seconds          => 20.01,
         Expected_TAI     => 25220.01,
         Expected_Hour    => 7,
         Expected_Minute  => 0,
         Expected_Seconds => 20.01);
   end Test_Hour_Minute_Seconds;

   procedure Test_String_TAI (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST        : in String;
         Expected_TAI : in Time_Type)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
         TAI_Time : constant Time_Type := To_TAI_Time (ASIST, Absolute => True);
      begin
         Check (ASIST & " TAI", TAI_Time, Expected_TAI);

         if ASIST'Length = 19 then
            Check (ASIST & " ASIST", To_ASIST_String (TAI_Time), ASIST);
         else
            Check (ASIST & " ASIST", To_ASIST_String (TAI_Time), ASIST (ASIST'First + 2 .. ASIST'Last));
         end if;
      end Check;
   begin

      Check ("70-001-00:00:00.000", 378691200.00);
      Check ("71-001-00:20:10.010", 410228410.01);
      Check ("69-001-05:40:20.010", 3502935620.01);
      Check ("00-021-05:40:20.010", 1327124420.01);
      Check ("10-021-05:40:20.010", 1642743620.01);
   end Test_String_TAI;

   procedure Test_Extended_ASIST_TAI (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST_Expected : in String;
         TAI            : in Time_Type)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
         ASIST_Computed : constant String    := To_Extended_ASIST_String (TAI);
         TAI_Computed   : constant Time_Type := To_TAI_Time (ASIST_Computed, Absolute => True);
      begin
         Check (ASIST_Expected & " TAI", TAI, TAI_Computed);

         Check (ASIST_Expected & " ASIST", ASIST_Computed, ASIST_Expected);

      end Check;
   begin
      Check ("1958-001-00:00:00.000", 0.0);
      Check ("1970-001-00:00:00.000", 378691200.00);
      Check ("1971-001-00:20:10.010", 410228410.01);
      Check ("2069-001-05:40:20.010", 3502935620.01);
      Check ("2000-021-05:40:20.010", 1327124420.01);
      Check ("2010-021-05:40:20.010", 1642743620.01);
   end Test_Extended_ASIST_TAI;

   procedure Test_TAI_Julian_Convert (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Input_TAI                 : in Time_Type;
         Expected_Julian_Centuries : in Long_Float)
      is
         use Standard.AUnit.Checks;

         Days_Tolerance : constant := 2.0e-6;
         --  There are 8.64e+4 seconds in a day.

         Centuries_Tolerance : constant := 5.0e-11;
         --  There are 3.15576e+9 seconds in a Julian century.

      begin
         Check ("Day", To_Julian_Day (Input_TAI), Expected_Julian_Centuries * Days_Per_Julian_Century, Days_Tolerance);
         Check ("Century", To_Julian_Century (Input_TAI), Expected_Julian_Centuries, Centuries_Tolerance);
      end Check;

   begin
      Check (Time_Type (378691200), 66.8196440793977);  --  1970-001-00:00:00
      Check (Time_Type (410228410), 66.8296376181966);  --  1971-001-00:20:10
      Check (Time_Type (1327124420), 67.1201844310087); --  2000-021-05:40:20
      Check (Time_Type (1642743620), 67.2201981202626); --  2010-021-05:40:20
      Check (Time_Type (2272167620), 67.4196505501052); --  2030-001-05:40:20
      Check (Time_Type (3502935620), 67.8096573947322); --  2069-001-05:40:20
   end Test_TAI_Julian_Convert;

   procedure Test_Julian_TAI_Convert (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Input_Julian_Day : in Long_Float;
         Expected_TAI     : in Time_Type)
      is
         use SAL.Time_Conversions.AUnit;
      begin
         Check ("", Julian_Days_TT_To_Seconds_TAI (Input_Julian_Day), Expected_TAI);
      end Check;

   begin
      Check (Julian_Days_1958_TAI + TT_Offset_Days, 0.0);
   end Test_Julian_TAI_Convert;

   procedure Test_J2000_Julian_Centuries (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Tolerance : constant Long_Float := 5.0e-11;
      --  There are 3.15576e+9 seconds in a Julian century.

      procedure Check (TAI_Time : in String; Expected_J2000_Julian_Centuries : in Long_Float)
      is begin
         Standard.AUnit.Checks.Check
           (TAI_Time,
            To_J2000_Julian_Centuries (To_TAI_Time (TAI_Time, Absolute => True)),
            Expected_J2000_Julian_Centuries,
            Tolerance);
      end Check;

      TT_Offset_Centuries : constant Long_Float := TT_Offset * Julian_Centuries_Per_Second;
   begin
      Check ("2000-001-12:00:00.000", 0.0 + TT_Offset_Centuries);
      Check ("2000-001-18:00:00.000", 0.25 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-002-12:00:00.000", 1.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-101-12:00:00.000", 100.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-365-12:00:00.000", 364.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-366-12:00:00.000", 365.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-367-12:00:00.000", 366.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2001-001-12:00:00.000", 366.0 / 36525.0 + TT_Offset_Centuries); -- 2000 is a leap year
   end Test_J2000_Julian_Centuries;

   procedure Test_Extended_String_TAI (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST        : in String;
         Expected_TAI : in Time_Type)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
         TAI_Time : constant Time_Type := To_TAI_Time (ASIST, Absolute => True);
      begin
         Check (ASIST & " TAI", TAI_Time, Expected_TAI);

         Check (ASIST & " ASIST", To_Extended_ASIST_String (TAI_Time), ASIST);
      end Check;
   begin

      Check ("1970-001-00:00:00.000", 378691200.00);
      Check ("1971-001-00:20:10.010", 410228410.01);
      Check ("2069-001-05:40:20.010", 3502935620.01);
      Check ("2000-021-05:40:20.010", 1327124420.01);
      Check ("2010-021-05:40:20.010", 1642743620.01);
   end Test_Extended_String_TAI;

   procedure Test_Floor_Unsigned_16 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit;
      use Interfaces;
      use Standard.AUnit.Assertions;
   begin
      Check ("1.0", Floor_Unsigned_16 (1.0), 1);
      Check ("1.1", Floor_Unsigned_16 (1.1), 1);
      Check ("0.000_000_001", Floor_Unsigned_16 (0.000_000_001), 0);
      Check ("1.000_000_001", Floor_Unsigned_16 (1.000_000_001), 1);
      Check ("~Unsigned_16'last", Floor_Unsigned_16 (65534.1), 65534);

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := Floor_Unsigned_16 (65535.01);
         Assert (False, "> Unsigned_16'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := Floor_Unsigned_16 (-0.01);
         Assert (False, "< 0 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

   end Test_Floor_Unsigned_16;

   procedure Test_To_Microseconds (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit;
      use Interfaces;
      use Standard.AUnit.Assertions;
   begin
      Check ("1.0", Unsigned_32'(To_Microseconds (1.0)), 1_000_000);
      Check ("0.000_001", Unsigned_32'(To_Microseconds (0.000_001)), 1);

      declare
         Temp : Unsigned_32;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Integer_64'Last / 2e9) + 0.000_001);
         Assert (False, "> Integer_64/2e9 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_32;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Unsigned_32'Last) / 1_000_000 + 0.000_001);
         Assert (False, "> Unsigned_32'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      Check ("1.0", Unsigned_16'(To_Microseconds (0.05)), 50_000);
      Check ("0.000_001", Unsigned_16'(To_Microseconds (0.000_001)), 1);

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Integer_64'Last / 2e9) + 0.000_001);
         Assert (False, "> Integer_64/2e9 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Unsigned_16'Last) / 1_000_000 + 0.000_001);
         Assert (False, "> Unsigned_16'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

   end Test_To_Microseconds;

   procedure Test_Create_Leap_Second_Table (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Leap_Table : constant Leap_Second_Table_Type := Create (Leap_Table_File_Name);

      procedure Check
        (Message              : in String;
         Index_Num            : in Integer;
         Expected_Start_Year  : in Integer;
         Expected_Start_Month : in Month_Type;
         Expected_Leap_Second : in Integer)
      is
         use Standard.AUnit.Checks;
         use SAL.Time_Conversions.AUnit;
      begin
         Check
           (Message & ".Start_TAI",
            Leap_Table (Index_Num).Start_TAI_Time,
            To_TAI_Time (Expected_Start_Year,
                         Day_Of_Year (Expected_Start_Year, Expected_Start_Month),
                         0.0,
                         Absolute => True));

         Check
           (Message & ".Leap_Second",
            Leap_Table (Index_Num).Leap_Second,
            Expected_Leap_Second);
      end Check;
   begin
      Check ("",
             Index_Num            => 1,
             Expected_Start_Year  => 1961,
             Expected_Start_Month => Jan,
             Expected_Leap_Second => 1);

      Check ("",
             Index_Num            => 13,
             Expected_Start_Year  => 1968,
             Expected_Start_Month => Feb,
             Expected_Leap_Second => 4);

      Check ("",
             Index_Num            => 30,
             Expected_Start_Year  => 1991,
             Expected_Start_Month => Jan,
             Expected_Leap_Second => 26);

      Check ("",
             Index_Num            => 36,
             Expected_Start_Year  => 1999,
             Expected_Start_Month => Jan,
             Expected_Leap_Second => 32);
   end Test_Create_Leap_Second_Table;

   procedure Test_Bad_Create_Leap_Second_Table (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      declare
         Leap_Table : constant Leap_Second_Table_Type := Create (Bad_Leap_Table_File_Name);
         pragma Unreferenced (Leap_Table);
      begin
         null;
      end;
      Standard.AUnit.Assertions.Assert (False, "did not get exception");
   exception
   when E : SAL.Initialization_Error =>
      Standard.AUnit.Checks.Check
        ("",
         Ada.Exceptions.Exception_Message (E),
         Bad_Leap_Table_File_Name &
           ": 9:0: start time not equal to previous end time.");
   end Test_Bad_Create_Leap_Second_Table;

   procedure Test_Calculate_Day_Of_Year (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      procedure Check
        (Message      : in String;
         Year         : in Integer;
         Month        : in Month_Type;
         Expected_Day : in Integer)
      is begin
         Standard.AUnit.Checks.Check (Message, Day_Of_Year (Year, Month), Expected_Day);
      end Check;
   begin
      Check
        ("1",
         Year         => 1961,
         Month        => May,
         Expected_Day => 121);

      Check
        ("2",
         Year         => 1960,
         Month        => Dec,
         Expected_Day => 336);

   end Test_Calculate_Day_Of_Year;

   procedure Test_TAI_To_UTC (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Leap_Table : constant Leap_Second_Table_Type := Create (Leap_Table_File_Name);

      --  We assume To_TAI_Time is correct; we are only testing the
      --  adding and subtracting of leap seconds found in leap_table.

      type Expected_Type is record
         TAI          : Time_Type;
         Leap_Seconds : Integer;
      end record;

      function To_TAI (Year : in Integer) return Time_Type
      is begin
         return To_TAI_Time (Year, 1, 0.0, Absolute => True);
      end To_TAI;

      Expected_Table : constant array (1 .. 7) of Expected_Type :=
        (1 => (To_TAI (1960), 0),   -- before leap second table
         2 => (To_TAI (1961), 1),   -- first line in table
         3 => (To_TAI (1970), 4),   -- random line
         4 => (To_TAI (1958), 0),   -- TAI origin
         5 => (To_TAI (2006), 33),  -- random line
         6 => (To_TAI (2009), 34),  -- last line in table
         7 => (To_TAI (2069), 34)); -- after table

      procedure Check (Index : in Integer)
      is
         use SAL.Time_Conversions.AUnit;
         Expected_Leap_Seconds : constant Time_Type := Time_Type (Expected_Table (Index).Leap_Seconds);
         Expected_TAI          : Time_Type renames Expected_Table (Index).TAI;
         Expected_UTC          : constant Time_Type := Expected_TAI - Expected_Leap_Seconds;

         Computed_UTC : constant Time_Type := TAI_To_UTC (Expected_Table (Index).TAI, Leap_Table);
         Computed_TAI : constant Time_Type := UTC_To_TAI (Computed_UTC, Leap_Table);
      begin
         Check (Integer'Image (Index) & ".TAI_To_UTC", Computed_UTC, Expected_UTC);
         Check (Integer'Image (Index) & ".UTC_To_TAI", Computed_TAI, Expected_TAI);
      end Check;
   begin
      for I in Expected_Table'Range loop
         Check (I);
      end loop;
   end Test_TAI_To_UTC;

   procedure Test_GPS (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Time_Conversions.AUnit;


      procedure Check
        (Message      : in String;
         Weeks        : in Interfaces.Unsigned_16;
         Seconds      : in Time_Type;
         Expected_TAI : in Time_Type)
      is
         use SAL.Interfaces_More.AUnit;
         GPS_Time : GPS_Time_Type;
         TAI_Time : constant Time_Type := To_TAI_Time (GPS_Time_Type'(Weeks, Seconds));
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         GPS_Time := To_GPS_Time (TAI_Time);

         Check (Message & ".Weeks", GPS_Time.Weeks, Weeks);
         Check (Message & ".Seconds", GPS_Time.Seconds_Of_Week, Seconds);

      end Check;
   begin
      --  Test for the GPS to/from TAI conversions
      Check ("0", GPS_Epoch_TAI, To_TAI_Time (GPS_Epoch_TAI_String, True));

      Check
        ("1",
         Weeks        => 0,
         Seconds      => 0.0,
         Expected_TAI => GPS_Epoch_TAI);

      Check
        ("2",
         Weeks        => 1629,
         Seconds      => 400522.0,
         Expected_TAI => GPS_Epoch_TAI + 985619722.0);
      --  (+ (* 1629 604800.0) 400522.00)

      Check
        ("3",
         Weeks        => 503,
         Seconds      => 319322.10,
         Expected_TAI => GPS_Epoch_TAI + 304533722.1);
      --  (+ (* 503 604800.0) 319322.10)
   end Test_GPS;

   procedure Test_Calendar (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Time_Conversions.AUnit;
      use Ada.Calendar;

      type Test_Time_Type is record
         Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration;
      end record;

      procedure Check_1
        (Message      : in String;
         Test_Time    : in Test_Time_Type;
         Expected_TAI : in Time_Type)
      is
         use Standard.AUnit.Checks;
         Cal_Time : constant Time := Time_Of (Test_Time.Year, Test_Time.Month, Test_Time.Day, Test_Time.Seconds);

         TAI_Time  : constant Time_Type := To_TAI_Time (Cal_Time);
         Cal_Temp  : Time;
         Test_Temp : Test_Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         Cal_Temp := To_Calendar_Time (TAI_Time);

         Split (Cal_Temp, Test_Temp.Year, Test_Temp.Month, Test_Temp.Day, Test_Temp.Seconds);

         Check (Message & ".Year", Test_Temp.Year, Test_Time.Year);
         Check (Message & ".Month", Test_Temp.Month, Test_Time.Month);
         Check (Message & ".Day", Test_Temp.Day, Test_Time.Day);
         Check (Message & ".Seconds", Test_Temp.Seconds, Test_Time.Seconds);

      end Check_1;

      procedure Check_2
        (Message           : in String;
         Test_Time         : in Time_Type;
         Expected_Calendar : in Test_Time_Type)
      is
         use Standard.AUnit.Checks;
         Computed_Cal_Time  : constant Time := To_Calendar_Time (Test_Time);
         Computed_Test_Time : Test_Time_Type;
      begin
         Split
           (Computed_Cal_Time,
            Computed_Test_Time.Year,
            Computed_Test_Time.Month,
            Computed_Test_Time.Day,
            Computed_Test_Time.Seconds);

         Check (Message & ".Year", Computed_Test_Time.Year, Expected_Calendar.Year);
         Check (Message & ".Month", Computed_Test_Time.Month, Expected_Calendar.Month);
         Check (Message & ".Day", Computed_Test_Time.Day, Expected_Calendar.Day);
         Check (Message & ".Seconds", Computed_Test_Time.Seconds, Expected_Calendar.Seconds);

      end Check_2;
   begin
      --  Test Ada.Calendar to/from TAI conversions

      Check_1 ("0", (1958, 1, 1, 0.0), 0.0);
      Check_1 ("1", (1958, 1, 1, 1.0), 1.0);
      Check_1 ("2", (1958, 1, 8, 0.0), Seconds_Per_Week);
      Check_1 ("3", (1958, 2, 1, 0.0), 31 * Seconds_Per_Day);
      Check_1 ("4", (1959, 1, 1, 0.0), Seconds_Per_Year);
      Check_1
        ("5", (1961, 2, 3, 5.0), 2 * Seconds_Per_Year + Seconds_Per_Leap_Year + (31 + 2) * Seconds_Per_Day + 5.0);

      --  errors from smm
      Check_2 ("10", 1319330827.301000000, (1999, 10, 23, 2827.301000000));
      Check_2 ("11", 1319073468.101000000, (1999, 10, 20, 4668.101000000));
      Check_2 ("12", 1729442442.194000000, (2012, 10, 20, 60042.194000000));
      Check_2 ("13", 1318934171.728000000, (1999, 10, 18, 38171.728000000));
      Check_2 ("14", 1319073468.101000000, (1999, 10, 20, 4668.101000000));
   end Test_Calendar;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Leap_Year'Access, "Test_Leap_Year");
      Register_Routine (T, Test_Year_Day_Seconds'Access, "Test_Year_Day_Seconds");
      Register_Routine (T, Test_Days_Seconds'Access, "Test_Days_Seconds");
      Register_Routine (T, Test_Hour_Minute_Seconds'Access, "Test_Hour_Minute_Seconds");
      Register_Routine (T, Test_String_TAI'Access, "Test_String_TAI");
      Register_Routine (T, Test_Extended_ASIST_TAI'Access, "Test_Extended_ASIST_TAI");
      Register_Routine (T, Test_TAI_Julian_Convert'Access, "Test_TAI_Julian_Convert");
      Register_Routine (T, Test_Julian_TAI_Convert'Access, "Test_Julian_TAI_Convert");
      Register_Routine (T, Test_J2000_Julian_Centuries'Access, "Test_J2000_Julian_Centuries");
      Register_Routine (T, Test_Extended_String_TAI'Access, "Test_Extended_String_TAI");
      Register_Routine (T, Test_Floor_Unsigned_16'Access, "Test_Floor_Unsigned_16");
      Register_Routine (T, Test_To_Microseconds'Access, "Test_To_Microseconds");
      Register_Routine (T, Test_Calculate_Day_Of_Year'Access, "Test_Calculate_Day_Of_Year");
      Register_Routine (T, Test_Create_Leap_Second_Table'Access, "Test_Create_Leap_Second_Table");
      Register_Routine (T, Test_Bad_Create_Leap_Second_Table'Access, "Test_Bad_Create_Leap_Second_Table");
      Register_Routine (T, Test_TAI_To_UTC'Access, "Test_TAI_To_UTC");
      Register_Routine (T, Test_GPS'Access, "Test_GPS");
      Register_Routine (T, Test_Calendar'Access, "Test_Calendar");
   end Register_Tests;

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'(GNAT.Source_Info.File);
   end Name;

end SAL.Time_Conversions.Test;
