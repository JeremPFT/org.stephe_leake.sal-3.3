--  ABSTRACT :
--
--  SEE spec.
--
--  Copyright (C) 2004 - 2013, 2015 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
package body SAL.Time_Conversions is

   Leap_Year_Month_Array : constant array (Month_Type) of Integer :=
     (1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336);
   --  First day of month as day of year

   Non_Leap_Year_Month_Array : constant array (Month_Type) of Integer :=
     (1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335);

   function Leap_Seconds
     (TAI_Time           : in Time_Type;
      Leap_Seconds_Table : in Leap_Second_Table_Type)
     return Integer
   is begin
      if TAI_Time < Leap_Seconds_Table (Leap_Seconds_Table'First).Start_TAI_Time then
         --   TAI time before table start; assume 0 leap seconds
         return 0;
      else
         for I in reverse Leap_Seconds_Table'First + 1 .. Leap_Seconds_Table'Last loop
            if TAI_Time >= Leap_Seconds_Table (I - 1).Start_TAI_Time and
              TAI_Time < Leap_Seconds_Table (I).Start_TAI_Time
            then
               return Leap_Seconds_Table (I - 1).Leap_Second;
            end if;
         end loop;
      end if;

      return Leap_Seconds_Table (Leap_Seconds_Table'Last).Leap_Second;
   end Leap_Seconds;

   function TAI_To_UTC
     (TAI_Input_Time     : in Time_Type;
      Leap_Seconds_Table : in Leap_Second_Table_Type)
     return Time_Type
   is
      Leap : constant Integer := Leap_Seconds (TAI_Input_Time, Leap_Seconds_Table);
   begin
      --  [2] file name is UTC-TAI, but the column header says leap_seconds = TAI - UTC
      --  http://tycho.usno.navy.mil/leapsec.html says TAI = UTC + leap_seconds
      return TAI_Input_Time - Time_Type (Leap);
   end TAI_To_UTC;

   function UTC_To_TAI
     (UTC_Input_Time     : in Time_Type;
      Leap_Seconds_Table : in Leap_Second_Table_Type)
     return Time_Type
   is
   begin
      if UTC_Input_Time < Leap_Seconds_Table (Leap_Seconds_Table'First).Start_UTC_Time then
         --   UTC time before table start; assume 0 leap seconds
         return UTC_Input_Time;
      else
         for I in reverse Leap_Seconds_Table'First + 1 .. Leap_Seconds_Table'Last loop
            if UTC_Input_Time >= Leap_Seconds_Table (I - 1).Start_UTC_Time and
              UTC_Input_Time < Leap_Seconds_Table (I).Start_UTC_Time
            then
               --  [2] file name is UTC-TAI, but the column header says leap_seconds = TAI - UTC
               --  http://tycho.usno.navy.mil/leapsec.html says TAI = UTC + leap_seconds
               return UTC_Input_Time + Time_Type ((Leap_Seconds_Table (I - 1).Leap_Second));
            end if;
         end loop;
      end if;

      return UTC_Input_Time + Time_Type (Leap_Seconds_Table (Leap_Seconds_Table'Last).Leap_Second);
   end UTC_To_TAI;

   function Create (File_Name : in String) return Leap_Second_Table_Type
   is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
      use Ada.Integer_Text_IO;

      Header_Line_Count : constant := 7;

      File                : File_Type;
      Line                : String (1 .. 128); --  standard file has no line longer than 128 chars
      Year_Start          : Integer;
      Year_End            : Integer;
      Month_Start         : Integer;
      Month_End           : Integer;
      Last_Read           : Integer;
      Line_Length         : Integer;
      String_Month_Start  : Month_Type;
      String_Month_End    : Month_Type;
      Leap_Second         : Float;
      Leap_Second_Int     : Integer;
      Start_TAI_Time      : Time_Type;
      Previous_Year_Start : Integer := 1961;   --  The first year in the file is always 1961
      Previous_Year_End   : Integer := 1961;
      Record_Index        : Integer := 0;
   begin
      begin
         Open (File, In_File, File_Name);
      exception
      when others =>
         raise Initialization_Error with "leap second file " & File_Name & " cannot be opened";
      end;

      --  Skip header for line count
      Skip_Line (File, Header_Line_Count);

      Count_Lines :
      begin
         loop
            --  This includes comments, but we will ignore them in the next loop.
            Skip_Line (File);
            Record_Index := Record_Index + 1;
         end loop;
      exception
      when End_Error =>
         null;
      end Count_Lines;

      declare
         Leap_Table   : Leap_Second_Table_Type (1 .. Record_Index);
         End_Tai_Time : array (1 .. Record_Index) of Time_Type;
      begin
         Reset (File);

         Skip_Line (File, Header_Line_Count);

         Record_Index := 0;

         loop
            --  Exit on End_Error

            --  Get the whole line
            Get_Line (File => File, Item => Line, Last => Line_Length);

            if Line (1 .. 5) = " ----" then
               --  skip any line that is a comment
               Skip_Line (File);

            else
               if Line (1 .. 5) = "     " then
                  Year_Start := Previous_Year_Start;
               else
                  Get (From => Line, Item => Year_Start, Last => Last_Read);
               end if;

               String_Month_Start := Month_Type'Value (Line (8 .. 10));

               if Line (18 .. 21) /= "    " then
                  Get (From => Line (17 .. Line'Last), Item => Year_End, Last => Last_Read);
               else
                  Year_End := Previous_Year_End;
               end if;

               if Line (24 .. 29) = "      " then
                  String_Month_End := Month_Type'Value ("Jan");
               else
                  String_Month_End := Month_Type'Value (Line (24 .. 26));
               end if;

               Get (From => Line (31 .. Line'Last), Item => Leap_Second, Last => Last_Read);

               Month_Start := Day_Of_Year (Year_Start, String_Month_Start);
               Month_End   := Day_Of_Year (Year_End, String_Month_End);

               Record_Index := Record_Index + 1;

               Leap_Second_Int := Integer (Float'Rounding (Leap_Second));
               Start_TAI_Time  := To_TAI_Time (Year_Start, Month_Start, 0.0, True);

               Leap_Table (Record_Index) :=
                 (Leap_Second    => Leap_Second_Int,
                  Start_TAI_Time => Start_TAI_Time,
                  Start_UTC_Time => Start_TAI_Time - Time_Type (Leap_Second));

               End_Tai_Time (Record_Index) := To_TAI_Time (Year_End, Month_End, 0.0, True);

               Previous_Year_Start := Year_Start;
               Previous_Year_End := Year_End;

               --   Check if the start time is equal to the previous end time.
               if Record_Index > Leap_Table'First then
                  if End_Tai_Time (Record_Index - 1) /= Leap_Table (Record_Index).Start_TAI_Time then
                     raise Initialization_Error with File_Name & ":" & Count'Image (Ada.Text_IO.Line (File) - 1) &
                       ":0: start time not equal to previous end time.";
                  end if;
               end if;

            end if;
         end loop;

      exception
      when End_Error =>
         Close (File);
         --  Normal exit.
         return Leap_Table (Leap_Table'First .. Record_Index);

      when Initialization_Error =>
         raise;

      when Data_Error =>
         declare
            use Ada.Strings;
            use Ada.Strings.Fixed;
            Line_Column : constant String := ":" &
              Trim (Ada.Text_IO.Count'Image (Ada.Text_IO.Line (File) - 1), Both) & ":" &
              Trim (Ada.Text_IO.Count'Image (Col (File)), Both);
         begin
            Close (File);
            raise Initialization_Error with File_Name & Line_Column & ": error: malformed input";
         end;

      when E : others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Initialization_Error with
           File_Name & ":0:0: error: " & Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E);
      end;
   end Create;

   function Day_Of_Year
     (In_Year  : in Integer;
      In_Month : in Month_Type)
     return Integer
   is begin
      if not Leap_Year (In_Year) then
         return Non_Leap_Year_Month_Array (In_Month);
      else
         return Leap_Year_Month_Array (In_Month);
      end if;
   end Day_Of_Year;

   function Floor (Item : in Time_Type) return Time_Type
   is
      use Interfaces;
      Temp : constant Time_Type := Time_Type (Integer_64 (Item));
   begin
      if Temp > Item then
         return Temp - 1.0;
      else
         return Temp;
      end if;
   end Floor;

   function Floor (Item : in Time_Type) return Integer
   is
      Temp : Integer;
   begin
      if not (Item in Time_Type (Integer'First) .. Time_Type (Integer'Last)) then
         raise Range_Error;
      end if;

      Temp := Integer (Item);

      if Time_Type (Temp) > Item then
         return Temp - 1;
      else
         return Temp;
      end if;
   end Floor;

   function Modulo (Dividend, Divisor : in Time_Type) return Time_Type
   is
      --  match names in Ada LRM 4.5.5

      A : Time_Type renames Dividend;
      B : Time_Type renames Divisor;

      N : constant Time_Type := Floor (A / B);
   begin
      return A - B * N;
   end Modulo;

   function Leap_Year (Year : in Integer) return Boolean
   is begin
      if Year mod 100 = 0 then
         return Year mod 400 = 0;
      else
         return Year mod 4 = 0;
      end if;
   end Leap_Year;

   procedure To_Year_Day_Seconds
     (TAI_Time       : in     Time_Type;
      Year           :    out Integer;
      Day_In_Year    :    out Integer;
      Seconds_In_Day :    out Time_Type)
   is
      Temp_Year    : Integer   := 1958;
      Temp_Day     : Integer   := 1;
      Temp_Seconds : Time_Type := TAI_Time;
   begin
      Find_Year :
      loop
         if Leap_Year (Temp_Year) then
            exit Find_Year when Temp_Seconds < Seconds_Per_Leap_Year;

            Temp_Seconds := Temp_Seconds - Seconds_Per_Leap_Year;
         else
            exit Find_Year when Temp_Seconds < Seconds_Per_Year;

            Temp_Seconds := Temp_Seconds - Seconds_Per_Year;
         end if;

         Temp_Year := Temp_Year + 1;
      end loop Find_Year;

      Year := Temp_Year;

      Find_Day :
      loop
         exit Find_Day when Temp_Seconds < Seconds_Per_Day;

         Temp_Seconds := Temp_Seconds - Seconds_Per_Day;

         Temp_Day := Temp_Day + 1;
      end loop Find_Day;

      Day_In_Year := Temp_Day;

      Seconds_In_Day := Temp_Seconds;

   end To_Year_Day_Seconds;

   procedure To_Days_Seconds
     (TAI_Time       : in     Time_Type;
      Days           :    out Integer;
      Seconds_In_Day :    out Time_Type)
   is
      Temp_Days : Integer := 0;
      Temp_Seconds : Time_Type := TAI_Time;
   begin
      Find_Day :
      loop
         exit Find_Day when Temp_Seconds < Seconds_Per_Day;

         Temp_Seconds := Temp_Seconds - Seconds_Per_Day;

         Temp_Days := Temp_Days + 1;
      end loop Find_Day;

      Days := Temp_Days;

      Seconds_In_Day := Temp_Seconds;

   end To_Days_Seconds;

   function To_Julian_Day (TAI_Time : in Time_Type) return Long_Float
   is begin
      return Julian_Days_1958_TAI + Long_Float (TAI_Time) * Julian_Days_Per_Second;
   end To_Julian_Day;

   function To_Julian_Century (TAI_Time : in Time_Type) return Long_Float
   is begin
      return Julian_Centuries_1958_TAI + Long_Float (TAI_Time) * Julian_Centuries_Per_Second;
   end To_Julian_Century;

   function Seconds_To_Julian_Centuries (TAI_Time : in Time_Type) return Long_Float
   is begin
      return Long_Float (TAI_Time) * Julian_Centuries_Per_Second;
   end Seconds_To_Julian_Centuries;

   function To_J2000_Julian_Centuries (TAI_Time : in Time_Type) return Long_Float
   is begin
      return (Long_Float (TAI_Time) - (Julian_Days_J2000_TAI - Julian_Days_1958_TAI) * Seconds_Per_Day)
        * Julian_Centuries_Per_Second;
   end To_J2000_Julian_Centuries;

   function Julian_Days_TT_To_Seconds_TAI (Julian_Days : in Long_Float) return Time_Type
   is begin
      return Time_Type (Julian_Days - TT_Offset_Days - Julian_Days_1958_TAI) / Julian_Days_Per_Second;
   end Julian_Days_TT_To_Seconds_TAI;

   function Julian_Days_TAI_To_Seconds_TAI (Julian_Days : in Long_Float) return Time_Type
   is begin
      return Time_Type (Julian_Days - Julian_Days_1958_TAI) / Julian_Days_Per_Second;
   end Julian_Days_TAI_To_Seconds_TAI;

   function To_TAI_Time
     (Year           : in Integer;
      Day_In_Year    : in Integer;
      Seconds_In_Day : in Time_Type;
      Absolute       : in Boolean)
     return Time_Type
   is
      Result : Time_Type;
   begin
      if Absolute then
         --  compute days since UTC origin
         if Year < 1958 then
            raise Range_Error;
         end if;

         Result := Time_Type (Days_Per_Year * (Year - 1958) + ((Year - 1957) / 4) + (Day_In_Year - 1));
      else
         --  ignore leap days, since we don't know the year origin
         Result := Time_Type (Days_Per_Year * Year + Day_In_Year - 1);
      end if;

      return Result * Seconds_Per_Day + Seconds_In_Day;
   end To_TAI_Time;

   function To_TAI_Time
     (Days           : in Integer;
      Seconds_Of_Day : in Time_Type)
     return Time_Type
   is
      Result : constant Time_Type := Time_Type (Days);
   begin
      return Result * Seconds_Per_Day + Seconds_Of_Day;
   end To_TAI_Time;

   function To_TAI_Time
     (Hours             : in Integer;
      Minutes           : in Integer;
      Seconds_In_Minute : in Time_Type)
     return Time_Type
   is begin
      return Seconds_In_Minute + 60.0 * (Time_Type (Minutes) + 60.0 * Time_Type (Hours));
   end To_TAI_Time;

   function To_Time (Time : in Ada.Real_Time.Time) return Time_Type
   is
      use Ada.Real_Time;
      Seconds : Seconds_Count;
      Span    : Time_Span;
   begin
      Split (Time, Seconds, Span);
      return Time_Type (Seconds) + Time_Type (To_Duration (Span));
   end To_Time;

   procedure To_Hour_Minute_Seconds
     (Seconds           : in     Time_Type;
      Hour              :    out Integer;
      Minute            :    out Integer;
      Seconds_In_Minute :    out Time_Type)
   is
      Temp_Seconds : Time_Type := Seconds;
   begin
      Hour := Integer (Temp_Seconds) / 3600;
      Temp_Seconds := Temp_Seconds - Time_Type (3600 * Hour);
      Minute := Integer (Temp_Seconds) / 60;
      Seconds_In_Minute := Temp_Seconds - Time_Type (60 * Minute);
   end To_Hour_Minute_Seconds;

   ----------
   --  Conversions for counter/timers

   function Floor_Unsigned_16 (Item : in Time_Type) return Interfaces.Unsigned_16
   is
      use Interfaces;
      Temp : Unsigned_16;
   begin
      if not (Item in 0.0 .. Time_Type (Unsigned_16'Last)) then
         raise Range_Error;
      end if;

      Temp := Unsigned_16 (Item);

      if Time_Type (Temp) > Item then
         return Temp - 1;
      else
         return Temp;
      end if;
   end Floor_Unsigned_16;

   function Floor_Unsigned_32 (Item : in Time_Type) return Interfaces.Unsigned_32
   is
      use Interfaces;
      Temp : Unsigned_32;
   begin
      if not (Item in 0.0 .. Time_Type (Unsigned_32'Last)) then
         raise Range_Error;
      end if;

      Temp := Unsigned_32 (Item);

      if Time_Type (Temp) > Item then
         return Temp - 1;
      else
         return Temp;
      end if;
   end Floor_Unsigned_32;

   function To_Time (Microseconds : in Interfaces.Unsigned_16) return SAL.Time_Conversions.Time_Type
   is begin
      return Time_Type (Microseconds) / 1_000_000;
   end To_Time;

   function To_Time (Microseconds : in Interfaces.Unsigned_32) return SAL.Time_Conversions.Time_Type
   is begin
      return Time_Type (Microseconds) / 1_000_000;
   end To_Time;

   function To_Microseconds (Time : in Time_Type) return Interfaces.Unsigned_16
   is
      use Interfaces;
   begin
      declare
         Temp : constant Integer_64 := Integer_64 (Time * 1_000_000);
      begin
         if Temp in Integer_64 (Unsigned_16'First) .. Integer_64 (Unsigned_16'Last) then
            return Unsigned_16 (Temp);
         else
            Ada.Exceptions.Raise_Exception
              (Range_Error'Identity,
               Time_Type'Image (Time) & " outside range of Unsigned_16 microseconds");
         end if;
      end;
   exception
   when Constraint_Error =>
      raise Range_Error with Time_Type'Image (Time) & " outside range of Integer_64 microseconds";
   end To_Microseconds;

   function To_Microseconds (Time : in Time_Type) return Interfaces.Unsigned_32
   is
      use Interfaces;
   begin
      declare
         Temp : constant Integer_64 := Integer_64 (Time * 1_000_000);
      begin
         if Temp in Integer_64 (Unsigned_32'First) .. Integer_64 (Unsigned_32'Last) then
            return Unsigned_32 (Temp);
         else
            raise Range_Error with Time_Type'Image (Time) & " outside range of Unsigned_32 microseconds";
         end if;
      end;
   exception
   when Constraint_Error =>
      raise Range_Error with Time_Type'Image (Time) & " outside range of Integer_64 microseconds";
   end To_Microseconds;

   function To_Microseconds (Time : in Time_Type) return SAL.Interfaces_More.Unsigned_15
   is
      use Interfaces;
      use SAL.Interfaces_More;
   begin
      declare
         Temp : constant Integer_64 := Integer_64 (Time * 1_000_000);
      begin
         if Temp in Integer_64 (Unsigned_15'First) .. Integer_64 (Unsigned_15'Last) then
            return Unsigned_15 (Temp);
         else
            raise Range_Error with Time_Type'Image (Time) & " outside range of Unsigned_15 microseconds";
         end if;
      end;
   exception
   when Constraint_Error =>
      raise Range_Error with Time_Type'Image (Time) & " outside range of Integer_64 microseconds";
   end To_Microseconds;

   function Checked_Unsigned_16 (Label : in String; Item : in Time_Type) return Interfaces.Unsigned_16
   is begin
      if not (Item in 0.0 .. Time_Type (Interfaces.Unsigned_16'Last)) then
         Ada.Exceptions.Raise_Exception
           (Range_Error'Identity,
            Label & Time_Type 'Image (Item) & " not in range of Unsigned_16");
      else
         return Interfaces.Unsigned_16 (Item);
      end if;
   end Checked_Unsigned_16;

   ----------
   --  ASIST time strings

   function To_TAI_Time_1
     (Time      : in String;
      Absolute  : in Boolean;
      Extended  : in Boolean;
      Day_First : in Integer)
     return Time_Type
   is
      Year    : Integer            := Integer'Value (Time (Time'First .. Day_First - 2));
      Day     : constant Integer   := Integer'Value (Time (Day_First ..  Day_First + 2));
      Hour    : constant Integer   := Integer'Value (Time (Day_First + 4 ..  Day_First + 5));
      Minute  : constant Integer   := Integer'Value (Time (Day_First + 7 .. Day_First + 8));
      Seconds : constant Time_Type := Time_Type'Value (Time (Day_First + 10 .. Day_First + 15));
   begin
      if Absolute and not Extended then
         if Year < 70 then
            Year := 2000 + Year;
         else
            Year := 1900 + Year;
         end if;
      end if;

      return To_TAI_Time (Year, Day, To_TAI_Time (Hour, Minute, Seconds), Absolute);

   end To_TAI_Time_1;

   function To_TAI_Time
     (Time     : in String;
      Absolute : in Boolean)
     return Time_Type
   is begin
      if Time (Time'First + 2) = '-' then
         return To_TAI_Time_1 (Time, Absolute, Extended => False, Day_First => Time'First + 3);
      elsif Time (Time'First + 4) = '-' then
         return To_TAI_Time_1 (Time, Absolute, Extended => True, Day_First => Time'First + 5);
      else
         raise Invalid_Format with """" & Time & """ invalid time format: expecting [YY]YY-DDD-HH:MM:SS.LLL";
      end if;
   exception
   when Constraint_Error =>
      raise Invalid_Format with """" & Time & """ invalid time format: expecting [YY]YY-DDD-HH:MM:SS.LLL";
   end To_TAI_Time;

   function To_ASIST_String (Time_TAI : in Time_Type) return ASIST_Time_String_Type
   is
      Year    : Integer;
      Day     : Integer;
      Hour    : Integer;
      Minute  : Integer;
      Seconds : Time_Type;
      Result  : ASIST_Time_String_Type;
   begin
      To_Year_Day_Seconds (Time_TAI, Year, Day, Seconds);

      Year := Year mod 100;

      To_Hour_Minute_Seconds (Seconds, Hour, Minute, Seconds);

      Result (1 ..  2)  := Image (Item => Year, Width => 2);
      Result (3)        := '-';
      Result (4 ..  6)  := Image (Item => Day, Width => 3);
      Result (7)        := '-';
      Result (8 ..  9)  := Image (Item => Hour, Width => 2);
      Result (10)       := ':';
      Result (11 .. 12) := Image (Item => Minute, Width => 2);
      Result (13)       := ':';
      Result (14 .. 19) := Image (Item => Seconds, Fore => 2, Aft => 3);
      return Result;
   end To_ASIST_String;

   function To_Extended_ASIST_String (Time_TAI : in Time_Type) return Extended_ASIST_Time_String_Type
   is
      Year    : Integer;
      Day     : Integer;
      Hour    : Integer;
      Minute  : Integer;
      Seconds : Time_Type;
      Result  : Extended_ASIST_Time_String_Type;
   begin
      To_Year_Day_Seconds (Time_TAI, Year, Day, Seconds);

      To_Hour_Minute_Seconds (Seconds, Hour, Minute, Seconds);

      Result (1 ..  4)  := Image (Item => Year, Width => 4);
      Result (5)        := '-';
      Result (6 ..  8)  := Image (Item => Day, Width => 3);
      Result (9)        := '-';
      Result (10 .. 11) := Image (Item => Hour, Width => 2);
      Result (12)       := ':';
      Result (13 .. 14) := Image (Item => Minute, Width => 2);
      Result (15)       := ':';
      Result (16 .. 21) := Image (Item => Seconds, Fore => 2, Aft => 3);
      return Result;
   end To_Extended_ASIST_String;

   function To_GPS_Time (TAI : in Time_Type) return GPS_Time_Type
   is
      Weeks   : Integer   := 0;
      Seconds : Time_Type := TAI - GPS_Epoch_TAI;
      Result  : GPS_Time_Type;
   begin
      Find_Week  :
      loop
         exit Find_Week when Seconds < Seconds_Per_Week;
         Seconds := Seconds - Seconds_Per_Week;
         Weeks   := Weeks + 1;
      end loop Find_Week;
      Result.Weeks := Interfaces.Unsigned_16 (Weeks);
      Result.Seconds_Of_Week := Seconds;
      return Result;
   end To_GPS_Time;

   function To_TAI_Time (GPS : in GPS_Time_Type) return Time_Type
   is
   begin
      return GPS_Epoch_TAI + GPS.Seconds_Of_Week + (Time_Type (GPS.Weeks) * 7 * 24 * 60 * 60);
   end To_TAI_Time;

   function To_Month_Type (Month : in Ada.Calendar.Month_Number) return Month_Type
   is begin
      return Month_Type'Val (Month - 1);
   end To_Month_Type;

   function To_Month_Number (Month : in Month_Type) return Ada.Calendar.Month_Number
   is begin
      return Month_Type'Pos (Month) + 1;
   end To_Month_Number;

   function To_TAI_Time (Cal : in Ada.Calendar.Time) return Time_Type
   is
      use Ada.Calendar;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Cal, Year, Month, Day, Seconds);

      return To_TAI_Time (Year, Day_Of_Year (Year, To_Month_Type (Month)) + Day - 1, Time_Type (Seconds), True);
   end To_TAI_Time;

   function To_Calendar_Time (TAI : in Time_Type) return Ada.Calendar.Time
   is
      use Ada.Calendar;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Integer;
      Seconds : Day_Duration;
   begin
      To_Year_Day_Seconds (TAI, Year, Day, Time_Type (Seconds));

      --  Convert Day to Month, Day_In_Month
      for I in reverse Month_Type loop
         if Leap_Year (Year) then
            if Day >= Leap_Year_Month_Array (I) then
               Month := To_Month_Number (I);
               Day := Day - Leap_Year_Month_Array (I) + 1;
               exit;
            end if;
         else
            if Day >= Non_Leap_Year_Month_Array (I) then
               Month := To_Month_Number (I);
               Day := Day - Non_Leap_Year_Month_Array (I) + 1;
               exit;
            end if;
         end if;
      end loop;

      return Time_Of (Year, Month, Day, Seconds);
   end To_Calendar_Time;

end SAL.Time_Conversions;
