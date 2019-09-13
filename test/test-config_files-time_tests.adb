--  Abstract :
--
--  See spec
--
--  Copyright (C) 2003 - 2005, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with Ada.Calendar;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada_Calendar_Config; use Ada_Calendar_Config;
with GNAT.Calendar.Time_IO;
with SAL.Config_Files;    use SAL.Config_Files;
package body Test.Config_Files.Time_Tests is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-time.config";

   Default_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year => 2000, Month => 2, Day => 2);

   ----------
   --  Local subprogram declarations

   procedure Check_Time (Key : in String; Expected : in Ada.Calendar.Time);
   procedure Test_Time (T : in out AUnit.Test_Cases.Test_Case'Class);

   ----------
   --  Subprogram bodies (alphabetical order)

   procedure Check_Time (Key : in String; Expected : in Ada.Calendar.Time)
   is
      use GNAT.Calendar.Time_IO;
      use Ada.Calendar;
      Result : constant Ada.Calendar.Time := Read (Config, Key, Default_Time, Missing_Key => Raise_Exception);
   begin
      AUnit.Assertions.Assert
        (Result = Expected,
         Key & ": " &
           "Expecting " & Image (Expected, "%x %r") & ' ' &
           "Got " & Image (Result, "%x %r"));
   end Check_Time;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("time tests");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Time'Access,
                        "Write, Read time value");

   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Delete File_Name if it exists, to erase previous tests.
      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, File_Name);
         Delete (File);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         --  File did not exist.
         null;
      end;

      Open (Config, File_Name, Missing_File => Ignore, Read_Only => False);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Close (Config);
   end Tear_Down_Case;

   procedure Test_Time (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Calendar;
      A_Birthday : constant Ada.Calendar.Time := Time_Of (1984, 3, 5);
      A_Time     : constant Ada.Calendar.Time := Time_Of (1901, 1, 1, 1800.24);
   begin
      Write (Config, "A Birthday", A_Birthday);
      Check_Time ("A Birthday", A_Birthday);

      Write (Config, "A Time", A_Time);
      Check_Time ("A Time", A_Time);
   end Test_Time;

end Test.Config_Files.Time_Tests;
