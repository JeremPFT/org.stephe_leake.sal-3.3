--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2009, 2015, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
with AUnit.Checks;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Duplicate_Key is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-duplicate_key.config";

   ----------
   --  Local subprograms

   procedure Keep_Last_Duplicate (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Open (Config, File_Name, Duplicate_Key => Keep_Last);
      AUnit.Checks.Check ("", Read (Config, "duplicate_key", ""), "Last");
      Close (Config);
   end Keep_Last_Duplicate;

   procedure Keep_First_Duplicate (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Open (Config, File_Name, Duplicate_Key => Keep_First);
      AUnit.Checks.Check ("", Read (Config, "duplicate_key", ""), "First");
      Close (Config);
   end Keep_First_Duplicate;

   procedure Error_Duplicate (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

   begin
      Open (Config, File_Name, Duplicate_Key => Raise_Exception);
      Assert (False, "no exception");
   exception
   when E : SAL.Config_File_Error =>
      AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":2:0: duplicate key");
   end Error_Duplicate;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Keep_First_Duplicate'Access, "Keep_First_Duplicate");
      Register_Routine (T, Keep_Last_Duplicate'Access, "Keep_Last_Duplicate");
      Register_Routine (T, Error_Duplicate'Access, "Error_Duplicate");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;
      File : File_Type;
   begin
      Delete_File (File_Name);

      Create (File, Out_File, File_Name);
      Put_Line (File, "duplicate_key = First");
      Put_Line (File, "duplicate_key = Last");

      Close (File);
   end Set_Up_Case;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Config_Files.Duplicate_Key");
   end Name;

end Test.Config_Files.Duplicate_Key;
