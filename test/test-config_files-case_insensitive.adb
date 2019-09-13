--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.IO_Exceptions;
with Ada.Text_IO;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Case_Insensitive is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-case_insensitive.config";

   ----------
   --  Local subprogram declarations

   procedure Test (T : in out AUnit.Test_Cases.Test_Case'Class);

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Config_Files.Case_Insensitive");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test'Access, "Test");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;

      File : File_Type;
   begin

      begin
         --  Delete the file if it currently exists, to erase previous
         --  tests.
         Open (File, In_File, File_Name);
         Delete (File);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         --  File did not exist.
         null;
      end;

      --  Write a file
      Create (File, Out_File, File_Name);

      Put_Line (File, "p1.a.size=1");

      Close (File);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, File_Name);
      Delete (File);
   end Tear_Down_Case;

   procedure Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin

      Open (Config, File_Name, Missing_File => Ignore, Read_Only => True, Case_Insensitive_Keys => True);

      AUnit.Assertions.Assert
        ("1" = Read (Config, "P1.A.Size"), "P1.A.Size");

      Close (Config);

   end Test;

end Test.Config_Files.Case_Insensitive;
