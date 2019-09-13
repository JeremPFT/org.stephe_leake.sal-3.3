--  Abstract :
--
--  See spec
--
--  Copyright (C) 2003, 2004, 2008, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL.Config_Files; use SAL.Config_Files;
with SAL.File_Names;
package body Test.Config_Files.Abs_File is

   procedure Test_Env_Var (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin

      Ada.Environment_Variables.Set ("SAL_TEST_FILE", "test-config_files-abs.config");

      declare
         Expanded_File_Name : constant String := SAL.File_Names.Replace_Environment_Variables ("$SAL_TEST_FILE");
         Abs_File_Name      : constant String := GNAT.OS_Lib.Normalize_Pathname (Expanded_File_Name);

         Config : Configuration_Type;
      begin
         --  Delete Abs_File_Name if it exists, to erase previous tests.
         declare
            use Ada.Text_IO;
            File : File_Type;
         begin
            Open (File, In_File, Abs_File_Name);
            Delete (File);
         exception
         when Ada.IO_Exceptions.Name_Error =>
            null;
         end;

         begin
            Open (Config, Abs_File_Name, Missing_File => Ignore, Read_Only => False);

            Write (Config, "Hello", "Hello");

            Close (Config);

            AUnit.Assertions.Assert (True, ""); --  Just count the test
         exception
         when E : others =>
            AUnit.Assertions.Assert (False, "unexpected exceptions raised: " & Ada.Exceptions.Exception_Name (E));
         end;

         --  Cleanup; delete file
         declare
            use Ada.Text_IO;
            File : File_Type;
         begin
            Open (File, In_File, Abs_File_Name);
            Delete (File);
         end;
      end;
   end Test_Env_Var;

   procedure Test_Open_Close (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      File_Name : constant String := "test-config_files-abs.config";

      Abs_File_Name : constant String := GNAT.OS_Lib.Normalize_Pathname (File_Name);

      Config : Configuration_Type;
   begin

      --  Delete Abs_File_Name if it exists, to erase previous tests.
      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, Abs_File_Name);
         Delete (File);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         null;
      end;

      begin
         Open (Config, File_Name, Missing_File => Ignore, Read_Only => False);

         Write (Config, "Hello", "Hello");

         Close (Config);

         AUnit.Assertions.Assert (True, ""); --  Just count the test
      exception
      when others =>
         AUnit.Assertions.Assert (False, "unexpected exceptions raised");
      end;

      --  Cleanup; delete file
      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, File_Name);
         Delete (File);
      end;
   end Test_Open_Close;

   ----------
   --  public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test.Config_Files.Abs_File");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Env_Var'Access, "Test_Env_Var");
      Register_Routine (T, Test_Open_Close'Access, "Test_Open_Close");
   end Register_Tests;

end Test.Config_Files.Abs_File;
