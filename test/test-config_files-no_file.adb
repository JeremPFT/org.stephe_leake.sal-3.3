--  Abstract :
--
--  See spec
--
--  Copyright (C) 2002 - 2004, 2009, 2015 Stephen Leake.  All Rights Reserved.
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

with Ada.IO_Exceptions;
with Ada.Text_IO;
with AUnit.Assertions;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.No_File is

   Config     : Configuration_Type;
   File_Name  : constant String := "test-config_files-no_file.config";

   ----------
   --  Local subprogram declarations

   procedure Test_Open_Error (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Read_Key_Error (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Write_Key_Error
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test.config_files.no_file");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open_Error'Access, "Test_Open_Error");
      Register_Routine (T, Test_Read_Key_Error'Access, "Test_Read_Key_Error");
      Register_Routine (T, Test_Write_Key_Error'Access, "Test_Write_Key_Error");
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
   end Set_Up_Case;

   procedure Test_Open_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Raised_Name_Error : Boolean;
   begin
      begin
         Open (Config, File_Name, Missing_File => Raise_Exception);
         Raised_Name_Error := False;
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Raised_Name_Error := True;
      end;

      AUnit.Assertions.Assert (Raised_Name_Error,
                               "Open did not raise Name_Error");
   end Test_Open_Error;

   procedure Test_Read_Key_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Raised_Config_File_Error : Boolean;
      Result                   : String (1 .. 10);
      Result_Last              : Integer;
   begin
      Open (Config, File_Name, Missing_File => Ignore);

      begin
         Read
           (Config,
            Key         => "Strings.Violins",
            Result      => Result,
            Result_Last => Result_Last,
            Missing_Key => Raise_Exception);

         Raised_Config_File_Error := False;
      exception
      when SAL.Config_File_Error =>
         Raised_Config_File_Error := True;
      end;

      Close (Config);

      AUnit.Assertions.Assert
        (Raised_Config_File_Error, "Read Strings.Violins did not raise Config_File_Error");
   end Test_Read_Key_Error;

   procedure Test_Write_Key_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Raised_Config_File_Error : Boolean;
   begin
      Open (Config, File_Name, Missing_File => Ignore, Read_Only => False);

      begin
         Write
           (Config,
            Key         => "Strings.Violins",
            Value       => "Stradivarious",
            Missing_Key => Raise_Exception);

         Raised_Config_File_Error := False;
      exception
      when SAL.Config_File_Error =>
         Raised_Config_File_Error := True;
      end;

      Close (Config);

      AUnit.Assertions.Assert
        (Raised_Config_File_Error, "Write Strings.Violins did not raise Config_File_Error");
   end Test_Write_Key_Error;

end Test.Config_Files.No_File;
