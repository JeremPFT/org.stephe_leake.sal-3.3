--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 - 2006, 2009 - 2010, 2015 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions; use Ada.Exceptions;
with AUnit.Assertions;
with SAL.Config_Files.Boolean; use SAL.Config_Files.Boolean;
with SAL.Config_Files.Duration; use SAL.Config_Files.Duration;
with SAL.Config_Files.Integer; use SAL.Config_Files.Integer;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Error_Message is

   type Modular_Type is mod 8;
   function Read is new Read_Modular (Modular_Type);

   function Read is new Read_Float (Float);

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-error_message.config";

   ----------
   --  Local subprograms

   procedure Invalid_Enumeral (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Boolean;
      pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Enumeral");
      Assert (False, "Enumeral: no exception");
   exception
   when E : SAL.Config_File_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":1:11: invalid enumeral - expecting: FALSE TRUE ",
         "got " & Exception_Message (E));
   end Invalid_Enumeral;

   procedure Invalid_Integer (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Integer;
      pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Integer");
      Assert (False, "Integer: no exception");
   exception
   when E : SAL.Config_File_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":2:10: invalid integer syntax or range",
         "got " & Exception_Message (E));

   end Invalid_Integer;

   procedure Invalid_Modular (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Modular_Type;
      pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Modular");
      Assert (False, "Modular: no exception");
   exception
   when E : SAL.Config_File_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":3:10: invalid modular integer syntax or range, valid range is  0 to  7",
         "got " & Exception_Message (E));

   end Invalid_Modular;

   procedure Invalid_Float (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Float;
      pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Float");
      Assert (False, "Float: no exception");
   exception
   when E : SAL.Config_File_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":4:8: invalid floating point syntax or range",
         "got " & Exception_Message (E));

   end Invalid_Float;

   procedure Invalid_Fixed (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Duration;
      pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Fixed");
      Assert (False, "Fixed: no exception");
   exception
   when E : SAL.Config_File_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":5:8: invalid fixed point syntax or range",
         "got " & Exception_Message (E));

   end Invalid_Fixed;

   procedure Root_Key (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Duration;
      pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Root");
      Assert (False, "Root_Key: no exception");
   exception
   when E : SAL.Config_File_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":6:11: Root has no value",
         "got " & Exception_Message (E));

   end Root_Key;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Invalid_Enumeral'Access, "Invalid_Enumeral");
      Register_Routine (T, Invalid_Integer'Access, "Invalid_Integer");
      Register_Routine (T, Invalid_Modular'Access, "Invalid_Modular");
      Register_Routine (T, Invalid_Float'Access, "Invalid_Float");
      Register_Routine (T, Invalid_Fixed'Access, "Invalid_Fixed");
      Register_Routine (T, Root_Key'Access, "Root_Key");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Delete_File (File_Name);

      Open (Config, File_Name, Missing_File => Ignore, Read_Only => False);
      Write (Config, "Enumeral", "in"); -- bad spelling for Enumeral_Type
      Write (Config, "Integer", "one"); -- bad syntax for integer
      Write (Config, "Modular", -1);    -- bad syntax for modular
      Write (Config, "Float", "float"); -- bad syntax for float
      Write (Config, "Fixed", "fixed"); -- bad syntax for fixed
      Write (Config, "Root.Key", "Root_Key"); -- attempt to read root key

      --  Close and reopen, so line, column are set properly.
      Close (Config);
      Open (Config, File_Name, Missing_File => Raise_Exception, Read_Only => False, Duplicate_Key => Raise_Exception);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Don't delete file here, in case we want to look at it.
      Close (Config);
   end Tear_Down_Case;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test.Config_Files.Error_Message");
   end Name;

end Test.Config_Files.Error_Message;
