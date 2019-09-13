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

with Ada.Text_IO;
with SAL.Config_Files; use SAL.Config_Files;
with AUnit.Checks; use AUnit.Checks;
package body Test.Config_Files.Append_File is

   Config    : Configuration_Type;
   File_Name_1 : constant String := "test-config_files-append_file_1.config";
   File_Name_2 : constant String := "test-config_files-append_file_2.config";

   ----------
   --  Test procedures

   procedure Test_Open_Append (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Open (Config, File_Name_1, Missing_File => Raise_Exception, Read_Only => True, Duplicate_Key => Raise_Exception);
      Append (Config, File_Name_2, Missing_File => Raise_Exception);

      Check ("Resistance.1", Read (Config, "Resistance.1"), "Resistance_1");
      Check ("Resistance.1.M_A", Read (Config, "Resistance.1.M_A"), "1.0");
      Check ("Resistance.1.M_B", Read (Config, "Resistance.1.M_B"), "2.0");
      Check ("Resistance.1.Base_AB", Read (Config, "Resistance.1.Base_AB"), "3.0");

      Close (Config);
   end Test_Open_Append;

   ----------
   --  Public bodies (alphabetical order)

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test.Config_Files.Append_File");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open_Append'Access, "Test_Open_Append");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;

      File : File_Type;
   begin

      Delete_File (File_Name_1);
      Delete_File (File_Name_2);

      --  Write two files that will be merged
      Create (File, Out_File, File_Name_1);

      Put_Line (File, "# file_1");
      Put_Line (File, "Resistance.1.M_A = 1.0");
      Put_Line (File, "Resistance.1.M_B = 2.0");
      Put_Line (File, "Resistance.1.Base_AB = 3.0");

      Close (File);

      Create (File, Out_File, File_Name_2);

      Put_Line (File, "# file_2");
      Put_Line (File, "Resistance.1 = Resistance_1");

      Close (File);
   end Set_Up_Case;

end Test.Config_Files.Append_File;
