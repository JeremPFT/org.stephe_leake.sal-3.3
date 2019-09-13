--  Abstract :
--
--  See spec
--
--  Copyright (C) 2005, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with AUnit.Checks;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Comment_No_Equal is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-comment_no_equal.config";

   ----------
   --  Local subprogram declarations

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class);

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Config_Files.Comment_No_Equal");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Run_Test'Access, "Run_Test");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;

      File : File_Type;
   begin
      Create (File, Out_File, File_Name);
      Put_Line (File, "# a comment");
      Put_Line (File, "! another comment");
      Put_Line (File, "[Geometry]");
      Put_Line (File, "Left_Top=( 10,  10)");
      Close (File);
      SAL.Config_Files.Open (Config, File_Name, Read_Only => True);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;
      File : File_Type;
   begin
      SAL.Config_Files.Close (Config);
      Open (File, In_File, File_Name);
      Delete (File);
   end Tear_Down_Case;

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      AUnit.Checks.Check ("[Geometry]", Is_Present (Config, "[Geometry]"), True);
      AUnit.Checks.Check ("Left_Top", Is_Present (Config, "Left_Top"), True);
   end Run_Test;

end Test.Config_Files.Comment_No_Equal;
