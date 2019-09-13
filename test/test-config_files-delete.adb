--  Abstract :
--
--  See spec
--
--  Copyright (C) 2009, 2015 Stephen Leake.  All Rights Reserved.
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

with Ada.Directories;  use Ada.Directories;
with GNAT.Source_Info;
with AUnit.Checks;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Delete is

   Config_File_Name : constant String := "test-config_files-delete.config";

   procedure Plain (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;

      Config : Configuration_Type;
   begin

      if Exists (Config_File_Name) then
         Delete_File (Config_File_Name);
      end if;

      Open (Config, Config_File_Name, Read_Only => False);

      Write (Config, "1", "one");
      Write (Config, "2", "two");
      Write (Config, "3", "three");

      --  Delete middle item
      --  Delete hidden by package name
      SAL.Config_Files.Delete (Config, "2");

      Check ("1.1", Is_Present (Config, "1"), True);
      Check ("1.2", Is_Present (Config, "2"), False);
      Check ("1.3", Is_Present (Config, "3"), True);

      --  Delete first item
      SAL.Config_Files.Delete (Config, "1");

      Check ("2.1", Is_Present (Config, "1"), False);
      Check ("2.2", Is_Present (Config, "2"), False);
      Check ("2.3", Is_Present (Config, "3"), True);

      --  Delete non-trivial last item
      Write (Config, "4", "four");

      SAL.Config_Files.Delete (Config, "4");

      Check ("3.1", Is_Present (Config, "1"), False);
      Check ("3.2", Is_Present (Config, "2"), False);
      Check ("3.3", Is_Present (Config, "3"), True);
      Check ("3.4", Is_Present (Config, "4"), False);

      --  Delete last item, check empty, add new
      SAL.Config_Files.Delete (Config, "3");

      Check ("4.e", Is_Null (Root (Config, "")), True);

      Write (Config, "5", "five");
      Check ("4.1", Is_Present (Config, "5"), True);
   end Plain;

   procedure Children (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;

      Config : Configuration_Type;
   begin

      if Exists (Config_File_Name) then
         Delete_File (Config_File_Name);
      end if;

      Open (Config, Config_File_Name, Read_Only => False);

      Write (Config, "1", "one");
      Write (Config, "2.1", "two.one");
      Write (Config, "2.2", "two.two");
      Write (Config, "2.3", "two.three");
      Write (Config, "3", "three");

      --  Delete middle child item
      --  Not clear why Delete is not correctly visible here
      SAL.Config_Files.Delete (Config, "2.2");

      Check ("1.1", Is_Present (Config, "1"), True);
      Check ("1.2.1", Is_Present (Config, "2.1"), True);
      Check ("1.2.2", Is_Present (Config, "2.2"), False);
      Check ("1.2.3", Is_Present (Config, "2.3"), True);
      Check ("1.3", Is_Present (Config, "3"), True);

      --  Delete first child item
      SAL.Config_Files.Delete (Config, "2.1");

      Check ("2.1", Is_Present (Config, "1"), True);
      Check ("2.2.1", Is_Present (Config, "2.1"), False);
      Check ("2.2.2", Is_Present (Config, "2.2"), False);
      Check ("2.2.3", Is_Present (Config, "2.3"), True);
      Check ("2.3", Is_Present (Config, "3"), True);

      --  Delete last child item
      SAL.Config_Files.Delete (Config, "2.3");

      Check ("3.1", Is_Present (Config, "1"), True);
      Check ("3.2", Is_Present (Config, "2"), True);
      Check ("3.2.1", Is_Present (Config, "2.3"), False);
      Check ("3.3", Is_Present (Config, "3"), True);

   end Children;

   procedure Iterators (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;

      Config : Configuration_Type;
      I      : Iterator_Type;
   begin

      if Exists (Config_File_Name) then
         Delete_File (Config_File_Name);
      end if;

      Open (Config, Config_File_Name, Read_Only => False);

      Write (Config, "1", "one");
      Write (Config, "2", "two");
      Write (Config, "3", "three");

      --  Delete middle item
      I := Root (Config, "2");
      SAL.Config_Files.Delete (Config, I);

      Check ("1.I", Is_Null (I), True);
      Check ("1.1", Is_Present (Config, "1"), True);
      Check ("1.2", Is_Present (Config, "2"), False);
      Check ("1.3", Is_Present (Config, "3"), True);

      Close (Config);
   end Iterators;

   ----------
   --  public bodies

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Plain'Access, "Plain");
      Register_Routine (T, Children'Access, "Children");
      Register_Routine (T, Iterators'Access, "Iterators");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Source_Common/Test/" & GNAT.Source_Info.File);
   end Name;

end Test.Config_Files.Delete;
