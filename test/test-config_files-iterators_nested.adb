--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.Directories;
with Ada.Text_IO;
with AUnit.Checks;
with SAL.Config_Files.Integer;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Iterators_Nested is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-iterator_nested.config";

   ----------
   --  Local subprograms

   procedure Nested (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use SAL.Config_Files.Integer;

      Cards : Iterator_Type := First (Config, "Card");
   begin

      Check ("Driver_ID_Hi", Read (Config, "driver_id_hi"), 2);

      loop
         exit when Is_Done (Cards);

         declare
            Card_Name  : constant String := Current (Cards);
            Components : Iterator_Type := First (Config, Cards, "Component");
         begin
            if Card_Name = "1" then
               Check ("Card.1.Slot", Read (Config, Cards, "Slot"), 9);
               Check ("Card.1.Driver_ID_Lo", Read (Config, Cards, "Driver_ID_Lo"), 1);

               Check ("card.1.Component.2", Current (Components), "2");
               Check ("card.1.Component.2.Type", Read (Config, Components, "Type"), "Thruster_Valve");
               Check ("card.1.Component.2.Name", Read (Config, Components, "Name"), "Thr_1");

               Next (Components);
               Check ("card.1.Component.3", Current (Components), "3");
               Check ("card.1.Component.3.Name", Read (Config, Components, "Name"), "Pyro_1");
               Check ("card.1.Component.3.Type", Read (Config, Components, "Type"), "Pyro_Valve");

               Next (Components);
               Check ("Is_Done (Components)", Is_Done (Components), True);

            elsif Card_Name = "2" then
               Check ("Card.2.Slot", Read (Config, Cards, "Slot"), 10);
               Check ("Card.2.Driver_ID_Lo", Read (Config, Cards, "Driver_ID_Lo"), 2);

               Check ("card.2.Component.4", Current (Components), "4");
               Check ("card.2.Components.4.Name", Read (Config, Components, "Name"), "Pyro_2");
               Check ("card.2.Components.4.Type", Read (Config, Components, "Type"), "Pyro_Valve");

               Next (Components);
               Check ("card.2.Component.5", Current (Components), "5");
               Check ("card.2.Components.5.Name", Read (Config, Components, "Name"), "Pyro_3");
               Check ("card.2.Components.5.Type", Read (Config, Components, "Type"), "Pyro_Valve");

               Next (Components);
               Check ("Is_Done (Components)", Is_Done (Components), True);
            else
               Assert (False, "unexpected card_name " & Card_Name);
            end if;

         end;
         Next (Cards);
      end loop;

   end Nested;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nested'Access, "Nested");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use Ada.Text_IO;
      File : File_Type;
   begin
      --  Delete File_Name if it exists, to erase previous tests.

      if Exists (File_Name) then
         Delete_File (File_Name);
      end if;

      Open (Config, File_Name, Missing_File => Ignore, Read_Only => False);
      Create (File, Out_File, File_Name);
      Put_Line (File, "Driver_ID_Hi = 2");
      Put_Line (File, "card.1.Slot         = 9");
      Put_Line (File, "card.1.Driver_ID_Lo = 1");
      Put_Line (File, "card.1.Component.2.Type = Thruster_Valve");
      Put_Line (File, "card.1.Component.2.Name = Thr_1");
      Put_Line (File, "card.1.Component.3.Type = Pyro_Valve");
      Put_Line (File, "card.1.Component.3.Name = Pyro_1");

      Put_Line (File, "card.2.Slot         = 10");
      Put_Line (File, "card.2.Driver_ID_Lo = 2");
      Put_Line (File, "card.2.Component.4.Name = Pyro_2");
      Put_Line (File, "card.2.Component.4.Type = Pyro_Valve");
      Put_Line (File, "card.2.Component.5.Name = Pyro_3");
      Put_Line (File, "card.2.Component.5.Type = Pyro_Valve");

      Close (File);

      Open (Config, File_Name, Missing_File => Raise_Exception);
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
      return new String'("Config_Files.Iterators_Nested");
   end Name;

end Test.Config_Files.Iterators_Nested;
