--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2006, 2008, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with GNAT.OS_Lib;
with AUnit.Checks;
with SAL.Config_Files.Integer; use SAL.Config_Files.Integer;
with SAL.Config_Files;         use SAL.Config_Files;
package body Test.Config_Files.Iterators is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-iterator.config";

   ----------
   --  Local subprograms

   function Read is new Read_Iterator_Integer (Integer);

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;

      Connectors   : Iterator_Type;
      Signal_Count : Integer := 0;
   begin

      Connectors := First (Config);
      loop
         exit when Is_Done (Connectors);
         declare
            Connector_Name : constant String := Current (Connectors);
            Bits           : Iterator_Type   := First (Config, Connector_Name);
         begin
            if Connector_Name = "P1" then
               Check ("P1", Read_Value (Config, Connectors), "P1 Value");
            elsif Connector_Name = "P2" then
               Check ("P2", Read_Value (Config, Connectors), "P2 Value");
            else
               Assert (False, "unexpected connector_name " & Connector_Name);
            end if;

            loop
               exit when Is_Done (Bits);
               declare
                  Bit_Name      : constant String  := Current (Bits);
                  Root_Bit_Name : constant String  := Connector_Name & "." & Bit_Name;
                  Size          : constant Integer := Read (Config, Bits, "Size");
                  Direction     : constant String  := Read (Config, Bits, "Direction");
               begin
                  Signal_Count := Signal_Count + 1;
                  case Signal_Count is
                  when 1 =>
                     Assert
                       (Root_Bit_Name = "P1.0" and
                          Size = Signal_Count and
                          Direction = "in",
                        "signal" & Integer'Image (Signal_Count) & " failed");

                  when 2 =>
                     Assert
                       (Root_Bit_Name = "P1.1" and
                          Size = Signal_Count and
                          Direction = "out",
                        "signal" & Integer'Image (Signal_Count) & " failed");

                  when 3 =>
                     Assert
                       (Root_Bit_Name = "P1.2" and
                          Size = Signal_Count and
                          Direction = "in",
                        "signal" & Integer'Image (Signal_Count) & " failed");

                  when 4 =>
                     Assert
                       (Root_Bit_Name = "P1.3" and
                          Size = Signal_Count and
                          Direction = "out",
                        "signal" & Integer'Image (Signal_Count) & " failed");

                  when 5 =>
                     Assert
                       (Root_Bit_Name = "P2.2" and
                          Size = Signal_Count and
                          Direction = "in",
                        "signal" & Integer'Image (Signal_Count) & " failed");

                  when 6 =>
                     Assert
                       (Root_Bit_Name = "P2.3" and
                          Size = Signal_Count and
                          Direction = "out",
                        "signal" & Integer'Image (Signal_Count) & " failed");

                  when others =>
                     Assert (False, "invalid signal count");
                  end case;
               end;
               Next (Bits);
            end loop;
         end;
         Next (Connectors);
      end loop;

      Assert (Signal_Count = 6, "signal count failure;" & Integer'Image (Signal_Count));

   end Nominal;

   procedure Line_Column (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Connectors   : Iterator_Type;
      Signal_Count : Integer := 0;

   begin
      Connectors := First (Config);
      loop
         exit when Is_Done (Connectors);
         declare
            Connector_Name : constant String := Current (Connectors);
            Bits           : Iterator_Type   := First (Config, Connector_Name);
         begin
            loop
               exit when Is_Done (Bits);
               declare
                  Line_Col : constant String := Line_Column (Bits);

                  procedure Check (Expected_Line_Col : in String)
                  is begin
                     Assert
                       (Line_Col = Expected_Line_Col,
                        "signal" & Integer'Image (Signal_Count) &
                          " expected " & Expected_Line_Col & " got " & Line_Col);
                  end Check;

               begin
                  Signal_Count := Signal_Count + 1;
                  case Signal_Count is
                  when 1 =>
                     Check ("2:4: ");

                  when 2 =>
                     Check ("4:4: ");

                  when 3 =>
                     Check ("6:4: ");

                  when 4 =>
                     Check ("8:4: ");

                  when 5 =>
                     Check ("11:4: ");

                  when 6 =>
                     Check ("13:4: ");

                  when others =>
                     Assert (False, "invalid signal count");
                  end case;
               end;
               Next (Bits);
            end loop;
         end;
         Next (Connectors);
      end loop;

      Assert (Signal_Count = 6, "signal count failure;" & Integer'Image (Signal_Count));

   end Line_Column;

   procedure Errors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Iterator : Iterator_Type;
   begin
      begin
         Iterator := First (Config, "P3");
         Assert (False, "didn't get Config_File_Error");
      exception
      when SAL.Config_File_Error =>
         Assert (True, "");
      end;

      begin
         declare
            Name : constant String := Current (Iterator);
            pragma Unreferenced (Name);
         begin
            Assert (False, "didn't get constraint_error on null iterator");
         end;
      exception
      when Constraint_Error =>
         Assert (True, "");
      end;
   end Errors;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Line_Column'Access, "Line_Column");
      Register_Routine (T, Errors'Access, "Errors");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);

      --  Delete File_Name if it exists, to erase previous tests.

      use GNAT.OS_Lib;
      File    : String_Access := Locate_Regular_File (File_Name, ".");
      Success : Boolean;
   begin
      if File /= null then
         Delete_File (File.all, Success);
         Free (File);
      end if;

      Open (Config, File_Name, Missing_File => Ignore, Read_Only => False);
      Write (Config, "P1", "P1 Value");
      Write (Config, "P1.0.Size", 1);
      Write (Config, "P1.0.Direction", "in");
      Write (Config, "P1.1.Size", 2);
      Write (Config, "P1.1.Direction", "out");
      Write (Config, "P1.2.Size", 3);
      Write (Config, "P1.2.Direction", "in");
      Write (Config, "P1.3.Size", 4);
      Write (Config, "P1.3.Direction", "out");

      Write (Config, "P2", "P2 Value");
      Write (Config, "P2.2.Size", 5);
      Write (Config, "P2.2.Direction", "in");
      Write (Config, "P2.3.Size", 6);
      Write (Config, "P2.3.Direction", "out");

      --  close and reopen so file line and column get set
      Close (Config);
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
      return new String'("Config_Files.Iterators");
   end Name;

end Test.Config_Files.Iterators;
