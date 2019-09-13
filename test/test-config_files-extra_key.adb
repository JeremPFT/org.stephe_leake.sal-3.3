--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 - 2006, 2008, 2009, 2015, 2018 Stephen Leake.  All Rights Reserved.
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
with Interfaces;
with AUnit.Checks;
with SAL.Config_Files.Boolean;
with SAL.Config_Files.Duration;
with SAL.Config_Files.Integer;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Extra_Key is

   function Read is new Read_Modular (Interfaces.Unsigned_8);
   function Read is new Read_Iterator_Modular (Interfaces.Unsigned_8);

   function Read is new Read_Float (Float);
   function Read is new Read_Iterator_Float (Float);

   Config    : Configuration_Type;
   Iterator  : Iterator_Type;
   File_Name : constant String := "test-config_files-extra_key.config";

   ----------
   --  Local subprograms

   procedure Test_String (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         --  Check sibling/child traversal in Check_Unread_Key; only
         --  need to do this for one value type.
         Put_Line (File, "Card.1.Slot = 9");
         Put_Line (File, "Card.1.Channels.5 = RWA_Tor_Cmd");
         Put_Line (File, "Card.1.Channel.1 = GCE_Floating_Ground");
         Close (File);
      end Create_Config;

   begin
      Create_Config;

      Open (Config, File_Name);
      declare
         Temp_1 : constant String := SAL.Config_Files.Read (Config, "Card.1.Slot");
         Temp_2 : constant String := SAL.Config_Files.Read (Config, "Card.1.Channel.1");
         pragma Unreferenced (Temp_1);
         pragma Unreferenced (Temp_2);
      begin
         Close (Config, Unread_Key => Raise_Exception);
         Assert (False, "no exception");
      exception
      when E : SAL.Config_File_Error =>
         AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":2:15: unread key");
      end;

      --  This time, verify that Is_Present sets the 'read' flag.
      Open (Config, File_Name);
      declare
         Temp_1 : constant Boolean := SAL.Config_Files.Is_Present (Config, "Card.1.Slot");
         Temp_2 : constant Boolean := SAL.Config_Files.Is_Present (Config, "Card.1.Channels.5");
         Temp_3 : constant Boolean := SAL.Config_Files.Is_Present (Config, "Card.1.Channel.1");
         pragma Unreferenced (Temp_1);
         pragma Unreferenced (Temp_2);
         pragma Unreferenced (Temp_3);
      begin
         null;
      end;
      Close (Config, Unread_Key => Raise_Exception);
   end Test_String;

   procedure Test_Enum (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Enum = True");
         Close (File);
      end Create_Config;

      Temp : Boolean;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      begin
         Close (Config, Unread_Key => Raise_Exception);
         Assert (False, "no exception");
      exception
      when E : SAL.Config_File_Error =>
         AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":1:7: unread key");
      end;

      Open (Config, File_Name);
      Temp := SAL.Config_Files.Boolean.Read (Config, "Enum");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Enum;

   procedure Test_Integer (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Integer = 1");
         Close (File);
      end Create_Config;

      Temp : Integer;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      begin
         Close (Config, Unread_Key => Raise_Exception);
         Assert (False, "no exception");
      exception
      when E : SAL.Config_File_Error =>
         AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":1:10: unread key");
      end;

      Open (Config, File_Name);
      Temp := SAL.Config_Files.Integer.Read (Config, "Integer");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Integer;

   procedure Test_Modular (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Modular = 10");
         Close (File);
      end Create_Config;

      Temp : Interfaces.Unsigned_8;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      begin
         Close (Config, Unread_Key => Raise_Exception);
         Assert (False, "no exception");
      exception
      when E : SAL.Config_File_Error =>
         AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":1:10: unread key");
      end;

      Open (Config, File_Name);
      Temp := Read (Config, "Modular");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Modular;

   procedure Test_Float (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Float = 1.0");
         Close (File);
      end Create_Config;

      Temp : Float;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      begin
         Close (Config, Unread_Key => Raise_Exception);
         Assert (False, "no exception");
      exception
      when E : SAL.Config_File_Error =>
         AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":1:8: unread key");
      end;

      Open (Config, File_Name);
      Temp := Read (Config, "Float");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Float;

   procedure Test_Fixed (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Fixed = 1");
         Close (File);
      end Create_Config;

      Temp : Duration;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      begin
         Close (Config, Unread_Key => Raise_Exception);
         Assert (False, "no exception");
      exception
      when E : SAL.Config_File_Error =>
         AUnit.Checks.Check ("", Ada.Exceptions.Exception_Message (E), File_Name & ":1:8: unread key");
      end;

      Open (Config, File_Name);
      Temp := SAL.Config_Files.Duration.Read (Config, "Fixed");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Fixed;

   ----------
   --  Iterators

   procedure Test_Iterator_String (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Iterator.String = foo");
         Close (File);
      end Create_Config;

   begin
      Create_Config;

      Open (Config, File_Name);
      Iterator := First (Config, "Iterator");
      declare
         Temp : constant String := SAL.Config_Files.Read_Value (Config, Iterator);
         pragma Unreferenced (Temp);
      begin
         null;
      end;
      Close (Config, Unread_Key => Raise_Exception);

      Open (Config, File_Name);
      Iterator := First (Config);
      declare
         Temp : constant String := SAL.Config_Files.Read (Config, Iterator, "String");
         pragma Unreferenced (Temp);
      begin
         null;
      end;
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Iterator_String;

   procedure Test_Iterator_Enum (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Iterator.Enum = True");
         Close (File);
      end Create_Config;

      Temp : Boolean;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      Iterator := First (Config);
      Temp := SAL.Config_Files.Boolean.Read (Config, Iterator, "Enum");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Iterator_Enum;

   procedure Test_Iterator_Integer (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Iterator.Integer = 1");
         Close (File);
      end Create_Config;

      Temp : Integer;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      Iterator := First (Config);
      Temp := SAL.Config_Files.Integer.Read (Config, Iterator, "Integer");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Iterator_Integer;

   procedure Test_Iterator_Modular (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Iterator.Modular = 10");
         Close (File);
      end Create_Config;

      Temp : Interfaces.Unsigned_8;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      Iterator := First (Config);
      Temp := Read (Config, Iterator, "Modular");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Iterator_Modular;

   procedure Test_Iterator_Float (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Iterator.Float = 1.0");
         Close (File);
      end Create_Config;

      Temp : Float;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      Iterator := First (Config);
      Temp := Read (Config, Iterator, "Float");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Iterator_Float;

   procedure Test_Iterator_Fixed (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Delete_File (File_Name);

         Create (File, Out_File, File_Name);
         Put_Line (File, "Iterator.Fixed = 1");
         Close (File);
      end Create_Config;

      Temp : Duration;
      pragma Unreferenced (Temp);
   begin
      Create_Config;

      Open (Config, File_Name);
      Iterator := First (Config);
      Temp := SAL.Config_Files.Duration.Read (Config, Iterator, "Fixed");
      Close (Config, Unread_Key => Raise_Exception);
   end Test_Iterator_Fixed;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_String'Access, "Test_String");
      Register_Routine (T, Test_Enum'Access, "Test_Enum");
      Register_Routine (T, Test_Integer'Access, "Test_Integer");
      Register_Routine (T, Test_Modular'Access, "Test_Modular");
      Register_Routine (T, Test_Float'Access, "Test_Float");
      Register_Routine (T, Test_Fixed'Access, "Test_Fixed");

      Register_Routine (T, Test_Iterator_String'Access, "Test_Iterator_String");
      Register_Routine (T, Test_Iterator_Enum'Access, "Test_Iterator_Enum");
      Register_Routine (T, Test_Iterator_Integer'Access, "Test_Iterator_Integer");
      Register_Routine (T, Test_Iterator_Modular'Access, "Test_Iterator_Modular");
      Register_Routine (T, Test_Iterator_Float'Access, "Test_Iterator_Float");
      Register_Routine (T, Test_Iterator_Fixed'Access, "Test_Iterator_Fixed");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test.Config_Files.Extra_Key");
   end Name;

end Test.Config_Files.Extra_Key;
