--  Abstract :
--
--  See spec
--
--  Copyright (C) 2002 - 2006, 2009, 2013, 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.Characters.Handling;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Interfaces.C;                use Interfaces.C;
with AUnit.Checks;
with SAL.Config_Files.Boolean;
with SAL.Config_Files.Duration;
with SAL.Config_Files;          use SAL.Config_Files;
package body Test.Config_Files.Base_Tests is

   function Read is new Read_Integer (int);
   procedure Write is new Write_Integer (int);

   function Read is new Read_Modular (unsigned);
   procedure Write is new Write_Modular (unsigned);

   function Read is new Read_Float (Float);
   procedure Write is new Write_Float (Float);

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files.config";

   Strad  : constant String := "Stradivarious";
   Quoted : constant String := "he said ""hi there & goodbye""";

   Interfaces_Section : constant String := "Numeric.Interfaces.C";
   An_Int             : constant int := 2;
   An_Unsigned        : constant unsigned := 16#7654321#;

   Float_Section : constant String := "Numeric.Float";
   A_Float       : constant Float := 3.14159;
   Epsilon       : constant Float := 10.0 ** (-(Float'Digits - 1));

   ----------
   --  Local subprogram declarations

   procedure Check_Boolean (Key : in String; Expected : in Boolean);
   procedure Check_Duration (Key : in String; Expected : in Duration);
   procedure Check_Float (Key : in String; Expected : in Float);
   procedure Check_Int (Key : in String; Expected : in int);
   procedure Check_String (Key : in String; Expected : in String);
   procedure Check_Unsigned (Key : in String; Expected : in unsigned);

   procedure Test_Boolean_Value (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Duration_Value (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_File_Name (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Float_Value (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Flush_Reopen (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Int_Value (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Is_Present (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Quoted_String_Value
     (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_String_Value (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Value (T : in out AUnit.Test_Cases.Test_Case'Class);


   ----------
   --  Subprogram bodies (alphabetical order)

   procedure Check_Boolean (Key : in String; Expected : in Boolean)
   is
      use SAL.Config_Files.Boolean;
      Result_Boolean : constant Boolean := Read (Config, Key, False, Missing_Key => Raise_Exception);
   begin
      AUnit.Assertions.Assert
        (Result_Boolean = Expected,
         Key & ": " &
           "Expecting " & Boolean'Image (Expected) & ' ' &
           "Got " & Boolean'Image (Result_Boolean));
   end Check_Boolean;

   procedure Check_Duration (Key : in String; Expected : in Duration)
   is
      use SAL.Config_Files.Duration;
      Result : constant Duration := Read (Config, Key, 0.0, Missing_Key => Raise_Exception);
   begin
      AUnit.Assertions.Assert
        (Result = Expected,
         Key & ": " &
           "Expecting " & Duration'Image (Expected) & ' ' &
           "Got " & Duration'Image (Result));
   end Check_Duration;

   procedure Check_Float (Key : in String; Expected : in Float)
   is
      Result_Float : constant Float := Read (Config, Key, 0.0);
   begin
      AUnit.Assertions.Assert
        ((Result_Float - Expected) <= Epsilon,
         Key & ": " &
           "Expecting " & Float'Image (Expected) & ' ' &
           "Got " & Float'Image (Result_Float));
   end Check_Float;

   procedure Check_Int (Key : in String; Expected : in int)
   is
      Result_Int : constant int := Read (Config, Key, 0);
   begin
      AUnit.Assertions.Assert
        (Result_Int = Expected,
         Key & ": " &
           "Expecting " & int'Image (Expected) & ' ' &
           "Got " & int'Image (Result_Int));
   end Check_Int;

   procedure Check_String (Key : in String; Expected : in String)
   is
      Result : constant String := Read (Config, Key, "default");
   begin
      AUnit.Assertions.Assert
        (Result = Expected,
         Key & ": " &
           "Expecting <" & Expected & '>' & ' ' &
           "Got <" & Result & '>');
   end Check_String;

   procedure Check_Unsigned (Key : in String; Expected : in unsigned)
   is
      Result_Unsigned : constant unsigned := Read (Config, Key, 0);
   begin
      AUnit.Assertions.Assert
        (Result_Unsigned = Expected,
         Key & ": " &
           "Expecting " & unsigned'Image (Expected) & ' ' &
           "Got " & unsigned'Image (Result_Unsigned));
   end Check_Unsigned;

   ----------
   --  Public routines

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test.Config_Files.Base_Tests");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Boolean_Value'Access, "Test_Boolean_Value");
      Register_Routine (T, Test_Duration_Value'Access, "Test_Duration_Value");
      Register_Routine (T, Test_File_Name'Access, "Test_File_Name");
      Register_Routine (T, Test_Float_Value'Access, "Test_Float_Value");
      Register_Routine (T, Test_Int_Value'Access, "Test_Int_Value");
      Register_Routine (T, Test_Quoted_String_Value'Access, "Test_Quoted_String_Value");
      Register_Routine (T, Test_String_Value'Access, "Test_String_Value");
      Register_Routine (T, Test_Unsigned_Value'Access, "Test_Unsigned_Value");

      --  These rely on values written by the above routines.
      Register_Routine (T, Test_Flush_Reopen'Access, "Test_Flush_Reopen");
      Register_Routine (T, Test_Is_Present'Access, "Test_Is_Present");
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
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Close (Config);
   end Tear_Down_Case;

   ----------
   --  Test routines

   procedure Test_Boolean_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      SAL.Config_Files.Boolean.Write (Config, "A_Boolean", True);
      Check_Boolean ("A_Boolean", True);
      SAL.Config_Files.Boolean.Write (Config, "Another_Boolean", False);
      Check_Boolean ("Another_Boolean", False);
   end Test_Boolean_Value;

   procedure Test_Duration_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      A_Duration : constant Duration := 1234.5;
   begin
      SAL.Config_Files.Duration.Write (Config, "A_Duration", A_Duration);
      Check_Duration ("A_Duration", A_Duration);
   end Test_Duration_Value;

   procedure Test_File_Name (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  There is no Ada standard way to get the full path name, so
      --  we use GNAT.OS_Lib. When Ada0Y.Directories becomes standard,
      --  we can use that.

      use GNAT.OS_Lib;
      use Ada.Characters.Handling;
      Found : GNAT.OS_Lib.String_Access;
   begin
      --  The file is created in cache by Set_Up_Case, but doesn't
      --  exist on the disk until we Flush

      Flush (Config);

      Found := Locate_Regular_File
        (Test.Config_Files.Base_Tests.File_Name,
         GNAT.Directory_Operations.Get_Current_Dir);

      --  There doesn't seem to be a way to get the GNAT file
      --  utilities to return consistent case for the Windows drive
      --  letter. So we use To_Lower.
      AUnit.Assertions.Assert
        (To_Lower (Found.all) = To_Lower (SAL.Config_Files.Writeable_File_Name (Config)),
         "Expecting '" & Found.all & "' " &
           "Got '" & SAL.Config_Files.Writeable_File_Name (Config) & "'");
   end Test_File_Name;

   procedure Test_Float_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Write (Config, "A_Float", A_Float);
      Check_Float ("A_Float", A_Float);
      Write (Config, Float_Section & ".A_Float", A_Float);
      Check_Float (Float_Section & ".A_Float", A_Float);
   end Test_Float_Value;

   procedure Test_Flush_Reopen (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Flush (Config);

      Open (Config, File_Name, Missing_File => Raise_Exception, Read_Only => False);
      Check_String ("Strings.Violins", Strad);
      Check_String ("Strings.Quoted", Quoted);
      Check_Float (Float_Section & ".A_Float", A_Float);
      Check_Int (Interfaces_Section & ".An_Int", An_Int);
      Check_Unsigned (Interfaces_Section & ".An_Unsigned", An_Unsigned);
   end Test_Flush_Reopen;

   procedure Test_Int_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Write (Config, "An_Int", An_Int);
      Check_Int ("An_Int", An_Int);
      Write (Config, Interfaces_Section & ".An_Int", An_Int);
      Check_Int (Interfaces_Section & ".An_Int", An_Int);
   end Test_Int_Value;

   procedure Test_Is_Present (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Check ("A_Boolean", Is_Present (Config, "A_Boolean"), True);
      Check ("No_Boolean", Is_Present (Config, "No_Boolean"), False);
   end Test_Is_Present;

   procedure Test_String_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Write
        (Config,
         Key         => "Violins",
         Value       => Strad,
         Missing_Key => Ignore);

      Check_String ("Violins", Strad);

      Write
        (Config,
         Key         => "Strings.Violins",
         Value       => Strad,
         Missing_Key => Ignore);

      Check_String ("Strings.Violins", Strad);
   end Test_String_Value;

   procedure Test_Quoted_String_Value
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Write
        (Config,
         Key         => "Strings.Quoted",
         Value       => Quoted,
         Missing_Key => Ignore);

      Check_String ("Strings.Quoted", Quoted);
   end Test_Quoted_String_Value;

   procedure Test_Unsigned_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Write (Config, Interfaces_Section & ".An_Unsigned", An_Unsigned);
      Check_Unsigned (Interfaces_Section & ".An_Unsigned", An_Unsigned);
   end Test_Unsigned_Value;

end Test.Config_Files.Base_Tests;
