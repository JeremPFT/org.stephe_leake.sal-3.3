--  Abstract :
--
--  See spec
--
--  Copyright (C) 2008, 2009, 2013, 2014, 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Directories;
with AUnit.Checks;
package body SAL.CSV.Test is

   procedure Check is new AUnit.Checks.Gen_Check_Unconstrained_Array
     (Item_Type   => Integer,
      Index_Type  => Positive,
      Array_Type  => Positive_Array_Integer_Type,
      Check_Index => AUnit.Checks.Check,
      Check_Item  => AUnit.Checks.Check);

   procedure Test_Read (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      CSV_File_Name : constant String := "sal.csv.test-file.csv";
      CSV_File      : File_Type;
   begin

      --  create a test file to read
      declare
         use Ada.Text_IO;
         Test_File : Ada.Text_IO.File_Type;
      begin

         if Ada.Directories.Exists (CSV_File_Name) then
            Ada.Directories.Delete_File (CSV_File_Name);
         end if;

         Create (Test_File, Out_File, CSV_File_Name);

         --  Quotes in header line to test former bug
         Put_Line (Test_File, """A"", ""B"", ""C"", ""D"", ""E""");
         Put_Line (Test_File, "1, 2, 3, 4, 5");
         Put_Line (Test_File, "6, ,,, 7");
         Put_Line (Test_File, """a,b"", c, "" """", "", e,");

         Close (Test_File);
      end;

      Open (CSV_File, CSV_File_Name, Max_Row_Size => 42);

      Check ("columns", Columns (CSV_File), 5);

      Check ("header commas", CSV_File.Commas.all, (4, 9, 14, 19));

      --  Deliberately read columns out of order to show that works
      Check ("header 1", Unquote (Read (CSV_File, 1)), "A");
      Check ("header 5", Unquote (Read (CSV_File, 5)), " E");
      Check ("header 2", Unquote (Read (CSV_File, 2)), " B");
      Check ("header 4", Unquote (Read (CSV_File, 4)), " D");
      Check ("header 3", Unquote (Read (CSV_File, 3)), " C");

      Next_Row (CSV_File);
      Check ("row 1 commas", CSV_File.Commas.all, (2, 5, 8, 11));
      Check ("row 1 1", Read (CSV_File, 1), "1");
      Check ("row 1 5", Read (CSV_File, 5), " 5");
      Check ("row 1 2", Read (CSV_File, 2), " 2");
      Check ("row 1 4", Read (CSV_File, 4), " 4");
      Check ("row 1 3", Read (CSV_File, 3), " 3");

      Next_Row (CSV_File);
      Check ("row 2 commas", CSV_File.Commas.all, (2, 4, 5, 6));
      Check ("row 2 1", Read (CSV_File, 1), "6");
      Check ("row 2 5", Read (CSV_File, 5), " 7");
      Check ("row 2 2", Read (CSV_File, 2), " ");
      Check ("row 2 4", Read (CSV_File, 4), "");
      Check ("row 2 3", Read (CSV_File, 3), "");

      Check ("End_Of_File false", End_Of_File (CSV_File), False);

      Next_Row (CSV_File);
      Check ("row 3 commas", CSV_File.Commas.all, (6, 9, 18, 21));
      Check ("row 3 1", Read (CSV_File, 1), """a,b""");
      Check ("row 3 2", Read (CSV_File, 2), " c");
      Check ("row 3 3", Read (CSV_File, 3), " "" """", """);
      Check ("row 3 4", Read (CSV_File, 4), " e");
      Check ("row 3 5", Read (CSV_File, 5), "");

      Check ("End_Of_File true", End_Of_File (CSV_File), True);
   end Test_Read;

   procedure Quote_Unquote (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;

      procedure Test
        (Label    : in String;
         Unquoted : in String;
         Quoted   : in String)
      is
         Quoted_Computed   : constant String := Quote (Unquoted);
         Unquoted_Computed : constant String := Unquote (Quoted);
      begin
         Check (Label & " Unquoted", Unquoted_Computed, Unquoted);
         Check (Label & " Quoted", Quoted_Computed, Quoted);
      end Test;

   begin
      Test ("1", "6", """6""");

      Test ("2", """a,b"", c", """""""a,b"""", c""");

      Test ("3", "c, "" """", "", e,", """c, """" """""""", """", e,""");

      Test ("4", "", """""");

      Test ("5", """", """""""""");

      Check ("6", Unquote ("6"), "6");
   end Quote_Unquote;

   procedure Newline_In_Quoted_String (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      Tab           : constant Character := ASCII.HT;
      CSV_File_Name : constant String    := "sal.csv.test-file.csv";
      CSV_File      : File_Type;
   begin
      --  Test newlines in a quoted text field

      --  create a test file to read
      declare
         use Ada.Text_IO;
         Test_File : Ada.Text_IO.File_Type;
      begin

         if Ada.Directories.Exists (CSV_File_Name) then
            Ada.Directories.Delete_File (CSV_File_Name);
         end if;

         Create (Test_File, Out_File, CSV_File_Name);

         Put_Line (Test_File, "A" & Tab & "B" & Tab & """C"""); -- header
         Put_Line (Test_File, "4" & Tab & "5" & Tab & """six");
         Put_Line (Test_File, "seven""");                       -- row 1
         Put_Line (Test_File, "8" & Tab & """nine");
         Put_Line (Test_File, "ten""" & Tab & """eleven""");    -- row 2

         Close (Test_File);
      end;

      Open
        (CSV_File,
         CSV_File_Name,
         Max_Row_Size => 21,
         Delimiter    => Tab);

      Check ("columns", Columns (CSV_File), 3);

      Check ("header delimiters", CSV_File.Commas.all, (2, 4));

      Check ("header 1", Read (CSV_File, 1), "A");
      Check ("header 2", Read (CSV_File, 2), "B");
      Check ("header 3", Read (CSV_File, 3), """C""");

      Next_Row (CSV_File);
      Check ("row 1 delimiters", CSV_File.Commas.all, (2, 4));
      Check ("row 1 1", Read (CSV_File, 1), "4");
      Check ("row 1 2", Read (CSV_File, 2), "5");
      Check ("row 1 3", Read (CSV_File, 3), """six" & ASCII.LF & "seven""");

      Next_Row (CSV_File);
      Check ("row 2 1", Read (CSV_File, 1), "8");
      Check ("row 2 2", Read (CSV_File, 2), """nine" & ASCII.LF & "ten""");
      Check ("row 2 3", Read (CSV_File, 3), """eleven""");

      Check ("End_Of_File true", End_Of_File (CSV_File), True);

   end Newline_In_Quoted_String;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("SAL.CSV.Test");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Read'Access, "Test_Read");
      Register_Routine (T, Quote_Unquote'Access, "Quote_Unquote");
      Register_Routine (T, Newline_In_Quoted_String'Access, "Newline_In_Quoted_String");
   end Register_Tests;

end SAL.CSV.Test;
