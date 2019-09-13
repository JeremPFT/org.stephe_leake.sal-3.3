--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2019 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks.Containers;
with SAL.Gen_Unbounded_Definite_Vectors_Sorted;
package body Test_Unbounded_Definite_Vectors_Sorted
is
   type Pair is record
      Key   : Integer := Integer'First;
      Value : Integer := Integer'First;
   end record;

   function To_Key (Item : in Pair) return Integer is (Item.Key);
   function Compare (Left, Right : in Integer) return SAL.Compare_Result is
     (if Left < Right then SAL.Less elsif Left = Right then SAL.Equal else SAL.Greater);

   package Sorted_Pair_Vectors is new SAL.Gen_Unbounded_Definite_Vectors_Sorted
     (Element_Type => Pair,
      Key_Type     => Integer,
      To_Key       => To_Key,
      Key_Compare  => Compare);
   use Sorted_Pair_Vectors;

   type Check_Array_Type is array (Positive range <>) of Pair;

   procedure Check
     (Label    : in String;
      Computed : in Pair;
      Expected : in Pair)
   is
      use AUnit.Checks;
   begin
      Check (Label & ".Key", Computed.Key, Expected.Key);
      Check (Label & ".Value", Computed.Value, Expected.Value);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Vector;
      Expected : in Check_Array_Type)
   is
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      I : Positive := Positive'First;
   begin
      Check (Label & ".Length", Computed.Length, Expected'Length);
      for Element of Computed loop
         Check (Label & "." & I'Image, Element, Expected (I));
         I := I + 1;
      end loop;
   end Check;

   procedure Test_Not_Found
     (Label : in String;
      List  : in Vector;
      Key   : in Integer)
   is
      use AUnit.Assertions;
      Computed : constant Find_Reference_Type := List.Find (Key);
   begin
      if Computed.Element /= null then
         Assert (False, Label & " key was found");
      end if;
   end Test_Not_Found;

   List : Vector;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Insert (List, (2, -2));
      Check ("1 insert", List, (1 => (2, -2)));
      Check ("1 find 2", Find (List, 2), (2, -2));
      Test_Not_Found ("1 find 0", List, 0);
      Test_Not_Found ("1 find 3", List, 3);

      Insert (List, (1, -1));
      Check ("2 insert", List, ((1, -1), (2, -2)));
      Check ("2 find 2", Find (List, 2), (2, -2));
      Check ("2 find 1", Find (List, 1), (1, -1));
      Test_Not_Found ("1 find 0", List, 0);
      Test_Not_Found ("1 find 3", List, 3);

      Insert (List, (4, -4));
      Check ("3 insert", List, ((1, -1), (2, -2), (4, -4)));
      Check ("3 find 2", Find (List, 2), (2, -2));
      Check ("3 find 1", Find (List, 1), (1, -1));
      Check ("3 find 4", Find (List, 4), (4, -4));
      Test_Not_Found ("1 find 0", List, 0);
      Test_Not_Found ("1 find 3", List, 3);

      Insert (List, (3, -3));
      Check ("4 insert", List, ((1, -1), (2, -2), (3, -3), (4, -4)));
      Check ("4 find 2", Find (List, 2), (2, -2));
      Test_Not_Found ("1 find 0", List, 0);
      Test_Not_Found ("1 find 5", List, 5);

      Insert (List, (5, -5));
      Check ("5 insert", List, ((1, -1), (2, -2), (3, -3), (4, -4), (5, -5)));
      Check ("5 find 1", Find (List, 1), (1, -1));
      Check ("5 find 2", Find (List, 2), (2, -2));
      Check ("5 find 3", Find (List, 3), (3, -3));
      Check ("5 find 4", Find (List, 4), (4, -4));
      Check ("5 find 5", Find (List, 5), (5, -5));
      Test_Not_Found ("1 find 0", List, 0);
      Test_Not_Found ("1 find 6", List, 6);
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_unbounded_definite_vectors_sorted.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Unbounded_Definite_Vectors_Sorted;
