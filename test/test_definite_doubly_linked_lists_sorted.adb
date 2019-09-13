--  Abstract:
--
--  Test Sal.Gen_Definite_Doubly_Linked_Lists_Sorted
--
--  Copyright (C) 2017 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks.Containers;
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Validate;
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_AUnit;
package body Test_Definite_Doubly_Linked_Lists_Sorted is

   function Compare_Int (Left, Right : in Integer) return SAL.Compare_Result
   is begin
      if Left > Right then
         return SAL.Greater;
      elsif Left = Right then
         return SAL.Equal;
      else
         return SAL.Less;
      end if;
   end Compare_Int;

   package Integer_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted (Integer, Compare_Int);
   use Integer_Lists;

   package Val is new Integer_Lists.Gen_Validate;

   package Lists_AUnit is new Integer_Lists.Gen_AUnit (AUnit.Checks.Check);

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;

      List : Integer_Lists.List;
      Cur : Cursor;
   begin
      Check ("0", List.Length, 0);

      Insert (List, 1);
      Insert (List, 5);
      Insert (List, 3);

      Val.Validate ("0", List);
      Cur := List.First;
      Check ("1a", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("1b", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("1c", Constant_Ref (Cur), 5);
      Next (Cur);
      Check ("1d", Has_Element (Cur), False);
      Check ("1e", Cur = No_Element, True);

      Cur := List.First;
      Next (Cur);
      Delete (List, Cur);
      Val.Validate ("2", List);
      Check ("2a", Cur = No_Element, True);
      Check ("2b", List.Length, 2);
      Cur := List.First;
      Check ("2c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("2d", Constant_Ref (Cur), 5);
      Next (Cur);
      Check ("2e", Cur = No_Element, True);

      declare
         B : Integer_Lists.List := List;
         Added : Boolean;

         A : Integer_Lists.List renames List;
      begin
         Check ("3a", B.Length, 2);
         Cur := B.First;
         Check ("3b", Constant_Ref (Cur), 1);
         Next (Cur);
         Check ("3c", Constant_Ref (Cur), 5);
         Next (Cur);
         Check ("3d", Cur = No_Element, True);

         Merge (A, B, Added);

         Check ("4a", A.Length, 2);
         Check ("4b", B.Length, 2);
         Check ("4c", Added, False);

         B.Insert (2);

         Merge (A, B, Added);

         Check ("5a", A.Length, 3);
         Check ("5b", B.Length, 3);
         Check ("5c", Added, True);
         Cur := List.First;
         Check ("5d", Constant_Ref (Cur), 1);
         Next (Cur);
         Check ("5e", Constant_Ref (Cur), 2);
         Next (Cur);
         Check ("5f", Constant_Ref (Cur), 5);
         Next (Cur);
         Check ("5g", Cur = No_Element, True);

         B.Insert (6);
         B.Insert (4);

         Merge (A, B, Added, Exclude => 6);

         Check ("6a", A.Length, 4);
         Check ("6b", B.Length, 5);
         Check ("6c", Added, True);
         Cur := List.First;
         Check ("6d", Constant_Ref (Cur), 1);
         Next (Cur);
         Check ("6e", Constant_Ref (Cur), 2);
         Next (Cur);
         Check ("6f", Constant_Ref (Cur), 4);
         Next (Cur);
         Check ("6g", Constant_Ref (Cur), 5);
         Next (Cur);
         Check ("6h", Cur = No_Element, True);
      end;
   end Nominal;

   procedure Test_Find (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use Lists_AUnit;

      List : Integer_Lists.List;
      Cur : Cursor;
   begin
      Check ("0", List.Length, 0);

      Cur := Find (List, 1);
      Check ("1", Cur, No_Element);

      Insert (List, 1);
      Check ("2a", Constant_Ref (Find (List, 1)), 1);
      Check ("2b", Find (List, 0), No_Element);
      Check ("2c", Find (List, 4), No_Element);

      Insert (List, 5);
      Check ("3a", Constant_Ref (Find (List, 1)), 1);
      Check ("3b", Constant_Ref (Find (List, 5)), 5);
      Check ("3c", Find (List, 0), No_Element);
      Check ("3d", Find (List, 4), No_Element);
      Check ("3e", Find (List, 6), No_Element);

      Insert (List, 3);

      Check ("4a", Constant_Ref (Find (List, 1)), 1);
      Check ("4b", Constant_Ref (Find (List, 3)), 3);
      Check ("4c", Constant_Ref (Find (List, 5)), 5);
      Check ("4d", Find (List, 0), No_Element);
      Check ("4e", Find (List, 4), No_Element);
      Check ("4f", Find (List, 6), No_Element);

      Insert (List, 6);
      Insert (List, 7);
      Insert (List, 8);

      Val.Validate ("5a", List);
      Check ("5b", List.Length, 6);
      Cur := List.First;
      Check ("5c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("5d", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("5e", Constant_Ref (Cur), 5);
      Next (Cur);
      Check ("5f", Constant_Ref (Cur), 6);
      Next (Cur);
      Check ("5g", Constant_Ref (Cur), 7);
      Next (Cur);
      Check ("5h", Constant_Ref (Cur), 8);
      Check ("5i", Constant_Ref (List.Find (8)), 8);
   end Test_Find;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Test_Find'Access, "Test_Find");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_definite_doubly_linked_lists_sorted.adb");
   end Name;

end Test_Definite_Doubly_Linked_Lists_Sorted;
