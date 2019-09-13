--  Abstract:
--
--  Test Sal.Gen_Definite_Doubly_Linked_Lists
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_Validate;
package body Test_Definite_Doubly_Linked_Lists is

   package Integer_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Integer);
   use Integer_Lists;

   package Val is new Integer_Lists.Gen_Validate;

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

      Append (List, 1);
      Append (List, 3);
      Append (List, 5);

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

      List.Prepend (0);
      Val.Validate ("2", List);
      Cur := List.First;
      Check ("2a", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("2b", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("2c", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("2d", Constant_Ref (Cur), 5);
      Check ("2e", List.Length, 4);

      Delete (List, Cur);
      Val.Validate ("3", List);
      Check ("3a", Cur = No_Element, True);
      Check ("3b", List.Length, 3);
      Cur := List.First;
      Check ("3c", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("3d", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("3e", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("3f", Cur = No_Element, True);

      Cur := List.First;
      Cur := Next (Cur);
      Delete (List, Cur);
      Val.Validate ("4", List);
      Check ("4a", List.Length, 2);
      Cur := List.First;
      Check ("4b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("4c", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("4d", Cur = No_Element, True);

      declare
         B : constant Integer_Lists.List := List;
      begin
         Check ("4a", B.Length, 2);
         Cur := B.First;
         Check ("4b", Constant_Ref (Cur), 0);
         Next (Cur);
         Check ("4c", Constant_Ref (Cur), 3);
         Next (Cur);
         Check ("4d", Cur = No_Element, True);
      end;

      Check ("5a", List.Length, 2);
      Cur := List.First;
      Check ("5b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("5c", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("5d", Cur = No_Element, True);

   end Nominal;

   procedure Test_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;

      List : Integer_Lists.List;
      Cur : Cursor;
   begin
      --  Insert into empty
      List.Insert (List.First, 2);

      Val.Validate ("1", List);
      Check ("1a", List.Length, 1);
      Cur := List.First;
      Check ("1b", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("1c", Has_Element (Cur), False);

      --  Insert before head
      List.Insert (List.First, 0);
      Val.Validate ("2", List);
      Check ("2a", List.Length, 2);
      Cur := List.First;
      Check ("2b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("2c", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("2d", Cur = No_Element, True);

      --  Insert before tail
      List.Insert (List.Last, 1);
      Val.Validate ("3", List);
      Check ("3a", List.Length, 3);
      Cur := List.First;
      Check ("3b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("3c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("3d", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("3e", Cur = No_Element, True);

      --  Insert after tail
      List.Insert (No_Element, 3);
      Val.Validate ("4", List);
      Check ("4a", List.Length, 4);
      Cur := List.First;
      Check ("4b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("4c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("4d", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("4e", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("4f", Cur = No_Element, True);

      --  Insert in middle
      Cur := List.First;
      Cur := Next (Cur);
      Cur := Next (Cur);
      Check ("5a", Constant_Ref (Cur), 2);

      List.Insert (Cur, -2);
      Val.Validate ("5", List);
      Check ("5a", List.Length, 5);
      Cur := List.First;
      Check ("5b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("5c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("5d", Constant_Ref (Cur), -2);
      Next (Cur);
      Check ("5e", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("5f", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("5g", Cur = No_Element, True);

   end Test_Insert;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Test_Insert'Access, "Test_Insert");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_definite_doubly_linked_lists.adb");
   end Name;

end Test_Definite_Doubly_Linked_Lists;
