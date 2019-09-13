--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2018 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected;
package body Test_Unbounded_Definite_Vectors_Protected
is
   package Integer_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (Index_Type      => Positive,
      Element_Type    => Integer,
      Default_Element => Integer'Last);

   package Task_Safe_Int_Vectors is new Integer_Vectors.Gen_Protected;
   use Task_Safe_Int_Vectors;

   Shared : Vector;

   task type Test_Task is
      entry Append (Value : in Integer);
      entry Prepend (Value : in Integer);

      entry Read (Index : in Positive; Value : out Integer);

      entry Wait;
   end Test_Task;

   task body Test_Task
   is
      use Ada.Exceptions;
      use Ada.Text_IO;
      use GNAT.Traceback.Symbolic;
      Temp_Value : Integer;
   begin
      loop
         select
            accept Append (Value : in Integer) do
               Temp_Value := Value;
            end Append;
            Shared.Append (Temp_Value);

         or
            accept Prepend (Value : in Integer) do
               Temp_Value := Value;
            end Prepend;
            Shared.Prepend (Temp_Value);

         or
            accept Read (Index : in Positive; Value : out Integer) do
               Value := Shared.Element (Index);
            end Read;

         --  WORKAROUND: see comment on iterators in spec
         --  or accept Inc_All;
         --     for Element of Shared loop
         --        --  Cursor holds read lock for duration of loop
         --        Element := Element + 1;
         --     end loop;

         or accept Wait;

         or
            terminate;
         end select;
      end loop;
   exception
   when E : others =>
      Put_Line ("test_task got exception " & Exception_Name (E) & " : " & Exception_Message (E));
      Put_Line (Symbolic_Traceback (E));
   end Test_Task;

   ----------
   --  Tests

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      Test_Task_1 : Test_Task;
      Test_Task_2 : Test_Task;
      Value       : Integer;
   begin
      Shared.Set_First_Last (10, 9); -- so there is room for prepend

      --  Start with empty vector; append must allocate.
      Test_Task_1.Append (1);
      Test_Task_2.Append (2);

      Test_Task_1.Wait;
      Test_Task_2.Wait;

      --  The order in which the two values get written is a race condition,
      --  but they should both be there, and the capacity should be 2, not 1
      --  or 4.

      Check ("1.length", Shared.Length, 2);
      Check ("1.capacity", Shared.Capacity, 2);

      --  Test Constant_Reference lock
      Value := Shared.Element (10);

      Check ("1a lock", Shared.Locked, False);

      Check
        ("1.data",
         (Shared.Element (10) = 1 and Shared.Element (11) = 2) or
           (Shared.Element (10) = 2 and Shared.Element (11) = 1),
         True);

      Check ("1b lock", Shared.Locked, False);

      --  WORKAROUND: see comment on iterators in spec
      --  Test Iterator and Variable_Reference lock.
      --
      --  Iterator holds read lock for duration of loop. Each Element is a
      --  Variable_Reference, which also holds a read lock.
      --  Check ("2a lock", Shared.Locked, False);
      --  for Element of Shared loop
      --     Element := Element + 1;
      --  end loop;
      --  Check ("2b lock", Shared.Locked, False);

      --  First prepend will allocate, second will not.
      Test_Task_1.Prepend (3);
      Test_Task_2.Prepend (4);

      Test_Task_1.Wait;
      Test_Task_2.Wait;

      --  The order in which the two values get written is a race condition,
      --  but they should both be there, and the capacity should be 4, not
      --  8.

      Check ("3.length", Shared.Length, 4);
      Check ("3.capacity", Shared.Capacity, 4);
      Check
        ("3.data",
         ((Shared.Element (8) = 3 and Shared.Element (9) = 4) or
            (Shared.Element (8) = 4 and Shared.Element (9) = 3)) and
           ((Shared.Element (10) = 1 and Shared.Element (11) = 2) or
              (Shared.Element (10) = 2 and Shared.Element (11) = 1)),
         True);

      Test_Task_1.Append (5);
      Test_Task_2.Read (11, Value);

      Test_Task_1.Wait;
      Test_Task_2.Wait;

      Check ("4.length", Shared.Length, 5);
      Check ("4.capacity", Shared.Capacity, 8);
      Check ("4.read_data", Value = 1 or Value = 2, True);
      Check ("4.written_data", Shared.Element (12), 5);

      --  WORKAROUND: see comment on iterators in spec
      --  Check ("5a lock", Shared.Locked, False);
      --  Test_Task_1.Inc_All;
      --  Test_Task_2.Append (6); -- Requires a write lock; should be appended after increment loop completes

      --  Test_Task_1.Wait;
      --  Test_Task_2.Wait;

      --  Check ("5b lock", Shared.Locked, False);
      --  Check ("5 length", Shared.Length, 6);
      --  Check ("5 data", Shared.Element (13), 6);

   end Nominal;

   --  FIXME: cursor/read, cursor/append

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_unbounded_definite_vectors_protected.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Unbounded_Definite_Vectors_Protected;
