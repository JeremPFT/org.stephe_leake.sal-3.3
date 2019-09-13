--  Abstract :
--
--  See spec.
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
with AUnit.Assertions;
with AUnit.Checks;
with SAL.AUnit;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Binary.Gen_Test;
package body Test_Min_Heap_Binary is

   type Element_Type is record
      Data : Float;
      Key  : Integer;
   end record;

   function Key (Element : in Element_Type) return Integer is (Element.Key);
   procedure Set_Key (Element : in out Element_Type; Key : in Integer)
   is begin
      Element.Key := Key;
   end Set_Key;

   package Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps_Binary
     (Element_Type => Element_Type,
      Key_Type     => Integer,
      Key          => Key,
      Set_Key      => Set_Key,
      "<"          => "<",
      Initial_Size => 6);

   package Heap_Test is new Heaps.Gen_Test;

   --  We don't 'use Heaps', to test that
   --  object.method notation works for heap operations.

   procedure Check
     (Label    : in String;
      Computed : in Element_Type;
      Expected : in Element_Type)
   is
      use AUnit.Checks;
   begin
         Check (Label & ".data", Computed.Data, Expected.Data);
         Check (Label & ".key", Computed.Key, Expected.Key);
   end Check;

   type Element_Array_Type is array (SAL.Peek_Type range <>) of Element_Type;

   procedure Check
     (Label    : in String;
      Computed : in Heaps.Heap_Type;
      Expected : in Element_Array_Type)
   is
      use SAL.AUnit;
   begin
      Check (Label & ".count", Computed.Count, Expected'Length);

      for I in Expected'Range loop
         Check (Label & SAL.Base_Peek_Type'Image (I), Heap_Test.Peek (Computed, I), Expected (I));
      end loop;
   end Check;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.AUnit;

      Min_Heap : Heaps.Heap_Type;
   begin

      Check ("0b", Min_Heap.Count, 0);

      --  At each step, we show the expected binary tree. Note that it is
      --  not completely ordered; invariant is that each parent is < all of
      --  its children.
      Min_Heap.Insert ((5.0, 5));
      --               5
      Check ("1a", Min_Heap, (1 => (5.0, 5)));

      Min_Heap.Insert ((4.0, 4));
      --               4
      --             5    ?
      Check ("1b", Min_Heap, ((4.0, 4), (5.0, 5)));

      Min_Heap.Insert ((3.0, 3));
      --               3
      --             5    4
      Check ("1c", Min_Heap, ((3.0, 3), (5.0, 5), (4.0, 4)));

      Min_Heap.Insert ((2.0, 2));
      --               2
      --             3    4
      --           5   ?    ?
      Check ("1d", Min_Heap, ((2.0, 2), (3.0, 3), (4.0, 4), (5.0, 5)));

      Min_Heap.Insert ((1.0, 1));
      --               1
      --             2    4
      --           5   3    ?
      Check ("1e", Min_Heap, ((1.0, 1), (2.0, 2), (4.0, 4), (5.0, 5), (3.0, 3)));
      Check ("1e", Min_Heap.Count, 5);

      Check ("2", Min_Heap.Get, (1.0, 1)); --  use rename
      --               2
      --             3    4
      --           5   ?    ?
      Check ("2a", Min_Heap, ((2.0, 2), (3.0, 3), (4.0, 4), (5.0, 5)));
      Check ("2b", Min_Heap.Count, 4);

      Min_Heap.Add ((6.0, 6));
      --               2
      --             3    4
      --           5   6    ?
      Check ("3a", Min_Heap, ((2.0, 2), (3.0, 3), (4.0, 4), (5.0, 5), (6.0, 6)));

      Check ("4", Min_Heap.Remove, (2.0, 2));
      Check ("5", Min_Heap.Remove, (3.0, 3));
      Check ("6", Min_Heap.Remove, (4.0, 4));
      Check ("7", Min_Heap.Remove, (5.0, 5));
      Check ("7", Min_Heap.Remove, (6.0, 6));
      Check ("8", Min_Heap.Count, 0);

      declare
         Junk : Element_Type;
      begin
         Junk := Min_Heap.Remove;
         AUnit.Assertions.Assert (False, "9 did not get exception");
      exception
      when SAL.Container_Empty =>
         null;
      end;

      for I in reverse 1 .. 3 loop
         Min_Heap.Insert ((Float (I), I)); -- use rename
      end loop;
      Check ("10", Min_Heap, ((1.0, 1), (3.0, 3), (2.0, 2)));

      Min_Heap.Clear;
      Check ("11", Min_Heap.Count, 0);
   end Nominal;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_min_heaps_binary.adb");
   end Name;

end Test_Min_Heap_Binary;
