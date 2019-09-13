--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009, 2012, 2015, 2018 Stephen Leake.  All Rights Reserved.
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
with AUnit.Checks;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with SAL.Gen_Randomize_Doubly_Linked_Lists;
package body Test_Randomize_Lists is

   type Integer_Array_Type is array (Positive range <>) of Integer;

   package Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);

   procedure Check
     (Label    : in String;
      Computed : in Lists.List;
      Expected : in Integer_Array_Type)
   is
      use AUnit.Checks;
      use Lists;

      J : Cursor := First (Computed);
   begin
      Check (Label & " count", Integer (Computed.Length), Expected'Length);

      for I in Expected'Range loop
         Check (Label & Integer'Image (I), Element (J), Expected (I));
         Next (J);
      end loop;
   exception
   when others =>
      --  Assume failed due to new compiler; display computed
      Ada.Text_IO.Put ("(");
      for I of Computed loop
         Ada.Text_IO.Put (Integer'Image (I) & ",");
      end loop;
      Ada.Text_IO.Put_Line (")");
      raise;
   end Check;

   ----------
   --  Test procedures

   procedure Randomize (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Lists;

      procedure Randomize is new SAL.Gen_Randomize_Doubly_Linked_Lists (Lists);

      List : Lists.List;
   begin
      for I in 1 .. 20 loop
         List.Append (I);
      end loop;

      Randomize (List, Seed => 0);

      --  Result is empirical, and could change with each new version
      --  of the compiler, if it changes the Random implementation.
      --  Correct for GNAT GPL 2014
      Check ("random", List, (10, 1, 11, 2, 14, 6, 20, 3, 9, 16, 13, 5, 17, 15, 18, 19, 7, 4, 8, 12));

   end Randomize;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Randomize'Access, "Randomize");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../test/test_randomize_lists.adb");
   end Name;

end Test_Randomize_Lists;
