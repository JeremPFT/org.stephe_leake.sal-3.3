--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2004, 2005, 2008, 2009, 2011, 2015, 2017 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks;
with SAL.AUnit;
with SAL.Gen_Bounded_Definite_Queues.Gen_Test;
package body Test_Bounded_Definite_Queues
is
   use all type SAL.Base_Peek_Type;

   package Integer_Queues is new SAL.Gen_Bounded_Definite_Queues (Integer);
   package Integer_Queues_Test is new Integer_Queues.Gen_Test;
   use Integer_Queues;

   procedure Check_Head_Tail
     (Label    : in String;
      Queue    : in Queue_Type;
      Exp_Head : in SAL.Base_Peek_Type;
      Exp_Tail : in SAL.Base_Peek_Type)
   is
      use Integer_Queues_Test;
      use SAL.AUnit;
   begin
      Check (Label & ".Head", Get_Head (Queue), Exp_Head);
      Check (Label & ".Tail", Get_Tail (Queue), Exp_Tail);
   end Check_Head_Tail;

   procedure Test_Remove
     (Label    : in     String;
      Queue    : in out Queue_Type;
      Exp_Head : in     SAL.Base_Peek_Type;
      Exp_Tail : in     SAL.Base_Peek_Type;
      Exp_Item : in     Integer)
   is
      use AUnit.Checks;
      Item  : constant Integer := Remove (Queue);
   begin
      Check (Label & ".Item", Item, Exp_Item);
      Check_Head_Tail (Label, Queue, Exp_Head, Exp_Tail);
   end Test_Remove;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use SAL.AUnit;

      pragma Warnings (Off, """Queue"" may be referenced before it has a Value");
      --  Queue is initialized by default, except Queue.Data. But since
      --  Queue.Item_Count is 0, Queue.Data cannot be accessed.
      Queue : Queue_Type (5);
   begin
      Check ("Is_Empty 0", Is_Empty (Queue), True);
      Add (Queue, 1);
      Check ("Is_Empty 1", Is_Empty (Queue), False);
      Check_Head_Tail ("1", Queue, Exp_Head => 1, Exp_Tail => 1);
      Check ("1 count", Count (Queue), 1);
      Check ("1 peek", Peek (Queue), 1);

      Test_Remove ("2", Queue, Exp_Head => 1, Exp_Tail => 1, Exp_Item => 1);
      Check ("Is_Empty 2", Is_Empty (Queue), True);
      Check ("2 count", Count (Queue), 0);

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Check_Head_Tail ("1234", Queue, Exp_Head => 1, Exp_Tail => 4);
      Check ("1234 count", Count (Queue), 4);
      Check ("Is_Empty 1234", Is_Empty (Queue), False);

      Test_Remove ("234", Queue, Exp_Head => 2, Exp_Tail => 4, Exp_Item => 1);
      Check ("234 count", Count (Queue), 3);
      Check ("234 peek", Peek (Queue), 2);

      Add (Queue, 5);
      Check_Head_Tail ("2345", Queue, Exp_Head => 2, Exp_Tail => 5);

      Add (Queue, 6);
      Check ("Is_Full 23456", Is_Full (Queue), True);
      Check_Head_Tail ("23456", Queue, Exp_Head => 2, Exp_Tail => 1);

      Test_Remove ("3456", Queue, Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
      Check ("Is_Full 3456", Is_Full (Queue), False);

      Add_To_Head (Queue, 2);
      Check_Head_Tail ("23456", Queue, Exp_Head => 2, Exp_Tail => 1);

      Test_Remove ("3456", Queue, Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_bounded_definite_queues.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Bounded_Definite_Queues;
