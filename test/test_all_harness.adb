--  Abstract :
--
--  Run all AUnit tests for SAL.
--
--  Copyright (C) 2003 - 2009, 2012, 2015 - 2019 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.CSV.Test;
with SAL.File_Names.Test;
with SAL.Time_Conversions.Test;
with Test.Config_Files.All_Suite;
with Test_Bounded_Definite_Queues;
with Test_Bounded_Definite_Vectors;
with Test_Bounded_Definite_Vectors_Sorted;
with Test_Definite_Doubly_Linked_Lists;
with Test_Definite_Doubly_Linked_Lists_Sorted;
with Test_Gen_Images;
with Test_Graphs;
with Test_Min_Heap_Binary;
with Test_Min_Heaps_Fibonacci;
with Test_Network_Order;
with Test_Randomize_Lists;
with Test_Red_Black_Trees;
with Test_Stacks;
with Test_Stats;
with Test_Unbounded_Definite_Vectors;
with Test_Unbounded_Definite_Vectors_Protected;
with Test_Unbounded_Definite_Vectors_Sorted;
with Test_Unbounded_Indefinite_Vectors;
procedure Test_All_Harness
is
   --  command line arguments (all optional, order matters):
   --  <verbose> test_name routine_name trace
   --  1         2         3            4
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  routine_name can be '' to set trace or cost for all routines.

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;
   Trace  : Integer;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

begin
   declare
      use Ada.Command_Line;
   begin
      Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

      case Argument_Count is
      when 0 | 1 =>
         null;

      when 2 =>
         Filter.Set_Name (Argument (2));

      when others =>
         declare
            Test_Name    : String renames Argument (2);
            Routine_Name : String renames Argument (3);
         begin
            if Test_Name = "" then
               Filter.Set_Name (Routine_Name);
            elsif Routine_Name = "" then
               Filter.Set_Name (Test_Name);
            else
               Filter.Set_Name (Test_Name & " : " & Routine_Name);
            end if;
         end;
      end case;
      Trace := (if Argument_Count >= 4 then Integer'Value (Argument (4)) else 0);
   end;

   --  This is first because it's a suite.
   Add_Test (Suite, Test.Config_Files.All_Suite);

   Add_Test (Suite, Test_Case_Access'(new SAL.CSV.Test.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new SAL.File_Names.Test.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new SAL.Time_Conversions.Test.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Gen_Images.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Graphs.Test_Case (Trace)));
   Add_Test (Suite, Test_Case_Access'(new Test_Bounded_Definite_Queues.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Bounded_Definite_Vectors.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Bounded_Definite_Vectors_Sorted.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Definite_Doubly_Linked_Lists.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Definite_Doubly_Linked_Lists_Sorted.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Min_Heap_Binary.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Min_Heaps_Fibonacci.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Network_Order.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Randomize_Lists.Test_Case (Debug => False)));
   Add_Test (Suite, Test_Case_Access'(new Test_Red_Black_Trees.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Stacks.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Stats.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Unbounded_Definite_Vectors.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Unbounded_Definite_Vectors_Protected.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Unbounded_Definite_Vectors_Sorted.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Unbounded_Indefinite_Vectors.Test_Case (Trace)));

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_All_Harness;
