--  Abstract :
--
--  Run one test, while working on it.

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Test_Unbounded_Definite_Vectors_Sorted;
with Test_Bounded_Definite_Vectors_Sorted;
procedure Test_One_Harness
is
   --  command line arguments (all optional, order matters):
   --  <verbose> test_name routine_name trace
   --  1         2         3            4
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  routine_name can be '' to set trace or cost for all routines.

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;
   Trace  : constant Integer := (if Argument_Count >= 4 then Integer'Value (Argument (4)) else 0);
   pragma Unreferenced (Trace);

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
   Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

   case Argument_Count is
   when 0 | 1 =>
      null;

   when 2 =>
      Filter.Set_Name (Argument (2)); -- test name only

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

   Add_Test (Suite, Test_Case_Access'(new Test_Unbounded_Definite_Vectors_Sorted.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Test_Bounded_Definite_Vectors_Sorted.Test_Case));

   Run (Suite, Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_One_Harness;
