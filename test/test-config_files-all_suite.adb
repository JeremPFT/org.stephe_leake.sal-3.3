--  Abstract :
--
--  AUnit test suite for all Config_Files tests.
--
--  Copyright (C) 2002 - 2005, 2007, 2009, 2015, 2018 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Test.Config_Files.Abs_File;
with Test.Config_Files.Append_File;
with Test.Config_Files.Bad_File_Name;
with Test.Config_Files.Base_Tests;
with Test.Config_Files.Case_Insensitive;
with Test.Config_Files.Comment_No_Equal;
with Test.Config_Files.Delete;
with Test.Config_Files.Duplicate_Key;
with Test.Config_Files.Error_Message;
with Test.Config_Files.Extra_Key;
with Test.Config_Files.Iterators;
with Test.Config_Files.Iterators_Nested;
with Test.Config_Files.No_File;
with Test.Config_Files.Time_Tests;
function Test.Config_Files.All_Suite return AUnit.Test_Suites.Access_Test_Suite
is
   Suite : constant Access_Test_Suite := new Test_Suite;
begin
   Add_Test (Suite, Test_Case_Access'(new Abs_File.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Append_File.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Bad_File_Name.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Base_Tests.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Case_Insensitive.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Comment_No_Equal.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Delete.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Duplicate_Key.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Extra_Key.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Error_Message.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Iterators.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Iterators_Nested.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new No_File.Test_Case));
   Add_Test (Suite, Test_Case_Access'(new Time_Tests.Test_Case));

   return Suite;
end Test.Config_Files.All_Suite;
