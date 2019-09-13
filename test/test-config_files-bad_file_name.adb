--  Abstract :
--
--  See spec
--
--  Copyright (C) 2003, 2004, 2009, 2015 Stephen Leake.  All Rights Reserved.
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

with Ada.IO_Exceptions;
with AUnit.Assertions;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Bad_File_Name is

   ----------
   --  Local subprogram declarations

   procedure Test_Open (T : in out AUnit.Test_Cases.Test_Case'Class);

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Bad file name tests");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open'Access, "Test_Open");
   end Register_Tests;

   procedure Test_Open (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Bad_File_Name : constant String := "/bad:/for Windows";

      Config : Configuration_Type;

      Name_Error_Raised : Boolean := False;
   begin

      begin
         Open (Config, Bad_File_Name, Missing_File => Ignore, Read_Only => False);
         Close (Config);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Name_Error_Raised := True;
      end;

      AUnit.Assertions.Assert (Name_Error_Raised, "did not raise Name_Error");

   end Test_Open;

end Test.Config_Files.Bad_File_Name;
