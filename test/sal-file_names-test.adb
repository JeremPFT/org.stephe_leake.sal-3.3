--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 - 2005, 2008 - 2012, 2015, 2016 Stephen Leake.  All Rights Reserved.
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

with GNAT.OS_Lib;
with AUnit.Checks;
package body SAL.File_Names.Test is

   function Shift (Name : in String) return String
   is
      Shifted : constant String (Name'First + 4 .. Name'Last + 4) := Name;
   begin
      return Shifted;
   end Shift;

   function To_OS (Item : in String) return String
   is
      Result : String := Item;
   begin
      if GNAT.OS_Lib.Directory_Separator = '/' then
         for I in Item'Range loop
            if Item (I) = '\' then
               Result (I) := '/';
            end if;
         end loop;
      else
         for I in Item'Range loop
            if Item (I) = '/' then
               Result (I) := '\';
            end if;
         end loop;
      end if;
      return Result;
   end To_OS;

   procedure Check
     (Message        : in String;
      File_Name      : in File_Name_Type;
      Full_Name      : in String;
      Device_Last    : in Natural;
      Path_Last      : in Natural;
      Name_First     : in Natural;
      Name_Last      : in Natural;
      Extension_Last : in Natural)
   is
      use AUnit.Checks;
   begin
      Check (Message & ".Full_Name", Ada.Strings.Unbounded.To_String (File_Name.Full_Name), To_OS (Full_Name));
      Check (Message & ".Device_Last", File_Name.Device_Last, Device_Last);
      Check (Message & ".Path_Last", File_Name.Path_Last, Path_Last);
      Check (Message & ".Name_First", File_Name.Name_First, Name_First);
      Check (Message & ".Name_Last", File_Name.Name_Last, Name_Last);
      Check (Message & ".Extension_Last", File_Name.Extension_Last, Extension_Last);
   end Check;

   ----------
   --  Test subprograms

   procedure Test_Create (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check
        ("1",
         Create ("e:\Ada\Windex\Source_Common\windex-file_parsing.ads"),
         Full_Name      => "e:\Ada\Windex\Source_Common\windex-file_parsing.ads",
         Device_Last    => 2,
         Path_Last      => 28,
         Name_First     => 29,
         Name_Last      => 47,
         Extension_Last => 51);

      Check
        ("2",
         Create ("Ada\Windex\Source_Common\windex-file_parsing.ads"),
         Full_Name      => "Ada\Windex\Source_Common\windex-file_parsing.ads",
         Device_Last    => 0,
         Path_Last      => 25,
         Name_First     => 26,
         Name_Last      => 44,
         Extension_Last => 48);

      Check
        ("3",
         Create ("windex-file_parsing.ads"),
         Full_Name      => "windex-file_parsing.ads",
         Device_Last    => 0,
         Path_Last      => 0,
         Name_First     => 1,
         Name_Last      => 19,
         Extension_Last => 23);

      Check
        ("4",
         Create ("windex-file_parsing"),
         Full_Name      => "windex-file_parsing",
         Device_Last    => 0,
         Path_Last      => 0,
         Name_First     => 1,
         Name_Last      => 19,
         Extension_Last => 0);

      Check
        ("4a",
         Create (Shift ("windex-file_parsing")),
         Full_Name      => "windex-file_parsing",
         Device_Last    => 0,
         Path_Last      => 0,
         Name_First     => 1,
         Name_Last      => 19,
         Extension_Last => 0);

      Check
        ("5",
         Create (".ads"),
         Full_Name      => ".ads",
         Device_Last    => 0,
         Path_Last      => 0,
         Name_First     => 0,
         Name_Last      => 0,
         Extension_Last => 4);

      Check
        ("6",
         Create ("e:\Ada\Windex\Source_Common\windex-file_parsing"),
         Full_Name      => "e:\Ada\Windex\Source_Common\windex-file_parsing",
         Device_Last    => 2,
         Path_Last      => 28,
         Name_First     => 29,
         Name_Last      => 47,
         Extension_Last => 0);

      Check
        ("7",
         Create ("e:\Ada\Windex\Source_Common\"),
         Full_Name      => "e:\Ada\Windex\Source_Common\",
         Device_Last    => 2,
         Path_Last      => 28,
         Name_First     => 0,
         Name_Last      => 0,
         Extension_Last => 0);

      Check
        ("8",  --  malformed name, but we can parse it
         Create ("e:windex-file_parsing.ads"),
         Full_Name      => "e:windex-file_parsing.ads",
         Device_Last    => 2,
         Path_Last      => 0,
         Name_First     => 3,
         Name_Last      => 21,
         Extension_Last => 25);

      Check
        ("9",  --  malformed name, but we can parse it
         Create (Shift ("e:windex-file_parsing")),
         Full_Name      => "e:windex-file_parsing",
         Device_Last    => 2,
         Path_Last      => 0,
         Name_First     => 3,
         Name_Last      => 21,
         Extension_Last => 0);

      Check
        ("10",
         Create (Shift (".")),
         Full_Name      => ".",
         Device_Last    => 0,
         Path_Last      => 1,
         Name_First     => 0,
         Name_Last      => 0,
         Extension_Last => 0);

      Check
        ("11",
         Create ("..\..\Stephe\foo"),
         Full_Name      => "..\..\Stephe\foo",
         Device_Last    => 0,
         Path_Last      => 13,
         Name_First     => 14,
         Name_Last      => 16,
         Extension_Last => 0);

      --  Plain dot in path
      Check
        ("12",
         Create ("/home/Stephe/Lynx_5.0/foo.txt"),
         Full_Name      => "/home/Stephe/Lynx_5.0/foo.txt",
         Device_Last    => 0,
         Path_Last      => 22,
         Name_First     => 23,
         Name_Last      => 25,
         Extension_Last => 29);

      --  Plain dot in path, no simple name
      Check
        ("13",
         Create ("/home/Stephe/Lynx_5.0/"),
         Full_Name      => "/home/Stephe/Lynx_5.0/",
         Device_Last    => 0,
         Path_Last      => 22,
         Name_First     => 0,
         Name_Last      => 0,
         Extension_Last => 0);

   end Test_Create;

   procedure Test_With_Default (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check
        ("1",
         With_Default
           (Create (""),
            Create ("e:\Ada\Windex\Source_Common\windex-file_parsing.ads")),
         Full_Name      => "e:\Ada\Windex\Source_Common\windex-file_parsing.ads",
         Device_Last    => 2,
         Path_Last      => 28,
         Name_First     => 29,
         Name_Last      => 47,
         Extension_Last => 51);

      Check
        ("2",
         With_Default
           (Create (""),
            Create ("..\..\Test\stephe.vcr")),
         Full_Name      => "..\..\Test\stephe.vcr",
         Device_Last    => 0,
         Path_Last      => 11,
         Name_First     => 12,
         Name_Last      => 17,
         Extension_Last => 21);

      Check
        ("3",
         With_Default
           (Create ("e:\Stephe\"),
            Create ("..\..\Test\stephe.vcr")),
         Full_Name      => "e:\Stephe\stephe.vcr",
         Device_Last    => 2,
         Path_Last      => 10,
         Name_First     => 11,
         Name_Last      => 16,
         Extension_Last => 20);

      Check
        ("4",
         With_Default
           (Create ("e:\Stephe\foo"),
            Create (".vcr")),
         Full_Name      => "e:\Stephe\foo.vcr",
         Device_Last    => 2,
         Path_Last      => 10,
         Name_First     => 11,
         Name_Last      => 13,
         Extension_Last => 17);

      Check
        ("5",
         With_Default
           (Create (".vcr"),
            Create ("e:\Stephe\foo")),
         Full_Name      => "e:\Stephe\foo.vcr",
         Device_Last    => 2,
         Path_Last      => 10,
         Name_First     => 11,
         Name_Last      => 13,
         Extension_Last => 17);

      Check
        ("6",
         With_Default
           (Create ("..\..\Stephe\foo"),
            Create (".vcr")),
         Full_Name      => "..\..\Stephe\foo.vcr",
         Device_Last    => 0,
         Path_Last      => 13,
         Name_First     => 14,
         Name_Last      => 16,
         Extension_Last => 20);

      Check
        ("7",
         With_Default
           (Create (".vcr"),
            Create ("..\..\Stephe\foo")),
         Full_Name      => "..\..\Stephe\foo.vcr",
         Device_Last    => 0,
         Path_Last      => 13,
         Name_First     => 14,
         Name_Last      => 16,
         Extension_Last => 20);

   end Test_With_Default;

   procedure Test_Replace_Environment_Variables (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;

      Env_Var_Name  : constant String := "SAL_FILE_NAMES_TEST_ENV";
      Env_Var_Value : constant String := "Env_Value";

   begin

      --  First test Setenv, Getenv

      GNAT.OS_Lib.Setenv (Env_Var_Name, Env_Var_Value);
      declare
         Env_Access : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv (Env_Var_Name);
         Result : constant String := Env_Access.all;
      begin
         GNAT.OS_Lib.Free (Env_Access);
         Check ("put get", Result, Env_Var_Value);
      end;

      --  Now test Replace_Environment_Variables

      declare
         Name : constant String := Replace_Environment_Variables ("$" & Env_Var_Name & "/bar");
      begin
         Check ("1", Name, Env_Var_Value & "/bar");
      end;

      declare
         Name : constant String := Replace_Environment_Variables
           ("$" & Env_Var_Name & "/bar/" & "$" & Env_Var_Name & "/foo");
      begin
         Check ("2", Name, Env_Var_Value & "/bar/" & Env_Var_Value & "/foo");
      end;

      declare
         Arg  : constant String := "/not_env_$" & Env_Var_Name & "/bar";
         Name : constant String := Replace_Environment_Variables (Arg);
      begin
         Check ("3", Name, Arg);
      end;

      declare
         Arg  : constant String := "/no_dollar";
         Name : constant String := Replace_Environment_Variables (Arg);
      begin
         Check ("4", Name, Arg);
      end;

      declare
         Arg  : constant String := "$" & Env_Var_Name & ".gsfc.nasa.gov";
         Name : constant String := Replace_Environment_Variables (Arg);
      begin
         Check ("5", Name, Env_Var_Value & ".gsfc.nasa.gov");
      end;

   end Test_Replace_Environment_Variables;

   procedure Test_Resolve_Relative (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin

      Check
        ("1",
         Resolve_Relative
           (File_Name         => Create ("..\..\Test\stephe.vcr"),
            Current_Directory => Create ("e:\Stephe\SAL\Build\x86_gnu_windows_debug\")),
         Full_Name            => "e:\Stephe\SAL\Test\stephe.vcr",
         Device_Last          => 2,
         Path_Last            => 19,
         Name_First           => 20,
         Name_Last            => 25,
         Extension_Last       => 29);

      Check
        ("2",
         Resolve_Relative
           (File_Name         => Create ("."),
            Current_Directory => Create ("e:\Stephe\SAL\Test\")),
         Full_Name            => "e:\Stephe\SAL\Test\",
         Device_Last          => 2,
         Path_Last            => 19,
         Name_First           => 0,
         Name_Last            => 0,
         Extension_Last       => 0);

      Check
        ("3",
         Resolve_Relative
           (File_Name         => Create ("test_file_menu_gap.cards"),
            Current_Directory => Create ("e:\Stephe\SAL\Test\")),
         Full_Name            => "e:\Stephe\SAL\Test\test_file_menu_gap.cards",
         Device_Last          => 2,
         Path_Last            => 19,
         Name_First           => 20,
         Name_Last            => 37,
         Extension_Last       => 43);

      Check
        ("4",
         Resolve_Relative
           (File_Name         => Create ("c:/Stephe/Ada/Cards/build/stephe.cards"),
            Current_Directory => Create ("e:/Stephe/Ada/Cards")),
         Full_Name            => "c:\Stephe\Ada\Cards\build\stephe.cards",
         Device_Last          => 2,
         Path_Last            => 26,
         Name_First           => 27,
         Name_Last            => 32,
         Extension_Last       => 38);

      --  No devices (on Lynx or Linux)
      Check
        ("5",
         Resolve_Relative
           (File_Name         => Create ("../misc/"),
            Current_Directory => Create ("/home/Stephe/")),
         Full_Name            => "/home/misc/",
         Device_Last          => 0,
         Path_Last            => 11,
         Name_First           => 0,
         Name_Last            => 0,
         Extension_Last       => 0);

      Check
        ("6",
         Resolve_Relative
           (File_Name         => Create ("/home/Stephe/misc/"),
            Current_Directory => Create ("/projects/books/")),
         Full_Name            => "/home/Stephe/misc/",
         Device_Last          => 0,
         Path_Last            => 18,
         Name_First           => 0,
         Name_Last            => 0,
         Extension_Last       => 0);

      Check
        ("7",
         Resolve_Relative
           (File_Name         => Create ("/home/Stephe/misc/books.db"),
            Current_Directory => Create ("/projects/books/")),
         Full_Name            => "/home/Stephe/misc/books.db",
         Device_Last          => 0,
         Path_Last            => 18,
         Name_First           => 19,
         Name_Last            => 23,
         Extension_Last       => 26);

      Check
        ("8",
         Resolve_Relative
           (File_Name         => Create ("../misc/"),
            Current_Directory => Create ("/home/Lynx_5.0/Stephe/")),
         Full_Name            => "/home/Lynx_5.0/misc/",
         Device_Last          => 0,
         Path_Last            => 20,
         Name_First           => 0,
         Name_Last            => 0,
         Extension_Last       => 0);

   end Test_Resolve_Relative;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("sal-file_names-test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Create'Access, "Test_Create");
      Register_Routine (T, Test_With_Default'Access, "Test_With_Default");
      Register_Routine (T, Test_Replace_Environment_Variables'Access, "Test_Replace_Environment_Variables");
      Register_Routine (T, Test_Resolve_Relative'Access, "Test_Resolve_Relative");
   end Register_Tests;

end SAL.File_Names.Test;
