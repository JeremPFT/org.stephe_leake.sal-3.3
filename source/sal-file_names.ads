--  Abstract:
--
--  Parsing file names into parts, and recombining them.
--
--  Copyright (C) 1997 - 2000, 2003, 2004, 2007, 2009, 2011, 2012 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with Ada.Strings.Unbounded;
package SAL.File_Names is
   pragma Elaborate_Body; --  body depends on GNAT packages.

   type File_Name_Type is private;

   procedure Create
     (File_Name : in out File_Name_Type;
      Full_Path : in     String);
   function Create (Full_Path : in String) return File_Name_Type;
   --  Directory separators '\' and '/' are replaced by operating
   --  system directory separator.

   function Full_Name (File_Name : in File_Name_Type) return String;
   --  Returns full path.

   function Length (File_Name : in File_Name_Type) return Natural;
   --  Return length of Full_Name (File_Name).

   function Device         (File_Name : in File_Name_Type) return String;
   function Path           (File_Name : in File_Name_Type) return String;
   function Device_Path    (File_Name : in File_Name_Type) return String;
   function Name           (File_Name : in File_Name_Type) return String;
   function Extension      (File_Name : in File_Name_Type) return String;
   function Name_Extension (File_Name : in File_Name_Type) return String;
   --  Return indicated portion of Full_Name from Create, null string
   --  if not present.
   --
   --  Device ends in ':'
   --  Path ends in '\'
   --  Extension starts with '.'

   function Has_Device    (File_Name : in File_Name_Type) return Boolean;
   function Has_Path      (File_Name : in File_Name_Type) return Boolean;
   function Has_Name      (File_Name : in File_Name_Type) return Boolean;
   function Has_Extension (File_Name : in File_Name_Type) return Boolean;

   function With_Default (File_Name, Default : in File_Name_Type) return File_Name_Type;
   --  Return a file name, with fields that are empty in File_Name
   --  filled by the values from Default.

   function Resolve_Relative (File_Name, Current_Directory : in File_Name_Type) return File_Name_Type;
   --  Resolve relative paths using Current_Directory if needed.
   --
   --  0) Current directory must not contain dots; Invalid_Format will
   --     be raised if it does.
   --
   --  1) If File_Name has no path, Current_Directory is prepended,
   --     and the result is returned.
   --
   --  2) If File_Name starts with "." (but not ".."),
   --     Current_Directory is prepended, the dots are resolved, and the
   --     result is returned.
   --
   --  3) Otherwise, the dots are resolved, and the result is
   --     returned.
   --
   --  "dots are resolved" means:
   --
   --  './' is dropped
   --  'foo/../' is dropped
   --
   --  Raises Invalid_Format for illegal constructs, such as c:/../

   ----------
   --  Related operations.

   function Replace_Environment_Variables (Path : in String) return String;
   --  Expand environment variables in Path.
   --
   --  Environment variables are indicated by a leading $, either at
   --  the start of the string, or immediately after a directory
   --  separator. The environment variable name is terminated by a
   --  directory separator, a dot (for partial IP addresses), or the
   --  end of the string.
   --
   --  Raises Constraint_Error if an environment variable is specified
   --  that is not defined in the environment.

private
   type File_Name_Type is record
      Full_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Device_Last    : Natural := 0;
      Path_Last      : Natural := 0;
      Name_First     : Natural := 0;
      Name_Last      : Natural := 0;
      Extension_Last : Natural := 0;
      --  First character in Full_Name is Element (Full_Name, 1).
      --  Device_Last is the colon, or zero if no device
      --  Path_last is the last backslash, or zero if no path
      --  Name_First is first letter of name, undefined if no name
      --  Name_Last is the letter before the period, or zero if no name
      --  Extension_Last is the last letter, or zero if no extension
   end record;

end SAL.File_Names;
