--  Abstract:
--
--  See spec.
--
--  Copyright (C) 1997 - 1999, 2003 - 2007, 2009 - 2010, 2012 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with GNAT.Directory_Operations;
package body SAL.File_Names is

   --  Local subprograms

   procedure Append
     (Source      : in     String;
      Target      : in out String;
      Target_Last : in out Natural)
   is
      Target_First : constant Natural := Target_Last + 1;
   begin
      Target_Last                          := Target_Last + Source'Length;
      Target (Target_First .. Target_Last) := Source;
   end Append;

   procedure Compact_Relative
     (Source      : in     String;
      Result      :    out String;
      Result_Last :    out Natural)
   is
      I             : Natural := Source'First;
      Dir_Seps      : array (1 .. Result'Length) of Natural;
      pragma Warnings (Off, Dir_Seps); --  compiler thinks we may reference before it has a value.

      Dir_Seps_Last : Integer := Dir_Seps'First - 1;
   begin
      Result_Last := Result'First - 1;

      loop
         exit when I > Source'Last;

         if Source (I) = '.' then
            if I = Source'Last then
               --  same as trailing ./
               I := I + 1;
            elsif Source (I + 1) = '.' then
               if I + 1 = Source'Last or else
                 (Source (I + 2) = '/' or Source (I + 2) = '\')
               then
                  --  delete one previous directory level
                  if Dir_Seps_Last < Dir_Seps'First + 1 then
                     --  there isn't one.
                     raise Invalid_Format with Source;
                  else
                     Result_Last   := Dir_Seps (Dir_Seps_Last - 1);
                     Dir_Seps_Last := Dir_Seps_Last - 1;
                     I             := I + 3;
                  end if;
               end if;
            elsif Source (I + 1) = '/' or Source (I + 1) = '\' then
               --  Ignore ./
               I := I + 2;
            else
               --  just a dot in a name
               Result_Last          := Result_Last + 1;
               Result (Result_Last) := Source (I);
               I := I + 1;
            end if;
         elsif Source (I) = '/' or Source (I) = '\' then
            Result_Last              := Result_Last + 1;
            Result (Result_Last)     := Source (I);
            Dir_Seps_Last            := Dir_Seps_Last + 1;
            Dir_Seps (Dir_Seps_Last) := Result_Last;
            I                        := I + 1;
         else
            Result_Last          := Result_Last + 1;
            Result (Result_Last) := Source (I);
            I                    := I + 1;
         end if;

      end loop;
   end Compact_Relative;

   procedure Copy
     (Source      : in     String;
      Target      : in out String;
      Target_Last :    out Natural)
   is begin
      Target_Last                          := Target'First + Source'Length - 1;
      Target (Target'First .. Target_Last) := Source;
   end Copy;

   ----------
   --  Public subprograms

   procedure Create
     (File_Name : in out File_Name_Type;
      Full_Path : in String)
   is begin
      File_Name :=
        (Full_Name      => Ada.Strings.Unbounded.To_Unbounded_String (Full_Path),
         Device_Last    => 0,
         Path_Last      => 0,
         Name_First     => 0,
         Name_Last      => 0,
         Extension_Last => 0);

      --  First assign all indices to Full_Path direct indices, then
      --  adjust to Unbounded indices. This makes calculating
      --  Name_First simpler.
      --
      --  First scan for directory info
      for I in Full_Path'Range loop
         if Full_Path (I) = ':' then
            File_Name.Device_Last := I;
         elsif Full_Path (I) = '\' then
            File_Name.Path_Last := I;
            Ada.Strings.Unbounded.Replace_Element (File_Name.Full_Name, I, GNAT.Directory_Operations.Dir_Separator);
         elsif Full_Path (I) = '/' then
            File_Name.Path_Last := I;
            Ada.Strings.Unbounded.Replace_Element (File_Name.Full_Name, I, GNAT.Directory_Operations.Dir_Separator);
         elsif Full_Path (I) = '.' then
            if (I + 1 <= Full_Path'Last and then Full_Path (I + 1) = '.') or
              (I - 1 >= Full_Path'First and then Full_Path (I - 1) = '.')
            then
               --  special case ".."
               null;
            elsif I = Full_Path'Last and I = Full_Path'First then
               --  Special case "."
               File_Name.Path_Last := Full_Path'First;
            else
               --  nothing special
               null;
            end if;
         end if;
      end loop;

      --  Now scan for simple name and extension info
      for I in reverse Integer'Max (File_Name.Path_Last + 1, Full_Path'First) .. Full_Path'Last loop
         if Full_Path (I) = '.' then
            if (I + 1 <= Full_Path'Last and then Full_Path (I + 1) = '.') or
              (I - 1 >= Full_Path'First and then Full_Path (I - 1) = '.')
            then
               --  special case ".."
               null;
            elsif I = Full_Path'Last and I = Full_Path'First then
               --  Special case "."
               null;
            else
               File_Name.Extension_Last := Full_Path'Last;
               --  Calculate Name_First, Name_Last
               if I = Full_Path'First then
                  --  Just extension, no name
                  null;
               elsif File_Name.Path_Last /= 0 then
                  File_Name.Name_First := File_Name.Path_Last + 1;
                  File_Name.Name_Last  := I - 1;
               elsif File_Name.Device_Last /= 0 then
                  File_Name.Name_First := File_Name.Device_Last + 1;
                  File_Name.Name_Last  := I - 1;
               else
                  File_Name.Name_First := Full_Path'First;
                  File_Name.Name_Last  := I - 1;
               end if;
               exit;
            end if;
         end if;
      end loop;

      if File_Name.Extension_Last = 0 and --  Name_* not set
        File_Name.Path_Last /= Full_Path'Last --  There is a name field
      then
         if File_Name.Path_Last = 0 then
            if File_Name.Device_Last = 0 then
               --  Name and nothing else; "foo"
               File_Name.Name_First := Full_Path'First;
               File_Name.Name_Last  := Full_Path'Last;
            else
               --  "e:foo"
               File_Name.Name_First := File_Name.Device_Last + 1;
               File_Name.Name_Last  := Full_Path'Last;
            end if;
         else
            --  Name but no extension; "e:/Stephe/foo"
            File_Name.Name_First := File_Name.Path_Last + 1;
            File_Name.Name_Last  := Full_Path'Last;
         end if;
      end if;

      --  Adjust to Unbounded indices; File_Name.Full_Name'First = 1.
      if File_Name.Device_Last /= 0 then
         File_Name.Device_Last := File_Name.Device_Last - Full_Path'First + 1;
      end if;
      if File_Name.Path_Last /= 0 then
         File_Name.Path_Last := File_Name.Path_Last - Full_Path'First + 1;
      end if;
      if File_Name.Name_First /= 0 then
         File_Name.Name_First := File_Name.Name_First - Full_Path'First + 1;
      end if;
      if File_Name.Name_Last /= 0 then
         File_Name.Name_Last := File_Name.Name_Last - Full_Path'First + 1;
      end if;
      if File_Name.Extension_Last /= 0 then
         File_Name.Extension_Last := File_Name.Extension_Last - Full_Path'First + 1;
      end if;
   end Create;

   function Create (Full_Path : in String) return File_Name_Type
   is
      Result : File_Name_Type;
   begin
      Create (Result, Full_Path);
      return Result;
   end Create;

   function Full_Name (File_Name : in File_Name_Type) return String
   is begin
      return Ada.Strings.Unbounded.To_String (File_Name.Full_Name);
   end Full_Name;

   function Length (File_Name : in File_Name_Type) return Natural
   is begin
      return Ada.Strings.Unbounded.Length (File_Name.Full_Name);
   end Length;

   function Device (File_Name : in File_Name_Type) return String
   is begin
      return Ada.Strings.Unbounded.Slice (File_Name.Full_Name, 1, File_Name.Device_Last);
   end Device;

   function Path (File_Name : in File_Name_Type) return String
   is begin
      return Ada.Strings.Unbounded.Slice (File_Name.Full_Name, File_Name.Device_Last + 1, File_Name.Path_Last);
   end Path;

   function Device_Path    (File_Name : in File_Name_Type) return String
   is begin
      return Ada.Strings.Unbounded.Slice (File_Name.Full_Name, 1, File_Name.Path_Last);
   end Device_Path;

   function Name           (File_Name : in File_Name_Type) return String
   is begin
      if File_Name.Name_First = 0 then
         return "";
      else
         return Ada.Strings.Unbounded.Slice
           (File_Name.Full_Name, File_Name.Name_First, File_Name.Name_Last);
      end if;
   end Name;

   function Extension      (File_Name : in File_Name_Type) return String
   is begin
      if File_Name.Extension_Last = 0 then
         return "";
      else
         return Ada.Strings.Unbounded.Slice
           (File_Name.Full_Name, File_Name.Name_Last + 1, File_Name.Extension_Last);
      end if;
   end Extension;

   function Name_Extension (File_Name : in File_Name_Type) return String
   is begin
      if File_Name.Extension_Last = 0 then
         if File_Name.Name_Last = 0 then
            return "";
         else
            return Ada.Strings.Unbounded.Slice
              (File_Name.Full_Name, File_Name.Name_First, File_Name.Name_Last);
         end if;
      else
         if File_Name.Name_Last = 0 then
            return Ada.Strings.Unbounded.Slice (File_Name.Full_Name, 1, File_Name.Extension_Last);
         else
            return Ada.Strings.Unbounded.Slice
              (File_Name.Full_Name, File_Name.Name_First, File_Name.Extension_Last);
         end if;
      end if;
   end Name_Extension;

   function Has_Device    (File_Name : in File_Name_Type) return Boolean
   is begin
      return File_Name.Device_Last /= 0;
   end Has_Device;

   function Has_Path      (File_Name : in File_Name_Type) return Boolean
   is begin
      return File_Name.Path_Last /= 0;
   end Has_Path;

   function Has_Name      (File_Name : in File_Name_Type) return Boolean
   is begin
      return File_Name.Name_First /= 0;
   end Has_Name;

   function Has_Extension (File_Name : in File_Name_Type) return Boolean
   is begin
      return File_Name.Extension_Last /= 0;
   end Has_Extension;

   function With_Default (File_Name, Default : in File_Name_Type) return File_Name_Type
   is
      Result_Device         : String (1 .. 2);
      Result_Device_Last    : Natural;
      Result_Path           : String (1 .. Length (File_Name) + Length (Default));
      Result_Path_Last      : Natural;
      Result_Name           : String (1 .. Result_Path'Length);
      Result_Name_Last      : Natural;
      Result_Extension      : String (1 .. Result_Path'Length);
      Result_Extension_Last : Natural;

      Result_File_Name : File_Name_Type;
   begin
      if Has_Device (File_Name) then
         Copy (Device (File_Name), Result_Device, Result_Device_Last);
      elsif Has_Device (Default) then
         Copy (Device (Default), Result_Device, Result_Device_Last);
      else
         Result_Device_Last := 0;
      end if;

      if Has_Path (File_Name) then
         Copy (Path (File_Name), Result_Path, Result_Path_Last);
      elsif Has_Path (Default) then
         Copy (Path (Default), Result_Path, Result_Path_Last);
      else
         Result_Path_Last := 0;
      end if;

      if Has_Name (File_Name) then
         Copy (Name (File_Name), Result_Name, Result_Name_Last);
      elsif Has_Name (Default) then
         Copy (Name (Default), Result_Name, Result_Name_Last);
      else
         Result_Name_Last := 0;
      end if;

      if Has_Extension (File_Name) then
         Copy (Extension (File_Name), Result_Extension, Result_Extension_Last);
      elsif Has_Extension (Default) then
         Copy (Extension (Default), Result_Extension, Result_Extension_Last);
      else
         Result_Extension_Last := 0;
      end if;

      Create
        (Result_File_Name,
         Result_Device (1 .. Result_Device_Last) &
           Result_Path (1 .. Result_Path_Last) &
           Result_Name (1 .. Result_Name_Last) &
           Result_Extension (1 .. Result_Extension_Last));

      return Result_File_Name;
   end With_Default;

   function Resolve_Relative (File_Name, Current_Directory : in File_Name_Type) return File_Name_Type
   is
      Result_Device      : String (1 .. 2);
      Result_Device_Last : Natural := 0; --  default no devices (on Lynx or Linux)
      Result_Path        : String (1 .. Length (File_Name) + Length (Current_Directory));
      Result_Path_Last   : Natural := Result_Path'First - 1;
      Result_File_Name   : File_Name_Type;
   begin
      if File_Name.Device_Last /= 0 then
         --  Assume we need nothing from Current_Directory
         Result_Device_Last := File_Name.Device_Last;
         Result_Device (1 .. Result_Device_Last) := Device (File_Name);

         Copy
           (Source      => Path (File_Name),
            Target      => Result_Path,
            Target_Last => Result_Path_Last);

      elsif '.' = Ada.Strings.Unbounded.Element (File_Name.Full_Name, File_Name.Device_Last + 1) or
        File_Name.Path_Last = 0
      then
         --  Need root path from current_directory
         if Current_Directory.Device_Last /= 0 then
            Result_Device_Last := Current_Directory.Device_Last;
            Result_Device (1 .. Result_Device_Last) := Device (Current_Directory);
         end if;

         Copy
           (Source      => Path (Current_Directory),
            Target      => Result_Path,
            Target_Last => Result_Path_Last);

         Append
           (Source      => Path (File_Name),
            Target      => Result_Path,
            Target_Last => Result_Path_Last);
      else
         Copy
           (Source      => Path (File_Name),
            Target      => Result_Path,
            Target_Last => Result_Path_Last);
      end if;

      Compact_Relative
        (Source      => Result_Path (1 .. Result_Path_Last),
         Result      => Result_Path,
         Result_Last => Result_Path_Last);

      Create
        (Result_File_Name,
         Result_Device (1 .. Result_Device_Last) &
           Result_Path (1 .. Result_Path_Last) &
           Name (File_Name) &
           Extension (File_Name));

      return Result_File_Name;
   end Resolve_Relative;

   function Replace_Environment_Variables (Path : in String) return String
   is
      use Ada.Strings.Maps;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      Separators  : constant Character_Set := To_Set ("/\.");
      Copied_Last : Integer                := Path'First - 1;
      Env_First   : Integer                := Index (Source => Path, Pattern => "$");
      Env_Last    : Integer;
      Result      : Unbounded_String;
   begin
      --  Special case of no dollar signs
      if Env_First = 0 then
         return Path;
      end if;

      Replace_All :
      loop
         Find_Next :
         loop
            Env_First := Index (Source => Path (Copied_Last + 1 .. Path'Last), Pattern => "$");
            exit Replace_All when Env_First = 0;

            exit Find_Next when Env_First = Path'First;

            if Is_In (Path (Env_First - 1), Separators) then
               exit Find_Next;
            else
               Result      := Result & Path (Copied_Last + 1 .. Env_First);
               Copied_Last := Env_First;
            end if;

         end loop Find_Next;

         Result := Result & Path (Copied_Last + 1 .. Env_First - 1);

         Env_Last := Index (Source => Path (Env_First .. Path'Last), Set => Separators);

         if Env_Last = 0 then
            Env_Last := Path'Last;
         else
            Env_Last := Env_Last - 1; --  Before directory separator
         end if;

         begin
            Result := Result & Ada.Environment_Variables.Value (Path (Env_First + 1 .. Env_Last));
         exception
         when Constraint_Error =>
            raise Constraint_Error with "environment variable '" & Path (Env_First + 1 .. Env_Last) & "' not defined";
         end;

         Copied_Last := Env_Last;

      end loop Replace_All;

      Result := Result & Path (Copied_Last + 1 .. Path'Last);
      return To_String (Result);
   end Replace_Environment_Variables;

end SAL.File_Names;
