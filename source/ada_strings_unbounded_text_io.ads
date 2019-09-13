--  Abstract :
--
--  Delimited Text_IO for Ada.Strings.Unbounded.
--
--  Copyright (C) 2002, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Text_IO;
with Ada.Strings.Unbounded;
package Ada_Strings_Unbounded_Text_IO is
   pragma Elaborate_Body; -- Ada.Text_IO is.

   procedure Put
     (File : in Ada.Text_IO.File_Type;
      Item : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Put (Item : in Ada.Strings.Unbounded.Unbounded_String);
   --  Put Item, delimited by double quotes. Double any embedded
   --  double quotes.

   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Ada.Strings.Unbounded.Unbounded_String;
      Single_Line       : in Boolean                                := True;
      Named_Association : in Boolean                                := False);
   --  Calls Put; Single_Line and Named_Association are ignored. For
   --  compatibility with Auto_Text_IO conventions.

   procedure Get
     (File : in     Ada.Text_IO.File_Type;
      Item :    out Ada.Strings.Unbounded.Unbounded_String);
   procedure Get (Item : out Ada.Strings.Unbounded.Unbounded_String);
   --  Skip leading whitespace. Check for leading double quote. Read
   --  Item, un-doubleing any doubled double quotes. Check trailing
   --  double quote.
   --
   --  Raises End_Error if an end of line is encountered within the
   --  string.

   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Ada.Strings.Unbounded.Unbounded_String;
      Single_Line       : in     Boolean                                := True;
      Named_Association : in     Boolean                                := False);
   --  Calls Get (File, Item); Single_Line and Named_Association are
   --  ignored. For compatibility with Auto_Text_IO conventions.

end Ada_Strings_Unbounded_Text_IO;
