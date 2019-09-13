--  Abstract :
--
--  Run-time utilities used by Auto_Text_IO generated files.
--
--  Copyright (C) 2001, 2003, 2004, 2005, 2009 Stephen Leake.  All Rights Reserved.
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
package SAL.Text_IO_Utils is

   Syntax_Error       : exception;
   Discriminant_Error : exception;

   procedure Skip_Whitespace;
   procedure Skip_Whitespace (File : in Ada.Text_IO.File_Type);
   --  Skip space, tab, and end of line in File.

   procedure Skip_Past (Delimiter : in Character);
   procedure Skip_Past (File : in Ada.Text_IO.File_Type; Delimiter : in Character);
   --  Skip past next Delimiter in File.

   procedure Check (Item : in String);
   procedure Check (File : in Ada.Text_IO.File_Type; Item : in String);
   --  Skip leading whitespace. Trim leading and trailing whitespace
   --  from item; read resulting length characters from File; if they
   --  don't equal trimmed Item, raise Syntax_Error with an "Expecting
   --  ..., found ..." message. Otherwise, skip trailing whitespace.
   --
   --  Ada.Text_IO.Get (String) is used, so there may be end of lines
   --  embedded in String.

   function Peek (File : in Ada.Text_IO.File_Type) return Character;
   --  Skip leading whitespace, return next character without consuming it.

end SAL.Text_IO_Utils;
