--  Abstract :
--
--  See spec
--
--  Copyright (C) 2001, 2002, 2003, 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
package body SAL.Text_IO_Utils is
   use Ada.Text_IO;

   procedure Skip_Whitespace
   is begin
      Skip_Whitespace (Current_Input);
   end Skip_Whitespace;

   procedure Skip_Whitespace (File : in File_Type)
   is
      Item        : Character;
      End_Of_Line : Boolean;
   begin
      loop
         Look_Ahead (File, Item, End_Of_Line);
         if End_Of_Line then
            if End_Of_File (File) then
               return;
            else
               Skip_Line (File);
            end if;
         elsif Item = ' ' or Item = ASCII.HT then
            Get (File, Item);
         else
            return;
         end if;
      end loop;
   end Skip_Whitespace;

   procedure Skip_Past (Delimiter : in Character)
   is begin
      Skip_Past (Current_Input, Delimiter);
   end Skip_Past;

   procedure Skip_Past (File : in Ada.Text_IO.File_Type; Delimiter : in Character)
   is
      Item : Character;
   begin
      loop
         Get (File, Item);
         if Item = Delimiter then
            return;
         end if;
      end loop;
   end Skip_Past;

   procedure Check (Item : in String)
   is begin
      Check (Current_Input, Item);
   end Check;

   procedure Check (File : in File_Type; Item : in String)
   is
      Trimmed_Item : constant String := Ada.Strings.Fixed.Trim (Item, Ada.Strings.Both);
      Temp         : String (Trimmed_Item'Range);
   begin
      Skip_Whitespace (File);
      Get (File, Temp);
      if Temp /= Trimmed_Item then
         Ada.Exceptions.Raise_Exception
           (Syntax_Error'Identity,
            Name (File) & ":" &
              Ada.Strings.Fixed.Trim (Count'Image (Line (File)), Ada.Strings.Both) & ":" &
              Ada.Strings.Fixed.Trim (Count'Image (Col (File)), Ada.Strings.Both) &
              ": Expecting """ & Item & """, found """ & Temp & """.");
      end if;
      Skip_Whitespace (File);
   end Check;

   function Peek (File : in Ada.Text_IO.File_Type) return Character
   is
      Item        : Character;
      End_Of_Line : Boolean;
   begin
      Skip_Whitespace (File);
      Look_Ahead (File, Item, End_Of_Line);
      --  Skip_Whitespace guarrantees End_Of_Line won't be True here.
      return Item;
   end Peek;

end SAL.Text_IO_Utils;
