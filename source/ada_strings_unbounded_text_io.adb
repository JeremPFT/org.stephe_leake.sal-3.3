--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2009 Stephen Leake.  All Rights Reserved.
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

with SAL.Text_IO_Utils;
package body Ada_Strings_Unbounded_Text_IO is

   procedure Put
     (File : in Ada.Text_IO.File_Type;
      Item : in Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Text_IO, Ada.Strings.Unbounded;
      Current : Character;
   begin
      Put (File, '"');
      for I in 1 .. Length (Item) loop
         Current := Element (Item, I);
         Put (File, Current);
         if Current = '"' then
            Put (File, Current);
         end if;
      end loop;
      Put (File, '"');
   end Put;

   procedure Put (Item : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Put (Ada.Text_IO.Current_Output, Item);
   end Put;

   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Ada.Strings.Unbounded.Unbounded_String;
      Single_Line       : in Boolean                                := True;
      Named_Association : in Boolean                                := False)
   is
      pragma Unreferenced (Named_Association);
      pragma Unreferenced (Single_Line);
   begin
      Put (File, Item);
   end Put_Item;

   procedure Get
     (File : in     Ada.Text_IO.File_Type;
      Item :    out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Text_IO, Ada.Strings.Unbounded;
      Current     : Character;
      Peek        : Character;
      End_Of_Line : Boolean;
   begin
      Item := Null_Unbounded_String;
      SAL.Text_IO_Utils.Skip_Past (File, '"');
      loop
         Get (File, Current);
         if Current = '"' then
            Look_Ahead (File, Peek, End_Of_Line);
            if End_Of_Line then
               return;
            elsif Peek = '"' then
               Get (File, Current);
            else
               return;
            end if;
         end if;
         Append (Item, Current);
      end loop;
   end Get;

   procedure Get (Item : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Get (Ada.Text_IO.Current_Input, Item);
   end Get;

   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Ada.Strings.Unbounded.Unbounded_String;
      Single_Line       : in     Boolean                                := True;
      Named_Association : in     Boolean                                := False)
   is
      pragma Unreferenced (Named_Association);
      pragma Unreferenced (Single_Line);
   begin
      Get (File, Item);
   end Get_Item;

end Ada_Strings_Unbounded_Text_IO;
