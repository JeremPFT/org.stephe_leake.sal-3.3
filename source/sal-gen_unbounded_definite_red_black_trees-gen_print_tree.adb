--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO;
procedure SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Print_Tree (Tree : in Pkg.Tree)
is
   function Max_Depth (Node : in Node_Access) return Integer
   is begin
      if Node = null then
         return 0;
      else
         return 1 + Integer'Max (Max_Depth (Node.Left), Max_Depth (Node.Right));
      end if;
   end Max_Depth;

   procedure Print_Line
     (Node        : in Node_Access;
      Depth       : in Integer;
      Item_Width  : in Integer;
      Total_Width : in Integer)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      if Node = null then
         Put (Total_Width * ' ');
      else
         if Depth = 1 then
            Put
              ((Total_Width - Item_Width) / 2 * ' ' &
                 Image (Key (Node.Element)) &
                 (case Node.Color is
                  when Black => "b",
                  when Red   => "r") &
                 (Total_Width - Item_Width) / 2 * ' ');
         else
            Print_Line (Node.Left, Depth - 1, Item_Width, Total_Width / 2);
            Print_Line (Node.Right, Depth - 1, Item_Width, Total_Width / 2);
         end if;
      end if;
   end Print_Line;

   Depth       : constant Integer := Max_Depth (Tree.Root);
   Item_Width  : constant Integer := 4; -- " ddc" 2 digits, color letter
   Total_Width : constant Integer := Item_Width * 2 ** Depth;
begin
   for I in 1 .. Depth loop
      Print_Line (Tree.Root, I, Item_Width, Total_Width);
      Ada.Text_IO.New_Line;
   end loop;
end SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Print_Tree;
