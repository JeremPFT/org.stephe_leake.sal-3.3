--  Abstract :
--
--  Exercise min_heap, to test for memory leaks/errors with gnatmem.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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

with SAL.Gen_Unbounded_Definite_Min_Heaps;
procedure Exercise_Min_Heap is

   type Element_Type is record
      Data : Float;
      Key  : Integer;
   end record;

   function Key (Element : in Element_Type) return Integer is (Element.Key);
   procedure Set_Key (Element : in out Element_Type; Key : in Integer)
   is begin
      Element.Key := Key;
   end Set_Key;

   package Unbounded_Definite_Min_Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps
     (Element_Type => Element_Type,
      Key_Type     => Integer,
      Key          => Key,
      Set_Key      => Set_Key,
      "<"          => "<",
      Initial_Size => 6);

   Min_Heap : Unbounded_Definite_Min_Heaps.Heap_Type;
   Junk : Element_Type;
   pragma Unreferenced (Junk);
begin

   --  Insert more than Initial_size, to force grow.
   for I in 1 .. 10 loop
      Min_Heap.Insert ((5.0, 5));
      Min_Heap.Insert ((4.0, 4));
      Min_Heap.Insert ((3.0, 3));
      Min_Heap.Insert ((2.0, 2));
      Min_Heap.Insert ((1.0, 1));
   end loop;

   for I in 1 .. 50 loop
      Junk := Min_Heap.Get;
   end loop;

end Exercise_Min_Heap;
