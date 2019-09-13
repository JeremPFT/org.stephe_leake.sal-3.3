--  Abstract :
--
--  Access to private part of parent for unit test
--
--  Copyright (C) 2004, 2017 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

generic
package SAL.Gen_Unbounded_Definite_Min_Heaps_Binary.Gen_Test is

   function Peek (Heap : in Heap_Type; I : in SAL.Peek_Type) return Element_Type;
   function Size (Heap : in Heap_Type) return Base_Peek_Type;

end SAL.Gen_Unbounded_Definite_Min_Heaps_Binary.Gen_Test;
