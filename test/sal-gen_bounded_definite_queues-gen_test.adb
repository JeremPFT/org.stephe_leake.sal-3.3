--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2017, 2019 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Bounded_Definite_Queues.Gen_Test is

   function Get_Head (Queue : in Queue_Type) return Base_Peek_Type is
   begin
      return Queue.Head;
   end Get_Head;

   function Get_Tail (Queue : in Queue_Type) return Base_Peek_Type is
   begin
      return Queue.Tail;
   end Get_Tail;

end SAL.Gen_Bounded_Definite_Queues.Gen_Test;
