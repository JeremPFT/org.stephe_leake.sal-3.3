--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
package body SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Validate is

   procedure Validate (Label : in String; Container : in List)
   is
      use AUnit.Assertions;
      I : Node_Access;
      J : Integer := 1;
   begin
      if Container.Head = null then
         Assert (Container.Tail = null, Label & ": head, tail not both null");
      else

         Assert (Container.Head.Prev = null, Label & ": head.prev /= null");
         Assert (Container.Tail.Next = null, Label & ": tail.next /= null");

         I := Container.Head;
         Test_Elements :
         loop
            if I.Next = null then
               Assert (Container.Tail = I, Label & Integer'Image (J) & ": tail not last item");
               exit Test_Elements;
            else
               Assert (I.Next.Prev = I, Label & Integer'Image (J) & ": next.prev /= current");
            end if;
            I := I.Next;
            J := J + 1;
         end loop Test_Elements;
      end if;
   end Validate;

end SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Validate;
