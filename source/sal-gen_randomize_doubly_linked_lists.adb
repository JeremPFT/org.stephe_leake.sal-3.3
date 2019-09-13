--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008 - 2009, 2012, 2015 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Ada.Numerics.Float_Random;
procedure SAL.Gen_Randomize_Doubly_Linked_Lists (Container : in out Lists.List; Seed : in Integer := 0)
is
   use Ada.Containers;
   use Lists;

   Result_Count : constant Count_Type := Container.Length;
   Source_Count : Count_Type          := Result_Count;
   Result       : List;
   I            : Cursor;

   Generator : Ada.Numerics.Float_Random.Generator;

   procedure Random_Next (I : out Cursor)
   is
      --  Produce an integer in range 0 .. source_count - 1, all with equal probability
      Rnd  : constant Float   := Ada.Numerics.Float_Random.Random (Generator);
      Step : constant Integer := Integer (Float'Floor ((Float (Source_Count) - Float'Model_Epsilon) * Rnd));
   begin
      I := First (Container);
      for J in 1 .. Step loop
         Next (I);
      end loop;
   end Random_Next;

begin
   if Seed /= 0 then
      Ada.Numerics.Float_Random.Reset (Generator, Seed);
   end if;

   for J in 1 .. Result_Count loop
      Random_Next (I);

      Splice
        (Target   => Result,
         Before   => No_Element,
         Source   => Container,
         Position => I);

      Source_Count := Source_Count - 1;
   end loop;

   Splice
     (Target => Container,
      Before => No_Element,
      Source => Result);

end SAL.Gen_Randomize_Doubly_Linked_Lists;
