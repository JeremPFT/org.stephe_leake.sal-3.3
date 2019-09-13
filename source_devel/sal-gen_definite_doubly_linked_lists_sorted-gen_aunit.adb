--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019 Stephen Leake All Rights Reserved.
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

with AUnit.Checks;
with AUnit.Assertions;
package body SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in List;
      Expected : in List)
   is
      use AUnit.Checks;
      I : Integer := 1;
      Cur_Computed : Cursor := Computed.First;
      Cur_Expected : Cursor := Expected.First;
   begin
      if Computed = Empty_List then
         Check (Label & ".empty", Expected = Empty_List, True);
      else
         loop
            exit when Cur_Computed = No_Element or Cur_Expected = No_Element;
            Check_Element (Label & "." & Integer'Image (I), Element (Cur_Computed), Element (Cur_Expected));
            Next (Cur_Computed);
            Next (Cur_Expected);
            I := I + 1;
         end loop;
         Check (Label & ".computed too long", Cur_Computed, No_Element);
         Check (Label & ".expected too long", Cur_Expected, No_Element);
      end if;
   end Check;

   procedure Check
     (Label : in String;
      Computed : in Cursor;
      Expected : in Cursor)
   is begin
      AUnit.Assertions.Assert
        (Computed.Container = Expected.Container and Computed.Ptr = Expected.Ptr, Label & ": cursors not equal");
   end Check;


end SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_AUnit;
