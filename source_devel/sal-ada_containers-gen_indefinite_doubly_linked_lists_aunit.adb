--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2019 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.

pragma License (GPL);

with AUnit.Checks;
package body SAL.Ada_Containers.Gen_Indefinite_Doubly_Linked_Lists_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Lists.List;
      Expected : in Lists.List)
   is
      use AUnit.Checks;
      use Lists;
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
         Check (Label & ".computed too long", Cur_Computed = No_Element, True);
         Check (Label & ".expected too long", Cur_Expected = No_Element, True);
      end if;
   end Check;

end SAL.Ada_Containers.Gen_Indefinite_Doubly_Linked_Lists_AUnit;
