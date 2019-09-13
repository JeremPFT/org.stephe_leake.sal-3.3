--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2018, 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Checks;
package body SAL.Gen_Definite_Doubly_Linked_Lists.Gen_AUnit is

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

   procedure Check is new AUnit.Checks.Gen_Check_Access (Node_Type, Node_Access);
   procedure Check is new AUnit.Checks.Gen_Check_Access (List, List_Access);

   procedure Check (Label : in String; Computed : in Cursor; Expected : in Cursor)
   is begin
      Check (Label & ".container", Computed.Container, Expected.Container);
      Check (Label & ".ptr", Computed.Ptr, Expected.Ptr);
   end Check;

end SAL.Gen_Definite_Doubly_Linked_Lists.Gen_AUnit;
