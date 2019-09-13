--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Checks;
procedure SAL.Ada_Containers.Gen_Doubly_Linked_Lists_AUnit
  (Label : in String; Computed : in Lists.List; Expected : in Lists.List)
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

end SAL.Ada_Containers.Gen_Doubly_Linked_Lists_AUnit;
