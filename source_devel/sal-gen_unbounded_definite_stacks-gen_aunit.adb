--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2017, 2018, 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with SAL.AUnit;
procedure SAL.Gen_Unbounded_Definite_Stacks.Gen_AUnit
  (Label    : in String;
   Computed : in Stack;
   Expected : in Stack)
is begin
   SAL.AUnit.Check (Label & ".Depth", Computed.Depth, Expected.Depth);
   for I in 1 .. Computed.Depth loop
      Check_Element (Label & "." & SAL.Base_Peek_Type'Image (I), Computed.Peek (I), Expected.Peek (I));
   end loop;
end SAL.Gen_Unbounded_Definite_Stacks.Gen_AUnit;
