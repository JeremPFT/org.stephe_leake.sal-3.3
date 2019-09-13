--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

procedure SAL.Gen_Unbounded_Indefinite_Vectors.Gen_AUnit (Label : in String; Computed, Expected : in Vector)
is begin
   Check_Index (Label & "'Last", Computed.Last, Expected.Last);
   for I in Computed.Elements'First .. Base_Peek_Type (Computed.Last - Index_Type'First + 1) loop
      Check_Element (Label & "." & Base_Peek_Type'Image (I), Computed.Elements (I).all, Expected.Elements (I).all);
   end loop;
end SAL.Gen_Unbounded_Indefinite_Vectors.Gen_AUnit;
