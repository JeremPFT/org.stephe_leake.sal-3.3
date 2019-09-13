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

procedure SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit (Label : in String; Computed, Expected : in Vectors.Vector)
is begin
   Check_Index (Label & "'Last", Computed.Last_Index, Expected.Last_Index);
   for I in Computed.First_Index .. Computed.Last_Index loop
      Check_Element (Label & "." & Index_Type'Image (I), Computed (I), Expected (I));
   end loop;
end SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit;
