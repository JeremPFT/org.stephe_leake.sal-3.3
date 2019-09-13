--  Abstract :
--
--  AUnit check for parent
--
--  Copyright (C) 2018 - 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

generic
   with procedure Check_Index (Label : in String; Computed, Expected : in Index_Type);
   with procedure Check_Element (Label : in String; Computed, Expected : in Element_Type);
package SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Vector;
      Expected : in Vector);

   procedure Check
     (Label    : in String;
      Computed : in Cursor;
      Expected : in Cursor);

end SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
