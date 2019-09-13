--  Abstract :
--
--  Utilities for AUnit tests of packages using Time_Conversions.
--
--  Copyright (C) 2004 - 2006, 2009, 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package SAL.Time_Conversions.AUnit is
   pragma Elaborate_Body; --  parent.

   Default_Tolerance : Time_Type := 0.0;
   --  User may need to set this to some small number for their tests.

   procedure Check
     (Label     : in String;
      Computed  : in Time_Type;
      Expected  : in Time_Type;
      Tolerance : in Time_Type := Default_Tolerance);

end SAL.Time_Conversions.AUnit;
