--  Abstract :
--
--  Utilities for AUnit checks of types in Interfaces, Interfaces_More.
--  See SAL.AUnit for types in Standard.
--
--  Copyright (C) 2004 - 2006, 2009 - 2010, 2015, 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Interfaces.C;
with AUnit.Checks; use AUnit.Checks;
package SAL.Interfaces_More.AUnit is
   pragma Elaborate_Body; -- Standard.AUnit.Assertions

   procedure Check is new Gen_Check_Discrete (Unsigned_2);
   procedure Check is new Gen_Check_Discrete (Unsigned_3);
   procedure Check is new Gen_Check_Discrete (Unsigned_4);
   procedure Check is new Gen_Check_Discrete (Unsigned_5);
   procedure Check is new Gen_Check_Discrete (Unsigned_6);
   procedure Check is new Gen_Check_Discrete (Unsigned_7);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_8);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_10);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_12);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_14);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_16);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_17);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_22);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_32);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_64);

   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_8);
   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_16);
   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_32);
   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_64);

   procedure Check_Binary
     (Label    : in String;
      Computed : in Interfaces.Unsigned_8;
      Expected : in Interfaces.Unsigned_8);
   --  Values in failure message are in binary

   procedure Check_Binary
     (Label    : in String;
      Computed : in Interfaces.Unsigned_16;
      Expected : in Interfaces.Unsigned_16);
   --  Values in failure message are in binary

   procedure Check_Hex
     (Label    : in String;
      Computed : in Interfaces.Unsigned_16;
      Expected : in Interfaces.Unsigned_16);
   --  Values in failure message are in decimal and hex

   procedure Check is new Gen_Check_Discrete (Interfaces.C.int);

   procedure Check
     (Label     : in String;
      Computed  : in Interfaces.Unsigned_16;
      Expected  : in Interfaces.Unsigned_16;
      Tolerance : in Interfaces.Unsigned_16);
   procedure Check
     (Label     : in String;
      Computed  : in Interfaces.Unsigned_32;
      Expected  : in Interfaces.Unsigned_32;
      Tolerance : in Interfaces.Unsigned_32);
   --  Sometimes we need a tolerance on these discrete types.

end SAL.Interfaces_More.AUnit;
