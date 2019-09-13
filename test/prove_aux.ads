--  Abstract :
--
--  Auxilliary types for Spark proofs
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);
with SAL;
package Prove_Aux is

   type Index_Type is range -10 .. 10;

   function Compare_Integer (Left, Right : in Integer) return SAL.Compare_Result
     is (if Left < Right then SAL.Less
         elsif Left = Right then SAL.Equal
         else SAL.Greater);

end Prove_Aux;
