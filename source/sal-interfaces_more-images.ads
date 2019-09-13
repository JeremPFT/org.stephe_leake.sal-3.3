--  Abstract :
--
--  Instantiations of SAL.Generic_Hex_Image and
--  SAL.Generic_Binary_Image for some types in Interfaces and
--  elsewhere.
--
--  Copyright (C) 2004 - 2007, 2009 - 2010 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Interfaces;
with SAL.Generic_Binary_Image;
with SAL.Generic_Decimal_Image_Unsigned;
with SAL.Generic_Hex_Image;
with SAL.Generic_Hex_Value;
package SAL.Interfaces_More.Images is
   pragma Preelaborate; --  Parent

   function Decimal_Image is new SAL.Generic_Decimal_Image_Unsigned (Unsigned_4);
   function Binary_Image is new SAL.Generic_Binary_Image (1, Unsigned_4);

   function Decimal_Image is new SAL.Generic_Decimal_Image_Unsigned (Unsigned_5);

   function Hex_Image is new SAL.Generic_Hex_Image (2, Unsigned_6);

   function Hex_Image is new SAL.Generic_Hex_Image (2, Interfaces.Unsigned_8);
   function Hex_Value is new SAL.Generic_Hex_Value (Interfaces.Unsigned_8);
   function Binary_Image is new SAL.Generic_Binary_Image (2, Interfaces.Unsigned_8);

   function Hex_Image is new SAL.Generic_Hex_Image (3, Unsigned_10);
   function Binary_Image is new SAL.Generic_Binary_Image (3, Unsigned_10);

   function Hex_Image is new SAL.Generic_Hex_Image (4, Interfaces.Unsigned_16);
   function Hex_Value is new SAL.Generic_Hex_Value (Interfaces.Unsigned_16);
   function Binary_Image is new SAL.Generic_Binary_Image (4, Interfaces.Unsigned_16);

   function Hex_Image is new SAL.Generic_Hex_Image (8, Interfaces.Unsigned_32);
   function Binary_Image is new SAL.Generic_Binary_Image (8, Interfaces.Unsigned_32);

   function Hex_Image is new SAL.Generic_Hex_Image (16, Interfaces.Unsigned_64);
   function Binary_Image is new SAL.Generic_Binary_Image (16, Interfaces.Unsigned_64);

end SAL.Interfaces_More.Images;
