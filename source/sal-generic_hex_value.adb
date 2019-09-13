--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005, 2006, 2009 Stephen Leake.  All Rights Reserved.
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

function SAL.Generic_Hex_Value (Item : in String) return Number_Type
is
   Hex_Value  : Number_Type;
   Exponent   : Natural;
   Char_Value : Number_Type;
   Temp       : Number_Type;
begin
   Hex_Value := 0;
   Exponent  := 0;
   for I in reverse Item'First .. Item'Last loop
      Temp := 0;
      Char_Value := Character'Pos (Item (I));
      if Char_Value in Character'Pos ('a') .. Character'Pos ('f') then
         Char_Value := Char_Value - 32;
      end if;
      if Char_Value >= Character'Pos ('A') then
         Temp := (Char_Value - Character'Pos ('A')) + 10;
      else
         Temp := Char_Value - Character'Pos ('0');
      end if;
      Hex_Value := Hex_Value + (Temp * (16 ** Exponent));
      Exponent  := Exponent  + 1;
   end loop;
   return Hex_Value;
end SAL.Generic_Hex_Value;
