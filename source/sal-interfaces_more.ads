--  Abstract:
--
--  Stuff that could be in Interfaces, but isn't.
--
--  Copyright (C) 2004 - 2007, 2009 - 2011 Stephen Leake.  All Rights Reserved.
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

with Ada.Unchecked_Conversion;
with Interfaces;
package SAL.Interfaces_More is
   pragma Preelaborate; --  Named access types

   function To_Integer_16 is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_16,
      Target => Interfaces.Integer_16);

   function To_Unsigned_16 is new Ada.Unchecked_Conversion
     (Source => Interfaces.Integer_16,
      Target => Interfaces.Unsigned_16);

   type Natural_8 is range 0 .. 2**8 - 1;
   for Natural_8'Size use 8;

   type Natural_16 is range 0 .. 2**16 - 1;
   for Natural_16'Size use 16;

   type Natural_32 is range 0 .. 2**32 - 1;
   for Natural_32'Size use 32;

   type Unsigned_2 is mod 2**2;
   type Unsigned_3 is mod 2**3;
   type Unsigned_4 is mod 2**4;
   type Unsigned_5 is mod 2**5;
   type Unsigned_6 is mod 2**6;
   type Unsigned_7 is mod 2**7;

   type Unsigned_9 is mod 2**9;
   type Unsigned_10 is mod 2**10;
   type Unsigned_11 is mod 2**11;
   type Unsigned_12 is mod 2**12;
   type Unsigned_13 is mod 2**13;
   type Unsigned_14 is mod 2**14;
   type Unsigned_15 is mod 2**15;

   type Unsigned_17 is mod 2**18;
   type Unsigned_18 is mod 2**18;
   type Unsigned_19 is mod 2**19;
   type Unsigned_20 is mod 2**20;

   type Unsigned_22 is mod 2**22;

   type Unsigned_24 is mod 2**24;
   type Unsigned_29 is mod 2**29;
   type Unsigned_30 is mod 2**30;
   type Unsigned_31 is mod 2**31;

   --  auto_text_io: ignore
   type Unsigned_8_Access_Type is access all Interfaces.Unsigned_8;
   --  auto_text_io: ignore
   type Unsigned_16_Access_Type is access all Interfaces.Unsigned_16;
   --  auto_text_io: ignore
   type Unsigned_32_Access_Type is access all Interfaces.Unsigned_32;

   type Unsigned_16_Split_Type is record
      High : aliased Interfaces.Unsigned_8;
      Low  : aliased Interfaces.Unsigned_8;
   end record;
   for Unsigned_16_Split_Type use record
      High at 0 range  8 .. 15;
      Low  at 0 range  0 .. 7;
   end record;
   for Unsigned_16_Split_Type'Size use 16;

   function To_Unsigned_16 is new Ada.Unchecked_Conversion
     (Source => Unsigned_16_Split_Type,
      Target => Interfaces.Unsigned_16);

   function To_Unsigned_16_Split is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_16,
      Target => Unsigned_16_Split_Type);

   type Unsigned_32_Split_Type is record
      High : aliased Interfaces.Unsigned_16;
      Low  : aliased Interfaces.Unsigned_16;
   end record;
   for Unsigned_32_Split_Type use record
      High at 0 range 16 .. 31;
      Low  at 0 range  0 .. 15;
   end record;
   for Unsigned_32_Split_Type'Size use 32;

   function To_Unsigned_32_Split is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_32,
      Target => Unsigned_32_Split_Type);

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (Source => Unsigned_32_Split_Type,
      Target => Interfaces.Unsigned_32);

   type Unsigned_32_Quad_Type is record
      High_Byte     : aliased Interfaces.Unsigned_8;
      Mid_High_Byte : aliased Interfaces.Unsigned_8;
      Mid_Low_Byte  : aliased Interfaces.Unsigned_8;
      Low_Byte      : aliased Interfaces.Unsigned_8;
   end record;
   for Unsigned_32_Quad_Type use record
      High_Byte     at 0 range 24 .. 31;
      Mid_High_Byte at 0 range 16 .. 23;
      Mid_Low_Byte  at 0 range  8 .. 15;
      Low_Byte      at 0 range  0 ..  7;
   end record;
   for Unsigned_32_Quad_Type'Size use 32;

   function To_Unsigned_32_Quad is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_32,
      Target => Unsigned_32_Quad_Type);

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (Source => Unsigned_32_Quad_Type,
      Target => Interfaces.Unsigned_32);

   subtype Bit_Index_16_Type is Integer range 0 .. 15;
   type Bit_Array_16_Type is array (Bit_Index_16_Type) of Boolean;
   pragma Pack (Bit_Array_16_Type);
   for Bit_Array_16_Type'Size use 16;

   function To_Bit_Array is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_16,
      Target => Bit_Array_16_Type);

   function To_Unsigned_16 is new Ada.Unchecked_Conversion
     (Source => Bit_Array_16_Type,
      Target => Interfaces.Unsigned_16);
   --  Foo (0) is LSB, Foo (15) is MSB.  (on Intel, at least)

   function Any (Item : in Bit_Array_16_Type) return Boolean;

   subtype Bit_Index_32_Type is Integer range 0 .. 31;
   type Bit_Array_32_Type is array (Bit_Index_32_Type) of Boolean;
   pragma Pack (Bit_Array_32_Type);
   for Bit_Array_32_Type'Size use 32;

   function To_Bit_Array is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_32,
      Target => Bit_Array_32_Type);

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (Source => Bit_Array_32_Type,
      Target => Interfaces.Unsigned_32);

end SAL.Interfaces_More;
