--  Abstract :
--
--  Define constants to reflect hardware bit and word endianness in
--  record representation clauses. Obviously, this file is highly
--  system-dependent.
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
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

with System;
package SAL.Endianness is
   pragma Pure;

   --  this is for gnat on an Intel 386 compatible processor
   System_Name : constant System.Name := System.SYSTEM_NAME_GNAT;

   type Byte_Order_Type is (Big_Endian, Little_Endian);
   Byte_Order : constant Byte_Order_Type := Little_Endian;

   Bit_Order      : constant := 1; -- 1 or -1
   High_Bit_First : constant := 0; -- 0 or 1
   Low_Bit_First  : constant := 1; -- opposite of High_Bit_First

   --  Use one of these, corresponding to the natural machine size of
   --  the record you are laying out.
   LSBit_8  : constant :=  0; -- 0 or  7
   LSBit_16 : constant :=  0; -- 0 or 15
   LSBit_32 : constant :=  0; -- 0 or 31
   LSBit_56 : constant :=  0; -- 0 or 55

   --  See Test/endianness_example.adb for a compilable example, an
   --  example of why 'Bit_Order doesn't work, and a reference to Norm
   --  Cohen's paper on this.

   --  If the bit numbers are correct for Low_Bit_First = 1, use these
   --  patterns:
   --
   --     type Thruster_Pulse_Activation_Type is record
   --        --  [1] Table 4.5.2.5
   --        Pulse_Activation : Boolean;
   --        Spare            : Interfaces_More.Unsigned_31;
   --     end record;
   --     for Thruster_Pulse_Activation_Type use record
   --        Pulse_Activation at 0 range LSBit_32 + Bit_Order * 0 .. LSBit_32 + Bit_Order * 0;
   --        Spare            at 0 range
   --           Low_Bit_First  * (LSBit_32 + Bit_Order * 1) + High_Bit_First * (LSBit_32 + Bit_Order * 31) ..
   --           High_Bit_First * (LSBit_32 + Bit_Order * 1) + Low_Bit_First  * (LSBit_32 + Bit_Order * 31);
   --     end record;
   --     for Thruster_Pulse_Activation_Type'Size use 32;

   --     type VEC_Analog_Collection_Status_Type is record
   --        MUX_Channel               : Interfaces_More.Unsigned_4;
   --        Spare_2                   : Interfaces_More.Unsigned_3;
   --        Analog_Collection_Running : Boolean;
   --     end record;
   --
   --     for VEC_Analog_Collection_Status_Type use record
   --        MUX_Channel               at 0 range
   --           Low_Bit_First  * (LSBit_8 + Bit_Order *  4) + High_Bit_First * (LSBit_8 + Bit_Order *  7) ..
   --           High_Bit_First * (LSBit_8 + Bit_Order *  4) + Low_Bit_First  * (LSBit_8 + Bit_Order *  7);
   --
   --        Spare_2                   at 0 range
   --           Low_Bit_First  * (LSBit_8 + Bit_Order *  1) + High_Bit_First * (LSBit_8 + Bit_Order *  3) ..
   --           High_Bit_First * (LSBit_8 + Bit_Order *  1) + Low_Bit_First  * (LSBit_8 + Bit_Order *  3);
   --
   --        Analog_Collection_Running at 0 range LSBit_8 + Bit_Order * 0 .. LSBit_8 + Bit_Order * 0;
   --     end record;
   --     for VEC_Analog_Collection_Status_Type'Size use 8;

   --     type Packed_Raw_Data_Type is record
   --        Channel_1      : Interfaces_More.Signed_17;
   --        Channel_2      : Interfaces_More.Signed_17;
   --        Channel_3      : Interfaces_More.Signed_17;
   --        ADC_Reset      : Boolean;
   --        Selected_Range : Range_Type; -- 'range' is a reserved word.
   --        Spare          : Interfaces_More.Unsigned_3;
   --     end record;
   --     for Packed_Raw_Data_Type use record
   --        Channel_1 at 0 range
   --           Low_Bit_First  * (LSBit_56 + Bit_Order *  0) + High_Bit_First * (LSBit_56 + Bit_Order * 16) ..
   --           High_Bit_First * (LSBit_56 + Bit_Order *  0) + Low_Bit_First  * (LSBit_56 + Bit_Order * 16);
   --
   --        Channel_2 at 0 range
   --           Low_Bit_First  * (LSBit_56 + Bit_Order * 17) + High_Bit_First * (LSBit_56 + Bit_Order * 33) ..
   --           High_Bit_First * (LSBit_56 + Bit_Order * 17) + Low_Bit_First  * (LSBit_56 + Bit_Order * 33);
   --
   --        Channel_3  at 0 range
   --           Low_Bit_First  * (LSBit_56 + Bit_Order * 34) + High_Bit_First * (LSBit_56 + Bit_Order * 50) ..
   --           High_Bit_First * (LSBit_56 + Bit_Order * 34) + Low_Bit_First  * (LSBit_56 + Bit_Order * 50);
   --
   --        Selected_Range at 0 range LSBit_56 + Bit_Order * 51 .. LSBit_56 + Bit_Order * 51;
   --        ADC_Reset      at 0 range LSBit_56 + Bit_Order * 52 .. LSBit_56 + Bit_Order * 52;
   --
   --        Spare at 0 range
   --           Low_Bit_First  * (LSBit_56 + Bit_Order * 53) + High_Bit_First * (LSBit_56 + Bit_Order * 55) ..
   --           High_Bit_First * (LSBit_56 + Bit_Order * 53) + Low_Bit_First  * (LSBit_56 + Bit_Order * 55);
   --
   --     end record;
   --     Packed_Raw_Data_Octet_Length : constant := 7;
   --     for Packed_Raw_Data_Type'Size use Packed_Raw_Data_Octet_Length * 8;


   --  Using bit numbers that are correct for High_Bit_First = 1 is
   --  not supported; we need different definitions for LSBit_* .

end SAL.Endianness;
