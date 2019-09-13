--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with SAL.Endianness; use SAL.Endianness;
with Ada.Unchecked_Conversion;
package body SAL.Network_Order.Gen_Scalar_32 is

   function To_4_Byte is new Ada.Unchecked_Conversion
     (Target => Network_Byte_Array_4_Type,
      Source => Host_32_Type);

   function From_4_Byte is new Ada.Unchecked_Conversion
     (Target => Host_32_Type,
      Source => Network_Byte_Array_4_Type);

   procedure To_Network
     (Item   : in     Host_32_Type;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      case Byte_Order is
      when Big_Endian =>
         Buffer (Last + 1 .. Last + 4) := To_4_Byte (Item);

      when Little_Endian =>
         declare
            Temp : constant Network_Byte_Array_4_Type := To_4_Byte (Item);
         begin
            Buffer (Last + 1) := Temp (4);
            Buffer (Last + 2) := Temp (3);
            Buffer (Last + 3) := Temp (2);
            Buffer (Last + 4) := Temp (1);
         end;
      end case;

      Last := Last + 4;
   end To_Network;

   procedure From_Network
     (Item   :    out Host_32_Type;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      case Byte_Order is
      when Big_Endian =>
         Item := From_4_Byte (Buffer (Last + 1 .. Last + 2));
      when Little_Endian =>
         declare
            Temp : Network_Byte_Array_4_Type;
         begin
            Temp (1) := Buffer (Last + 4);
            Temp (2) := Buffer (Last + 3);
            Temp (3) := Buffer (Last + 2);
            Temp (4) := Buffer (Last + 1);
            Item := From_4_Byte (Temp);
         end;
      end case;

      Last := Last + 4;
   end From_Network;

end SAL.Network_Order.Gen_Scalar_32;
