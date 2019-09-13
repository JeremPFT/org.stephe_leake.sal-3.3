--  Abstract :
--
--  Provide conversions between network order storage element arrays
--  and scalar types.
--
--  Network order is little-bit-endian, big-byte-endian, 8 bit bytes.
--
--  The package SAL.Endianness defines the bit and word endianness for
--  the current processor.
--
--  The To_Network functions copy words from Item to Buffer (Last + 1
--  ..), changing to network byte order. On return, Last points to the
--  last byte written.
--
--  The From_Network functions copy words from Buffer (Last + 1 ..) to Item,
--  changing from network byte order. On return, Last points to the last byte
--  read.
--
--  For scalar host types of size 16 or 32 bits, either use a type
--  conversion in a call to one of the functions below, or instantiate
--  one of the child packages provided (to avoid the type conversion).
--
--  Copyright (C) 2004, 2005, 2009, 2011, 2019 Stephen Leake.  All Rights Reserved.
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
with Ada.Streams;
package SAL.Network_Order is
   pragma Pure;

   procedure To_Network
     (Item   : in     Ada.Streams.Stream_Element;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     Ada.Streams.Stream_Element_Array;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);
   --  Useful in unit tests

   procedure From_Network
     (Item   :    out Ada.Streams.Stream_Element;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     Interfaces.Integer_8;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Interfaces.Integer_8;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     Boolean;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Boolean;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     Interfaces.Unsigned_8;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Interfaces.Unsigned_8;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);


   procedure To_Network
     (Item   : in     Interfaces.Integer_16;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Interfaces.Integer_16;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     Interfaces.Unsigned_16;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Interfaces.Unsigned_16;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);


   procedure To_Network
     (Item   : in     Interfaces.Integer_32;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Interfaces.Integer_32;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     Interfaces.Unsigned_32;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out Interfaces.Unsigned_32;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure To_Network
     (Item   : in     String;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

   procedure From_Network
     (Item   :    out String;
      Buffer : in     Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Offset);

private
   --  Visible for child packages, also used in body.
   subtype Network_Byte_Array_2_Type is Ada.Streams.Stream_Element_Array (1 .. 2);
   subtype Network_Byte_Array_4_Type is Ada.Streams.Stream_Element_Array (1 .. 4);
   subtype Network_Byte_Array_8_Type is Ada.Streams.Stream_Element_Array (1 .. 8);

end SAL.Network_Order;
