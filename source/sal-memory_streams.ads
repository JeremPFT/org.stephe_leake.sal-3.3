--  Abstract:
--
--  A memory stream type, for reading objects from some communication
--  channel.
--
--  Copyright (C) 2005, 2009, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Streams;
with System;
package SAL.Memory_Streams is

   type Stream_Type (Max_Length : Ada.Streams.Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type with private;

   procedure Create (Stream : in out Stream_Type);
   --  create an empty Stream with direction Out_Stream, for writing.

   procedure Create
     (Stream : in out Stream_Type;
      Data   : in     Ada.Streams.Stream_Element_Array);
   --  create a Stream with data, with direction In_Stream, for reading.
   --  raises Constraint_Error if Data overflows Stream

   procedure Create
     (Stream  : in out Stream_Type;
      Address : in     System.Address);
   --  create a Stream with data from Address, copying
   --  Stream.Max_Length bytes, with direction In_Stream, for reading.

   procedure Reset (Stream : in out Stream_Type);
   --  Reset to start of stream, for reading again.

   function Length (Stream : in Stream_Type) return Ada.Streams.Stream_Element_Count;
   --  for an In_Stream, the amount of data left to be read.
   --  for an Out_Stream, the amount of data written.

   function Address (Stream : in Stream_Type) return System.Address;
   --  for an In_Stream, raises Status_Error.
   --  for an Out_Stream, the address of the first element of the raw
   --  Stream, for passing to system routines.

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);
   --  for an In_Stream, reads elements from Stream, storing them in
   --  Item. Stops when Item'Last or end of Stream is reached, setting
   --  Last to last element of Item written.
   --
   --  for an Out_Stream, raises Status_Error.

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array);
   --  for an In_Stream, raises Status_Error.
   --
   --  for an Out_Stream, writes elements from Item to the Stream,
   --  stopping when Item'last is reached. Raises End_Error if attempt
   --  to write past end of Stream.

private

   type Direction_Type is (In_Stream, Out_Stream);

   type Stream_Type (Max_Length : Ada.Streams.Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type with
   record
      --  Direction is not a discriminant, because we anticipate
      --  changing direction on some streams.
      Direction : Direction_Type;
      Last      : Ada.Streams.Stream_Element_Offset := 0; -- last element of Raw that has been read/written
      Raw       : Ada.Streams.Stream_Element_Array (1 .. Max_Length);
   end record;

end SAL.Memory_Streams;
