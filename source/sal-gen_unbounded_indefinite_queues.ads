--  Abstract:
--
--  An unbounded queue of indefinite non-limited elements.
--
--  Copyright (C) 2017, 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
generic
   type Element_Type (<>) is private;
package SAL.Gen_Unbounded_Indefinite_Queues is

   type Queue_Type is tagged private;

   Empty_Queue : constant Queue_Type;

   procedure Clear (Queue : in out Queue_Type);
   --  Empty Queue.

   function Count (Queue : in Queue_Type) return Base_Peek_Type;
   --  Return count of items in the Queue

   function Length (Queue : in Queue_Type) return Base_Peek_Type renames Count;

   function Is_Empty (Queue : in Queue_Type) return Boolean;
   --  Return true if no items are in Queue.

   function Is_Full (Queue : in Queue_Type) return Boolean is (False);
   --  Return true if Queue is full.

   function Remove (Queue : in out Queue_Type) return Element_Type;
   --  Remove head/front item from Queue, return it.
   --
   --  Raises Container_Empty if Is_Empty.

   function Get (Queue : in out Queue_Type) return Element_Type renames Remove;

   procedure Drop (Queue : in out Queue_Type);
   --  Remove head/front item from Queue, discard it.
   --
   --  Raise Container_Empty if Is_Empty.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private
   with Implicit_Dereference => Element;

   function Peek (Queue : in Queue_Type; N : Peek_Type := 1) return Constant_Reference_Type;
   pragma Inline (Peek);
   --  Return a constant reference to a queue item. N = 1 is the queue
   --  head.
   --
   --  Raises Parameter_Error if N > Count

   type Variable_Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference => Element;

   function Variable_Peek (Queue : in out Queue_Type; N : Peek_Type := 1) return Variable_Reference_Type;
   --  Return a variable reference to a queue item. N = 1 is the queue
   --  head.
   --
   --  Raises Parameter_Error if N > Count

   procedure Add (Queue : in out Queue_Type; Item : in Element_Type);
   --  Add Element to the tail/back of Queue.

   procedure Put (Queue : in out Queue_Type; Item : in Element_Type) renames Add;

   procedure Add_To_Head (Queue : in out Queue_Type; Item : in Element_Type);
   --  Add Element to the head/front of Queue.

private

   package Element_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type);

   --  We don't provide cursors, so we don't need any tampering checks.
   --
   --  It is possible for two tasks to be doing a variable Peek on the
   --  same element; this container is not task safe anyway (two tasks
   --  can do put/get).

   type Queue_Type is tagged record
      Data : Element_Lists.List;
      --  Add at Tail/Back = Last, remove at Head/Front = First.
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_Queue : constant Queue_Type := (Data => Element_Lists.Empty_List);

end SAL.Gen_Unbounded_Indefinite_Queues;
