--  Abstract :
--
--  Task-safe version of parent.
--
--  An internal protected object guards against simultaneous
--  read/write of the vector indices (ie, any operation that might
--  grow the underlying array); such access will block.
--
--  This does not guard simultaneous read/write of individual element
--  content.
--
--  Copyright (C) 2018 - 2019 Stephen Leake All Rights Reserved.
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

generic
package SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected is

   type Vector is new Ada.Finalization.Controlled with private;

   type Vector_Access_Constant is access constant Vector;
   for Vector_Access_Constant'Storage_Size use 0;

   overriding procedure Initialize (Container : in out Vector);
   overriding procedure Finalize (Container : in out Vector);
   overriding procedure Adjust (Container : in out Vector);

   procedure Clear (Container : in out Vector);

   function Length (Container : in Vector) return Ada.Containers.Count_Type;
   function Capacity (Container : in Vector) return Ada.Containers.Count_Type;

   function First_Index (Container : Vector) return Extended_Index;
   function Last_Index (Container : Vector) return Extended_Index;

   function Element (Container : Vector; Index : Index_Type) return Element_Type;

   procedure Append (Container : in out Vector; New_Item : in Element_Type);

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type);

   procedure Set_First_Last (Container : in out Vector; First : in Index_Type; Last : in Index_Type);

   --  WORKAROUND: GNAT GPL 2017 does not properly finalize the
   --  *_Reference object after a reference completes, leaving the Vector
   --  read-locked. On the other hand, this allows Lock_Type to be
   --  Limited, which allows it to include write lock.
   --
   --  type Constant_Reference_Type
   --    (Element   : not null access constant Element_Type;
   --     Container : not null access constant Vector)
   --  is private
   --  with Implicit_Dereference => Element;

   --  function Constant_Reference
   --    (Container : aliased in Vector;
   --     Index     :         in Index_Type)
   --    return Constant_Reference_Type;

   --  type Variable_Reference_Type
   --    (Container : not null access Vector;
   --     Element   : not null access Element_Type)
   --  is private
   --  with Implicit_Dereference => Element;

   --  function Variable_Reference
   --    (Container : aliased in out Vector;
   --     Index     :         in     Index_Type)
   --    return Variable_Reference_Type;

   type Lock_Type (Container : not null access constant Vector; Write : Boolean)
     is new Ada.Finalization.Limited_Controlled with private;
   --  For applications that require an extended read or write lock. If
   --  Write, lock for write; otherwise lock for read.
   --
   --  Note that iterators do _not_ hold a read lock (due to a bug in
   --  GNAT GPL 2017), so a separate lock is needed for iterator use.

   overriding procedure Initialize (Lock : in out Lock_Type);
   --  Acquire lock.

   overriding procedure Finalize (Lock : in out Lock_Type);
   --  Release lock.

   function Locked (Container : in Vector) return Boolean;
   --  True if Container is locked for read or write.

private

   protected type Guard is

      entry Acquire_Read;
      --  Available when there is no writer.

      procedure Release_Read;

      entry Acquire_Write;
      --  Available when there are no readers, and no writer.

      procedure Release_Write;

      --  It is tempting to provide an operation "update_read_to_write", so
      --  we spend as little time as possible with a write lock. But that
      --  leads to deadlock, if two tasks aquire read lock, then both attempt
      --  to upgrade.

      function Any_Readers_Or_Writers return Boolean;

   private
      Readers : Integer := 0;
      Writer  : Boolean := False;
   end Guard;

   type Guard_Access is access Guard;
   procedure Free is new Ada.Unchecked_Deallocation (Guard, Guard_Access);

   type Vector is new Ada.Finalization.Controlled with record
      Super : Gen_Unbounded_Definite_Vectors.Vector;
      --  We wrap Super, rather than deriving from it, because it is not
      --  possible to add a Lock_Type to Constant_Reference_Type.

      Guard : Guard_Access;
      --  We use an access type here for two reasons:
      --  1 - it allows Vector to be non-limited
      --  2 - it allows writing to Guard when the Vector object is 'in'.
   end record;

   type Lock_Type (Container : not null access constant Vector; Write : Boolean)
     is new Ada.Finalization.Limited_Controlled with
   record
      Initialized : Boolean := False;
      --  Enforce Initialize called only once (sometimes it's hard to tell
      --  if Initialize will be called automatically).

      Finalized : Boolean := False;
      --  Finalize can be called more than once; don't release the guard
      --  more than once.
   end record;

   --  type Constant_Reference_Type
   --    (Element   : not null access constant Element_Type;
   --     Container : not null access constant Vector)
   --    is record
   --     Lock : Lock_Type (Container);
   --  end record;

   --  type Variable_Reference_Type
   --    (Container : not null access Vector;
   --     Element   : not null access Element_Type)
   --    is record
   --     Lock : Lock_Type (Container);
   --  end record;

   --  WORKAROUND: GNAT GPL 2017 does not properly finalize the Iterator
   --  object after a loop completes, leaving the Vector read-locked.
   --  type Cursor is record
   --     Super : Gen_Unbounded_Definite_Vectors.Cursor;
   --     --  This type is not visible, and is only useable with an Iterator, so
   --     --  it does not need a read lock.
   --  end record;

   --  function Has_Element (Position : Cursor) return Boolean;

   --  function Constant_Reference
   --    (Container : aliased in out Vector;
   --     Position  :         in     Cursor)
   --    return Constant_Reference_Type;

   --  function Variable_Reference
   --    (Container : aliased in out Vector;
   --     Position  :         in     Cursor)
   --    return Variable_Reference_Type;

   --  package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   --  type Iterator (Container : not null access Vector) is new Vector_Iterator_Interfaces.Reversible_Iterator with
   --  record
   --     Super : Gen_Unbounded_Definite_Vectors.Iterator;
   --     Lock  : Lock_Type (Container);
   --  end record;

   --  function Iterate (Container : aliased in out Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   --  overriding function First (Object : in Iterator) return Cursor;
   --  overriding function Last  (Object : in Iterator) return Cursor;
   --  overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor;
   --  overriding function Previous (Object : in Iterator; Position : in Cursor) return Cursor;

end SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected;
