--  Abstract :
--
--  see spec.
--
--  An internal protected object guards against simultaneous
--  read/write of the vector indices (ie, any operation that might
--  grow the underlying array), but not individual element content.
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

package body SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected is

   ----------
   --  Public subprograms

   overriding procedure Initialize (Container : in out Vector)
   is begin
      if Container.Guard /= null then
         raise Programmer_Error with "Initialize called twice on a SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected";
      end if;

      Container.Guard := new Gen_Protected.Guard;
   end Initialize;

   overriding procedure Finalize (Container : in out Vector)
   is begin
      Free (Container.Guard);
   end Finalize;

   overriding procedure Adjust (Container : in out Vector)
   is begin
      if Container.Guard.Any_Readers_Or_Writers then
         raise Programmer_Error with
           "Adjust called with active readers or writers on a SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected";
      end if;

      Container.Guard := new Gen_Protected.Guard;
   end Adjust;

   procedure Clear (Container : in out Vector)
   is begin
      Container.Super.Clear;
   end Clear;

   function Length (Container : in Vector) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
   begin
      Container.Guard.Acquire_Read;
      return Result : constant Count_Type := Container.Super.Length do
         Container.Guard.Release_Read;
      end return;
   end Length;

   function Capacity (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      Container.Guard.Acquire_Read;
      return Result : constant Ada.Containers.Count_Type := Container.Super.Capacity do
         Container.Guard.Release_Read;
      end return;
   end Capacity;

   function First_Index (Container : Vector) return Extended_Index
   is begin
      Container.Guard.Acquire_Read;
      return Result : constant Extended_Index := Container.Super.First_Index do
         Container.Guard.Release_Read;
      end return;
   end First_Index;

   function Last_Index (Container : Vector) return Extended_Index
   is begin
      Container.Guard.Acquire_Read;
      return Result : constant Extended_Index := Container.Super.Last_Index do
         Container.Guard.Release_Read;
      end return;
   end Last_Index;

   function Element (Container : Vector; Index : Index_Type) return Element_Type
   is begin
      Container.Guard.Acquire_Read;
      return Result : constant Element_Type := Container.Super.Elements (To_Peek_Type (Index)) do
         Container.Guard.Release_Read;
      end return;
   end Element;

   procedure Append
     (Container : in out Vector;
      New_Item  : in     Element_Type)
   is begin
      Container.Guard.Acquire_Write;
      Container.Super.Append (New_Item);
      Container.Guard.Release_Write;
   end Append;

   procedure Prepend
     (Container : in out Vector;
      New_Item  : in     Element_Type)
   is begin
      Container.Guard.Acquire_Write;
      Container.Super.Prepend (New_Item);
      Container.Guard.Release_Write;
   end Prepend;

   procedure Set_First_Last
     (Container : in out Vector;
      First : in Index_Type;
      Last : in Index_Type)
   is begin
      Container.Guard.Acquire_Write;
      Container.Super.Set_First_Last (First, Last);
      Container.Guard.Release_Write;
   end Set_First_Last;

   --  function Constant_Reference
   --    (Container : aliased in Vector;
   --     Index     :         in Index_Type)
   --    return Constant_Reference_Type
   --  is begin
   --     return Result : Constant_Reference_Type
   --       (Element   => Container.Super.Elements (To_Peek_Type (Index))'Access,
   --        Container => Container'Access);
   --  end Constant_Reference;

   --  function Variable_Reference
   --    (Container : aliased in out Vector;
   --     Index     :         in     Index_Type)
   --    return Variable_Reference_Type
   --  is begin
   --     return Result : Variable_Reference_Type
   --       (Element   => Container.Super.Elements (To_Peek_Type (Index))'Access,
   --        Container => Container'Access);
   --  end Variable_Reference;

   overriding procedure Initialize (Lock : in out Lock_Type)
   is begin
      if Lock.Initialized then
         raise Programmer_Error with "read_lock initialize called twice";
      end if;
      if Lock.Write then
         Lock.Container.Guard.Acquire_Write;
      else
         Lock.Container.Guard.Acquire_Read;
      end if;
      Lock.Initialized := True;
      Lock.Finalized   := False;
   end Initialize;

   overriding procedure Finalize (Lock : in out Lock_Type)
   is begin
      if not Lock.Finalized then
         if Lock.Write then
            Lock.Container.Guard.Release_Write;
         else
            Lock.Container.Guard.Release_Read;
         end if;
         Lock.Finalized   := True;
         Lock.Initialized := False;
      end if;
   end Finalize;

   function Locked (Container : in Vector) return Boolean
   is begin
      return Container.Guard.Any_Readers_Or_Writers;
   end Locked;

   ----------
   --  Spec private subprograms

   protected body Guard is

      entry Acquire_Read when not Writer
      is begin
         Readers := Readers + 1;
      end Acquire_Read;

      procedure Release_Read
      is begin
         Readers := Readers - 1;
      end Release_Read;

      entry Acquire_Write when not Writer and Readers = 0
      is begin
         Writer := True;
      end Acquire_Write;

      procedure Release_Write
      is begin
         Writer := False;
      end Release_Write;

      function Any_Readers_Or_Writers return Boolean
      is begin
         return Readers /= 0 or Writer;
      end Any_Readers_Or_Writers;
   end Guard;

   --  WORKAROUND: see comment on iterators in spec
   --  function Has_Element (Position : Cursor) return Boolean
   --  is begin
   --     return Has_Element (Position.Super);
   --  end Has_Element;

   --  function Constant_Reference
   --    (Container : aliased in out Vector;
   --     Position  :         in     Cursor)
   --    return Constant_Reference_Type
   --  is begin
   --     return Result : Constant_Reference_Type
   --       (Element   => Container.Super.Elements (Position.Super.Index)'Access,
   --        Container => Container'Access);
   --  end Constant_Reference;

   --  function Variable_Reference
   --    (Container : aliased in out Vector;
   --     Position  :         in     Cursor)
   --    return Variable_Reference_Type
   --  is begin
   --     return Result : Variable_Reference_Type
   --       (Element   => Container.Super.Elements (Position.Super.Index)'Access,
   --        Container => Container'Access);
   --  end Variable_Reference;

   --  function Iterate (Container : aliased in out Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   --  is begin
   --     return Result : Iterator (Container => Container'Access)
   --     do
   --        Result.Super := Gen_Unbounded_Definite_Vectors.Iterator (Container.Super.Iterate);
   --     end return;
   --  end Iterate;

   --  overriding function First (Object : in Iterator) return Cursor
   --  is begin
   --     return (Super => Object.Super.First);
   --  end First;

   --  overriding function Last  (Object : in Iterator) return Cursor
   --  is begin
   --     return (Super => Object.Super.Last);
   --  end Last;

   --  overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor
   --  is begin
   --     --  Next reads Container.Last, but this is only used in the context of
   --     --  an iterator, which holds a read lock.
   --     return (Super => Object.Super.Next (Position.Super));
   --  end Next;

   --  overriding function Previous (Object : in Iterator; Position : in Cursor) return Cursor
   --  is begin
   --     --  Previous reads Container.First, but this is only used in the
   --     --  context of an iterator, which holds a read lock.
   --     return (Super => Object.Super.Previous (Position.Super));
   --  end Previous;

end SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected;
