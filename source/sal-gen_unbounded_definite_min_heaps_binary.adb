--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

package body SAL.Gen_Unbounded_Definite_Min_Heaps_Binary is

   ----------
   --  Hidden operations

   subtype Index is Base_Peek_Type;

   procedure Grow (Heap : in out Heap_Type)
   is
      Temp : Element_Array_Access := Heap.Data;
   begin
      if Heap.Data = null then
         Heap.Data := new Element_Array (1 .. Initial_Size);
      else
         Heap.Data := new Element_Array (1 .. 2 * Temp.all'Length);
         Heap.Data (1 .. Temp.all'Last) := Temp.all;
         Free (Temp);
      end if;
   end Grow;

   function Parent (I : in Index) return Index
   is begin
      --  [1] 6.1
      return I / 2;
   end Parent;

   function Left (I : in Index) return Index
   is begin
      --  [1] 6.1
      return 2 * I;
   end Left;

   function Right (I : in Index) return Index
   is begin
      --  [1] 6.1
      return 2 * I + 1;
   end Right;

   procedure Exchange (Heap : in out Heap_Type; I, J : in Index)
   is
      A    : Element_Array renames Heap.Data.all;
      Temp : constant Element_Type := A (J);
   begin
      A (J) := A (I);
      A (I) := Temp;
   end Exchange;

   procedure Heapify (Heap : in out Heap_Type; I : in Index)
   is
      --  [1] 6.2 MAX-HEAPIFY, adapted for min
      A : Element_Array renames Heap.Data.all;

      L        : constant Index := Left (I);
      R        : constant Index := Right (I);
      Smallest : Index :=
        (if L <= Heap.Count and then Key (A (L)) < Key (A (I))
         then L
         else I);
   begin
      if R <= Heap.Count and then Key (A (R)) < Key (A (Smallest))
      then
         Smallest := R;
      end if;

      if Smallest /= I then
         Exchange (Heap, Smallest, I);
         Heapify (Heap, Smallest);
      end if;
   end Heapify;

   procedure Internal_Decrease_Key (Heap : in out Heap_Type; J : in Index)
   is
      A : Element_Array renames Heap.Data.all;
      I : Index := J;
   begin
      --  [1] 6.5 HEAP-INCREASE-KEY modified for decrease; skip first two
      --  steps (already done by caller).
      while I > A'First and then Key (A (I)) < Key (A (Parent (I))) loop
         Exchange (Heap, I, Parent (I));
         I := Parent (I);
      end loop;
   end Internal_Decrease_Key;

   ----------
   --  Visible operations

   overriding
   procedure Initialize (Object : in out Heap_Type)
   is begin
      --  Data is null by default.
      Object.Count := 0;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Heap_Type)
   is begin
      Free (Object.Data);
      Object.Count := 0;
   end Finalize;

   overriding
   procedure Adjust (Object : in out Heap_Type)
   is begin
      if Object.Data /= null then
         Object.Data := new Element_Array'(Object.Data.all);
      end if;
   end Adjust;

   procedure Clear (Heap : in out Heap_Type)
   is begin
      --  User can call Finalize if they want to free memory.
      Heap.Count := 0;
   end Clear;

   function Count (Heap : in Heap_Type) return Base_Peek_Type
   is begin
      return Heap.Count;
   end Count;

   function Remove (Heap : in out Heap_Type) return Element_Type
   is begin
      if Heap.Count = 0 then
         raise Container_Empty;
      end if;

      return A : constant Element_Type := Heap.Data (1)
      do
         --  [1] 6.5 HEAP-EXTRACT-MAX; rename operations for min, our root is
         --  0.
         Heap.Data (1) := Heap.Data (Heap.Count);
         Heap.Count    := Heap.Count - 1;
         Heapify (Heap, 1);
      end return;
   end Remove;

   function Min_Key (Heap : in out Heap_Type) return Key_Type
   is begin
      return Key (Heap.Data (1));
   end Min_Key;

   procedure Drop (Heap : in out Heap_Type)
   is
      Junk : Element_Type := Remove (Heap);
      pragma Unreferenced (Junk);
   begin
      null;
   end Drop;

   procedure Add (Heap : in out Heap_Type; Item : in Element_Type)
   is begin
      --  [1] 6.5 MAX-HEAP-INSERT, adapted for min
      if Heap.Data = null or else Heap.Data'Length <= Heap.Count + 1 then
         Grow (Heap);
      end if;

      Heap.Count := Heap.Count + 1;
      Heap.Data (Heap.Count) := Item;
      Internal_Decrease_Key (Heap, Heap.Count);
   end Add;

end SAL.Gen_Unbounded_Definite_Min_Heaps_Binary;
