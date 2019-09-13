--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017, 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Checks;
with SAL.Ada_Containers.Gen_Doubly_Linked_Lists_AUnit;
with SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit;
generic
   with procedure Check_Edge_Data (Label : in String; Computed, Expected : in Edge_Data);
package SAL.Gen_Graphs.Gen_AUnit is

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Vertex_Index);
   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Base_Edge_ID);

   procedure Check
     (Label    : in String;
      Computed : in Edge_Item;
      Expected : in Edge_Item);

   procedure Check is new SAL.Ada_Containers.Gen_Doubly_Linked_Lists_AUnit
     (Element_Type  => Edge_Item,
      Lists         => Edge_Lists,
      Check_Element => Check);

   function "&" (Left : in Edge_Lists.List; Right : in Edge_Item) return Edge_Lists.List;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Base_Edge_ID);

   function Image (Item : in Edge_Lists.List) return String;
   --  In unit test expected path building format

   procedure Check
     (Label    : in String;
      Computed : in Path_Item;
      Expected : in Path_Item);

   procedure Check_Path is new AUnit.Checks.Gen_Check_Unconstrained_Array
     --  Has "Strict_Indices" param
     (Item_Type   => Path_Item,
      Index_Type  => Positive,
      Array_Type  => Path,
      Check_Index => AUnit.Checks.Check,
      Check_Item  => Check);

   procedure Check
     (Label    : in String;
      Computed : in Path;
      Expected : in Path);
   --  For composing.

   function Image (Item : in Path) return String;
   --  In unit test expected path building format

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Path_Index);

   procedure Check is new SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit
     (Index_Type    => Path_Index,
      Element_Type  => Path,
      Vectors       => Path_Arrays,
      Check_Index   => Check,
      Check_Element => Check);

   procedure Check is new SAL.Ada_Containers.Gen_Doubly_Linked_Lists_AUnit
     (Element_Type  => Vertex_Index,
      Lists         => Vertex_Lists,
      Check_Element => Check);

   function "+" (Right : in Vertex_Index) return Vertex_Lists.List;
   function "&" (Left : in Vertex_Lists.List; Right : in Vertex_Index) return Vertex_Lists.List;

   procedure Check is new SAL.Ada_Containers.Gen_Doubly_Linked_Lists_AUnit
     (Element_Type  => Vertex_Lists.List,
      "="           => Vertex_Lists."=",
      Lists         => Component_Lists,
      Check_Element => Check);

end SAL.Gen_Graphs.Gen_AUnit;
