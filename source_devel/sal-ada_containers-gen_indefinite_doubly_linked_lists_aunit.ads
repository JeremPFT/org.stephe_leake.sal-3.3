--  Abstract :
--
--  AUnit check for Ada.Containers.Indefinite_Doubly_Linked_Lists
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
generic
   type Element_Type (<>) is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type, "=");

   with procedure Check_Element (Label : in String; Computed, Expected : in Element_Type);

package SAL.Ada_Containers.Gen_Indefinite_Doubly_Linked_Lists_AUnit is

   procedure Check (Label : in String; Computed : in Lists.List; Expected : in Lists.List);

end SAL.Ada_Containers.Gen_Indefinite_Doubly_Linked_Lists_AUnit;
