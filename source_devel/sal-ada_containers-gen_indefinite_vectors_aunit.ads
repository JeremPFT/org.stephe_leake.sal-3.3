--  Abstract :
--
--  AUnit checks for instantiations of Ada.Containers.Indefinite_Vectors.
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

with Ada.Containers.Indefinite_Vectors;
generic
   type Index_Type is range <>;
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with package Vectors is new Ada.Containers.Indefinite_Vectors (Index_Type, Element_Type, "=");
   with procedure Check_Index (Label : in String; Computed, Expected : in Index_Type);
   with procedure Check_Element (Label : in String; Computed, Expected : in Element_Type);
procedure SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit (Label : in String; Computed, Expected : in Vectors.Vector);
