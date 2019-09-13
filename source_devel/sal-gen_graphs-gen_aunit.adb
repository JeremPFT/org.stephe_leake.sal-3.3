--  Abstract :
--
--  see spec.
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

with Ada.Strings.Unbounded;
package body SAL.Gen_Graphs.Gen_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Edge_Item;
      Expected : in Edge_Item)
   is begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check_Edge_Data (Label & ".Data", Computed.Data, Expected.Data);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Path_Item;
      Expected : in Path_Item)
   is begin
      Check (Label & ".Vertex", Computed.Vertex, Expected.Vertex);
      Check (Label & ".Edges", Computed.Edges, Expected.Edges);
   end Check;

   function "&" (Left : in Edge_Lists.List; Right : in Edge_Item) return Edge_Lists.List
   is
      use Edge_Lists;
   begin
      return Result : List := Left do
         Append (Result, Right);
      end return;
   end "&";

   function Image (Item : in Edge_Lists.List) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      First  : Boolean          := True;
   begin
      for E of Item loop
         if First then
            Result := To_Unbounded_String ("+");
            First  := False;
         else
            Result := Result & " & ";
         end if;
         Result := Result & "(" & Trimmed_Image (E.ID) & ", " & Edge_Image (E.Data) & ")";
      end loop;
      return To_String (Result);
   end Image;

   procedure Check
     (Label    : in String;
      Computed : in Path;
      Expected : in Path)
   is begin
      Check_Path (Label, Computed, Expected, Strict_Indices => True);
   end Check;

   function Image (Item : in Path) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String ("(");
   begin
      for I in Item'Range loop
         Result := Result & "(" & Trimmed_Image (Item (I).Vertex) & ", " & Gen_AUnit.Image (Item (I).Edges) & ")";
         if I /= Item'Last then
            Result := Result & ", ";
         end if;
      end loop;
      Result := Result & ")";
      return To_String (Result);
   end Image;

   function "+" (Right : in Vertex_Index) return Vertex_Lists.List
   is
      use Vertex_Lists;
   begin
      return Result : List do
         Append (Result, Right);
      end return;
   end "+";

   function "&" (Left : in Vertex_Lists.List; Right : in Vertex_Index) return Vertex_Lists.List
   is
      use Vertex_Lists;
   begin
      return Result : List := Left do
         Append (Result, Right);
      end return;
   end "&";

end SAL.Gen_Graphs.Gen_AUnit;
