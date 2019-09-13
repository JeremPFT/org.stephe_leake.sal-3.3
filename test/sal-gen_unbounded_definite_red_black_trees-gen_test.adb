--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with AUnit.Checks;
with AUnit.Assertions;
package body SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test is

   function Root (Tree : in SAL.Gen_Unbounded_Definite_Red_Black_Trees.Tree) return Cursor
   is begin
      return
        (Node       => Tree.Root,
         Direction  => Unknown,
         Left_Done  => True,
         Right_Done => True);
   end Root;

   function Parent (Cursor : in Pkg.Cursor) return Pkg.Cursor
   is begin
      return
        (Node       => Cursor.Node.Parent,
         Direction  => Unknown,
         Left_Done  => True,
         Right_Done => True);
   end Parent;

   function Left (Cursor : in Pkg.Cursor) return Pkg.Cursor
   is begin
      return
        (Node       => Cursor.Node.Left,
         Direction  => Unknown,
         Left_Done  => True,
         Right_Done => True);
   end Left;

   function Right (Cursor : in Pkg.Cursor) return Pkg.Cursor
   is begin
      return
        (Node       => Cursor.Node.Right,
         Direction  => Unknown,
         Left_Done  => True,
         Right_Done => True);
   end Right;

   procedure Check_Null
     (Label  : in String;
      Cursor : in Pkg.Cursor)
   is begin
      AUnit.Assertions.Assert (Cursor.Node = null, Label & " expected null, got non-null");
   end Check_Null;

   procedure Check_Non_Null
     (Label  : in String;
      Cursor : in Pkg.Cursor)
   is begin
      AUnit.Assertions.Assert (Cursor.Node /= null, Label & " expected not-null, got null");
   end Check_Non_Null;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Color);

   procedure Check_Color
     (Label      : in String;
      Cursor     : in Pkg.Cursor;
      Expect_Red : in Boolean)
   is begin
      Check (Label, Cursor.Node.Color, (if Expect_Red then Red else Black));
   end Check_Color;

   procedure Validate_Children_Of_Red (Label : in String; Node : in Node_Access; Nil : in Node_Access)
   is
      use AUnit.Assertions;
   begin
      --  We also check that all Parent links are correct.
      if Node.Color = Red then
         if Node.Left /= Nil then
            Assert (Node.Left.Color = Black, Label & ".left child of red not black");
         end if;
         if Node.Right /= Nil then
            Assert (Node.Right.Color = Black, Label & ".right child of red not black");
         end if;
      end if;
      if Node.Left /= Nil then
         Assert (Node.Left.Parent = Node, Label & ".left child parent not = node");
         Validate_Children_Of_Red (Label, Node.Left, Nil);
      end if;
      if Node.Right /= Nil then
         Assert (Node.Right.Parent = Node, Label & ".right child parent not = node");
         Validate_Children_Of_Red (Label, Node.Right, Nil);
      end if;
   end Validate_Children_Of_Red;

   function Black_Height (Node : in Node_Access; Nil : in Node_Access) return Integer
   is begin
      if Node.Left = Nil then
         return 0;
      else
         return Black_Height (Node.Left, Nil) + (if Node.Left.Color = Black then 1 else 0);
      end if;
   end Black_Height;

   function Black_Height (Tree : in Pkg.Tree; Cursor : in Pkg.Cursor) return Integer
   is begin
      if Cursor.Node = null then
         return 0;
      else
         return Black_Height (Cursor.Node, Tree.Nil);
      end if;
   end Black_Height;

   procedure Validate_Black_Height (Label : in String; Node : in Node_Access; Nil : in Node_Access)
   is
      use AUnit.Assertions;
   begin
      if Node.Left = Nil then
         if Node.Right = Nil then
            null;
         else
            declare
               Right : constant Integer := Black_Height (Node.Right, Nil) +
                 (if Node.Right.Color = Black then 1 else 0);
            begin
               Assert
                 (Right = 0,
                  Label & "." & Image (Key (Node.Element)) & ".siblings have different black height 0," &
                    Integer'Image (Right));
            end;
            Validate_Black_Height (Label, Node.Right, Nil);
         end if;
      elsif Node.Right = Nil then
         declare
            Left  : constant Integer := Black_Height (Node.Left, Nil) +
              (if Node.Left.Color = Black then 1 else 0);
         begin
            Assert
              (Left = 0,
               Label & "." & Image (Key (Node.Element)) & ".siblings have different black height" &
                 Integer'Image (Left) & ", 0");
         end;
         Validate_Black_Height (Label, Node.Left, Nil);
      else
         declare
            Left  : constant Integer := Black_Height (Node.Left, Nil) + (if Node.Left.Color = Black then 1 else 0);
            Right : constant Integer := Black_Height (Node.Right, Nil) + (if Node.Right.Color = Black then 1 else 0);
         begin
            Assert
              (Left = Right,
               Label & "." & Image (Key (Node.Element)) & ".siblings have different black height" &
                 Integer'Image (Left) & "," & Integer'Image (Right));
         end;
         Validate_Black_Height (Label, Node.Left, Nil);
         Validate_Black_Height (Label, Node.Right, Nil);
      end if;
   end Validate_Black_Height;

   procedure Validate (Label : in String; Tree : in Pkg.Tree)
   is begin
      if Tree.Root = Tree.Nil then
         return;
      end if;

      --  [1] 13.1 properties
      --  1. Every node is red or black - trivially true.
      --  2. The root is black
      Check (Label & ".root = black", Tree.Root.Color, Black);

      --  3. Every leaf is black; trivially true (all leaves are Nil = black).
      --  4. If a node is red, both its children are black
      Validate_Children_Of_Red (Label, Tree.Root, Tree.Nil);

      --  5. For each node, all simple paths from the node to descendant
      --  leaves contain the same number of black nodes.
      Validate_Black_Height (Label, Tree.Root, Tree.Nil);
   end Validate;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
