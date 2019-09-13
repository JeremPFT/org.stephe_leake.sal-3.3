--  Abstract :
--
--  Access to tree internals for testing parent.
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

generic
   with function Image (Key : in Key_Type) return String;
   --  Identify a node for error message
package SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test is

   function Root (Tree : in Pkg.Tree) return Cursor;

   function Parent (Cursor : in Pkg.Cursor) return Pkg.Cursor;
   function Left (Cursor : in Pkg.Cursor) return Pkg.Cursor;

   function Right (Cursor : in Pkg.Cursor) return Pkg.Cursor;

   procedure Check_Null
     (Label  : in String;
      Cursor : in Pkg.Cursor);

   procedure Check_Non_Null
     (Label  : in String;
      Cursor : in Pkg.Cursor);

   procedure Check_Color
     (Label      : in String;
      Cursor     : in Pkg.Cursor;
      Expect_Red : in Boolean);

   function Black_Height (Tree : in Pkg.Tree; Cursor : in Pkg.Cursor) return Integer;

   procedure Validate (Label : in String; Tree : in Pkg.Tree);
   --  Check that Tree has all red-black properties

end SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
