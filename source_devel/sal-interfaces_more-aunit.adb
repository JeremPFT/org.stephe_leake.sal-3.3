--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 - 2006, 2009, 2015 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;           use AUnit.Assertions;
with SAL.Interfaces_More.Images; use SAL.Interfaces_More.Images;
package body SAL.Interfaces_More.AUnit is

   procedure Check_Binary
     (Label    : in String;
      Computed : in Interfaces.Unsigned_8;
      Expected : in Interfaces.Unsigned_8)
   is
      use type Interfaces.Unsigned_8;
   begin
      Assert
        (Computed = Expected,
         Label & " failed; expected " & Binary_Image (Expected) &
           " got " & Binary_Image (Computed));
   end Check_Binary;

   procedure Check_Binary
     (Label    : in String;
      Computed : in Interfaces.Unsigned_16;
      Expected : in Interfaces.Unsigned_16)
   is
      use type Interfaces.Unsigned_16;
   begin
      Assert
        (Computed = Expected,
         Label & " failed; expected " & Binary_Image (Expected) &
           " got " & Binary_Image (Computed));
   end Check_Binary;

   procedure Check_Hex
     (Label    : in String;
      Computed : in Interfaces.Unsigned_16;
      Expected : in Interfaces.Unsigned_16)
   is
      use Interfaces;
   begin
      Assert
        (Computed = Expected,
         Label & " failed; expected " & Unsigned_16'Image (Expected) & " (x" & Hex_Image (Expected) & ")" &
           " got " & Unsigned_16'Image (Computed) & " (x" & Hex_Image (Computed) & ")");
   end Check_Hex;

   procedure Check
     (Label     : in String;
      Computed  : in Interfaces.Unsigned_16;
      Expected  : in Interfaces.Unsigned_16;
      Tolerance : in Interfaces.Unsigned_16)
   is
      use Interfaces;
      Diff : Unsigned_16;
   begin
      if Expected > Computed then
         Diff := Expected - Computed;
      else
         Diff := Computed - Expected;
      end if;

      Standard.AUnit.Assertions.Assert
        (Diff <= Tolerance,
         Label & " got" & Unsigned_16'Image (Computed) & " expecting" & Unsigned_16'Image (Expected));
   end Check;

   procedure Check
     (Label     : in String;
      Computed  : in Interfaces.Unsigned_32;
      Expected  : in Interfaces.Unsigned_32;
      Tolerance : in Interfaces.Unsigned_32)
   is
      use Interfaces;
      Diff : Unsigned_32;
   begin
      if Expected > Computed then
         Diff := Expected - Computed;
      else
         Diff := Computed - Expected;
      end if;

      Standard.AUnit.Assertions.Assert
        (Diff <= Tolerance,
         Label & " got" & Unsigned_32'Image (Computed) & " expecting" & Unsigned_32'Image (Expected));
   end Check;

end SAL.Interfaces_More.AUnit;
