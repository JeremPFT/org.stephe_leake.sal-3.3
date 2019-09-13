--  Abstract :
--
--  Statistics; mean, standard deviation, min, max.
--
--  Copyright (C) 2003, 2005, 2009, 2012, 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Numerics.Generic_Elementary_Functions;
generic
   type Real_Type is digits <>;
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Stats is
   pragma Pure;

   type Stats_Type is tagged private;
   --  tagged to allow "object.method" notation

   Null_Stats : constant Stats_Type;

   procedure Reset (Stats : in out Stats_Type);
   --  Reset accumulated values, count to zero.

   procedure Accumulate (Stats : in out Stats_Type; Value : in Real_Type);

   function Standard_Deviation (Stats : in Stats_Type) return Real_Type;
   --  Returns 0.0 if count is < 2.

   function Count (Stats : in Stats_Type) return Integer;

   function Mean (Stats : in Stats_Type) return Real_Type;

   function Min (Stats : in Stats_Type) return Real_Type;

   function Max (Stats : in Stats_Type) return Real_Type;

   function Sum (Stats : in Stats_Type) return Real_Type;

   type Display_Type is record
      Count              : Integer;
      Mean               : Real_Type;
      Standard_Deviation : Real_Type;
      Min                : Real_Type;
      Max                : Real_Type;
   end record;

   function Display (Stats : in Stats_Type) return Display_Type;

private
   type Stats_Type is tagged record
      Count       : Integer   := 0;
      Sum         : Real_Type := 0.0;
      Sum_Squared : Real_Type := 0.0;
      Min         : Real_Type := Real_Type'Last;
      Max         : Real_Type := Real_Type'First;
   end record;

   Null_Stats : constant Stats_Type :=
     (Count       => 0,
      Sum         => 0.0,
      Sum_Squared => 0.0,
      Min         => Real_Type'Last,
      Max         => Real_Type'First);

end SAL.Gen_Stats;
