--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2005, 2009, 2012, 2015 Stephen Leake.  All Rights Reserved.
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
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body SAL.Gen_Stats is

   procedure Accumulate (Stats : in out Stats_Type; Value : in Real_Type)
   is begin
      Stats :=
        (Count       => Stats.Count + 1,
         Sum_Squared => Stats.Sum_Squared + Value ** 2,
         Sum         => Stats.Sum + Value,
         Min         => Real_Type'Min (Stats.Min, Value),
         Max         => Real_Type'Max (Stats.Max, Value));
   end Accumulate;

   function Count (Stats : in Stats_Type) return Integer
   is begin
      return Stats.Count;
   end Count;

   function Max (Stats : in Stats_Type) return Real_Type is
   begin
      return Stats.Max;
   end Max;

   function Sum (Stats : in Stats_Type) return Real_Type is
   begin
      return Stats.Sum;
   end Sum;

   function Mean (Stats : in Stats_Type) return Real_Type
   is begin
      if Stats.Count > 0 then
         return Stats.Sum / Real_Type (Stats.Count);
      else
         return Stats.Sum;
      end if;
   end Mean;

   function Min (Stats : in Stats_Type) return Real_Type is
   begin
      return Stats.Min;
   end Min;

   procedure Reset (Stats : in out Stats_Type) is
   begin
      Stats :=
        (Count       => 0,
         Sum_Squared => 0.0,
         Sum         => 0.0,
         Min         => Real_Type'Last,
         Max         => Real_Type'First);
   end Reset;

   function Standard_Deviation (Stats : in Stats_Type) return Real_Type
   is
      Sigma : Real_Type;
   begin
      if Stats.Count > 1 then
         Sigma := abs (Real_Type (Stats.Count) * Stats.Sum_Squared - Stats.Sum * Stats.Sum);
         Sigma := Elementary.Sqrt (Sigma / (Real_Type (Stats.Count) * Real_Type (Stats.Count - 1)));
      else
         Sigma := 0.0;
      end if;

      return Sigma;
   end Standard_Deviation;

   function Display (Stats : in Stats_Type) return Display_Type
   is begin
      return (Stats.Count, Mean (Stats), Standard_Deviation (Stats), Stats.Min, Stats.Max);
   end Display;

end SAL.Gen_Stats;
