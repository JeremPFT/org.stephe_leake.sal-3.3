--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2005, 2009, 2010, 2012, 2014, 2015, 2016 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;
with AUnit.Assertions;
with SAL.Gen_Stats.Gen_Image;
package body Test_Stats is

   Default_Mean_Threshold : constant := 10.0e-5;
   Default_SD_Threshold   : constant := 2.0e-6;

   package Stats is new SAL.Gen_Stats (Float, Ada.Numerics.Elementary_Functions);
   package Stats_Image is new Stats.Gen_Image (Ada.Float_Text_IO);

   procedure Check
     (Message   : in String;
      Computed  : in Float;
      Expected  : in Float;
      Threshold : in Float)
   is begin
      AUnit.Assertions.Assert
        (abs (Computed - Expected) < Threshold,
         Message &
           " failed; expected " & Float'Image (Expected) &
           " got " & Float'Image (Computed));
   end Check;

   procedure Check
     (Message        : in String;
      Computed       : in Stats.Stats_Type;
      Mean           : in Float;
      SD             : in Float;
      Min            : in Float;
      Max            : in Float;
      Mean_Threshold : in Float := Default_Mean_Threshold;
      SD_Threshold   : in Float := Default_SD_Threshold)
   is
      Computed_Mean : constant Float := Stats.Mean (Computed);
      Computed_SD   : constant Float := Stats.Standard_Deviation (Computed);
      Computed_Min  : constant Float := Stats.Min (Computed);
      Computed_Max  : constant Float := Stats.Max (Computed);
   begin
      Check (Message & ".Mean", Computed_Mean, Mean, Mean_Threshold);
      Check (Message & ".SD", Computed_SD, SD, SD_Threshold);
      Check (Message & ".Min", Computed_Min, Min, Mean_Threshold);
      Check (Message & ".Max", Computed_Max, Max, Mean_Threshold);
   end Check;

   procedure Check
     (Message : in String;
      Computed : in String;
      Expected : in String)
   is begin
      AUnit.Assertions.Assert
        (Computed = Expected,
         Message &
           " failed; expected '" & Expected &
           "' got '" & Computed);
   end Check;

   procedure Test_Stats (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Stats;
      Stats : Stats_Type;
   begin
      for I in 1 .. 10 loop
         if I <= 5 then
            Accumulate (Stats, 1.1);
         else
            Accumulate (Stats, 0.9);
         end if;
      end loop;
      Check ("case_one", Stats, Mean => 1.0, SD => 0.105411, Min => 0.9, Max => 1.1);

      declare
         Image : constant String := Stats_Image.Image
           (Display (Stats),
            Mean_Fore => 1,
            Mean_Aft  => 1,
            Mean_Exp  => 0,
            SD_Fore   => 1,
            SD_Aft    => 4,
            SD_Exp    => 3);
      begin
         Check ("case_one.image", Image, "( 1.0, 1.0541E-01,  0.9,  1.1)");
      end;

      Reset (Stats);

      for I in 1 .. 10 loop
         if I <= 5 then
            Accumulate (Stats, -10.1);
         else
            Accumulate (Stats, -9.9);
         end if;
      end loop;

      declare
         Image : String := Stats_Image.Image
           (Display (Stats),
            Mean_Fore => 1,
            Mean_Aft  => 1,
            Mean_Exp  => 0,
            SD_Fore   => 1,
            SD_Aft    => 4,
            SD_Exp    => 3);
      begin
         --  Last digit changes between 32 and 64 bit compilers; mask it.
         Image (13) := 'M';
         Check ("overflow.image", Image, "(****, 1.053ME-01, ****, -9.9)");
         --  Value correct for GNAT 7.1.2
      end;

      declare
         Image : String := Stats_Image.Image
           (Display (Stats),
            Mean_Fore => 2,
            Mean_Aft  => 2,
            Mean_Exp  => 0,
            SD_Fore   => 1,
            SD_Aft    => 4,
            SD_Exp    => 3);
      begin
         --  Last digit changes between 32 and 64 bit compilers; mask it.
         Image (15) := 'M';
         Check ("case_two.image", Image, "(-10.00, 1.053ME-01, -10.10,  -9.90)");
         --  Value correct for GNAT 7.1.2
      end;
   end Test_Stats;

   ----------
   --  Public routines

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Stats");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Stats'Access, "Test_Stats");
   end Register_Tests;

end Test_Stats;
