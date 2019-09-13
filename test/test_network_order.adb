--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2004, 2009, 2015 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Streams; use Ada.Streams;
with AUnit.Assertions;
with Interfaces;
with SAL.Network_Order.Gen_Scalar_32;
with SAL.Network_Order.Gen_Scalar_64;
with SAL.Network_Order;       use SAL.Network_Order;
package body Test_Network_Order
is
   type Fixed_32_Type is delta 0.1 digits 9;
   for Fixed_32_Type'Size use 32;

   package Float_32_Order is new SAL.Network_Order.Gen_Scalar_32 (Interfaces.IEEE_Float_32);
   package Float_64_Order is new SAL.Network_Order.Gen_Scalar_64 (Interfaces.IEEE_Float_64);
   package Fixed_32_Order is new SAL.Network_Order.Gen_Scalar_32 (Fixed_32_Type);

   use Float_32_Order, Float_64_Order, Fixed_32_Order;

   type Host_Data_Type is record
      Element     : Stream_Element;
      Signed_8    : Interfaces.Integer_8;
      Unsigned_8  : Interfaces.Unsigned_8;
      Signed_16   : Interfaces.Integer_16;
      Unsigned_16 : Interfaces.Unsigned_16;
      Signed_32   : Interfaces.Integer_32;
      Unsigned_32 : Interfaces.Unsigned_32;
      Float_32    : Interfaces.IEEE_Float_32;
      Float_64    : Interfaces.IEEE_Float_64;
      Fixed_32    : Fixed_32_Type;
   end record;

   Host_Data : constant Host_Data_Type :=
     (Element     => 16#F0#,
      Signed_8    => 16#12#,
      Unsigned_8  => 16#34#,
      Signed_16   => 16#5678#,
      Unsigned_16 => 16#9ABC#,
      Signed_32   => 16#01234567#,
      Unsigned_32 => 16#89ABCDEF#,
      Float_32    => 1.2,
      Float_64    => 1234567.8,
      Fixed_32    => 2.0);

   Read_Data : Host_Data_Type;

   Network_Data      : Stream_Element_Array (0 .. 30);
   Network_Data_Last : Stream_Element_Offset;

   Expected : constant Stream_Element_Array (0 .. 30) :=
     (0  => 16#F0#, -- Element
      1  => 16#12#, -- Signed_8
      2  => 16#34#, -- Unsigned_8
      3  => 16#56#, -- Signed_16
      4  => 16#78#,
      5  => 16#9A#, -- Unsigned_16
      6  => 16#BC#,
      7  => 16#01#, -- Signed_32
      8  => 16#23#,
      9  => 16#45#,
      10 => 16#67#,
      11 => 16#89#, -- Unsigned_32
      12 => 16#AB#,
      13 => 16#CD#,
      14 => 16#EF#,
      15 => 16#3f#, -- Float_32
      16 => 16#99#,
      17 => 16#99#,
      18 => 16#9a#,
      19 => 16#41#, -- Float_64
      20 => 16#32#,
      21 => 16#d6#,
      22 => 16#87#,
      23 => 16#cc#,
      24 => 16#cc#,
      25 => 16#cc#,
      26 => 16#cd#,
      27 => 16#00#, -- Fixed_32
      28 => 16#00#,
      29 => 16#00#,
      30 => 16#14#);

   procedure Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Network_Data_Last := Network_Data'First - 1;
      To_Network (Host_Data.Element, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Signed_8, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Unsigned_8, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Signed_16, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Unsigned_16, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Signed_32, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Unsigned_32, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Float_32, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Float_64, Network_Data, Network_Data_Last);
      To_Network (Host_Data.Fixed_32, Network_Data, Network_Data_Last);

      AUnit.Assertions.Assert
        (Network_Data_Last = Expected'Last and
           Expected = Network_Data (Network_Data'First .. Network_Data_Last),
         "to_network failure");

      Network_Data_Last := Network_Data'First - 1;
      From_Network (Read_Data.Element, Network_Data, Network_Data_Last);
      From_Network (Read_Data.Signed_8, Network_Data, Network_Data_Last);
      From_Network (Read_Data.Unsigned_8, Network_Data, Network_Data_Last);
      From_Network (Read_Data.Signed_16, Network_Data, Network_Data_Last);
      From_Network (Interfaces.Unsigned_16 (Read_Data.Unsigned_16), Network_Data, Network_Data_Last);
      From_Network (Interfaces.Integer_32 (Read_Data.Signed_32), Network_Data, Network_Data_Last);
      From_Network (Interfaces.Unsigned_32 (Read_Data.Unsigned_32), Network_Data, Network_Data_Last);
      From_Network (Read_Data.Float_32, Network_Data, Network_Data_Last);
      From_Network (Read_Data.Float_64, Network_Data, Network_Data_Last);
      From_Network (Read_Data.Fixed_32, Network_Data, Network_Data_Last);

      AUnit.Assertions.Assert
        (Network_Data_Last = Network_Data'Last and
           Read_Data = Host_Data,
         "From_Network failed");
   end Test;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("sal-network_order.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test'Access, "all data types");
   end Register_Tests;

end Test_Network_Order;
