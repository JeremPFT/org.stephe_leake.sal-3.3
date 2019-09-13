--  Abstract :
--
--  Config file subprograms for parent.
--
--  Copyright (C) 2004, 2009, 2017, 2019 Stephen Leake.  All Rights Reserved.
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
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with SAL.Config_Files;
package SAL.Time_Conversions.Config is
   --  pragma Elaborate_Body; --  SAL.Config_Files is, but we have no body
   pragma Warnings (Off, "instance does not use primitive operation ""mod"""); -- GNAT < 2019
   pragma Warnings
     (Off, -- GNAT = 2019
      "instance uses predefined operation, not primitive operation ""mod"" at sal-time_conversions.ads:97");


   function Read is new SAL.Config_Files.Read_Fixed (SAL.Time_Conversions.Time_Type);
   function Read is new SAL.Config_Files.Read_Iterator_Fixed (SAL.Time_Conversions.Time_Type);

   procedure Write is new SAL.Config_Files.Write_Fixed (SAL.Time_Conversions.Time_Type);

end SAL.Time_Conversions.Config;
