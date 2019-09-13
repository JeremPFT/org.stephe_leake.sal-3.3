--  Abstract :
--
--  Support for Ada.Calendar.Time type.
--
--  Copyright (C) 2004, 2005, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Calendar;
with SAL.Config_Files;
package Ada_Calendar_Config is
   --  Note that this is _not_ Ada . Calendar . Config; that's illegal

   pragma Elaborate_Body; --  SAL.Config_Files is.

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Key         : in String;
      Default     : in Ada.Calendar.Time;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Ignore)
     return Ada.Calendar.Time;
   --  Read a time from Config.

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Ada.Calendar.Time;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Ada.Calendar.Time;
   --  Read a time from Config.

   procedure Write
     (Config      : in out SAL.Config_Files.Configuration_Type;
      Key         : in     String;
      Value       : in     Ada.Calendar.Time;
      Missing_Key : in     SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Ignore);

end Ada_Calendar_Config;
