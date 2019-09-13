--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with GNAT.OS_Lib;
package body Test.Config_Files is

   procedure Delete_File (Name : in String)
   is
      use GNAT.OS_Lib;
      File    : String_Access := Locate_Regular_File (Name, ".");
      Success : Boolean;
   begin
      if File /= null then
         Delete_File (File.all, Success);
         Free (File);
      end if;
   end Delete_File;

end Test.Config_Files;
