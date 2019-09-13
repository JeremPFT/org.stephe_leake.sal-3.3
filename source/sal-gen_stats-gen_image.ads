--  Abstract :
--
--  Image function for Gen_Stats
--
--  Copyright (C) 2003, 2005, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
generic
   with package Real_IO is new Ada.Text_IO.Float_IO (Real_Type);
   Default_Mean_Fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
   Default_Mean_Aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
   Default_Mean_Exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp;
   Default_SD_Fore   : in Ada.Text_IO.Field := Real_IO.Default_Fore;
   Default_SD_Aft    : in Ada.Text_IO.Field := Real_IO.Default_Aft;
   Default_SD_Exp    : in Ada.Text_IO.Field := Real_IO.Default_Exp;
package SAL.Gen_Stats.Gen_Image is
   pragma Elaborate_Body; --  Ada.Text_IO is

   function Image
     (Item      : in Display_Type;
      Mean_Fore : in Ada.Text_IO.Field := Default_Mean_Fore;
      Mean_Aft  : in Ada.Text_IO.Field := Default_Mean_Aft;
      Mean_Exp  : in Ada.Text_IO.Field := Default_Mean_Exp;
      SD_Fore   : in Ada.Text_IO.Field := Default_SD_Fore;
      SD_Aft    : in Ada.Text_IO.Field := Default_SD_Aft;
      SD_Exp    : in Ada.Text_IO.Field := Default_SD_Exp)
     return String;
   --  "(mean, standard_deviation, min, max)"
   --  Mean_* is used for mean, min, max.
   --
   --  If a field is not large enough to show the value with the
   --  format given, that field is filled with asterisks.

end SAL.Gen_Stats.Gen_Image;
