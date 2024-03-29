--  Abstract :
--
--  Config file operations for types in Ada.*
--
--  Copyright (C) 2003, 2004, 2009, 2015 Stephen Leake.  All Rights Reserved.
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
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with SAL.Config_Files; use SAL.Config_Files;
package Ada_Config is
   --  Note that this is _not_ Ada . Config; that's illegal

   function Read is new Read_Enum (Standard.Boolean);
   procedure Write is new Write_Enum (Standard.Boolean);

   function Read is new Read_Iterator_Enum (Standard.Boolean);

   function Read is new Read_Integer (Standard.Integer);
   procedure Write is new Write_Integer (Standard.Integer);

   function Read is new Read_Iterator_Integer (Standard.Integer);

end Ada_Config;
