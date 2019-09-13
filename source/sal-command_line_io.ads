--  Abstract:
--
--  Provide generic routines for reading parameters from the command line.
--
--  Each Get_* procedure reads a particular parameter type from
--  Command_Line.Argument(Next_Arg) and then increments Next_Arg by
--  one. PARAMETER_ERROR is raised for *any* error, after putting an
--  appropriate message to Standard_Error.
--
--  Copyright (C) 1996 - 1997, 2008, 2009, 2015 Stephen Leake.  All Rights Reserved.
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

package SAL.Command_Line_IO is

   procedure Get_String
     (Item      :    out String;
      Last      :    out Natural;
      Expecting : in     String;
      Next_Arg  : in out Positive);
   --  Last is set to the index of the last element read into Item.

   function Get_String
     (Expecting : in     String;
      Next_Arg  : in out Positive)
     return String;

   generic
      type Discrete_Type is (<>);
      Default_Expecting : in String;
      Next_Arg : in out Natural; -- index of next argument to read
   procedure Gen_Get_Discrete_Proc
     (Item      :    out Discrete_Type;
      Expecting : in     String        := Default_Expecting);

   generic
      type Discrete_Type is (<>);
      Default_Expecting : in String;
   function Gen_Get_Discrete_Func
     (Next_Arg  : in out Natural;
      Expecting : in     String := Default_Expecting)
     return Discrete_Type;

   generic
      type Float_Type is digits <>;
      Default_Expecting : in String;
      Next_Arg : in out Natural; -- index of next argument to read
   procedure Gen_Get_Float
     (Item      :    out Float_Type;
      Expecting : in     String     := Default_Expecting);

end SAL.Command_Line_IO;
