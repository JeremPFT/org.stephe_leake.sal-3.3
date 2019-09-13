--  Abstract:
--
--  See spec.
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


with Ada.Command_Line;
with Ada.Exceptions;
with System;
with Ada.Text_IO;
package body SAL.Command_Line_IO is

   type System_Float_Type is digits System.Max_Digits;

   package System_Float_IO is new Ada.Text_IO.Float_IO (System_Float_Type);

   procedure Get_String
     (Item      :    out String;
      Last      :    out Natural;
      Expecting : in     String;
      Next_Arg  : in out Positive)
   is begin

      if Next_Arg > Ada.Command_Line.Argument_Count then
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Expecting & " required");
         raise Parameter_Error;
      else
         Last := Ada.Command_Line.Argument (Next_Arg)'Length;
         Item (Item'First .. Last) := Ada.Command_Line.Argument (Next_Arg);
         Next_Arg := Next_Arg + 1;
      end if;

   end Get_String;

   function Get_String
     (Expecting : in     String;
      Next_Arg  : in out Positive)
     return String
   is
      Current_Arg : constant Positive := Next_Arg;
   begin
      if Next_Arg > Ada.Command_Line.Argument_Count then
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Expecting & " required");
         raise Parameter_Error;
      else
         Next_Arg := Next_Arg + 1;
         return Ada.Command_Line.Argument (Current_Arg);
      end if;
   end Get_String;

   procedure Gen_Get_Discrete_Proc
     (Item      :    out Discrete_Type;
      Expecting : in     String        := Default_Expecting)
   is begin

      if Next_Arg > Ada.Command_Line.Argument_Count then
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Expecting & " required");
         raise Parameter_Error;
      else
         begin
            Item := Discrete_Type'Value (Ada.Command_Line.Argument (Next_Arg));
            Next_Arg := Next_Arg + 1;
         exception
         when Error : others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "exception " & Ada.Exceptions.Exception_Name (Error) &
                 " expecting " & Expecting & ", found " &
                 Ada.Command_Line.Argument (Next_Arg));
            raise Parameter_Error;
         end;
      end if;

   end Gen_Get_Discrete_Proc;

   function Gen_Get_Discrete_Func
     (Next_Arg  : in out Natural;
      Expecting : in     String := Default_Expecting)
     return Discrete_Type
   is
      Result : Discrete_Type;
   begin

      if Next_Arg > Ada.Command_Line.Argument_Count then
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Expecting & " required");
         raise Parameter_Error;
      else
         begin
            Result := Discrete_Type'Value (Ada.Command_Line.Argument (Next_Arg));
            Next_Arg := Next_Arg + 1;
            return Result;
         exception
         when Error : others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "exception " & Ada.Exceptions.Exception_Name (Error) &
                 " expecting " & Expecting & ", found " &
                 Ada.Command_Line.Argument (Next_Arg));
            raise Parameter_Error;
         end;
      end if;

   end Gen_Get_Discrete_Func;

   procedure Gen_Get_Float
     (Item      :    out Float_Type;
      Expecting : in     String     := Default_Expecting)
   is
      Last : Natural;
   begin

      if Next_Arg > Ada.Command_Line.Argument_Count then
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Expecting & " required");
         raise Parameter_Error;
      else
         begin
            System_Float_IO.Get (Ada.Command_Line.Argument (Next_Arg),
                                 System_Float_Type (Item),
                                 Last);
            Next_Arg := Next_Arg + 1;
         exception
         when Error : others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "exception " & Ada.Exceptions.Exception_Name (Error) &
                 " expecting " & Expecting & ", found " &
                 Ada.Command_Line.Argument (Next_Arg));
            raise Parameter_Error;
         end;
      end if;

   end Gen_Get_Float;

end SAL.Command_Line_IO;
