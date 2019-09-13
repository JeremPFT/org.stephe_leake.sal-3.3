--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008 - 2018 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
package body SAL.CSV is

   procedure Free is new Ada.Unchecked_Deallocation (String, Ada.Strings.Unbounded.String_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Positive_Array_Integer_Type, Positive_Array_Integer_Access_Type);

   overriding procedure Finalize (File : in out File_Type)
   is
      use type Ada.Strings.Unbounded.String_Access;
   begin
      if File.Line /= null then
         Ada.Text_IO.Close (File.File);
         Free (File.Line);
         Free (File.Commas);
      end if;
   end Finalize;

   procedure Scan_Commas
     (File        : in out File_Type;
      Commas      : in     Positive_Array_Integer_Access_Type;
      Comma_Count :    out Integer)
   is
      use Ada.Text_IO;

      Embedded_Quote : Boolean := False;
      In_String      : Boolean := False;
      I              : Integer := 1;
   begin
      Comma_Count := 0;
      loop
         if In_String then
            if File.Line (I) = '"' then
               if Embedded_Quote then
                  Embedded_Quote := False;
               else
                  if I < File.Last and File.Line (I + 1) = '"' then
                     Embedded_Quote := True;
                  else
                     In_String := False;
                  end if;
               end if;
            end if;
         else
            if File.Line (I) = '"' then
               if Embedded_Quote then
                  Embedded_Quote := False;
               else
                  if I < File.Last and File.Line (I + 1) = '"' then
                     Embedded_Quote := True;
                  else
                     In_String := True;
                  end if;
               end if;
            elsif File.Line (I) = File.Delimiter (1) then
               Comma_Count := Comma_Count + 1;

               if Commas /= null then
                  begin
                     Commas (Comma_Count) := I;
                  exception
                  when Constraint_Error =>
                     --  too many commas in this line
                     raise Constraint_Error with "too many delimiters: '" & File.Line (1 .. File.Last) & "'";
                  end;
               end if;

            end if;
         end if;

         if I = File.Last then
            if In_String then
               --  Quoted field continues on next line; append to current line with
               --  ASCII.LF separator.
               --
               --  File.Line'Last is 1 greater than Max_Row_Size, so there is room for LF
               File.Line (File.Last + 1) := ASCII.LF;

               Get_Line (File.File, File.Line (File.Last + 2 .. File.Line'Last), File.Last);

               if File.Last = File.Line'Last then
                  raise Initialization_Error with
                    "row" & Count'Image (Line (File.File)) &
                    " continued from previous line, is longer than Max_Row_Size (" &
                    Integer'Image (File.Line'Last - 1) & ")";
               end if;
            else
               exit;
            end if;
         end if;

         I := I + 1;
      end loop;
   end Scan_Commas;

   procedure Open
     (File         : in out File_Type;
      Name         : in     String;
      Max_Row_Size : in     Integer;
      Delimiter    : in     Character         := ',';
      Columns      : in     Integer           := 0;
      Skip_Lines   : in     Ada.Text_IO.Count := 0)
   is
      use Ada.Text_IO;
      Comma_Count : Integer := 0;
   begin
      Open (File.File, In_File, Name);

      File.Delimiter (1) := Delimiter;
      File.Line          := new String (1 .. Max_Row_Size + 1); -- +1 so we can check for longer lines.

      if Columns = 0 then
         --  Read first line to determine how many columns there are.
         Get_Line (File.File, File.Line.all, File.Last);

         Scan_Commas (File, null, Comma_Count);

         Reset (File.File);

      else
         Comma_Count := Columns - 1;
      end if;

      File.Commas := new Positive_Array_Integer_Type (1 .. Comma_Count);

      File.Skip_Lines := Skip_Lines;
      if Skip_Lines > 0 then
         Skip_Line (File.File, Skip_Lines);
      end if;

      File.Row := Integer (Skip_Lines); -- so spreadsheet lines match reported rows
      Next_Row (File);
   end Open;

   procedure Reset (File : in out File_Type)
   is
      use all type Ada.Text_IO.Count;
   begin
      Ada.Text_IO.Reset (File.File);
      if File.Skip_Lines > 0 then
         Ada.Text_IO.Skip_Line (File.File, File.Skip_Lines);
      end if;

      File.Row := Integer (File.Skip_Lines);
      Next_Row (File);
   end Reset;

   function Columns (File : in File_Type) return Integer
   is begin
      return File.Commas'Last + 1;
   end Columns;

   function Row (File : in File_Type) return Integer
   is begin
      return File.Row;
   end Row;

   function End_Of_File (File : in File_Type) return Boolean
   is begin
      return Ada.Text_IO.End_Of_File (File.File);
   end End_Of_File;

   procedure Next_Row (File : in out File_Type)
   is
      use Ada.Text_IO;
      Comma_Count : Integer := 0;
   begin
      if End_Of_File (File.File) then
         raise End_Error;
      end if;

      Get_Line (File.File, File.Line.all, File.Last);

      if File.Line'Last = File.Last then
         raise Initialization_Error with
           "row" & Count'Image (Line (File.File)) &
           " longer than Max_Row_Size (" & Integer'Image (File.Line'Last - 1) & ")";
      end if;

      Scan_Commas (File, File.Commas, Comma_Count);

      if Comma_Count /= File.Commas'Last then
         raise Initialization_Error with
           "row" & Integer'Image (File.Row + 1) & " has" &
           Integer'Image (Comma_Count) & " delimiters: '" & File.Line (1 .. File.Last) & "'";
      end if;

      File.Row := File.Row + 1;
   end Next_Row;

   function Read (File : in File_Type; Column : in Positive) return String
   is
      First : Integer;
      Last  : Integer;
   begin
      if Column > File.Commas'Last + 1 then
         raise Ada.Text_IO.Use_Error with "invalid column" & Integer'Image (Column);
      end if;

      if Column = 1 then
         First := File.Line'First;
         Last  := File.Commas (1) - 1;
      elsif Column = File.Commas'Last + 1 then
         First := File.Commas (File.Commas'Last) + 1;
         Last  := File.Last;
      else
         First := File.Commas (Column - 1) + 1;
         Last  := File.Commas (Column) - 1;
      end if;

      return File.Line (First .. Last);
   end Read;

   function Gen_Read_Float (File : in File_Type; Column : in Positive) return Float_Type
   is
      use Ada.Strings.Fixed;
      Value : String renames Read (File, Column);
   begin
      if Value'Length = 0 or
        Value = Value'Length * ' '
      then
         return 0.0;
      else
         return Float_Type'Value (Value);
      end if;
   exception
   when others =>
      raise Constraint_Error with "'" & Value & "' is not valid float syntax";
   end Gen_Read_Float;

   function Gen_Read_Integer (File : in File_Type; Column : in Positive) return Integer_Type
   is
      use Ada.Strings.Fixed;
      Value : String renames Read (File, Column);
   begin
      if Value'Length = 0 or
        Value = Value'Length * ' '
      then
         return 0;
      else
         return Integer_Type'Value (Value);
      end if;
   exception
   when others =>
      raise Constraint_Error with "'" & Value & "' is not valid integer syntax";
   end Gen_Read_Integer;

   function Quote (Item : in String) return String
   is
      Result      : String (1 .. 2 + 2 * Item'Length); -- might be all quotes
      Result_Last : Integer := Result'First;
   begin
      Result (Result_Last) := '"';

      --  Just double all quotes
      for I in Item'Range loop
         if Item (I) = '"' then
            Result_Last              := Result_Last + 2;
            Result (Result_Last - 1) := '"';
            Result (Result_Last)     := '"';
         else
            Result_Last := Result_Last + 1;
            Result (Result_Last) := Item (I);
         end if;
      end loop;

      Result_Last := Result_Last + 1;
      Result (Result_Last) := '"';
      return Result (Result'First .. Result_Last);
   end Quote;

   function Unquote (Item : in String) return String
   is
      Result         : String (Item'First .. Item'Last);
      Result_Last    : Integer := Result'First - 1;
      In_String      : Boolean := False;
      Embedded_Quote : Boolean := False;
   begin
      --  Special cases
      case Item'Length is
      when 0 | 1 =>
         return Item;
      when 2 =>
         if Item = """""" then
            return "";
         end if;
      when 3 =>
         --  A mal-formed Ada string, but do something reasonable
         if Item = """""""" then
            return """""";
         end if;
      when 4 =>
         if Item = """""""""" then
            return """";
         end if;
      when others =>
         null;
      end case;

      for I in Item'Range loop
         if In_String then
            if Item (I) = '"' then
               if Embedded_Quote then
                  Embedded_Quote := False;
               else
                  if I < Item'Last and then Item (I + 1) = '"' then
                     Embedded_Quote := True;
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := '"';
                  else
                     In_String := False;
                  end if;
               end if;
            else
               Result_Last := Result_Last + 1;
               Result (Result_Last) := Item (I);
            end if;
         else
            if Item (I) = '"' then
               if Embedded_Quote then
                  Embedded_Quote := False;
               else
                  if I < Item'Last and then Item (I + 1) = '"' then
                     Embedded_Quote := True;
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := '"';
                  else
                     In_String := True;
                  end if;
               end if;
            else
               Result_Last := Result_Last + 1;
               Result (Result_Last) := Item (I);
            end if;
         end if;
      end loop;

      return Result (Result'First .. Result_Last);
   end Unquote;

   function Is_Open (File : in File_Type) return Boolean
   is begin
      return Ada.Text_IO.Is_Open (File.File);
   end Is_Open;

   function Name (File : in File_Type) return String
   is begin
      return Ada.Text_IO.Name (File.File);
   end Name;

   function Line (File : in File_Type) return Ada.Text_IO.Positive_Count
   is begin
      return Ada.Text_IO.Line (File.File);
   end Line;

end SAL.CSV;
