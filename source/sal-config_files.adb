--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2002 - 2013, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations;
with GNAT.Strings;
with SAL.File_Names;
package body SAL.Config_Files is

   type Node_Type is record
      Tag          : GNAT.OS_Lib.String_Access;

      Value        : GNAT.OS_Lib.String_Access;
      --  Null if not a leaf node.

      Read         : Boolean;
      --  True if accessed by a Read subprogram.

      Line         : Ada.Text_IO.Count;
      Column       : Ada.Text_IO.Count;
      --  Line and column (of start of value) in file; set for leaf
      --  nodes read from a file only (0 otherwise). Used for error
      --  messages.

      Parent       : Node_Access_Type;
      --  Null if root of tree. Used to build full key name from iterator.

      Child        : Node_Access_Type;
      --  Null if a leaf node

      Next_Sibling : Node_Access_Type;
      --  May be non-null for either leaf or non-leaf.
   end record;
   --  The root node of the tree (Config.Data) has the Tag "Config",
   --  and is always present (created by Open if the file does not
   --  exist). The root node has no siblings, and no value.
   --  Eventually, we may store attributes there, like read-only.
   --
   --  Key nodes have the key name as tag, and the value as value.
   --  Note that a Tag value never has dots.

   ----------
   --  Subprogram specs (alphabetical)

   procedure Add_Child
     (Key_Root         : in Node_Access_Type;
      Key              : in String;
      Value            : in String;
      Line             : in Ada.Text_IO.Count;
      Column           : in Ada.Text_IO.Count;
      Case_Insensitive : in Boolean);

   procedure Add_Node
     (Config           : in out Configuration_Type;
      Key              : in     String;
      Value            : in     String;
      Line             : in     Ada.Text_IO.Count;
      Column           : in     Ada.Text_IO.Count;
      Case_Insensitive : in     Boolean);

   procedure Add_Key
     (Key_Root         : in Node_Access_Type;
      Key              : in String;
      Value            : in String;
      Line             : in Ada.Text_IO.Count;
      Column           : in Ada.Text_IO.Count;
      Case_Insensitive : in Boolean);

   procedure Add_Sibling
     (Node   : in Node_Access_Type;
      Tag    : in String;
      Value  : in String;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count);

   function Add_Sibling
     (Node             : in Node_Access_Type;
      Tag              : in String;
      Line             : in Ada.Text_IO.Count;
      Column           : in Ada.Text_IO.Count;
      Case_Insensitive : in Boolean)
     return Node_Access_Type;
   --  Return node with Tag; add if necessary.

   procedure Check_Open (Config : in Configuration_Type);
   --  Raise Ada.IO_Exceptions.Use_Error if Config is not open.

   procedure Check_Unread_Key
     (Config : in Configuration_Type;
      Node   : in Node_Access_Type);
   --  Raise Config_File_Error if any key in Node subtree has Read = False.

   procedure Error_Line_Column
     (Config : in Configuration_Type;
      Node   : in Node_Access_Type;
      Label  : in String);
   --  Raise Constraint_Error with "file:line:column message" in exception message.
   pragma No_Return (Error_Line_Column);

   function Find_Key
     (Root             : in Node_Access_Type;
      Key              : in String;
      Case_Insensitive : in Boolean)
     return Node_Access_Type;
   --  If Key is not found, return null.

   function Find_Node
     (Config  : in Configuration_Type;
      Key     : in String)
     return Node_Access_Type;

   procedure Flush_Value
     (File : in Ada.Text_IO.File_Type;
      Node : in Node_Access_Type;
      Tag  : in String);
   --  Write Value, Children of Node to File, using Tag as the key name.

   procedure Flush
     (File : in Ada.Text_IO.File_Type;
      Tree : in Node_Access_Type;
      Tag  : in String);
   --  Write all of Tree to File, using Tag as the root key name.

   procedure Free (Tree : in out Node_Access_Type);

   function Format_Line_Column
     (Config : in Configuration_Type;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count)
     return String;

   function Full_Key (Node : in Node_Access_Type; Leaf : in String) return String;
   --  Return full key; prefix is made by tracing Node.Parent to the
   --  tree root, suffix is Leaf.

   function Is_Equal
     (Left, Right      : in String;
      Case_Insensitive : in Boolean)
     return Boolean;
   --  Return Left = Right, but if Config.Case_Insensitive_Keys, use
   --  case insenstive compare.

   procedure Key_No_Value
     (Config : in Configuration_Type;
      Node   : in Node_Access_Type;
      Key    : in String);
   pragma No_Return (Key_No_Value);

   procedure Key_Not_Found
     (Config : in Configuration_Type;
      Key    : in String;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count);
   procedure Key_Not_Found
     (Config   : in Configuration_Type;
      Iterator : in Iterator_Type;
      Leaf     : in String);
   procedure Key_Not_Found
     (Config : in Configuration_Type;
      Key    : in String);
   pragma No_Return (Key_Not_Found);
   --  Raise Config_File_Error, with appropriate message.

   procedure Parse
     (File   : in out Ada.Text_IO.File_Type;
      Config : in out Configuration_Type);

   procedure Parse (Config : in out Configuration_Type; File_Name : in String);

   procedure Set_Value
     (Node   : in Node_Access_Type;
      Value  : in String;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count);

   function Trim (Item : in String) return String;
   --  Trim leading and trailing spaces.

   ----------
   --  Private subprogram bodies; alphabetical order.

   procedure Add_Child
     (Key_Root         : in Node_Access_Type;
      Key              : in String;
      Value            : in String;
      Line             : in Ada.Text_IO.Count;
      Column           : in Ada.Text_IO.Count;
      Case_Insensitive : in Boolean)
   is
      Temp_Root   : Node_Access_Type := Key_Root;
      Tag_Last    : Integer          := Ada.Strings.Fixed.Index (Key, ".") - 1;
      Temp_Column : Ada.Text_IO.Count;
   begin
      if Tag_Last = -1 then
         --  This is a leaf key
         Tag_Last    := Key'Last;
         Temp_Column := Column;
      else
         Temp_Column := Ada.Text_IO.Count (Tag_Last);
      end if;

      if Temp_Root.Child = null then
         Temp_Root.Child := new Node_Type'
           (Tag          => new String'(Key (Key'First .. Tag_Last)),
            Value        => null,
            Read         => False,
            Parent       => Temp_Root,
            Child        => null,
            Next_Sibling => null,
            Line         => Line,
            Column       => Temp_Column);

         Temp_Root := Temp_Root.Child;
      else
         Temp_Root := Add_Sibling
           (Temp_Root.Child,
            Key (Key'First .. Tag_Last),
            Line             => Line,
            Column           => Temp_Column,
            Case_Insensitive => Case_Insensitive);
      end if;

      if Tag_Last = Key'Last then
         Set_Value (Temp_Root, Value, Line, Column);
      else
         --  Add sub key.
         declare
            Sub_Key : constant String := Key (Tag_Last + 2 .. Key'Last);
         begin
            Add_Child (Temp_Root, Sub_Key, Value, Line, Temp_Column, Case_Insensitive);
         end;
      end if;
   end Add_Child;

   procedure Add_Key
     (Key_Root         : in Node_Access_Type;
      Key              : in String;
      Value            : in String;
      Line             : in Ada.Text_IO.Count;
      Column           : in Ada.Text_IO.Count;
      Case_Insensitive : in Boolean)
   is
      Dot : constant Integer := Ada.Strings.Fixed.Index (Key, ".");
   begin
      --  Key does not exist in tree under Key_Root; add it.

      if Dot = 0 then
         Add_Sibling (Key_Root, Key, Value, Line, Column);
      else
         --  Add sub keys.
         declare
            Tag      : constant String  := Key (Key'First .. Dot - 1);
            Sub_Key  : constant String  := Key (Dot + 1 .. Key'Last);
            Tag_Root : Node_Access_Type := Find_Key (Key_Root, Tag, Case_Insensitive);
         begin
            if Tag_Root = null then
               Tag_Root := Add_Sibling (Key_Root, Tag, Line, Column, Case_Insensitive);
            end if;

            Add_Child (Tag_Root, Sub_Key, Value, Line, Column, Case_Insensitive);
         end;
      end if;
   end Add_Key;

   procedure Add_Node
     (Config           : in out Configuration_Type;
      Key              : in     String;
      Value            : in     String;
      Line             : in     Ada.Text_IO.Count;
      Column           : in     Ada.Text_IO.Count;
      Case_Insensitive : in     Boolean)
   is begin
      --  Key does not exist in Config; add it.

      Check_Open (Config);

      if Config.Data.Child = null then
         --  First key in Config.Data tree.
         Add_Child (Config.Data, Key, Value, Line, Column, Case_Insensitive);
      else
         Add_Key (Config.Data.Child, Key, Value, Line, Column, Case_Insensitive);
      end if;
   end Add_Node;

   procedure Add_Sibling
     (Node   : in Node_Access_Type;
      Tag    : in String;
      Value  : in String;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count)
   is
      Temp_Node : Node_Access_Type := Node;
   begin
      loop
         exit when Temp_Node.Next_Sibling = null;
         Temp_Node := Temp_Node.Next_Sibling;
      end loop;

      Temp_Node.Next_Sibling := new Node_Type'
        (Tag          => new String'(Tag),
         Value        => new String'(Value),
         Read         => False,
         Line         => Line,
         Column       => Column,
         Parent       => Node.Parent,
         Child        => null,
         Next_Sibling => null);
   end Add_Sibling;

   function Add_Sibling
     (Node             : in Node_Access_Type;
      Tag              : in String;
      Line             : in Ada.Text_IO.Count;
      Column           : in Ada.Text_IO.Count;
      Case_Insensitive : in Boolean)
     return Node_Access_Type
   is
      Temp_Node : Node_Access_Type := Find_Key (Node, Tag, Case_Insensitive);
   begin
      if Temp_Node /= null then
         return Temp_Node;
      end if;

      Temp_Node := Node;

      loop
         exit when Temp_Node.Next_Sibling = null;
         Temp_Node := Temp_Node.Next_Sibling;
      end loop;

      Temp_Node.Next_Sibling := new Node_Type'
        (Tag          => new String'(Tag),
         Value        => null,
         Read         => False,
         Line         => Line,
         Column       => Column,
         Parent       => Node.Parent,
         Child        => null,
         Next_Sibling => null);

      return Temp_Node.Next_Sibling;
   end Add_Sibling;

   procedure Check_Open (Config : in Configuration_Type)
   is
      use type GNAT.OS_Lib.String_Access;
   begin
      if Config.Data = null then
         raise Ada.IO_Exceptions.Use_Error with
           (if Config.Writeable_File_Name /= null
            then Config.Writeable_File_Name.all & " not open"
            else "unnamed config file not open");
      end if;
   end Check_Open;

   procedure Check_Unread_Key
     (Config : in Configuration_Type;
      Node   : in Node_Access_Type)
   is
      use type GNAT.Strings.String_Access;
      Temp_Node : Node_Access_Type := Node;
   begin
      if Node = null then
         return;
      end if;

      loop
         exit when Temp_Node = null;

         if Temp_Node.Value /= null and not Temp_Node.Read then
            Error_Line_Column (Config, Temp_Node, "unread key");
         end if;

         Check_Unread_Key (Config, Temp_Node.Child);

         Temp_Node := Temp_Node.Next_Sibling;

      end loop;
   end Check_Unread_Key;

   procedure Delete (Config : in out Configuration_Type; Node : in out Node_Access_Type)
   is
      Prev : Node_Access_Type;
      Next : constant Node_Access_Type := Node.Next_Sibling;
   begin
      if Node.Parent = null then
         Prev := Config.Data.Child;
      else
         Prev := Node.Parent.Child;
      end if;

      if Prev = Node then
         if Node.Parent = null then
            Config.Data.Child := Next;
         else
            Node.Parent.Child := Next;
         end if;
      else
         loop
            exit when Prev.Next_Sibling = Node;
            Prev := Prev.Next_Sibling;
         end loop;

         Prev.Next_Sibling := Next;
      end if;

      Node.Next_Sibling := null; --  don't free rest of siblings.

      Free (Node);
   end Delete;

   procedure Error_Line_Column
     (Config : in Configuration_Type;
      Node   : in Node_Access_Type;
      Label  : in String)
   is begin
      raise Config_File_Error with Format_Line_Column (Config, Node.Line, Node.Column) & Label;
   end Error_Line_Column;

   overriding procedure Finalize (Config : in out Configuration_Type)
   is
      use GNAT.OS_Lib;
   begin
      if Config.Writeable_File_Name /= null then
         if not Config.Read_Only then
            begin
               Flush (Config);
            exception
            when E : others =>
               --  Open failed somehow, or disk full, or something.
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Finalizing Config_File " &
                    Config.Writeable_File_Name.all &
                    " raised exception " &
                    Ada.Exceptions.Exception_Name (E) &
                    " : " &
                    Ada.Exceptions.Exception_Message (E));
            end;
         end if;

         Free (Config.Writeable_File_Name);
         Free (Config.Data);
      end if;
   exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Finalizing Config_File " &
           Config.Writeable_File_Name.all &
           " raised exception " &
           Ada.Exceptions.Exception_Name (E) &
           " : " &
           Ada.Exceptions.Exception_Message (E));
   end Finalize;

   function Find_Key
     (Root             : in Node_Access_Type;
      Key              : in String;
      Case_Insensitive : in Boolean)
     return Node_Access_Type
   is
      Dot : constant Integer := Ada.Strings.Fixed.Index (Key, ".");
   begin
      if Root = null then
         return null;
      end if;

      if Dot = 0 then
         if Is_Equal (Root.Tag.all, Key, Case_Insensitive) then
            return Root;
         else
            declare
               Result : Node_Access_Type := Root.Next_Sibling;
            begin
               loop
                  if Result = null then
                     return Result;
                  elsif Is_Equal (Result.Tag.all, Key, Case_Insensitive) then
                     return Result;
                  else
                     Result := Result.Next_Sibling;
                  end if;
               end loop;
            end;
         end if;
      else
         declare
            Tag      : constant String           := Key (Key'First .. Dot - 1);
            Sub_Key  : constant String           := Key (Dot + 1 .. Key'Last);
            Key_Root : constant Node_Access_Type := Find_Key (Root, Tag, Case_Insensitive);
         begin
            if Key_Root = null or else Key_Root.Child = null then
               return null;
            else
               return Find_Key (Key_Root.Child, Sub_Key, Case_Insensitive);
            end if;
         end;
      end if;
   end Find_Key;

   function Find_Node
     (Config  : in Configuration_Type;
      Key     : in String)
     return Node_Access_Type
   is begin
      --  If Key is not found, null is returned.

      Check_Open (Config);
      return Find_Key (Config.Data.Child, Key, Config.Case_Insensitive_Keys);
   end Find_Node;

   procedure Flush_Value
     (File : in Ada.Text_IO.File_Type;
      Node : in Node_Access_Type;
      Tag  : in String)
   is
      use Ada.Text_IO;
   begin
      if Tag = "" then
         Put_Line (File, Node.Tag.all & " = " & Node.Value.all);
      else
         Put_Line (File, Tag & "." & Node.Tag.all & " = " & Node.Value.all);
      end if;
   end Flush_Value;

   procedure Flush
     (File : in Ada.Text_IO.File_Type;
      Tree : in Node_Access_Type;
      Tag  : in String)
   is
      use type GNAT.OS_Lib.String_Access;
   begin
      if Tree.Value /= null then
         Flush_Value (File, Tree, Tag);
      end if;

      if Tree.Child /= null then
         if Tag = "" then
            Flush (File, Tree.Child, Tree.Tag.all);
         else
            Flush (File, Tree.Child, Tag & "." & Tree.Tag.all);
         end if;
      end if;

      if Tree.Next_Sibling /= null then
         Flush (File, Tree.Next_Sibling, Tag);
      end if;
   end Flush;

   function Format_Line_Column
     (Config : in Configuration_Type;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count)
     return String
   is
      Line_Image   : constant String := Trim (Ada.Text_IO.Count'Image (Line));
      Column_Image : constant String := Trim (Ada.Text_IO.Count'Image (Column));
   begin
      return Config.Error_File_Name.all & ":" & Line_Image & ":" & Column_Image & ": ";
   end Format_Line_Column;

   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Node_Type, Node_Access_Type);

   procedure Free (Tree : in out Node_Access_Type)
   is
      use GNAT.OS_Lib;
   begin
      --  Free tree rooted at Tree.

      if Tree = null then
         return;
      end if;

      if Tree.Tag /= null then
         Free (Tree.Tag);
      end if;

      if Tree.Value /= null then
         Free (Tree.Value);
      end if;

      if Tree.Child /= null then
         Free (Tree.Child);
      end if;

      if Tree.Next_Sibling /= null then
         Free (Tree.Next_Sibling);
      end if;

      Free_Node (Tree);
   end Free;

   function Full_Key (Node : in Node_Access_Type; Leaf : in String) return String
   is begin
      if Node = null or else Node.Parent = null then
         return Leaf;
      else
         return Full_Key (Node.Parent, Node.Tag.all & "." & Leaf);
      end if;
   end Full_Key;

   function Is_Equal
     (Left, Right      : in String;
      Case_Insensitive : in Boolean)
     return Boolean
   is
      use Ada.Characters.Handling;
   begin
      if Case_Insensitive then
         if Left'Length /= Right'Length then
            return False;
         else
            for I in Left'Range loop
               if To_Lower (Left (I)) /= To_Lower (Right (Right'First + (I - Left'First))) then
                  return False;
               end if;
            end loop;
            return True;
         end if;
      else
         return Left = Right;
      end if;
   end Is_Equal;

   procedure Key_No_Value
     (Config : in Configuration_Type;
      Node : in Node_Access_Type;
      Key    : in String)
   is begin
      Error_Line_Column (Config, Node, Key & " has no value");
   end Key_No_Value;

   procedure Key_Not_Found
     (Config : in Configuration_Type;
      Key    : in String;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count)
   is begin
      raise Config_File_Error with Format_Line_Column (Config, Line, Column) & Key & " not found";
   end Key_Not_Found;

   procedure Key_Not_Found
     (Config   : in Configuration_Type;
      Iterator : in Iterator_Type;
      Leaf     : in String)
   is begin
      Key_Not_Found (Config, Full_Key (Node_Access_Type (Iterator), Leaf), Iterator.Line, Iterator.Column);
   end Key_Not_Found;

   procedure Key_Not_Found
     (Config : in Configuration_Type;
      Key    : in String)
   is begin
      Ada.Exceptions.Raise_Exception
        (Config_File_Error'Identity, Config.Error_File_Name.all & ":0:0: " & Key & " not found");
   end Key_Not_Found;

   procedure Parse
     (File   : in out Ada.Text_IO.File_Type;
      Config : in out Configuration_Type)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Unbounded.Text_IO;
      Line   : Unbounded_String;
      Equals : Natural;
   begin
      loop
         exit when Ada.Text_IO.End_Of_File (File);

         Line := Get_Line (File);

         --  Handle empty line (or empty file)
         if Length (Line) > 0 then

            --  IMPROVEME: java properties spec says skip whitespace first
            if Element (Line, 1) = '#' or
              Element (Line, 1) = '!'
            then
               null;
            else
               Equals := Index (Line, "=");

               --  Check for '='. not required according to Java Properties spec.
               if Equals = 0 then
                  Equals := Length (Line) + 1;
               end if;

               declare
                  use type Ada.Text_IO.Count;
                  use type GNAT.Strings.String_Access;

                  Key   : constant String := Trim (Slice (Line, 1, Equals - 1));
                  Value : constant String :=
                    Trim (Slice (Line, Integer'Min (Equals + 1, Length (Line) + 1), Length (Line)));

                  --  Check for duplicate keys in file (possible if user
                  --  edited directly).
                  Node : constant Node_Access_Type := Find_Node (Config, Key);
               begin
                  if Node = null then
                     Add_Node
                       (Config,
                        Key              => Key,
                        Value            => Value,
                        Line             => Ada.Text_IO.Line (File) - 1,
                        Column           => Ada.Text_IO.Count (Equals) + 1,
                        Case_Insensitive => Config.Case_Insensitive_Keys);

                  elsif Node.Value = null then
                     --  Adding a value to a root key in Append
                     Set_Value
                       (Node,
                        Value,
                        Line   => Ada.Text_IO.Line (File) - 1,
                        Column => Ada.Text_IO.Count (Equals) + 1);

                  else
                     case Config.Duplicate_Key is
                     when Raise_Exception =>
                        raise Config_File_Error with Format_Line_Column
                          (Config, Ada.Text_IO.Line (File) - 1, 0) & "duplicate key";

                     when Keep_First =>
                        null;

                     when Keep_Last =>
                        Set_Value
                          (Node,
                           Value  => Value,
                           Line   => Ada.Text_IO.Line (File) - 1,
                           Column => Ada.Text_IO.Count (Equals) + 1);
                     end case;
                  end if;
               end;
            end if;
         end if;
      end loop;
   end Parse;

   procedure Parse (Config : in out Configuration_Type; File_Name : in String)
   is
      OS_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (OS_File, Ada.Text_IO.In_File, File_Name);

      begin
         Parse (OS_File, Config);
      exception
      when others =>
         --  already has a good error message
         Ada.Text_IO.Close (OS_File);
         Free (Config.Data);

         raise;
      end;

      Ada.Text_IO.Close (OS_File);
   end Parse;

   procedure Set_Value
     (Node   : in Node_Access_Type;
      Value  : in String;
      Line   : in Ada.Text_IO.Count;
      Column : in Ada.Text_IO.Count)
   is begin
      GNAT.OS_Lib.Free (Node.Value);
      Node.Value  := new String'(Trim (Value));
      Node.Line   := Line;
      Node.Column := Column;
   end Set_Value;

   function Trim (Item : in String) return String
   is begin
      return Ada.Strings.Fixed.Trim (Item, Ada.Strings.Both);
   end Trim;

   ----------
   --  Public subprograms

   procedure Append
     (Config       : in out Configuration_Type;
      Name         : in     String;
      Missing_File : in     Missing_Key_Type   := Ignore)
   is
      use GNAT.OS_Lib;
      Expanded_Name : constant String := SAL.File_Names.Replace_Environment_Variables (Name);

      Found : constant String_Access := Locate_Regular_File (Expanded_Name, GNAT.Directory_Operations.Get_Current_Dir);
   begin
      if Found = null then
         --  File does not exist; either report an error, or do nothing.

         case Missing_File is
         when Ignore =>
            return;

         when Raise_Exception =>
            Ada.Exceptions.Raise_Exception
              (Ada.IO_Exceptions.Name_Error'Identity,
               Name & " not found");
         end case;

      else
         --  File does exist; read it in.

         Parse (Config, Found.all);

      end if;

   end Append;

   function Base_File_Name (Config : in Configuration_Type) return String
   is begin
      return Config.Error_File_Name.all;
   end Base_File_Name;

   procedure Close
     (Config     : in out Configuration_Type;
      Unread_Key : in     Missing_Key_Type   := Ignore)
   is begin
      case Unread_Key is
      when Raise_Exception =>
         begin
            Check_Unread_Key (Config, Config.Data);
         exception
         when Config_File_Error =>
            Finalize (Config);
            raise;
         end;
      when Ignore =>
         null;
      end case;

      Finalize (Config);
   end Close;

   procedure Close_No_Save (Config : in out Configuration_Type)
   is begin
      Config.Read_Only := True;
      Finalize (Config);
   end Close_No_Save;

   procedure Delete
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is
      Node  : Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Raise_Exception =>
            Ada.Exceptions.Raise_Exception
              (Ada.IO_Exceptions.Name_Error'Identity,
               Config.Error_File_Name.all & ":0:0: " & Key & " not found");
         when Ignore =>
            null;
         end case;
      else
         Delete (Config, Node);
      end if;
   end Delete;

   procedure Delete
     (Config   : in out Configuration_Type;
      Iterator : in out Iterator_Type)
   is begin
      Delete (Config, Node_Access_Type (Iterator));
   end Delete;

   function Find
     (Config   : in Configuration_Type;
      Root_Key : in String;
      Key      : in String)
     return Iterator_Type
   is
      Key_Trimmed : constant String := Trim (Key);
      I           : Iterator_Type   := First (Config, Root_Key);
   begin
      loop
         if I = null then
            raise Not_Found;
         end if;

         if Trim (I.Tag.all) = Key_Trimmed then
            return I;
         end if;

         Next (I);
      end loop;
   end Find;

   procedure Flush (Config : in Configuration_Type)
   is
      use GNAT.OS_Lib;
      File  : Ada.Text_IO.File_Type;
      Found : String_Access;
   begin
      Check_Open (Config);

      Found := Locate_Regular_File (Config.Writeable_File_Name.all, "");

      if Found = null then
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, Config.Writeable_File_Name.all);
      else
         Ada.Text_IO.Open
           (File, Ada.Text_IO.Out_File, Config.Writeable_File_Name.all);
      end if;

      if Config.Data.Child /= null then
         Flush (File, Config.Data.Child, "");
      end if;

      Ada.Text_IO.Close (File);
   end Flush;

   procedure Free (Item : in out Configuration_Access_Type)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (Configuration_Type, Configuration_Access_Type);
   begin
      Deallocate (Item);
   end Free;

   function Is_Open (Config : in Configuration_Type) return Boolean
   is begin
      return Config.Data /= null;
   end Is_Open;

   function Is_Present (Config : in Configuration_Type; Key : in String) return Boolean
   is
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node /= null then
         Node.Read := True;
         return True;
      else
         return False;
      end if;
   end Is_Present;

   procedure Open
     (Config                : in out Configuration_Type;
      Name                  : in     String;
      Missing_File          : in     Missing_Key_Type   := Ignore;
      Duplicate_Key         : in     Duplicate_Key_Type := Keep_Last;
      Read_Only             : in     Boolean            := True;
      Case_Insensitive_Keys : in     Boolean            := True)
   is
      use GNAT.OS_Lib;
      Expanded_Name : constant String := SAL.File_Names.Replace_Environment_Variables (Name);

      Found : constant String_Access := Locate_Regular_File (Expanded_Name, GNAT.Directory_Operations.Get_Current_Dir);
   begin
      Config.Duplicate_Key         := Duplicate_Key;
      Config.Read_Only             := Read_Only;
      Config.Case_Insensitive_Keys := Case_Insensitive_Keys;

      if Found = null then
         --  File does not exist; either report an error, or create an
         --  empty config object and file.

         case Missing_File is
         when Ignore =>

            Free (Config.Data);

            Config.Writeable_File_Name := new String'(GNAT.OS_Lib.Normalize_Pathname (Expanded_Name));

            if not Read_Only then
               --  Create an empty file, to ensure it is a valid file
               --  name in a writeable directory.
               declare
                  use Ada.Text_IO;
                  File : File_Type;
               begin
                  Create (File, Ada.Text_IO.Out_File, Config.Writeable_File_Name.all);
                  Close (File);
               exception
               when others =>
                  declare
                     Temp : constant String := Config.Writeable_File_Name.all;
                  begin
                     Config.Writeable_File_Name := null;
                     Ada.Exceptions.Raise_Exception
                       (Ada.IO_Exceptions.Name_Error'Identity,
                        Temp & " not writeable");
                  end;
               end;
            end if;

            Config.Error_File_Name :=
              new String'(GNAT.Directory_Operations.Base_Name (Config.Writeable_File_Name.all));

            Config.Data := new Node_Type'
              (Tag          => new String'("Config"),
               Value        => null,
               Read         => False,
               Line         => 0,
               Column       => 0,
               Parent       => null,
               Child        => null,
               Next_Sibling => null);

         when Raise_Exception =>
            Ada.Exceptions.Raise_Exception
              (Ada.IO_Exceptions.Name_Error'Identity,
               Name & " not found");
         end case;

      else
         --  File does exist; read it in.

         Free (Config.Data);

         Config.Data := new Node_Type'
           (Tag          => new String'("Config"),
            Value        => null,
            Read         => False,
            Line         => 0,
            Column       => 0,
            Parent       => null,
            Child        => null,
            Next_Sibling => null);

         Free (Config.Writeable_File_Name);
         Config.Writeable_File_Name := Found;
         Config.Error_File_Name     := new String'(GNAT.Directory_Operations.Base_Name (Found.all));

         Parse (Config, Found.all);

      end if;

   end Open;

   function Open
     (Name                  : in     String;
      Missing_File          : in     Missing_Key_Type   := Ignore;
      Duplicate_Key         : in     Duplicate_Key_Type := Keep_Last;
      Read_Only             : in     Boolean            := True;
      Case_Insensitive_Keys : in     Boolean            := True)
     return Configuration_Access_Type
   is
      Result : constant Configuration_Access_Type := new Configuration_Type;
   begin
      Open (Result.all, Name, Missing_File, Duplicate_Key, Read_Only, Case_Insensitive_Keys);
      return Result;
   end Open;

   function Read
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in String             := "";
      Missing_Key : in Missing_Key_Type   := Ignore)
     return String
   is
      use type GNAT.OS_Lib.String_Access;
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Key);
      else
         Node.Read := True;
         return Node.Value.all;
      end if;
   end Read;

   function Read_Enum
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Enum_Type          := Enum_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Enum_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Key);
      else
         Node.Read := True;
         return Enum_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      declare
         use Ada.Strings.Unbounded;
         Message : Unbounded_String := To_Unbounded_String ("invalid enumeral - expecting: ");
      begin
         for I in Enum_Type'Range loop
            Message := Message & Enum_Type'Image (I) & " ";
         end loop;
         Error_Line_Column (Config, Node, To_String (Message));
      end;
   end Read_Enum;

   function Read_Fixed
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Fixed_Type         := Fixed_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Fixed_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Key);
      else
         Node.Read := True;
         return Fixed_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid fixed point syntax or range");
   end Read_Fixed;

   function Read_Float
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Float_Type         := Float_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Float_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Key);
      else
         Node.Read := True;
         return Float_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid floating point syntax or range");
   end Read_Float;

   function Read_Integer
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Integer_Type       := Integer_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Integer_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Key);
      else
         Node.Read := True;
         return Integer_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid integer syntax or range");
   end Read_Integer;

   function Read_Modular
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Modular_Type       := Modular_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Modular_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Key);
      else
         Node.Read := True;
         return Modular_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid modular integer syntax or range, valid range is "
                           & Modular_Type'Image (Modular_Type'First) & " to "
                           & Modular_Type'Image (Modular_Type'Last));
   end Read_Modular;

   procedure Read_String
     (Config      : in     Configuration_Type;
      Key         : in     String;
      Result      :    out String;
      Result_Last :    out Natural;
      Default     : in     String             := "";
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is
      Temp : constant String := Read (Config, Key, Default, Missing_Key);
   begin
      if Temp'Length > Result'Length then
         raise Constraint_Error;
      else
         Result_Last := Result'First + Temp'Length - 1;
         Result (Result'First .. Result_Last) := Temp;
      end if;
   end Read_String;

   procedure Write_Enum
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Enum_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is begin
      Write_String (Config, Key, Enum_Type'Image (Value), Missing_Key);
   end Write_Enum;

   procedure Write_Fixed
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Fixed_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is begin
      Write_String (Config, Key, Fixed_Type'Image (Value), Missing_Key);
   end Write_Fixed;

   procedure Write_Float
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Float_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is begin
      Write_String (Config, Key, Float_Type'Image (Value), Missing_Key);
   end Write_Float;

   procedure Write_Integer
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Integer_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is begin
      Write_String (Config, Key, Integer_Type'Image (Value), Missing_Key);
   end Write_Integer;

   procedure Write_Modular
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Modular_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is begin
      Write_String (Config, Key, Modular_Type'Image (Value), Missing_Key);
   end Write_Modular;

   procedure Write_String
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is
      Node : constant Node_Access_Type := Find_Node (Config, Key);
   begin
      if Config.Read_Only then
         raise Ada.IO_Exceptions.Use_Error with Config.Error_File_Name.all & " opened Read_Only";
      end if;

      if Node = null then
         case Missing_Key is
         when Ignore =>

            Add_Node
              (Config,
               Key,
               Value,
               Line             => 0,
               Column           => 0,
               Case_Insensitive => Config.Case_Insensitive_Keys);

         when Raise_Exception =>
            Key_Not_Found (Config, Key);
         end case;
      else
         Set_Value (Node, Value, Line => 0, Column => 0);
      end if;
   end Write_String;

   function Writeable_File_Name (Config : in Configuration_Type) return String
   is begin
      return Config.Writeable_File_Name.all;
   end Writeable_File_Name;

   ----------
   --  Iterators

   function First
     (Config      : in Configuration_Type;
      Root_Key    : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Iterator_Type
   is
      Temp : Node_Access_Type;
   begin
      if Root_Key = "" then
         return Iterator_Type (Config.Data.Child);
      else
         Temp := Find_Key (Config.Data.Child, Root_Key, Config.Case_Insensitive_Keys);
         if Temp = null then
            case Missing_Key is
            when Raise_Exception =>
               Key_Not_Found (Config, Root_Key);
            when Ignore =>
               return null;
            end case;
         else
            loop
               if Temp.Child /= null then
                  return Iterator_Type (Temp.Child);
               elsif Temp.Next_Sibling /= null then
                  Temp := Temp.Next_Sibling;
               else
                  return null;
               end if;
            end loop;
         end if;
      end if;
   end First;

   function First
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Root_Key    : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Iterator_Type
   is
      Temp : Node_Access_Type;
   begin
      if Root_Key = "" then
         return Iterator_Type (Iterator.Child);
      end if;

      Temp := Find_Key (Iterator.Child, Root_Key, Config.Case_Insensitive_Keys);

      if Temp = null then
         case Missing_Key is
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Root_Key);
         when Ignore =>
            return null;
         end case;
      else
         loop
            if Temp.Child /= null then
               return Iterator_Type (Temp.Child);
            elsif Temp.Next_Sibling /= null then
               Temp := Temp.Next_Sibling;
            else
               return null;
            end if;
         end loop;
      end if;
   end First;

   function Root
     (Config      : in Configuration_Type;
      Root_Key    : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Iterator_Type
   is
      Temp : Node_Access_Type;
   begin
      if Root_Key = "" then
         return Iterator_Type (Config.Data.Child);
      else
         Temp := Find_Key (Config.Data.Child, Root_Key, Config.Case_Insensitive_Keys);
         if Temp = null then
            case Missing_Key is
            when Raise_Exception =>
               Key_Not_Found (Config, Root_Key);
            when Ignore =>
               return null;
            end case;
         else
            return Iterator_Type (Temp);
         end if;
      end if;
   end Root;

   function Is_Done (Iterator : in Iterator_Type) return Boolean
   is begin
      return Iterator = null;
   end Is_Done;

   function Count (Config : in Configuration_Type; Key : in String) return Natural
   is
      Result : Integer       := 0;
      I      : Iterator_Type := First (Config, Key, Missing_Key => Ignore);
   begin
      loop
         exit when Is_Done (I);
         Result := Result + 1;
         Next (I);
      end loop;

      return Result;
   end Count;

   procedure Next (Iterator : in out Iterator_Type)
   is begin
      Iterator := Iterator_Type (Iterator.Next_Sibling);
   end Next;

   function Current (Iterator : in Iterator_Type) return String
   is begin
      Iterator.Read := True;
      return Trim (Iterator.Tag.all);
   end Current;

   function Is_Present (Config : in Configuration_Type; Iterator : in Iterator_Type; Leaf : in String) return Boolean
   is
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node /= null then
         Node.Read := True;
         return True;
      else
         return False;
      end if;
   end Is_Present;

   function File_Line_Column (Config : in Configuration_Type; Iterator : in Iterator_Type) return String
   is begin
      if Iterator = null then
         return Format_Line_Column (Config, 0, 0);
      else
         return Format_Line_Column (Config, Iterator.Line, Iterator.Column);
      end if;
   end File_Line_Column;

   function Line_Column (Iterator : in Iterator_Type) return String
   is
      Line_Image   : constant String := Trim (Ada.Text_IO.Count'Image (Iterator.Line));
      Column_Image : constant String := Trim (Ada.Text_IO.Count'Image (Iterator.Column));
   begin
      return Line_Image & ":" & Column_Image & ": ";
   end Line_Column;

   function Read_Value
     (Config        : in Configuration_Type;
      Iterator      : in Iterator_Type;
      Default       : in String             := "";
      Missing_Value : in Missing_Key_Type   := Raise_Exception)
     return String
   is
      use type GNAT.OS_Lib.String_Access;
   begin
      Iterator.Read := True;
      if Iterator.Value = null then
         case Missing_Value is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            raise Config_File_Error with Format_Line_Column (Config, Iterator.Line, Iterator.Column) &
              " no value given for " & Iterator.Tag.all;
         end case;
      else
         return Iterator.Value.all;
      end if;
   end Read_Value;

   function Read
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return String
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Leaf);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Leaf);
      else
         Node.Read := True;
         return Trim (Node.Value.all);
      end if;
   end Read;

   procedure Write
     (Config      : in out Configuration_Type;
      Iterator    : in     Iterator_Type;
      Leaf        : in     String;
      Value       : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore)
   is
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Config.Read_Only then
         raise Ada.IO_Exceptions.Use_Error with Config.Error_File_Name.all & " opened Read_Only";
      end if;

      if Node = null then
         case Missing_Key is
         when Ignore =>

            Add_Node
              (Config,
               Full_Key (Node_Access_Type (Iterator), Leaf),
               Value,
               Line             => 0,
               Column           => 0,
               Case_Insensitive => Config.Case_Insensitive_Keys);

         when Raise_Exception =>
            Key_Not_Found (Config, Full_Key (Node_Access_Type (Iterator), Leaf));
         end case;
      else
         Set_Value (Node, Value, Line => 0, Column => 0);
      end if;

   end Write;

   function Read_Iterator_Enum
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Enum_Type          := Enum_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Enum_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Leaf);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Leaf);
      else
         Node.Read := True;
         return Enum_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      declare
         use Ada.Strings.Unbounded;
         Message : Unbounded_String := To_Unbounded_String ("invalid enumeral - expecting: ");
      begin
         for I in Enum_Type'Range loop
            Message := Message & Enum_Type'Image (I) & " ";
         end loop;
         Error_Line_Column (Config, Node, To_String (Message));
      end;
   end Read_Iterator_Enum;

   function Read_Iterator_Float
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Float_Type         := Float_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Float_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Leaf);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Leaf);
      else
         Node.Read := True;
         return Float_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid floating point syntax or range");
   end Read_Iterator_Float;

   function Read_Iterator_Integer
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Integer_Type       := Integer_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Integer_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Leaf);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Leaf);
      else
         Node.Read := True;
         return Integer_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid integer syntax or range");
   end Read_Iterator_Integer;

   function Read_Iterator_Modular
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Modular_Type       := Modular_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Modular_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Leaf);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Leaf);
      else
         Node.Read := True;
         return Modular_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid modular syntax or range");
   end Read_Iterator_Modular;

   function Read_Iterator_Fixed
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Fixed_Type         := Fixed_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Fixed_Type
   is
      use type GNAT.Strings.String_Access;
      Node : constant Node_Access_Type := Find_Key (Iterator.Child, Leaf, Config.Case_Insensitive_Keys);
   begin
      if Node = null then
         case Missing_Key is
         when Ignore =>
            return Default;
         when Raise_Exception =>
            Key_Not_Found (Config, Iterator, Leaf);
         end case;
      elsif Node.Value = null then
         Key_No_Value (Config, Node, Leaf);
      else
         Node.Read := True;
         return Fixed_Type'Value (Node.Value.all);
      end if;
   exception
   when Constraint_Error =>
      Error_Line_Column (Config, Node, "invalid fixed point syntax or range");
   end Read_Iterator_Fixed;

end SAL.Config_Files;
