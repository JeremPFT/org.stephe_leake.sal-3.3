--  Abstract:
--
--  Standard interface to configuration files.
--
--  Design:
--
--  See config_files.texinfo
--
--  All Read and Write subprograms raise Ada.IO_Exceptions.Use_Error
--  if Config is not open.
--
--  All Read and Write subprograms have the following behavior if the
--  requested Key does not exist in Config.Data:
--
--     If Missing_Key is Raise_Exception, Config_File_Error is raised,
--     with an error message containing the file name, line and column
--     numbers.
--
--     If Missing_Key is Ignore, a write creates the key, a read
--     returns the default value.
--
--  Read raises Config_File_Error if the string value in the file for
--  Key is not appropriate for the type.
--
--  Read raises Config_File_Error if the Key has no specified value;
--  for example, reading a root key.
--
--  The visible, non-implementation dependent parts of this package
--  spec are in the public domain. The private, implementation
--  dependent parts are subject to the following copyright and
--  license.

--  Copyright (C) 2002 - 2009, 2011, 2012, 2015 Stephen Leake.  All Rights Reserved.
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

--  start implementation dependent
with Ada.Finalization;
with GNAT.OS_Lib;
--  end implementation dependent

package SAL.Config_Files is
   pragma Elaborate_Body;
   --  Almost any package that does file IO will not be preelaborable.

   type Configuration_Type is limited private;
   --  Limited because it probably contains a file object.

   type Configuration_Access_Type is access all Configuration_Type;
   --  For including a Configuration_Type component in a non-limited
   --  type.

   procedure Free (Item : in out Configuration_Access_Type);

   type Missing_Key_Type is (Raise_Exception, Ignore);
   --  See design comments above.

   type Duplicate_Key_Type is (Raise_Exception, Keep_First, Keep_Last);
   --  See design comments above.

   procedure Open
     (Config                : in out Configuration_Type;
      Name                  : in     String;
      Missing_File          : in     Missing_Key_Type   := Ignore;
      Duplicate_Key         : in     Duplicate_Key_Type := Keep_Last;
      Read_Only             : in     Boolean            := True;
      Case_Insensitive_Keys : in     Boolean            := True);
   --  Free Config.Data. Open the file named Name, relative to the
   --  current directory. Read all data into Config.Data. Save full
   --  path of file found in Config.Writeable_File_Name.
   --
   --  If Read_Only, all Write operations will raise
   --  Ada.Text_IO.Use_Error. In addition, the file is not written.
   --
   --  If Case_Insensitive_Keys, all searches for keys will be case
   --  insensitive (via Ada.Characters.Handling.To_Lower).
   --
   --  If not Read_Only, and Name is an illegal file name on the
   --  current operating system, Ada.IO_Exceptions.Name_Error is
   --  raised.
   --
   --  If Name is not found, and Missing_File is Ignore, Config.Data
   --  is set to null, and Config.Writeable_File_Name is set to
   --  current directory & Name.
   --
   --  If Name is not found, and Missing_File is Raise_Exception,
   --  Ada.IO_Exceptions.Name_Error is raised.
   --
   --  If Name is found, but cannot be opened,
   --  Ada.IO_Exceptions.Use_Error is raised.
   --
   --  If Name is found, but contains bad syntax, Config_File_Error is
   --  raised, with a message giving the file name, line, and column,
   --  in Gnu format.
   --
   --  If any exception is raised, the config object is not open.

   function Open
     (Name                  : in     String;
      Missing_File          : in     Missing_Key_Type   := Ignore;
      Duplicate_Key         : in     Duplicate_Key_Type := Keep_Last;
      Read_Only             : in     Boolean            := True;
      Case_Insensitive_Keys : in     Boolean            := True)
     return Configuration_Access_Type;

   procedure Append
     (Config       : in out Configuration_Type;
      Name         : in     String;
      Missing_File : in     Missing_Key_Type   := Ignore);
   --  Add data from Name file to Config (which must be Open'ed).

   function Is_Open (Config : in Configuration_Type) return Boolean;
   --  True if Open has been called, and Close has not.

   procedure Flush (Config : in Configuration_Type);
   --  Write Config.Data to Config.Writeable_File_Name.

   procedure Close
     (Config     : in out Configuration_Type;
      Unread_Key : in     Missing_Key_Type   := Ignore);
   --  Flush and close the file.
   --
   --  If Unread_Key is Raise_Exception, check that all keys in Config
   --  have been read. If not, raise Config_File_Error, with message
   --  pointing to first unread key.

   procedure Close_No_Save (Config : in out Configuration_Type);
   --  Close Config without writing changes to file.

   function Writeable_File_Name (Config : in Configuration_Type) return String;
   --  Return writeable absolute file path.

   function Base_File_Name (Config : in Configuration_Type) return String;
   --  Return base file name (no directory path).

   function Is_Present (Config : in Configuration_Type; Key : in String) return Boolean;
   --  Return True if Key is in Config, False otherwise. If True,
   --  marks Key as 'read' for Unread_Key in Close.

   procedure Delete
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   --  Delete Key and contained keys or value from Config.Data.

   procedure Read_String
     (Config      : in     Configuration_Type;
      Key         : in     String;
      Result      :    out String;
      Result_Last :    out Natural;
      Default     : in     String             := "";
      Missing_Key : in     Missing_Key_Type   := Ignore);
   procedure Read
     (Config      : in     Configuration_Type;
      Key         : in     String;
      Result      :    out String;
      Result_Last :    out Natural;
      Default     : in     String             := "";
      Missing_Key : in     Missing_Key_Type   := Ignore)
     renames Read_String;
   --  Read the string value associated with Key from Config.Data, store
   --  in Result. Result_Last is set to last character of Result
   --  written.

   function Read
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in String             := "";
      Missing_Key : in Missing_Key_Type   := Ignore)
     return String;
   --  Return string value associated with Key from Config.Data.

   procedure Write_String
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   procedure Write
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore)
     renames Write_String;
   --  Write string value Value to Key in Config.Data.

   generic
      type Enum_Type is (<>);
   function Read_Enum
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Enum_Type          := Enum_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Enum_Type;
   --  Return enumeration value associated with Key from Config.Data.

   generic
      type Enum_Type is (<>);
   procedure Write_Enum
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Enum_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   --  Write enumeration value Value to Key in Config.

   generic
      type Integer_Type is range <>;
   function Read_Integer
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Integer_Type       := Integer_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Integer_Type;
   --  Return integer value associated with Key in Config.Data.

   generic
      type Integer_Type is range <>;
   procedure Write_Integer
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Integer_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   --  Write integer value Value to Key in Config.

   generic
      type Modular_Type is mod <>;
   function Read_Modular
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Modular_Type       := Modular_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Modular_Type;
   --  Return modular integer value associated with Key from
   --  Config.Data.

   generic
      type Modular_Type is mod <>;
   procedure Write_Modular
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Modular_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   --  Write modular integer Value to Key in Config.Data.

   generic
      type Float_Type is digits <>;
   function Read_Float
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Float_Type         := Float_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Float_Type;
   --  Return floating point value associated with Key from
   --  Config.Data.

   generic
      type Float_Type is digits <>;
   procedure Write_Float
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Float_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   --  Write float value Value to Key in Config.Data.

   generic
      type Fixed_Type is delta <>;
   function Read_Fixed
     (Config      : in Configuration_Type;
      Key         : in String;
      Default     : in Fixed_Type         := Fixed_Type'First;
      Missing_Key : in Missing_Key_Type   := Ignore)
     return Fixed_Type;
   --  Return fixed point value associated with Key from
   --  Config.Data.

   generic
      type Fixed_Type is delta <>;
   procedure Write_Fixed
     (Config      : in out Configuration_Type;
      Key         : in     String;
      Value       : in     Fixed_Type;
      Missing_Key : in     Missing_Key_Type   := Ignore);
   --  Write fixed point value Value to Key in Config.Data.

   ----------
   --  Iterators
   --
   --  Example
   --
   --  Assume Foo.Bar.Size and Foo.Bar.Type are keys in the tree.
   --
   --  After I := First (Config, "Foo.Bar"); I points to Foo.Bar.Size.
   --  Current (I); returns "Size"
   --  After Next (I); I points to Foo.Bar.Type

   type Iterator_Type is private;

   Null_Iterator : constant Iterator_Type;

   function First
     (Config      : in Configuration_Type;
      Root_Key    : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Iterator_Type;
   --  Return iterator pointing to first child of Root_Key. If
   --  Root_Key is "", this is the first root key.
   --
   --  if Root_Key is not "", and is not found:
   --  case Missing_Key is
   --  when Raise_Exception => Config_File_Error is raised
   --  when Ignore => result iterator is null (Is_Done is true).

   function First
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Root_Key    : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Iterator_Type;
   --  Return iterator pointing to first child of Iterator.Root_Key.
   --  If Root_Key is "", this is the first child of Iterator.
   --
   --  If Root_Key is not found:
   --  case Missing_Key is
   --  when Raise_Exception => Config_File_Error is raised
   --  when Ignore => result iterator is null (Is_Done is true).

   function Root
     (Config      : in Configuration_Type;
      Root_Key    : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Iterator_Type;
   --  Return iterator pointing to Root_Key. If Root_Key is "", this
   --  is the first root key.
   --
   --  if Root_Key is not "", and is not found:
   --  case Missing_Key is
   --  when Raise_Exception => Config_File_Error is raised
   --  when Ignore => result iterator is null (Is_Done is true).

   function Is_Done (Iterator : in Iterator_Type) return Boolean;
   function Is_Null (Iterator : in Iterator_Type) return Boolean renames Is_Done;

   procedure Next (Iterator : in out Iterator_Type);

   function Count (Config : in Configuration_Type; Key : in String) return Natural;
   --  Return count of keys with root Key

   function Current (Iterator : in Iterator_Type) return String;
   --  Return leaf of child key at Iterator; this may be a leaf key or
   --  the next layer root key. Mark value as read.

   function Is_Present (Config : in Configuration_Type; Iterator : in Iterator_Type; Leaf : in String) return Boolean;
   --  Return True if Iterator.Leaf is in Config, False otherwise. If True,
   --  marks Key as 'read' for Unread_Key in Close.

   function Find
     (Config   : in Configuration_Type;
      Root_Key : in String;
      Key      : in String)
     return Iterator_Type;
   --  Raise Not_Found if Key is not found under Root_Key; otherwise
   --  return iterator pointing to Key.

   procedure Delete
     (Config   : in out Configuration_Type;
      Iterator : in out Iterator_Type);
   --  Delete key at Iterator and contained keys and values from Config.Data.

   function Read_Value
     (Config        : in Configuration_Type;
      Iterator      : in Iterator_Type;
      Default       : in String             := "";
      Missing_Value : in Missing_Key_Type   := Raise_Exception)
     return String;
   --  Return Value for Iterator key.
   --
   --  Named Read_Value instead of Read so it is not ambiguous with
   --  Read (Config, Iterator, Leaf) below.

   function Line_Column (Iterator : in Iterator_Type) return String;
   --  Return "line:column: " where the current value was read, for
   --  error messages.

   function File_Line_Column (Config : in Configuration_Type; Iterator : in Iterator_Type) return String;
   --  Return "file:line:column: " where the current value was read, for
   --  error messages.

   function Read
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in String             := "";
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return String;
   --  Does equivalent of Read (Current (Iterator) & "." & Leaf,
   --  Default, Missing_Key). Exception message has the format
   --  "file:line:column: <key> not found", where line, column are
   --  from Iterator.

   procedure Write
     (Config      : in out Configuration_Type;
      Iterator    : in     Iterator_Type;
      Leaf        : in     String;
      Value       : in     String;
      Missing_Key : in     Missing_Key_Type   := Ignore);

   generic
      type Enum_Type is (<>);
   function Read_Iterator_Enum
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Enum_Type          := Enum_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Enum_Type;

   generic
      type Integer_Type is range <>;
   function Read_Iterator_Integer
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Integer_Type       := Integer_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Integer_Type;

   generic
      type Float_Type is digits <>;
   function Read_Iterator_Float
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Float_Type         := Float_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Float_Type;

   generic
      type Modular_Type is mod <>;
   function Read_Iterator_Modular
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Modular_Type       := Modular_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Modular_Type;

   generic
      type Fixed_Type is delta <>;
   function Read_Iterator_Fixed
     (Config      : in Configuration_Type;
      Iterator    : in Iterator_Type;
      Leaf        : in String;
      Default     : in Fixed_Type         := Fixed_Type'First;
      Missing_Key : in Missing_Key_Type   := Raise_Exception)
     return Fixed_Type;

private
   --  start implementation dependent

   type Node_Type;
   type Node_Access_Type is access all Node_Type;

   type Configuration_Type is new Ada.Finalization.Limited_Controlled with record
      Duplicate_Key         : Duplicate_Key_Type;
      Read_Only             : Boolean;
      Case_Insensitive_Keys : Boolean;
      Writeable_File_Name   : GNAT.OS_Lib.String_Access; --  full path
      Error_File_Name       : GNAT.OS_Lib.String_Access; --  just file name
      Data                  : Node_Access_Type;          -- root of tree.
   end record;

   overriding procedure Finalize (Config : in out Configuration_Type);

   ----------
   --  Iterators

   type Iterator_Type is new Node_Access_Type;

   Null_Iterator : constant Iterator_Type := null;

end SAL.Config_Files;
