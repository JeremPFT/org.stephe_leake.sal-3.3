--  Abstract:
--
--  Generic Base 2 image, with '_' every four bits
--
generic
   Nibbles : Natural;
   type Number_Type is mod <>;
function SAL.Generic_Binary_Image (Item : in Number_Type) return String;
--  Return a base 2 image of Item, padded with leading zeros to
--  Nibbles * 4 bits, and inserting '_' between nibbles. If Nibbles *
--  4 is too small, leading bits are silently truncated.
pragma Pure (SAL.Generic_Binary_Image);
