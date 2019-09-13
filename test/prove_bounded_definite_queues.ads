--  Abstract:
--
--  Instantiation allow Spark proof.
--
--  Copyright (C) 2019 Free Software Foundation, Inc.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.

pragma License (GPL);

pragma Spark_Mode;
with SAL.Gen_Bounded_Definite_Queues;
package Prove_Bounded_definite_Queues is new SAL.Gen_Bounded_Definite_Queues (Integer);
