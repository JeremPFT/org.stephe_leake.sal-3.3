--  Abstract :
--
--  See spec.
--
--  References:
--
--  see sal-gen_graphs.ads
--
--  Copyright (C) 2017 - 2019 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);
with AUnit.Checks.Containers;
with Ada.Text_IO;
with SAL.Gen_Graphs.Gen_AUnit;
with SAL.Gen_Trimmed_Image;
package body Test_Graphs is

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Integer);

   ----------
   --  Test procedures

   procedure Test_Find_Path (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Tst);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      --  Test uses the graph in [1] figure 22.3. That does not have
      --  labels or weights on the edges; we add an integer to each to
      --  allow testing edge_data. We also treat it as a directed graph.

      R : constant Integer := 1;
      S : constant Integer := 2;
      T : constant Integer := 3;
      U : constant Integer := 4;
      V : constant Integer := 5;
      W : constant Integer := 6;
      X : constant Integer := 7;
      Y : constant Integer := 8;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => 0,
         Path_Index        => Positive,
         Edge_Image        => Integer'Image);
      use Graphs;

      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);
      use Graphs_AUnit;

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Arrays.Vector;
   begin
      --  Fill graph.

      Graph.Add_Edge (V, R, 1);
      Graph.Add_Edge (R, S, 2);
      Graph.Add_Edge (S, W, 3);
      Graph.Add_Edge (W, T, 4);
      Graph.Add_Edge (W, X, 5);
      Graph.Add_Edge (T, U, 6);
      Graph.Add_Edge (T, X, 7);
      Graph.Add_Edge (U, Y, 8);
      Graph.Add_Edge (X, U, 9);
      Graph.Add_Edge (X, Y, 10);

      Check ("Multigraph", Graph.Multigraph, False);

      Computed := Graph.Find_Paths (V, 2);
      Check ("v - 2.length", Computed.Length, 1);
      Check ("v - 2.first", Computed (Computed.First), ((V, +(1, 1)), (R, +(2, 2))));

      Computed := Graph.Find_Paths (V, 10);
      Check ("v - 10.length", Computed.Length, 1);
      Check
        ("v - 10.first",
         Computed (Computed.First),
         ((V, +(1, 1)), (R, +(2, 2)), (S, +(3, 3)), (W, +(5, 5)), (X, +(10, 10))));

   end Test_Find_Path;

   procedure Test_Find_Cycles (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;

      Test : Test_Case renames Test_Case (Tst);

      --  Test uses the graph in [2] figure 1.

      type Base_Vertex_Index is range 0 .. 5;
      subtype Vertex_Index is Base_Vertex_Index range 1 .. 5;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer, -- "edge number"
         Default_Edge_Data => 0,
         Vertex_Index      => Vertex_Index,
         Invalid_Vertex    => 6,
         Path_Index        => Positive,
         Edge_Image        => Integer'Image);
      use Graphs;

      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);
      use Graphs_AUnit;

      Graph            : Graphs.Graph;
      Computed         : Graphs.Path_Arrays.Vector;
      Expected_Tiernan : Graphs.Path_Arrays.Vector;
      Expected_Johnson : Graphs.Path_Arrays.Vector;

      Path_1 : constant Path := ((1, +(7, 7)), (2, +(1, 1)), (3, +(3, 3)), (5, +(5, 5)));
      Path_2 : constant Path := ((1, +(7, 7)), (2, +(1, 1)), (4, +(4, 4)), (3, +(6, 6)), (5, +(5, 5)));
      Path_3 : constant Path := (1 => (2, +(2, 2)));
   begin
      --  Fill graph.

      Graph.Add_Edge (1, 2, 1);
      Graph.Add_Edge (2, 2, 2);
      Graph.Add_Edge (2, 3, 3);
      Graph.Add_Edge (2, 4, 4);
      Graph.Add_Edge (3, 5, 5);
      Graph.Add_Edge (4, 3, 6);
      Graph.Add_Edge (5, 1, 7);

      Check ("Multigraph", Graph.Multigraph, False);

      --  Set expected as in [2] fig 2 page 723
      Expected_Tiernan.Append (Path_1);
      Expected_Tiernan.Append (Path_2);
      Expected_Tiernan.Append (Path_3);

      Computed := Graph.Find_Cycles_Tiernan;
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Tiernan:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Cycle));
         end loop;
      end if;

      Check ("Tiernan", Computed, Expected_Tiernan);

      Expected_Johnson.Append (Path_3);
      Expected_Johnson.Append (Path_1);
      Expected_Johnson.Append (Path_2);

      Computed := Graph.Find_Cycles;
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Johnson:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Cycle));
         end loop;
      end if;

      Check ("Johnson", Computed, Expected_Johnson);
   end Test_Find_Cycles;

   procedure Test_Conflict_Name (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (Tst);
      use AUnit.Checks;

      --  Graph is from WisiToken conflict_name.wy grammar.
      --
      --  Nodes are nonterminals, edges are occurence of nonterminal in a
      --  production.
      --
      --  Nonterminals:
      --   6 => wisitoken_accept
      --   7 => aggregate
      --   8 => attribute_reference
      --   9 => attribute_designator
      --  10 => name
      --  11 => qualified_expression

      --  Productions:
      --   6.0 wisitoken_accept <= name Wisi_EOI
      --
      --   7.0 aggregate <= LEFT_PAREN name RIGHT_PAREN
      --
      --   8.0 attribute_reference <= name TICK attribute_designator
      --
      --   9.0 attribute_designator <= name
      --
      --        name
      --  10.0  <= IDENTIFIER
      --  10.1  | attribute_reference
      --  10.2  | qualified_expression
      --
      --  11.0 qualified_expression <= name TICK aggregate


      --  In WisiToken, we might want Edge_Data to be:
      --  type Edge is record
      --     LHS       : Positive  := 1;
      --     RHS       : Natural   := 10;
      --     Token     : Positive  := 10;
      --     Recursive : Recursion := None;
      --  end record;
      --
      --  But using that here just makes the test harder to read

      type Unknown_Recursion_Index is range 0 .. Integer'Last;
      subtype Recursion_Index is Unknown_Recursion_Index range 1 .. Unknown_Recursion_Index'Last;
      Invalid_Recursion_Index : constant Unknown_Recursion_Index := 0;
      pragma Unreferenced (Invalid_Recursion_Index);

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => Integer'Last,
         Path_Index        => Recursion_Index,
         Edge_Image        => Integer'Image);
      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);

      Graph            : Graphs.Graph;
      Computed         : Graphs.Path_Arrays.Vector;
      Expected_Tiernan : Graphs.Path_Arrays.Vector;
      Expected_Johnson : Graphs.Path_Arrays.Vector;

      use Graphs;
      use Graphs_AUnit;
      Path_1 : constant Path := ((7, +(9, 9)), (10, +(2, 2)), (11, +(7, 7)));
      Path_2 : constant Path := ((8, +(6, 6)), (10, +(3, 3)));
      Path_3 : constant Path := ((8, +(6, 6)), (9, +(4, 4)), (10, +(5, 5)));
      Path_4 : constant Path := ((10, +(8, 8)), (11, +(7, 7)));
   begin
      Graph.Add_Edge  (6, 10, 1);
      Graph.Add_Edge  (7, 10, 2);
      Graph.Add_Edge  (8, 10, 3);
      Graph.Add_Edge  (8,  9, 4);
      Graph.Add_Edge  (9, 10, 5);
      Graph.Add_Edge (10,  8, 6);
      Graph.Add_Edge (10, 11, 7);
      Graph.Add_Edge (11, 10, 8);
      Graph.Add_Edge (11,  7, 9);

      Check ("Multigraph", Graph.Multigraph, False);

      --  Cycles are found in start nonterminal order, arbitrary within
      --  start nonterminal; cycles start with lowest nonterm.
      Expected_Tiernan.Append (Path_1);
      Expected_Tiernan.Append (Path_2);
      Expected_Tiernan.Append (Path_3);
      Expected_Tiernan.Append (Path_4);

      Computed := Graph.Find_Cycles_Tiernan;
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Tiernan:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Cycle));
         end loop;
      end if;

      Check ("Tiernan", Computed, Expected_Tiernan);

      Expected_Johnson.Append (Path_1);
      Expected_Johnson.Append (Path_2);
      Expected_Johnson.Append (Path_3);
      Expected_Johnson.Append (Path_4);

      Computed := Graph.Find_Cycles;
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("strong components:");
         declare
            Components : constant Graphs.Component_Lists.List := Graphs.Strongly_Connected_Components
              (To_Adjancency (Graph));
         begin
            for Comp of Components loop
               Ada.Text_IO.Put_Line (Graphs.Image (Comp));
            end loop;
         end;
         Ada.Text_IO.Put_Line ("Johnson cycles:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Cycle));
         end loop;
      end if;

      Check ("Johnson", Computed, Expected_Johnson);
   end Test_Conflict_Name;

   procedure Test_Ada_Lite_Name (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (Tst);
      use AUnit.Checks;

      --  Graph is from a small subset of the WisiToken ada_lite.wy name
      --  grammar; it did not find the loop 11.0 -> 11.0.
      --
      --  There are multiple edges between the same two nodes; this requires
      --  the algorithm to use edge ids.
      --
      --  Nonterminals:
      --  8.0: wisitoken_accept <= name Wisi_EOI
      --  9.0: actual_parameter_part <= LEFT_PAREN association_opt RIGHT_PAREN
      --  10.0: association_opt <= IDENTIFIER EQUAL_GREATER primary
      --  10.1: association_opt <= primary
      --  10.2: association_opt <=
      --  11.0: name <= name LEFT_PAREN range_list RIGHT_PAREN
      --  11.1: name <= name actual_parameter_part
      --  11.2: name <= IDENTIFIER
      --  12.0: paren_primary <= LEFT_PAREN primary RIGHT_PAREN
      --  13.0: primary <= NUMERIC_LITERAL
      --  13.1: primary <= name
      --  13.2: primary <= paren_primary
      --  14.0: range_g <= primary DOT_DOT primary
      --  15.0: range_list <= range_list COMMA range_g
      --  15.1: range_list <= range_g

      type Unknown_Recursion_Index is range 0 .. Integer'Last;
      subtype Recursion_Index is Unknown_Recursion_Index range 1 .. Unknown_Recursion_Index'Last;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => Integer'Last,
         Path_Index        => Recursion_Index,
         Edge_Image        => Integer'Image);
      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);

      Graph            : Graphs.Graph;
      Computed         : Graphs.Path_Arrays.Vector;
      Expected_Tiernan : Graphs.Path_Arrays.Vector;
      Expected_Johnson : Graphs.Path_Arrays.Vector;

      use Graphs;
      use Graphs_AUnit;
      Path_1 : constant Path := ((9, +(8, 8)), (10, +(2, 2)), (13, +(3, 3) & (4, 4)), (11, +(10, 10)));
      Path_2 : constant Path := ((11, +(10, 10)), (15, +(6, 6)), (14, +(15, 15) & (16, 16)),
                                 (13, +(12, 12) & (13, 13)));
      Path_3 : constant Path := (1 => (11, +(5, 5) & (7, 7)));
      Path_4 : constant Path := ((12, +(11, 11)), (13, +(9, 9)));
      Path_5 : constant Path := (1 => (15, +(14, 14)));
   begin
      Graph.Add_Edge  (8, 11,  1);
      Graph.Add_Edge  (9, 10,  2);
      Graph.Add_Edge (10, 13,  3); -- 10.0
      Graph.Add_Edge (10, 13,  4); -- 10.1
      Graph.Add_Edge (11, 11,  5); -- 11.0 name
      Graph.Add_Edge (11, 15,  6); -- 11.0 range_list
      Graph.Add_Edge (11, 11,  7); -- 11.1 name
      Graph.Add_Edge (11,  9,  8); -- 11.1 actual_parameter_part
      Graph.Add_Edge (12, 13,  9);
      Graph.Add_Edge (13, 11, 10);
      Graph.Add_Edge (13, 12, 11);
      Graph.Add_Edge (14, 13, 12); -- 14.0 left primary
      Graph.Add_Edge (14, 13, 13); -- 14.0 right primary
      Graph.Add_Edge (15, 15, 14); -- 15.0 range_list
      Graph.Add_Edge (15, 14, 15); -- 15.0 range_g
      Graph.Add_Edge (15, 14, 16); -- 15.1 range_g

      Check ("Multigraph", Graph.Multigraph, True);

      --  Cycles are found in start nonterminal order, longest first within
      --  start nonterminal; cycles start with lowest nonterm.
      Expected_Tiernan.Append (Path_1);
      Expected_Tiernan.Append (Path_2);
      Expected_Tiernan.Append (Path_3);
      Expected_Tiernan.Append (Path_4);
      Expected_Tiernan.Append (Path_5);

      Computed := Graph.Find_Cycles_Tiernan;

      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Tiernan:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Cycle));
         end loop;
      end if;

      Check ("1", Computed, Expected_Tiernan);

      Expected_Johnson.Append (Path_3);
      Expected_Johnson.Append (Path_5);
      Expected_Johnson.Append (Path_1);
      Expected_Johnson.Append (Path_2);
      Expected_Johnson.Append (Path_4);

      Computed := Graph.Find_Cycles;
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Johnson cycles:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Cycle));
         end loop;
      end if;

      Check ("Johnson", Computed, Expected_Johnson);
   end Test_Ada_Lite_Name;

   procedure Test_Strong_Components (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (Tst);

      --  Test uses the graph in [4] figure 3

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => Integer'Last,
         Path_Index        => Natural,
         Edge_Image        => Integer'Image);
      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);
      use Graphs_AUnit;

      Graph    : Graphs.Adjacency_Structures.Vector;
      Computed : Graphs.Component_Lists.List;
      Expected : Graphs.Component_Lists.List;
   begin
      Graph.Set_First_Last (1, 8);
      Graph (1) := +2;
      Graph (2) := +3 & 8;
      Graph (3) := +4 & 7;
      Graph (4) := +5;
      Graph (5) := +3 & 6;
      --  6 none
      Graph (7) := +4 & 6;
      Graph (8) := +1 & 7;

      Expected.Append (+6);
      Expected.Append (+7 & 5 & 4 & 3);
      Expected.Append (+8 & 2 & 1);

      Computed := Graphs.Strongly_Connected_Components (Graph);
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Computed 1");
         for Comp of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Comp));
         end loop;
      end if;
      Check ("1", Computed, Expected);

   end Test_Strong_Components;

   procedure Test_Strong_Components_Ada_Lite (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (Tst);

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => Integer'Last,
         Path_Index        => Natural,
         Edge_Image        => Integer'Image);
      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);
      use Graphs_AUnit;

      Graph    : Graphs.Adjacency_Structures.Vector;
      Computed : Graphs.Component_Lists.List;
      Expected : Graphs.Component_Lists.List;
   begin
      Graph.Set_First_Last (56, 139);
      Graph (56) := +71;
      Graph (57) := +61;
      Graph (58) := +61;
      Graph (59) := +103 & 88;
      Graph (60) := +88 & 87;
      Graph (61) := +61 & 60;
      Graph (64) := +63 & 76 & 96 & 97;
      Graph (65) := +115 & 66;
      Graph (66) := +131;
      Graph (67) := +88 & 69;
      Graph (68) := +125;
      Graph (69) := +69 & 68;
      Graph (70) := +106 & 132 & 130 & 95 & 129;
      Graph (71) := +70;
      Graph (72) := +98 & 67 & 101 & 64 & 90;
      Graph (73) := +116 & 99;
      Graph (74) := +65 & 105 & 132 & 137;
      Graph (75) := +74;
      Graph (76) := +75;
      Graph (77) := +134 & 116;
      Graph (78) := +88 & 125;
      Graph (79) := +79 & 78;
      Graph (81) := +80;
      Graph (82) := +103;
      Graph (83) := +82 & 125;
      Graph (84) := +84 & 83;
      Graph (85) := +84;
      Graph (86) := +97 & 88 & 97;
      Graph (87) := +121 & 118 & 119 & 120;
      Graph (88) := +87;
      Graph (89) := +134;
      Graph (90) := +89;
      Graph (91) := +112;
      Graph (92) := +110;
      Graph (93) := +138;
      Graph (94) := +103;
      Graph (94) := +107;
      Graph (95) := +103;
      Graph (96) := +125 & 85 & 125;
      Graph (98) := +88 & 125 & 79;
      Graph (99) := +77;
      Graph (100) := +77;
      Graph (101) := +100 & 125 & 97;
      Graph (103) := +117 & 57 & 123;
      Graph (104) := +103;
      Graph (105) := +134 & 88 & 134;
      Graph (106) := +103 & 58 & 76 & 96 & 104 & 58;
      Graph (107) := +92;
      Graph (108) := +92;
      Graph (109) := +88;
      Graph (110) := +109;
      Graph (111) := +88;
      Graph (112) := +103 & 111;
      Graph (113) := +103;
      Graph (114) := +103 & 108;
      Graph (115) := +106 & 130;
      Graph (116) := +126;
      Graph (117) := +117 & 116;
      Graph (118) := +121;
      Graph (119) := +121;
      Graph (120) := +121;
      Graph (121) := +126 & 122 & 126;
      Graph (123) := +103;
      Graph (124) := +129;
      Graph (125) := +124;
      Graph (126) := +139 & 136;
      Graph (127) := +88;
      Graph (128) := +59 & 86 & 113 & 127;
      Graph (129) := +128 & 72;
      Graph (130) := +133 & 58 & 76 & 96 & 104;
      Graph (131) := +133;
      Graph (132) := +133;
      Graph (133) := +114 & 94;
      Graph (134) := +103 & 73;
      Graph (135) := +135 & 102 & 91;
      Graph (136) := +62 & 135;
      Graph (137) := +93;
      Graph (138) := +81;

      Expected.Append (+139);
      Expected.Append (+62);
      Expected.Append (+102);
      Expected.Append (+122);
      Expected.Append
        (+123 & 60 & 61 & 57 & 120 & 119 & 118 & 121 & 87 & 88 & 111 & 112 & 91 & 135 & 136 & 126 & 116 & 117 & 103);
      Expected.Append (+58);
      Expected.Append (+109);
      Expected.Append (+110);
      Expected.Append (+92);
      Expected.Append (+108);
      Expected.Append (+114);
      Expected.Append (+107);
      Expected.Append (+94);
      Expected.Append (+133);
      Expected.Append (+59);
      Expected.Append (+97);
      Expected.Append (+86);
      Expected.Append (+113);
      Expected.Append (+127);
      Expected.Append (+128);
      Expected.Append (+99 & 73 & 134 & 77);
      Expected.Append (+100);
      Expected.Append (+63);
      Expected.Append (+89);
      Expected.Append (+90);
      Expected.Append (+82);
      Expected.Append (+104);
      Expected.Append (+131);
      Expected.Append (+66);
      Expected.Append (+105);
      Expected.Append (+132);
      Expected.Append (+80);
      Expected.Append (+81);
      Expected.Append (+138);
      Expected.Append (+93);
      Expected.Append (+137);
      Expected.Append
        (+83 & 84 & 85 & 64 & 101 & 68 & 69 & 67 & 78 & 79 & 98 & 72 & 129 & 124 & 125 & 96 & 130 & 115 & 65 & 74 &
           75 & 76 & 106);
      Expected.Append (+95);
      Expected.Append (+70);
      Expected.Append (+71);
      Expected.Append (+56);

      Computed := Graphs.Strongly_Connected_Components (Graph);
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Computed 1");
         for Comp of Computed loop
            Ada.Text_IO.Put_Line (Graphs.Image (Comp));
         end loop;
      end if;
      Check ("1", Computed, Expected);
   end Test_Strong_Components_Ada_Lite;

   procedure Test_Ada_Lite (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (Tst);
      use AUnit.Checks;

      --  Graph is from a small subset of the WisiToken ada_lite.wy name
      --  grammar; it did not find the loop 11.0 -> 11.0.
      --
      --  There are multiple edges between the same two nodes; this requires
      --  the algorithm to use edge ids.
      --
      --  Nonterminals:
      --  8.0: wisitoken_accept <= name Wisi_EOI
      --  9.0: actual_parameter_part <= LEFT_PAREN association_opt RIGHT_PAREN
      --  10.0: association_opt <= IDENTIFIER EQUAL_GREATER primary
      --  10.1: association_opt <= primary
      --  10.2: association_opt <=
      --  11.0: name <= name LEFT_PAREN range_list RIGHT_PAREN
      --  11.1: name <= name actual_parameter_part
      --  11.2: name <= IDENTIFIER
      --  12.0: paren_primary <= LEFT_PAREN primary RIGHT_PAREN
      --  13.0: primary <= NUMERIC_LITERAL
      --  13.1: primary <= name
      --  13.2: primary <= paren_primary
      --  14.0: range_g <= primary DOT_DOT primary
      --  15.0: range_list <= range_list COMMA range_g
      --  15.1: range_list <= range_g

      type Unknown_Recursion_Index is range 0 .. Integer'Last;
      subtype Recursion_Index is Unknown_Recursion_Index range 1 .. Unknown_Recursion_Index'Last;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => Integer'Last,
         Path_Index        => Recursion_Index,
         Edge_Image        => Trimmed_Image);
      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Arrays.Vector;
      Expected : Graphs.Path_Arrays.Vector;

      use Graphs;
      use Graphs_AUnit;
   begin
      Graphs.Trace := Test.Trace - 1;

      Graph.Add_Edge (56, 71, 1);
      Graph.Add_Edge (57, 61, 2);
      Graph.Add_Edge (58, 61, 3);
      Graph.Add_Edge (59, 103, 4);
      Graph.Add_Edge (59, 88, 5);
      Graph.Add_Edge (60, 88, 6);
      Graph.Add_Edge (60, 87, 7);
      Graph.Add_Edge (61, 61, 8);
      Graph.Add_Edge (61, 60, 9);
      Graph.Add_Edge (61, 60, 10);
      Graph.Add_Edge (64, 63, 11);
      Graph.Add_Edge (64, 76, 12);
      Graph.Add_Edge (64, 96, 13);
      Graph.Add_Edge (64, 97, 14);
      Graph.Add_Edge (64, 63, 15);
      Graph.Add_Edge (64, 96, 16);
      Graph.Add_Edge (64, 97, 17);
      Graph.Add_Edge (65, 115, 18);
      Graph.Add_Edge (65, 66, 19);
      Graph.Add_Edge (66, 131, 20);
      Graph.Add_Edge (67, 88, 21);
      Graph.Add_Edge (67, 69, 22);
      Graph.Add_Edge (68, 125, 23);
      Graph.Add_Edge (69, 69, 24);
      Graph.Add_Edge (69, 68, 25);
      Graph.Add_Edge (69, 68, 26);
      Graph.Add_Edge (70, 106, 27);
      Graph.Add_Edge (70, 132, 28);
      Graph.Add_Edge (70, 130, 29);
      Graph.Add_Edge (70, 95, 30);
      Graph.Add_Edge (70, 129, 31);
      Graph.Add_Edge (71, 71, 32);
      Graph.Add_Edge (71, 70, 33);
      Graph.Add_Edge (71, 70, 34);
      Graph.Add_Edge (72, 98, 35);
      Graph.Add_Edge (72, 67, 36);
      Graph.Add_Edge (72, 101, 37);
      Graph.Add_Edge (72, 64, 38);
      Graph.Add_Edge (72, 90, 39);
      Graph.Add_Edge (73, 116, 40);
      Graph.Add_Edge (73, 99, 41);
      Graph.Add_Edge (74, 65, 42);
      Graph.Add_Edge (74, 105, 43);
      Graph.Add_Edge (74, 132, 44);
      Graph.Add_Edge (74, 137, 45);
      Graph.Add_Edge (75, 75, 46);
      Graph.Add_Edge (75, 74, 47);
      Graph.Add_Edge (75, 74, 48);
      Graph.Add_Edge (76, 75, 49);
      Graph.Add_Edge (77, 134, 50);
      Graph.Add_Edge (77, 116, 51);
      Graph.Add_Edge (78, 88, 52);
      Graph.Add_Edge (78, 125, 53);
      Graph.Add_Edge (79, 79, 54);
      Graph.Add_Edge (79, 78, 55);
      Graph.Add_Edge (79, 78, 56);
      Graph.Add_Edge (80, 80, 57);
      Graph.Add_Edge (81, 80, 58);
      Graph.Add_Edge (82, 103, 59);
      Graph.Add_Edge (83, 82, 60);
      Graph.Add_Edge (83, 125, 61);
      Graph.Add_Edge (84, 84, 62);
      Graph.Add_Edge (84, 83, 63);
      Graph.Add_Edge (84, 83, 64);
      Graph.Add_Edge (85, 84, 65);
      Graph.Add_Edge (86, 97, 66);
      Graph.Add_Edge (86, 88, 67);
      Graph.Add_Edge (86, 97, 68);
      Graph.Add_Edge (87, 121, 69);
      Graph.Add_Edge (87, 118, 70);
      Graph.Add_Edge (87, 119, 71);
      Graph.Add_Edge (87, 120, 72);
      Graph.Add_Edge (88, 87, 73);
      Graph.Add_Edge (89, 134, 74);
      Graph.Add_Edge (90, 89, 75);
      Graph.Add_Edge (91, 112, 76);
      Graph.Add_Edge (91, 112, 77);
      Graph.Add_Edge (92, 110, 78);
      Graph.Add_Edge (93, 138, 79);
      Graph.Add_Edge (94, 103, 80);
      Graph.Add_Edge (94, 107, 81);
      Graph.Add_Edge (95, 103, 82);
      Graph.Add_Edge (95, 103, 83);
      Graph.Add_Edge (95, 103, 84);
      Graph.Add_Edge (95, 103, 85);
      Graph.Add_Edge (96, 125, 86);
      Graph.Add_Edge (96, 85, 87);
      Graph.Add_Edge (96, 125, 88);
      Graph.Add_Edge (98, 88, 89);
      Graph.Add_Edge (98, 125, 90);
      Graph.Add_Edge (98, 79, 91);
      Graph.Add_Edge (98, 125, 92);
      Graph.Add_Edge (98, 88, 93);
      Graph.Add_Edge (98, 125, 94);
      Graph.Add_Edge (98, 125, 95);
      Graph.Add_Edge (98, 88, 96);
      Graph.Add_Edge (98, 125, 97);
      Graph.Add_Edge (98, 79, 98);
      Graph.Add_Edge (98, 88, 99);
      Graph.Add_Edge (98, 125, 100);
      Graph.Add_Edge (99, 77, 101);
      Graph.Add_Edge (100, 77, 102);
      Graph.Add_Edge (101, 100, 103);
      Graph.Add_Edge (101, 125, 104);
      Graph.Add_Edge (101, 97, 105);
      Graph.Add_Edge (101, 125, 106);
      Graph.Add_Edge (101, 97, 107);
      Graph.Add_Edge (103, 103, 108);
      Graph.Add_Edge (103, 117, 109);
      Graph.Add_Edge (103, 103, 110);
      Graph.Add_Edge (103, 57, 111);
      Graph.Add_Edge (103, 123, 112);
      Graph.Add_Edge (104, 103, 113);
      Graph.Add_Edge (105, 134, 114);
      Graph.Add_Edge (105, 88, 115);
      Graph.Add_Edge (105, 134, 116);
      Graph.Add_Edge (106, 103, 117);
      Graph.Add_Edge (106, 58, 118);
      Graph.Add_Edge (106, 76, 119);
      Graph.Add_Edge (106, 96, 120);
      Graph.Add_Edge (106, 104, 121);
      Graph.Add_Edge (106, 103, 122);
      Graph.Add_Edge (106, 58, 123);
      Graph.Add_Edge (106, 76, 124);
      Graph.Add_Edge (106, 104, 125);
      Graph.Add_Edge (107, 92, 126);
      Graph.Add_Edge (108, 92, 127);
      Graph.Add_Edge (109, 88, 128);
      Graph.Add_Edge (110, 110, 129);
      Graph.Add_Edge (110, 109, 130);
      Graph.Add_Edge (110, 109, 131);
      Graph.Add_Edge (111, 88, 132);
      Graph.Add_Edge (112, 103, 133);
      Graph.Add_Edge (112, 111, 134);
      Graph.Add_Edge (113, 103, 135);
      Graph.Add_Edge (114, 103, 136);
      Graph.Add_Edge (114, 108, 137);
      Graph.Add_Edge (115, 106, 138);
      Graph.Add_Edge (115, 130, 139);
      Graph.Add_Edge (116, 126, 140);
      Graph.Add_Edge (116, 126, 141);
      Graph.Add_Edge (117, 117, 142);
      Graph.Add_Edge (117, 116, 143);
      Graph.Add_Edge (117, 116, 144);
      Graph.Add_Edge (118, 118, 145);
      Graph.Add_Edge (118, 121, 146);
      Graph.Add_Edge (118, 121, 147);
      Graph.Add_Edge (118, 121, 148);
      Graph.Add_Edge (119, 119, 149);
      Graph.Add_Edge (119, 121, 150);
      Graph.Add_Edge (119, 121, 151);
      Graph.Add_Edge (119, 121, 152);
      Graph.Add_Edge (120, 120, 153);
      Graph.Add_Edge (120, 121, 154);
      Graph.Add_Edge (120, 121, 155);
      Graph.Add_Edge (120, 121, 156);
      Graph.Add_Edge (121, 126, 157);
      Graph.Add_Edge (121, 122, 158);
      Graph.Add_Edge (121, 126, 159);
      Graph.Add_Edge (121, 126, 160);
      Graph.Add_Edge (123, 103, 161);
      Graph.Add_Edge (124, 124, 162);
      Graph.Add_Edge (124, 129, 163);
      Graph.Add_Edge (124, 129, 164);
      Graph.Add_Edge (125, 124, 165);
      Graph.Add_Edge (126, 139, 166);
      Graph.Add_Edge (126, 136, 167);
      Graph.Add_Edge (126, 136, 168);
      Graph.Add_Edge (127, 88, 169);
      Graph.Add_Edge (128, 59, 170);
      Graph.Add_Edge (128, 86, 171);
      Graph.Add_Edge (128, 113, 172);
      Graph.Add_Edge (128, 127, 173);
      Graph.Add_Edge (129, 128, 174);
      Graph.Add_Edge (129, 72, 175);
      Graph.Add_Edge (130, 133, 176);
      Graph.Add_Edge (130, 58, 177);
      Graph.Add_Edge (130, 76, 178);
      Graph.Add_Edge (130, 96, 179);
      Graph.Add_Edge (130, 104, 180);
      Graph.Add_Edge (131, 133, 181);
      Graph.Add_Edge (132, 133, 182);
      Graph.Add_Edge (133, 114, 183);
      Graph.Add_Edge (133, 94, 184);
      Graph.Add_Edge (134, 103, 185);
      Graph.Add_Edge (134, 73, 186);
      Graph.Add_Edge (134, 103, 187);
      Graph.Add_Edge (135, 135, 188);
      Graph.Add_Edge (135, 102, 189);
      Graph.Add_Edge (135, 91, 190);
      Graph.Add_Edge (135, 91, 191);
      Graph.Add_Edge (136, 136, 192);
      Graph.Add_Edge (136, 62, 193);
      Graph.Add_Edge (136, 135, 194);
      Graph.Add_Edge (136, 135, 195);
      Graph.Add_Edge (137, 93, 196);
      Graph.Add_Edge (138, 81, 197);

      Check ("Multigraph", Graph.Multigraph, True);

      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (88, +(6, 6)), (87, +(73, 73)), (119, +(71, 71)),
          (121, +(150, 150) & (151, 151) & (152, 152)), (126, +(157, 157) & (159, 159) & (160, 160)),
          (136, +(167, 167) & (168, 168)), (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)), (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (87, +(7, 7)), (121, +(69, 69)),
          (126, +(157, 157) & (159, 159) & (160, 160)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)), (112, +(76, 76) & (77, 77)),
          (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (87, +(7, 7)), (118, +(70, 70)),
          (121, +(146, 146) & (147, 147) & (148, 148)), (126, +(157, 157) & (159, 159) & (160, 160)),
          (136, +(167, 167) & (168, 168)), (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)),
          (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (87, +(7, 7)), (120, +(72, 72)),
          (121, +(154, 154) & (155, 155) & (156, 156)), (126, +(157, 157) & (159, 159) & (160, 160)),
          (136, +(167, 167) & (168, 168)), (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)), (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (88, +(6, 6)), (87, +(73, 73)), (121, +(69, 69)),
          (126, +(157, 157) & (159, 159) & (160, 160)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)), (112, +(76, 76) & (77, 77)),
          (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (88, +(6, 6)), (87, +(73, 73)), (118, +(70, 70)),
          (121, +(146, 146) & (147, 147) & (148, 148)), (126, +(157, 157) & (159, 159) & (160, 160)),
          (136, +(167, 167) & (168, 168)), (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)), (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (87, +(7, 7)), (119, +(71, 71)),
          (121, +(150, 150) & (151, 151) & (152, 152)), (126, +(157, 157) & (159, 159) & (160, 160)),
          (136, +(167, 167) & (168, 168)), (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)),
          (103, +(133, 133))));
      Expected.Append
        (((57, +(111, 111)), (61, +(2, 2)), (60, +(9, 9) & (10, 10)), (88, +(6, 6)), (87, +(73, 73)),
          (120, +(72, 72)), (121, +(154, 154) & (155, 155) & (156, 156)),
          (126, +(157, 157) & (159, 159) & (160, 160)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)), (112, +(76, 76) & (77, 77)),
          (103, +(133, 133))));
      Expected.Append ((1 => (61, +(8, 8))));
      Expected.Append
        (((64, +(38, 38)), (76, +(12, 12)), (75, +(49, 49)), (74, +(47, 47) & (48, 48)), (65, +(42, 42)),
          (115, +(18, 18)), (130, +(139, 139)), (96, +(179, 179)), (125, +(86, 86) & (88, 88)),
          (124, +(165, 165)), (129, +(163, 163) & (164, 164)), (72, +(175, 175))));
      Expected.Append
        (((64, +(38, 38)), (96, +(13, 13) & (16, 16)), (85, +(87, 87)), (84, +(65, 65)),
          (83, +(63, 63) & (64, 64)), (125, +(61, 61)), (124, +(165, 165)),
          (129, +(163, 163) & (164, 164)), (72, +(175, 175))));
      Expected.Append
        (((64, +(38, 38)), (76, +(12, 12)), (75, +(49, 49)), (74, +(47, 47) & (48, 48)), (65, +(42, 42)),
          (115, +(18, 18)), (106, +(138, 138)), (96, +(120, 120)), (125, +(86, 86) & (88, 88)),
          (124, +(165, 165)), (129, +(163, 163) & (164, 164)), (72, +(175, 175))));
      Expected.Append
        (((64, +(38, 38)), (96, +(13, 13) & (16, 16)), (125, +(86, 86) & (88, 88)), (124, +(165, 165)),
          (129, +(163, 163) & (164, 164)), (72, +(175, 175))));
      Expected.Append
        (((64, +(38, 38)), (76, +(12, 12)), (75, +(49, 49)), (74, +(47, 47) & (48, 48)), (65, +(42, 42)),
          (115, +(18, 18)), (106, +(138, 138)), (96, +(120, 120)), (85, +(87, 87)), (84, +(65, 65)),
          (83, +(63, 63) & (64, 64)), (125, +(61, 61)), (124, +(165, 165)), (129, +(163, 163) & (164, 164)),
          (72, +(175, 175))));
      Expected.Append
        (((64, +(38, 38)), (76, +(12, 12)), (75, +(49, 49)), (74, +(47, 47) & (48, 48)), (65, +(42, 42)),
          (115, +(18, 18)), (130, +(139, 139)), (96, +(179, 179)), (85, +(87, 87)), (84, +(65, 65)),
          (83, +(63, 63) & (64, 64)), (125, +(61, 61)), (124, +(165, 165)), (129, +(163, 163) & (164, 164)),
          (72, +(175, 175))));
      Expected.Append
        (((65, +(42, 42)), (115, +(18, 18)), (106, +(138, 138)), (76, +(119, 119) & (124, 124)), (75, +(49, 49)),
          (74, +(47, 47) & (48, 48))));
      Expected.Append
        (((65, +(42, 42)), (115, +(18, 18)), (130, +(139, 139)), (76, +(178, 178)), (75, +(49, 49)),
          (74, +(47, 47) & (48, 48))));
      Expected.Append
        (((67, +(36, 36)), (69, +(22, 22)), (68, +(25, 25) & (26, 26)), (125, +(23, 23)), (124, +(165, 165)),
          (129, +(163, 163) & (164, 164)), (72, +(175, 175))));
      Expected.Append ((1 => (69, +(24, 24))));
      Expected.Append ((1 => (71, +(32, 32))));
      Expected.Append
        (((72, +(175, 175)), (98, +(35, 35)), (79, +(91, 91) & (98, 98)), (78, +(55, 55) & (56, 56)),
          (125, +(53, 53)), (124, +(165, 165)), (129, +(163, 163) & (164, 164))));
      Expected.Append
        (((72, +(175, 175)), (101, +(37, 37)), (125, +(104, 104) & (106, 106)), (124, +(165, 165)),
          (129, +(163, 163) & (164, 164))));
      Expected.Append
        (((72, +(175, 175)), (98, +(35, 35)),
          (125, +(90, 90) & (92, 92) & (94, 94) & (95, 95) & (97, 97) & (100, 100)),
          (124, +(165, 165)), (129, +(163, 163) & (164, 164))));
      Expected.Append (((73, +(186, 186)), (99, +(41, 41)), (77, +(101, 101)), (134, +(50, 50))));
      Expected.Append ((1 => (75, +(46, 46))));
      Expected.Append ((1 => (79, +(54, 54))));
      Expected.Append ((1 => (80, +(57, 57))));
      Expected.Append ((1 => (84, +(62, 62))));
      Expected.Append
        (((87, +(73, 73)), (120, +(72, 72)), (121, +(154, 154) & (155, 155) & (156, 156)),
          (126, +(157, 157) & (159, 159) & (160, 160)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)), (112, +(76, 76) & (77, 77)),
          (111, +(134, 134)), (88, +(132, 132))));
      Expected.Append
        (((87, +(73, 73)), (119, +(71, 71)), (121, +(150, 150) & (151, 151) & (152, 152)),
          (126, +(157, 157) & (159, 159) & (160, 160)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)), (111, +(134, 134)), (88, +(132, 132))));
      Expected.Append
        (((87, +(73, 73)), (118, +(70, 70)), (121, +(146, 146) & (147, 147) & (148, 148)),
          (126, +(157, 157) & (159, 159) & (160, 160)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)), (112, +(76, 76) & (77, 77)),
          (111, +(134, 134)), (88, +(132, 132))));
      Expected.Append
        (((87, +(73, 73)), (121, +(69, 69)), (126, +(157, 157) & (159, 159) & (160, 160)),
          (136, +(167, 167) & (168, 168)), (135, +(194, 194) & (195, 195)), (91, +(190, 190) & (191, 191)),
          (112, +(76, 76) & (77, 77)), (111, +(134, 134)), (88, +(132, 132))));
      Expected.Append
        (((91, +(190, 190) & (191, 191)), (112, +(76, 76) & (77, 77)), (103, +(133, 133)), (117, +(109, 109)),
          (116, +(143, 143) & (144, 144)), (126, +(140, 140) & (141, 141)), (136, +(167, 167) & (168, 168)),
          (135, +(194, 194) & (195, 195))));
      Expected.Append (((103, +(161, 161)), (123, +(112, 112))));
      Expected.Append ((1 => (103, +(108, 108) & (110, 110))));
      Expected.Append ((1 => (110, +(129, 129))));
      Expected.Append ((1 => (117, +(142, 142))));
      Expected.Append ((1 => (118, +(145, 145))));
      Expected.Append ((1 => (119, +(149, 149))));
      Expected.Append ((1 => (120, +(153, 153))));
      Expected.Append ((1 => (124, +(162, 162))));
      Expected.Append ((1 => (135, +(188, 188))));
      Expected.Append ((1 => (136, +(192, 192))));

      Sort_Paths.Sort (Expected);

      Computed := Graph.Find_Cycles_Tiernan;
      Sort_Paths.Sort (Computed);
      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Tiernan:");
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs_AUnit.Image (Cycle));
         end loop;
         Ada.Text_IO.New_Line;
      end if;

      Check ("Tiernan", Computed, Expected);

      if Test.Trace > 0 then
         Ada.Text_IO.Put_Line ("Johnson cycles:");
      end if;
      Computed := Graph.Find_Cycles;
      Sort_Paths.Sort (Computed);
      if Test.Trace > 0 then
         if Test.Trace > 1 then
            Ada.Text_IO.New_Line;
         end if;
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs_AUnit.Image (Cycle));
         end loop;
         Ada.Text_IO.New_Line;
      end if;

      Check ("Johnson", Computed, Expected);
   end Test_Ada_Lite;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Path'Access, "Test_Find_Path");
      Register_Routine (T, Test_Find_Cycles'Access, "Test_Find_Cycles");
      Register_Routine (T, Test_Conflict_Name'Access, "Test_Conflict_Name");
      Register_Routine (T, Test_Ada_Lite_Name'Access, "Test_Ada_Lite_Name");
      Register_Routine (T, Test_Strong_Components'Access, "Test_Strong_Components");
      Register_Routine (T, Test_Strong_Components_Ada_Lite'Access, "Test_Strong_Components_Ada_Lite");
      Register_Routine (T, Test_Ada_Lite'Access, "Test_Ada_Lite");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_graphs.adb");
   end Name;

end Test_Graphs;
