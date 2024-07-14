% ! Solved Nurikabe To Build Up And Test The Rules Of The Puzzle:
% ? Fixed Cells :
%fxd_cell(1,2,3).
%fxd_cell(1,6,1).
%fxd_cell(3,1,2).
%fxd_cell(3,4,1).
%fxd_cell(5,2,1).
%fxd_cell(5,5,2).
%fxd_cell(6,3,2).
%fxd_cell(7,1,1).
%fxd_cell(7,5,1).
%fxd_cell(7,7,5).
% ? Solved Cells :
%solve_cell(1, 1, blue).
%solve_cell(1, 2, green).
%solve_cell(1, 3, green).
%solve_cell(1, 4, green).
%solve_cell(1, 5, blue).
%solve_cell(1, 6, green).
%solve_cell(1, 7, blue).
%solve_cell(2, 1, blue).
%solve_cell(2, 2, blue).
%solve_cell(2, 3, blue).
%solve_cell(2, 4, blue).
%solve_cell(2, 5, blue).
%solve_cell(2, 6, blue).
%solve_cell(2, 7, blue).
%solve_cell(3, 1, green).
%solve_cell(3, 2, green).
%solve_cell(3, 3, blue).
%solve_cell(3, 4, green).
%solve_cell(3, 5, blue).
%solve_cell(3, 6, green).
%solve_cell(3, 7, green).
%solve_cell(4, 1, blue).
%solve_cell(4, 2, blue).
%solve_cell(4, 3, blue).
%solve_cell(4, 4, blue).
%solve_cell(4, 5, blue).
%solve_cell(4, 6, blue).
%solve_cell(4, 7, green).
%solve_cell(5, 1, blue).
%solve_cell(5, 2, green).
%solve_cell(5, 3, blue).
%solve_cell(5, 4, green).
%solve_cell(5, 5, green).
%solve_cell(5, 6, blue).
%solve_cell(5, 7, green).
%solve_cell(6, 1,blue).
%solve_cell(6, 2,blue).
%solve_cell(6, 3,green).
%solve_cell(6, 4,blue).
%solve_cell(6, 5,blue).
%solve_cell(6, 6,blue).
%solve_cell(6, 7,green).
%solve_cell(7, 1,green).
%solve_cell(7, 2,blue).
%solve_cell(7, 3,green).
%solve_cell(7, 4,blue).
%solve_cell(7, 5,green).
%solve_cell(7, 6,blue).
%solve_cell(7, 7,green).
% ! --------------------------------------------------------------------------------
% ? Making The Puzzle Dynamic : 
:-dynamic solve_cell/3.
:-dynamic fxd_cell/3.
% Predicate to initialize the grid
initialize_grid(Rows, Cols, FixedCells) :-
    retractall(solve_cell(_, _, _)),
    retractall(fxd_cell(_, _, _)),
    create_grid(Rows, Cols),
    initialize_fixed_cells(FixedCells).
% Helper predicate to create the grid
create_grid(Rows, Cols) :-
    between(1, Rows, Row),
    between(1, Cols, Col),
    assertz(solve_cell(Row, Col, empty)),
    fail.
create_grid(_, _).
% Predicate to set fixed cells directly from a list
initialize_fixed_cells([]).
initialize_fixed_cells([(Row, Col, Value)|Rest]) :-
    assertz(fxd_cell(Row, Col, Value)),
    retract(solve_cell(Row,Col,empty)),
    assertz(solve_cell(Row, Col, green)),
    initialize_fixed_cells(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
print_grid:-
    grid(R,C),\+print_grid_helper(R,C).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
print_grid_helper(Rows, Cols) :-
    between(1, Rows, Row),
    between(1, Cols, Col),
    (fxd_cell(Row, Col, Value) -> write(Value) ; (solve_cell(Row, Col, State) -> print_cell(State) ; write(' '))),
    (Col =:= Cols -> nl; true),
    fail.
print_grid(_, _).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
print_cell(green) :- write('G').
print_cell(blue) :- write('B').
print_cell(empty) :- write('.').
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
color_cell_blue(X,Y):-
    retract(solve_cell(X,Y,_)),
    assertz(solve_cell(X,Y,blue)).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
color_cell_green(X,Y):-
    retract(solve_cell(X,Y,_)),
    assertz(solve_cell(X,Y,green)).
% * Solved Checkers :
% * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
within_the_grid(X,Y):-
    grid(R,C) , X > 0 , X =< C , Y > 0 , Y =< R.
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
neighbor((X,Y),(Xl,Y)):- Xl is X-1 , within_the_grid(Xl,Y).
neighbor((X,Y),(Xr,Y)):- Xr is X+1 , within_the_grid(Xr,Y).
neighbor((X,Y),(X,Yu)):- Yu is Y-1 , within_the_grid(X,Yu).
neighbor((X,Y),(X,Yd)):- Yd is Y+1 , within_the_grid(X,Yd).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
validate_no_2_by_2_sea(Rows, Cols) :-
    \+ (between(1, Rows, Row),
        between(1, Cols, Col),
        Row1 is Row + 1, Col1 is Col + 1,
        solve_cell(Row, Col, blue),
        solve_cell(Row1, Col, blue),
        solve_cell(Row, Col1, blue),
        solve_cell(Row1, Col1, blue)).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
no_2x2_sea:-
    grid(R,C),validate_no_2_by_2_sea(R,C).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
one_sea:-
    findall((X,Y),solve_cell(X,Y,blue),BlueCells),
    BlueCells \= [],
    BlueCells = [(X,Y)|_],
    sea((X,Y), Sea),
    length(BlueCells, BlueCount),
    length(Sea, SeaCount),
    BlueCount =:= SeaCount.
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
sea((X,Y),Sea):-
    sea_helper([(X,Y)],[],Sea).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
sea_helper([] , Sea , Sea).
sea_helper([(X,Y)|Rest] , Vis , Sea):-
    solve_cell(X,Y,blue),
    \+member((X,Y),Vis),
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    append(Rest,Neighbors,NewRest),
    sea_helper(NewRest,[(X,Y)|Vis] , Sea).
sea_helper([(X,Y)|Rest] , Vis , Sea):-
    (member((X,Y),Vis);\+solve_cell(X,Y,blue)),
    sea_helper(Rest,Vis,Sea).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
one_fixed_cell_in_island:-
    findall((X,Y),solve_cell(X,Y,green),LandCells),
    islands(LandCells,Islands),
    check_islands_has_each_exactly_one_fixed_cell(Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
island((X,Y),Island):-island_helper([(X,Y)],[],Island).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
island_helper([],Island,Island).
island_helper([(X,Y)|Rest],Vis,Island):-
    solve_cell(X,Y,green),
    \+member((X,Y),Vis),
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    append(Rest,Neighbors,NewRest),
    island_helper(NewRest,[(X,Y)|Vis] , Island).
island_helper([(X,Y)|Rest],Vis,Island):-
    (member((X,Y),Vis);\+solve_cell(X,Y,green)),
    island_helper(Rest,Vis , Island).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
islands(LandCells,Islands):-islands_helper(LandCells,[],[],Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
islands_helper([(X,Y)|Rest], Vis , Acc ,Islands):-
    \+member((X,Y),Vis),
    island((X,Y),Island),
    append(Acc, [Island] ,NewIslands),
    append(Vis, Island ,NewVis),
    islands_helper(Rest,NewVis,NewIslands,Islands).
islands_helper([(X,Y)|Rest],Vis,Acc,Islands):-
    member((X,Y),Vis),
    islands_helper(Rest,Vis,Acc,Islands).
islands_helper([],_,Islands,Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
get_island_fixed_cells_helper([],FixedCells,FixedCells).
get_island_fixed_cells_helper([(X,Y)|Rest],Acc,FixedCells):-
    fxd_cell(X,Y,Z) , get_island_fixed_cells_helper(Rest,[(X,Y,Z)|Acc],FixedCells).
get_island_fixed_cells_helper([(X,Y)|Rest],Acc,FixedCells):-
    \+fxd_cell(X,Y,_) , get_island_fixed_cells_helper(Rest,Acc,FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_island_fixed_cells(Island,IslandFixedCells):-
    get_island_fixed_cells_helper(Island,[],IslandFixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_islands_has_each_exactly_one_fixed_cell([]).
check_islands_has_each_exactly_one_fixed_cell([Island|Islands]):-
    get_island_fixed_cells(Island,FixedCells),
    length(FixedCells, L),
    L =:= 1,
    check_islands_has_each_exactly_one_fixed_cell(Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
island_number_equals_size:-
    findall((X,Y),solve_cell(X,Y,green),LandCells),
    islands(LandCells,Islands),
    check_islands_each_has_size_equals_fixed_cell_number(Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_islands_each_has_size_equals_fixed_cell_number([]).
check_islands_each_has_size_equals_fixed_cell_number([Island|Islands]):-
    get_island_fixed_cells(Island,FixedCells),
    FixedCells = [(_,_,L)|_],
    length(Island, S),
    L =:= S,
    check_islands_each_has_size_equals_fixed_cell_number(Islands).
% * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% ! The Solver :
% ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
solve_backtracking :-
    % Get all empty cells
    findall((Row, Col), solve_cell(Row, Col, empty), EmptyCells),
    % Try to solve the puzzle
    solve_cells(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
solve_cells([]) :- 
    solved , nl , print_grid , nl , writeln("Nurikabe Solved :)").
% Recursive case: try to solve for one cell and continue
solve_cells([(Row, Col)|Rest]) :-
    % Try to color the cell blue
    color_cell_blue(Row, Col),
    solve_cells(Rest);
    % If it fails, backtrack and try to color the cell green
    color_cell_green(Row, Col), 
    solve_cells(Rest).
% ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% * Logical Steps :
% ? I - Solving For Fixed Cells With Clue Equals One :
solve_for_fixed_cells_with_clue_equals_one:-
    findall((X,Y),fxd_cell(X,Y,1),FixedCellsWithClueEqualsOne),
    color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue(FixedCellsWithClueEqualsOne).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_list_blue([]).
color_list_blue([(X,Y)|Rest]):-
    color_cell_blue(X,Y) , print_grid , nl , nl , color_list_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue([]).
color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue([(X,Y)|Rest]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    color_list_blue(Neighbors),color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? II - Solving For Cells With All Blue Neighbors:
solve_for_cells_with_all_neighbors_blue:-
    findall((X,Y),solve_cell(X,Y,empty),CellsWithAllNeighborsBlue),
    color_the_cells_that_all_their_neighbors_are_blue(CellsWithAllNeighborsBlue).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_for_neighbors_being_blue([]).
check_for_neighbors_being_blue([(X,Y)|Neighbors]):-
    solve_cell(X,Y,blue) , check_for_neighbors_being_blue(Neighbors).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_the_cells_that_all_their_neighbors_are_blue([]).
color_the_cells_that_all_their_neighbors_are_blue([(X,Y)|Rest]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    \+check_for_neighbors_being_blue(Neighbors) , color_the_cells_that_all_their_neighbors_are_blue(Rest).
color_the_cells_that_all_their_neighbors_are_blue([(X,Y)|Rest]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    check_for_neighbors_being_blue(Neighbors) , color_cell_blue(X,Y), print_grid , nl , nl , color_the_cells_that_all_their_neighbors_are_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? III - Solving For Sea Cells With One Way Out :
seas_one_way_out:-
    findall((X,Y),solve_cell(X,Y,blue),SeaCells),
    seas_one_way_out_helper(SeaCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
seas_one_way_out_helper([]).
seas_one_way_out_helper([(X,Y)|Rest]):-
    sea_one_way_out(X,Y) ,seas_one_way_out_helper(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sea_one_way_out(X,Y):-
    grid(R,C) , Xr is X + 1 ,  Xl is X-1  , Yu is Y-1 ,  Yd is Y+1 , 
    (
        (
            Xl > 0,
            solve_cell(Xl,Y,empty),
            (solve_cell(Xr,Y,green);Xr > C),
            (solve_cell(X,Yu,green);Yu =< 0),
            (solve_cell(X,Yd,green);Yd > R),
            color_cell_blue(Xl,Y)
        )
        ;
        (
            Xr =< C ,
            (solve_cell(Xl,Y,green);Xl =< 0),
            solve_cell(Xr,Y,empty),
            (solve_cell(X,Yu,green);Yu =< 0),
            (solve_cell(X,Yd,green);Yd > R),
            color_cell_blue(Xr,Y)
        )
        ;
        (
            Yu > 0 ,
            (solve_cell(Xl,Y,green);Xl =< 0),
            (solve_cell(Xr,Y,green);Xr > C),
            solve_cell(X,Yu,empty),
            (solve_cell(X,Yd,green);Yd > R),
            color_cell_blue(X,Yu)
        )
        ;
        (
            Yd =< R ,
            (solve_cell(Xl,Y,green);Xl =< 0),
            (solve_cell(Xr,Y,green);Xr > C),
            (solve_cell(X,Yu,green);Yu =< 0),
            solve_cell(X,Yd,empty),
            color_cell_blue(X,Yd)
        )
    ),print_grid , nl , nl.
sea_one_way_out(_,_).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? IV - Solving For Complete Islands :
isolate_completed_islands:-
    findall((X,Y,N),fxd_cell(X,Y,N),FixedCells),
    isolate_completed_islands_helper(FixedCells).
isolate_completed_islands_helper([]).
isolate_completed_islands_helper([(X,Y,N)|FixedCells]):-
    island((X,Y),Island),
    length(Island,L),
    L =:= N,
    surround_the_completed_island_with_blue(Island),
    isolate_completed_islands_helper(FixedCells).
isolate_completed_islands_helper([(X,Y,N)|FixedCells]):-
    island((X,Y),Island),
    length(Island,L),
    L =\= N,
    isolate_completed_islands_helper(FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
surround_the_completed_island_with_blue([]).
surround_the_completed_island_with_blue([(X,Y)|Island]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    surround_the_completed_island_cell_with_blue_helper(Neighbors),
    surround_the_completed_island_with_blue(Island).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
surround_the_completed_island_cell_with_blue_helper([]).
surround_the_completed_island_cell_with_blue_helper([(X,Y)|Neighbors]):-
    solve_cell(X,Y,empty) , color_cell_blue(X,Y) , print_grid , nl , nl , surround_the_completed_island_cell_with_blue_helper(Neighbors).
surround_the_completed_island_cell_with_blue_helper([(X,Y)|Neighbors]):-
    \+solve_cell(X,Y,empty) , surround_the_completed_island_cell_with_blue_helper(Neighbors).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? V - Solving For Diagonally Adjacent Fixed Cells :
solve_for_diag_adj_fxd_cells:-
    findall((X,Y),fxd_cell(X,Y,_),FixedCells),
    solve_for_diag_adj_fxd_cells_helper(FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
solve_for_diag_adj_fxd_cells_helper([]).
solve_for_diag_adj_fxd_cells_helper([(X,Y)|FixedCells]):-
    Xur is X+1,
    Yur is Y+1,
    fxd_cell(Xur,Yur,_),
    color_cell_blue(Xur,Y),
    color_cell_blue(X,Yur), print_grid , nl , nl , solve_for_diag_adj_fxd_cells_helper(FixedCells).
solve_for_diag_adj_fxd_cells_helper([(X,Y)|FixedCells]):-
    Xur is X+1,
    Yur is Y+1,
    \+fxd_cell(Xur,Yur,_),
    solve_for_diag_adj_fxd_cells_helper(FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? VI - Solving For Preventing 2x2 Blue Blocks :
solve_for_no_2x2_blue_blocks:-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    prevent_2x2_blue_blocks(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if_it_will_form_2x2_blue_block_color_it_green(X,Y):-
    Xr is X + 1,
    Xl is X - 1,
    Yu is Y - 1,
    Yd is Y + 1,
    (
        (
            solve_cell(Xr,Y,blue), 
            solve_cell(X,Yd,blue), 
            solve_cell(Xr,Yd,blue)
        );
        (
            solve_cell(Xl,Y,blue), 
            solve_cell(X,Yu,blue), 
            solve_cell(Xl,Yu,blue)
        )
    ),color_cell_green(X,Y),print_grid,nl,nl.
if_it_will_form_2x2_blue_block_color_it_green(_,_).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
prevent_2x2_blue_blocks([]).
prevent_2x2_blue_blocks([(X,Y)|Rest]):-
    if_it_will_form_2x2_blue_block_color_it_green(X,Y),
    prevent_2x2_blue_blocks(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? VII - solving for fixed cells that are vertically or horizontally separated by one cell :
solve_for_fixed_cells_that_are_one_cell_separated:-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    separate_fxd_cells(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fxd_cell_separator(X,Y):-
    Xr is X + 1,
    Xl is X - 1,
    Yu is Y - 1,
    Yd is Y + 1,
    (
        (fxd_cell(Xr,Y,_),fxd_cell(Xl,Y,_))
        ;
        (fxd_cell(X,Yd,_),fxd_cell(X,Yu,_))
    ),color_cell_blue(X,Y), print_grid , nl , nl.
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
separate_fxd_cells([]).
separate_fxd_cells([(X,Y)|Rest]):-
    fxd_cell_separator(X,Y) , separate_fxd_cells(Rest).
separate_fxd_cells([(X,Y)|Rest]):-
    \+fxd_cell_separator(X,Y) , separate_fxd_cells(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? VIII - Solving For Unreachable Cells :
solve_for_unreachable_cells:-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    color_unreachable_cells_by_blue(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_unreachable_cells_by_blue([]).
color_unreachable_cells_by_blue([(X,Y)|Rest]):-
    findall((Xf,Yf,N),fxd_cell(Xf,Yf,N),FixedCells),
    find_reachable_cells(FixedCells,[],NestedReachableCells),
    flatten(NestedReachableCells, ReachableCells),
    \+member((X,Y),ReachableCells),
    color_cell_blue(X,Y),print_grid,nl,nl,
    color_unreachable_cells_by_blue(Rest).
color_unreachable_cells_by_blue([(X,Y)|Rest]):-
    findall((Xf,Yf,N),fxd_cell(Xf,Yf,N),FixedCells),
    find_reachable_cells(FixedCells,[],NestedReachableCells),
    flatten(NestedReachableCells, ReachableCells),
    member((X,Y),ReachableCells),
    color_unreachable_cells_by_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_reachable_cells([],ReachableCells,ReachableCells).
find_reachable_cells([(X,Y,N)|Rest],ReachableCells,Acc):-
    find_paths((X,Y),N,CellPaths),
    append(CellPaths,ReachableCells,NewReachableCells),
    find_reachable_cells(Rest,NewReachableCells,Acc).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_path((X,Y),C,Vis,Path):-
    C > 0,
    neighbor((X,Y),(Xn,Yn)),
    \+member((X,Y),Vis),
    C1 is C-1,
    find_path((Xn,Yn),C1,[(X,Y)|Vis],Path).
find_path((_,_),0,Path,Path).
find_paths((X,Y),L,Paths):-
    setof(Path,find_path((X,Y),L,[],Path),Paths).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
grid(7,7).
init:-
    grid(R,C),
    %initialize_grid(R,C,[(1,2,3),(1,6,1),(3,1,2),(3,4,1),(5,2,1),(5,5,2),(6,3,2),(7,1,1),(7,5,1),(7,7,6)]).
    %initialize_grid(R,C,[(1,1,1),(2,3,1),(3,1,1),(3,5,4),(4,3,2),(5,5,1)]).
    initialize_grid(R,C,[(2,1,2),(2,3,1),(2,5,2),(2,7,2),(4,3,1),(4,5,1),(6,1,2),(6,3,1),(6,5,1),(6,7,2)]).
    %initialize_grid(R,C,[(1,3,2),(2,2,3),(2,6,1),(3,4,4),(5,4,2),(6,2,1),(6,6,1)]).
    %initialize_grid(R,C,[(1,2,3),(1,4,4),(3,3,1),(5,2,2),(5,4,1)]).
solve_logically:-
    solve_for_diag_adj_fxd_cells,
    solve_for_fixed_cells_with_clue_equals_one,
    solve_for_fixed_cells_that_are_one_cell_separated,    
    seas_one_way_out,
    solve_for_cells_with_all_neighbors_blue,
    isolate_completed_islands,

    solve_for_no_2x2_blue_blocks,
    solve_for_unreachable_cells.
solved:-
    no_2x2_sea,
    one_sea,
    island_number_equals_size,
    one_fixed_cell_in_island.
solve_logic(N):-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    length(EmptyCells,L),
    L =\= N,
    solve_logically,
    solve_logic(L).
solve_logic(N):-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    length(EmptyCells,L),
    L =:= N,
    solve_logically.
solve_nurikabe:-
    solve_logic(0),
    solve_backtracking.