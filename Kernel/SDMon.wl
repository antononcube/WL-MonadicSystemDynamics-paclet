BeginPackage["AntonAntonov`MonadicSystemDynamics`SDMon`"];

Begin["`Private`"];

Needs["AntonAntonov`MonadicSystemDynamics`"];
Needs["AntonAntonov`MonadicSystemDynamics`InteractiveInterfacesFunctions`"];
Needs["AntonAntonov`MonadMakers`"];
Needs["AntonAntonov`DataReshapers`"];
Needs["AntonAntonov`SSparseMatrix`"];
Needs["AntonAntonov`TileStats`"];
Needs["AntonAntonov`EpidemiologicalModeling`"];


(**************************************************************)
(* Non-monadic functions                                      *)
(**************************************************************)

Clear[CoordinatesToValuesAssociationQ];
CoordinatesToValuesAssociationQ[arg_] := MatchQ[arg, <|({_?NumericQ, _?NumericQ} -> _?NumericQ) ...|>];

Clear[BinaryOperateByKeys];
BinaryOperateByKeys[a1_?AssociationQ, a2_?AssociationQ, oper_] :=
    Merge[{a1, KeyTake[a2, Keys[a1]]}, If[Length[#] > 1, oper[ #[[1]], #[[2]] ], #[[1]]] &];

Clear[SubtractByKeys];
SubtractByKeys[a1_?AssociationQ, a2_?AssociationQ] :=
    Merge[{a1, KeyTake[a2, Keys[a1]]}, If[Length[#] > 1, #[[1]] - #[[2]], #[[1]]] &];

Clear[DeriveSusceptiblePopulation];
DeriveSusceptiblePopulation[
  aPopulations_?CoordinatesToValuesAssociationQ,
  aInfected_?CoordinatesToValuesAssociationQ,
  aDead_?CoordinatesToValuesAssociationQ] :=
    Block[{aRes},
      aRes = SubtractByKeys[aPopulations, aInfected];
      SubtractByKeys[aRes, aDead]
    ];

Clear[StocksSpecQ];
StocksSpecQ[x_] := MatchQ[ x, All | _String | {_String..} | _StringExpression | {_StringExpression ..} ];


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of SDMon monad (through StMon.) *)

GenerateStateMonadCode[ "AntonAntonov`MonadicSystemDynamics`SDMon", "FailureSymbol" -> $SDMonFailure, "StringContextNames" -> False ];

(**************************************************************)
(* Setters and takers                                         *)
(**************************************************************)

GenerateMonadAccessors[
  "AntonAntonov`MonadicSystemDynamics`SDMon",
  {"singleSiteModel", "multiSiteModel", "grid", "solution", "graph", "adjacencyMatrix" },
  "FailureSymbol" -> $SDMonFailure ];


(**************************************************************)
(* SDMonUnit                                                 *)
(**************************************************************)

Clear[SDMonUnit];

SDMonUnit[$SDMonFailure] := $SDMonFailure;

SDMonUnit[] := SDMon[None, Association[]];

SDMonUnit[{x_, c_Association}] := SDMon[x, c];

SDMonUnit[x_] := SDMon[x, Association[]];

SDMonUnit[x_?EpidemiologyModelQ] := SDMonUnit[x, Association[]];

SDMonUnit[x_, c_Association] := SDMon[x, c];

SDMonUnit[x_?EpidemiologyModelQ, c_Association] := SDMon[x, Join[c, <|"singleSiteModel" -> x|>] ];

SDMonUnit[___][$SDMonFailure] := $SDMonFailure;


(**************************************************************)
(* SDMonGetDefaultModel                                      *)
(**************************************************************)

Clear[SDMonGetDefaultModel];

SyntaxInformation[SDMonGetDefaultModel] = { "ArgumentsPattern" -> {} };

SDMonGetDefaultModel[___][$SDMonFailure] := $SDMonFailure;

SDMonGetDefaultModel[xs_, context_Association] := SDMonGetDefaultModel[][xs, context];

SDMonGetDefaultModel[][xs_, context_] :=
    Block[{model},

      Which[
        EpidemiologyModelQ[xs],
        model = xs,

        KeyExistsQ[context, "multiSiteModel"] && EpidemiologyModelQ[context["multiSiteModel"]],
        model = context["multiSiteModel"],

        KeyExistsQ[context, "singleSiteModel"] && EpidemiologyModelQ[context["singleSiteModel"]],
        model = context["singleSiteModel"],

        True,
        Echo["Cannot find a model.", "SDMonGetDefaultModel:"];
        Return[$SDMonFailure]
      ];

      SDMonUnit[model, context]
    ];

SDMonGetDefaultModel[__][___] :=
    Block[{},
      Echo["No arguments are expected.", "SDMonGetDefaultModel:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonEchoModelGridTableForm                               *)
(**************************************************************)

Clear[SDMonEchoModelGridTableForm];

SyntaxInformation[SDMonEchoModelGridTableForm] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonEchoModelGridTableForm] = Options[ModelGridTableForm];

SDMonEchoModelGridTableForm[___][$SDMonFailure] := $SDMonFailure;

SDMonEchoModelGridTableForm[xs_, context_Association] := SDMonEchoModelGridTableForm[][xs, context];

SDMonEchoModelGridTableForm[][xs_, context_Association] := SDMonEchoModelGridTableForm[Automatic][xs, context];

SDMonEchoModelGridTableForm[ opts : OptionsPattern[] ][xs_, context_] :=
    SDMonEchoModelGridTableForm[Automatic, opts];

SDMonEchoModelGridTableForm[ spec_, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{model, res},

      model = Fold[ SDMonBind, SDMonUnit[xs, context], {SDMonGetDefaultModel, SDMonTakeValue}];

      If[ TrueQ[ model === $SDMonFailure ],
        Return[$SDMonFailure];
      ];

      Which[
        TrueQ[ spec === Automatic ] || TrueQ[ spec === All ],
        res = ModelGridTableForm[model, opts],

        Length[ Intersection[ Keys[model], Flatten[{spec}] ] ] > 0,
        res = KeyTake[ ModelGridTableForm[model, opts], spec ],

        True,
        Echo["Unknown specification.", "SDMonEchoModelGridTableForm:"];
        Return[$SDMonFailure]
      ];

      Echo[res];

      SDMonUnit[res, context]
    ];

SDMonEchoModelGridTableForm[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonEchoModelGridTableForm[ spec_, opts___ ]"
            <> " or SDMonEchoModelGridTableForm[OptionsPattern[]].",
        "SDMonEchoModelGridTableForm:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonSetInitialConditions                                 *)
(**************************************************************)


(**************************************************************)
(* SDMonMakePolygonGrid                                      *)
(**************************************************************)

Clear[SDMonMakePolygonGrid];

SyntaxInformation[SDMonMakePolygonGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[SDMonMakePolygonGrid] = { "Coordinates" -> None, "Radius" -> None, "BinningFunction" -> Automatic };

SDMonMakePolygonGrid[___][$SDMonFailure] := $SDMonFailure;

SDMonMakePolygonGrid[xs_, context_Association] := SDMonMakePolygonGrid[][xs, context];

SDMonMakePolygonGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{coords, radius},

      coords = OptionValue[ SDMonMakePolygonGrid, "Coordinates" ];

      If[ ! MatchQ[coords, { {_?NumericQ, _?NumericQ} .. }],
        Echo[
          "The value of the option \"Coordinates\" is expected to be a list of numeric pairs.",
          "SDMonMakePolygonGrid:"
        ];
        Return[$SDMonFailure]
      ];

      radius = OptionValue[ SDMonMakePolygonGrid, "Radius" ];

      If[ ! ( NumericQ[radius] && radius > 0 ),
        Echo[
          "The value of the option \"Radius\" is expected to be a positive number.",
          "SDMonMakePolygonGrid:"
        ];
        Return[$SDMonFailure]
      ];

      SDMonMakePolygonGrid[ coords, radius ][xs, context]
    ];

SDMonMakePolygonGrid[coords : { { _?NumericQ, _?NumericQ } .. }, radius_?NumericQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{aGrid},

      aGrid = MakePolygonGrid[coords, radius, FilterRules[{opts}, Options[MakePolygonGrid]] ];

      SDMonUnit[aGrid, Join[context, <| "grid" -> aGrid |>]]
    ];

SDMonMakePolygonGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonMakePolygonGrid[coordinates: { {_?NumericQ, _?NumericQ} .. }, radius_?NumericQ, opts___ ]"
            <> " or SDMonMakePolygonGrid[OptionsPattern[]].",
        "SDMonMakePolygonGrid:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonPlotGrid                                             *)
(**************************************************************)

Clear[SDMonPlotGrid];

SyntaxInformation[SDMonPlotGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[SDMonPlotGrid] = Join[ {"Echo" -> True, "CellIDs" -> False }, Options[Graphics] ];

SDMonPlotGrid[___][$SDMonFailure] := $SDMonFailure;

SDMonPlotGrid[xs_, context_Association] := SDMonPlotGrid[][xs, context];

SDMonPlotGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{echoQ, cellIDsQ, gr},

      echoQ = TrueQ[ OptionValue[SDMonPlotGrid, "Echo"] ];

      cellIDsQ = TrueQ[ OptionValue[SDMonPlotGrid, "CellIDs"] ];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found.", "SDMonPlotGrid:"];
        Return[$SDMonFailure]
      ];

      gr =
          Graphics[
            {
              FaceForm[LightBlue], EdgeForm[Red], Values[Map[#Cell &, context["grid"]["Cells"]]],
              If[ cellIDsQ,
                Values[Map[Text[ #ID, #Center] &, context["grid"]["Cells"]]],
                Nothing
              ]
            },
            FilterRules[ {opts}, Options[Graphics] ],
            ImageSize -> Medium, Frame -> True];

      If[echoQ,
        Echo[ gr, "grid:" ]
      ];

      SDMonUnit[gr, context]
    ];

SDMonPlotGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is SDMonPlotGrid[OptionsPattern[]].",
        "SDMonPlotGrid:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonPlotGridHistogram                                    *)
(**************************************************************)

Clear[SDMonPlotGridHistogram];

SyntaxInformation[SDMonPlotGridHistogram] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[SDMonPlotGridHistogram] = Join[ {"Echo" -> True, "ShowDataPoints" -> True }, Options[HextileHistogram] ];

SDMonPlotGridHistogram[___][$SDMonFailure] := $SDMonFailure;

SDMonPlotGridHistogram[xs_, context_Association] := SDMonPlotGridHistogram[None][xs, context];

SDMonPlotGridHistogram[ aData_?CoordinatesToValuesAssociationQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{echoQ, showDataPointsQ, aDataToGrid, histFunc, grHexHist, gr},

      echoQ = TrueQ[ OptionValue[SDMonPlotGridHistogram, "Echo"] ];

      showDataPointsQ = TrueQ[ OptionValue[SDMonPlotGridHistogram, "ShowDataPoints"] ];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found.", "SDMonPlotGridHistogram:"];
        Return[$SDMonFailure]
      ];

      aDataToGrid = AggregateForCellIDs[ context["grid"], aData];

      histFunc =
          If[ Length[PolygonCoordinates[ context["grid"]["Cells"][[1]]["Cell"]]] == 4,
            TileHistogram,
            (*ELSE*)
            HextileHistogram
          ];

      grHexHist =
          histFunc[ aData, context["grid"]["CellRadius"],
            FilterRules[{opts}, Options[histFunc]],
            ColorFunction -> (Opacity[#, Blue] &),
            PlotRange -> All, ImageSize -> Medium];

      If[ showDataPointsQ,
        gr =
            Show[{
              grHexHist,
              Graphics[{Red, PointSize[0.001 * context["grid"]["CellRadius"]], Point[Keys[aData]]}]
            }],
        (* ELSE *)
        gr = grHexHist
      ];

      If[echoQ,
        Echo[ gr, "grid:" ]
      ];

      SDMonUnit[gr, context]
    ];

SDMonPlotGridHistogram[__][___] :=
    Block[{},
      Echo[
        "The expected signature is SDMonPlotGrid[data : <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, OptionsPattern[]].",
        "SDMonPlotGridHistogram:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonExtendByGrid                                         *)
(**************************************************************)

Clear[SDMonExtendByGrid];

SyntaxInformation[SDMonExtendByGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[SDMonExtendByGrid] = { "Grid" -> None, "Populations" -> None, "Factor" -> 1 };

SDMonExtendByGrid[___][$SDMonFailure] := $SDMonFailure;

SDMonExtendByGrid[xs_, context_Association] := SDMonExtendByGrid[][xs, context];

SDMonExtendByGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{grid, populations, factor},

      grid = OptionValue[ SDMonMakePolygonGrid, "Grid" ];

      If[ ! GridObjectQ[grid],
        Echo[
          "The value of the option \"Grid\" is expected to be a grid object. (See GridObjectQ.)",
          "SDMonExtendByGrid:"
        ];
        Return[$SDMonFailure]
      ];

      populations = OptionValue[ SDMonMakePolygonGrid, "Populations" ];

      If[ ! CoordinatesToValuesAssociationQ[populations],
        Echo[
          "The value of the option \"Populations\" is expected to be a association of coordinates to values.",
          "SDMonExtendByGrid:"
        ];
        Return[$SDMonFailure]
      ];

      factor = OptionValue[ SDMonExtendByGrid, "Factor" ];

      If[ ! ( NumericQ[factor] && factor > 0 ),
        Echo[
          "The value of the option \"Factor\" is expected to be a positive number.",
          "SDMonExtendByGrid:"
        ];
        Return[$SDMonFailure]
      ];

      SDMonExtendByGrid[ grid, factor ][xs, context]
    ];

SDMonExtendByGrid[ aPopulations_Association, factor_?NumericQ ][xs_, context_] :=
    Block[{},
      If[ KeyExistsQ[context, "grid"],
        SDMonExtendByGrid[context["grid"], aPopulations, factor][xs, context],
        (* ELSE *)
        SDMonExtendByGrid[None][xs, context]
      ]
    ];

SDMonExtendByGrid[aGrid_?GridObjectQ, aPopulations_Association, factor_?NumericQ ][xs_, context_] :=
    Block[{ aICValues, matGridTraffic, singleSiteModel, modelMultiSite},

      (* Check is there are context member gridPopulations. *)
      (* If yes, use the gridPopulations to make the matrix. *)
      aICValues = AggregateForCellIDs[aGrid, aPopulations ];

      matGridTraffic =
          SparseArray[
            Map[ (List @@ #) -> factor * Mean[Map[aICValues[#] &, List @@ #]] &, Most[ArrayRules[aGrid["AdjacencyMatrix"]]][[All, 1]] ],
            Dimensions[aGrid["AdjacencyMatrix"]]
          ];

      If[ !KeyExistsQ[ context, "singleSiteModel"],
        Echo["No single-site, seed model is found. Making one with SEI2RModel.", "SDMonExtendByGrid:"];
        singleSiteModel = SEI2RModel[Global`t, "InitialConditions" -> True, "RateRules" -> True, "TotalPopulationRepresentation" -> "AlgebraicEquation"],
        (*ELSE*)
        singleSiteModel = context["singleSiteModel"]
      ];

      modelMultiSite = ToSiteCompartmentsModel[ singleSiteModel, matGridTraffic, "MigratingPopulations" -> Automatic];

      SDMonUnit[modelMultiSite, Join[context, <| "singleSiteModel" -> singleSiteModel, "multiSiteModel" -> modelMultiSite |>]]

    ] /; CoordinatesToValuesAssociationQ[aPopulations];

SDMonExtendByGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonExtendByGrid[ grid_?GridObjectQ, populations: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, factor_?NumericQ ]"
            <> " or SDMonExtendByGrid[OptionsPattern[]].",
        "SDMonExtendByGrid:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonExtendByGraph                                        *)
(**************************************************************)

Clear[SDMonExtendByGraph];

SyntaxInformation[SDMonExtendByGraph] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SDMonExtendByGraph] = { "Graph" -> None, "Factor" -> 1 };

SDMonExtendByGraph[___][$SDMonFailure] := $SDMonFailure;

SDMonExtendByGraph[xs_, context_Association] := SDMonExtendByGraph[][xs, context];

SDMonExtendByGraph[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{graph, factor},

      graph = OptionValue[ SDMonExtendByGraph, "Graph" ];

      If[ ! MatrixQ[graph, NumberQ],
        Echo[
          "The value of the option \"Graph\" is expected to be a numerical matrix.",
          "SDMonExtendByGraph:"
        ];
        Return[$SDMonFailure]
      ];

      factor = OptionValue[ SDMonExtendByGraph, "Factor" ];

      If[ ! ( NumericQ[factor] && factor > 0 ),
        Echo[
          "The value of the option \"Factor\" is expected to be a positive number.",
          "SDMonExtendByGraph:"
        ];
        Return[$SDMonFailure]
      ];

      SDMonExtendByGraph[ graph ][xs, context]
    ];

SDMonExtendByGraph[ graph_Graph, factor_?NumberQ : 1.0 ][xs_, context_] :=
    Block[{matAdj},

      matAdj = AdjacencyMatrix[graph] * factor;

      Fold[ SDMonBind, SDMonUnit[xs, context], { SDMonExtendByAdjacencyMatrix, SDMonAddToContext[<|"graph" -> graph|>] }]
    ];

SDMonExtendByGraph[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonExtendByGraph[ graph_Graph, factor_?NumberQ ]"
            <> " or SDMonExtendByGraph[OptionsPattern[]].",
        "SDMonExtendByGraph:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonExtendByAdjacencyMatrix                              *)
(**************************************************************)

Clear[SDMonExtendByAdjacencyMatrix];

SyntaxInformation[SDMonExtendByAdjacencyMatrix] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonExtendByAdjacencyMatrix] = { "AdjacencyMatrix" -> None, "MigratingPopulations" -> Automatic };

SDMonExtendByAdjacencyMatrix[___][$SDMonFailure] := $SDMonFailure;

SDMonExtendByAdjacencyMatrix[xs_, context_Association] := SDMonExtendByAdjacencyMatrix[][xs, context];

SDMonExtendByAdjacencyMatrix[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{adjacencyMatrix},

      adjacencyMatrix = OptionValue[ SDMonExtendByAdjacencyMatrix, "AdjacencyMatrix" ];

      If[ ! MatrixQ[adjacencyMatrix, NumberQ],
        Echo[
          "The value of the option \"AdjacencyMatrix\" is expected to be a numerical matrix.",
          "SDMonExtendByAdjacencyMatrix:"
        ];
        Return[$SDMonFailure]
      ];

      SDMonExtendByAdjacencyMatrix[ adjacencyMatrix, opts ][xs, context]
    ];

SDMonExtendByAdjacencyMatrix[ matAdj_?MatrixQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{ singleSiteModel, modelMultiSite},

      If[ !KeyExistsQ[ context, "singleSiteModel"],
        Echo["No single-site, seed model is found. Making one with SEI2RModel.", "SDMonExtendByGrid:"];
        singleSiteModel = SEI2RModel[Global`t, "InitialConditions" -> True, "RateRules" -> True, "TotalPopulationRepresentation" -> "AlgebraicEquation"],
        (*ELSE*)
        singleSiteModel = context["singleSiteModel"]
      ];

      (* Maybe we should also make a corresponding grid object. *)

      modelMultiSite = ToSiteCompartmentsModel[ singleSiteModel, matAdj, FilterRules[{opts}, Options[ToSiteCompartmentsModel]] ];

      SDMonUnit[modelMultiSite, Join[context, <| "singleSiteModel" -> singleSiteModel, "multiSiteModel" -> modelMultiSite, "adjacencyMatrix" -> matAdj |>]]

    ] /; MatrixQ[matAdj, NumberQ];

SDMonExtendByAdjacencyMatrix[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonExtendByAdjacencyMatrix[ mat_?MatrixQ ]"
            <> " or SDMonExtendByAdjacencyMatrix[OptionsPattern[]].",
        "SDMonExtendByAdjacencyMatrix:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonMakeTravelingPatternsMatrix                          *)
(**************************************************************)

Clear[SDMonMakeTravelingPatternsMatrix];

SyntaxInformation[SDMonMakeTravelingPatternsMatrix] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SDMonMakeTravelingPatternsMatrix] = { "OriginDestinationToTravelers" -> None, "LocationToLongitudeLatitude" -> None };

SDMonMakeTravelingPatternsMatrix[___][$SDMonFailure] := $SDMonFailure;

(*SDMonMakeTravelingPatternsMatrix[xs_, context_Association] := $SDMonFailure;*)

SDMonMakeTravelingPatternsMatrix[][xs_, context_Association] := $SDMonFailure;

SDMonMakeTravelingPatternsMatrix[ aOriginDestinationToTravelers_Association, aLocationToLonLat_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{matAT, aCellIDToLocation, aLocationToCellID, aLonLatToLocation, aOriginDestinationToTravelersByCells, m, n},

      If[ !MatchQ[ aOriginDestinationToTravelers, Association[ ( {_, _} -> _?NumberQ ) .. ] ],
        Echo["The first argument is expected to a be an association that maps origin-destination pairs to values.", "SDMonMakeTravelingPatternsMatrix:"];
        Return[$SDMonFailure]
      ];

      If[ !MatchQ[ aLocationToLonLat, Association[ ( _ -> { _?NumberQ, _?NumberQ } ) .. ] ],
        Echo["The second argument is expected to a be an association that location ID's to coordinates (longitude-latitude pairs.)", "SDMonMakeTravelingPatternsMatrix:"];
        Return[$SDMonFailure]
      ];

      If[ !KeyExistsQ[context, "grid"],
        Echo["Cannot find grid object.", "SDMonMakeTravelingPatternsMatrix:"];
        Return[$SDMonFailure]
      ];

      aLonLatToLocation = Association[ Reverse /@ Normal[aLocationToLonLat] ];

      aCellIDToLocation = AggregateForCellIDs[ context["grid"], aLonLatToLocation, "AggregationFunction" -> Identity];

      aLocationToCellID = Association[Flatten[Thread[Reverse[#]] & /@ Normal[aCellIDToLocation]]];

      (* "Modify the airport-to-airport contingency matrix to cellID-to-cellID ... *)

      aOriginDestinationToTravelersByCells = KeyMap[ # /. aLocationToCellID&, aOriginDestinationToTravelers];

      matAT = CrossTabulate[ Map[ Flatten, List @@@ Normal[aOriginDestinationToTravelersByCells] ], "Sparse" -> True];
      matAT = ToSSparseMatrix[matAT];

      {m, n} = Dimensions[context["grid"]["AdjacencyMatrix"]];
      matAT = ImposeColumnNames[matAT, ToString /@ Range[n]];
      matAT = ImposeRowNames[matAT, ToString /@ Range[m]];

      SDMonUnit[matAT, context]
    ];

SDMonMakeTravelingPatternsMatrix[__][__] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonMakeTravelingPatternsMatrix[ _Association, _Association, opts___ ]"
            <> " or SDMonMakeTravelingPatternsMatrix[OptionsPattern[]].",
        "SDMonMakeTravelingPatternsMatrix:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonEvaluateSolutionsOverGraph                           *)
(**************************************************************)

Clear[SDMonEvaluateSolutionsOverGraph];

SyntaxInformation[SDMonEvaluateSolutionsOverGraph] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonEvaluateSolutionsOverGraph] =
    Join[
      { "Stocks" -> "Infected Normally Symptomatic Population", "TimeRange" -> None },
      Options[EvaluateSolutionsOverGraph]
    ];

SDMonEvaluateSolutionsOverGraph[___][$SDMonFailure] := $SDMonFailure;

SDMonEvaluateSolutionsOverGraph[xs_, context_Association] := SDMonEvaluateSolutionsOverGraph[][xs, context];

SDMonEvaluateSolutionsOverGraph[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, timeRange},

      stocks = OptionValue[ SDMonEvaluateSolutionsOverGraph, "Stocks" ];

      If[ ! MatchQ[ stocks, ( _String | { _String ..} | _StringExpression | { _StringExpression ..} ), population ],
        Echo[
          "The value of the option \"Stocks\" is expected to be a valid stocks specification: " <>
              "( _String | { _String ..} | _StringExpression | { _StringExpression ..} ).",
          "SDMonEvaluateSolutionsOverGraph:"
        ];
        Return[$SDMonFailure]
      ];

      timeRange = OptionValue[ SDMonEvaluateSolutionsOverGraph, "TimeRange" ];

      If[ ! MatchQ[ timeRange, _?NumberQ | {_?NumberQ, _?NumberQ} | {_?NumberQ, _?NumberQ, _?NumberQ} ],
        Echo[
          "The value of the option \"TimeRange\" is expected to be a valid range specification: " <>
              "( maxTime_?NumberQ | {minTime_?NumberQ, maxTime_?NumberQ} | {minTime_?NumberQ, maxTime_?NumberQ, timeStep_?NumberQ} ) .",
          "SDMonEvaluateSolutionsOverGraph:"
        ];
        Return[$SDMonFailure]
      ];

      SDMonEvaluateSolutionsOverGraph[ population, timeRange, opts ][xs, context]
    ];

SDMonEvaluateSolutionsOverGraph[ populationSpec_, timeRangeSpec_, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{gr, matAdj, model, sol, res},

      (* Separate checks if grid object is present, or only adjacency matrix is present. *)
      If[ KeyExistsQ[context, "graph"],
        gr = context["graph"],
        (*ELSE*)
        matAdj = SDMonBind[ SDMonUnit[xs, context], SDMonTakeAdjacencyMatrix ];
        Print[matAdj];
        If[ !MatrixQ[ matAdj ],
          Echo["Cannot find graph or adjacency matrix.", "SDMonEvaluateSolutionsOverGraph:" ];
          Return[$SDMonFailure]
        ];

        gr = AdjacencyGraph[ Unitize[matAdj] ];

        Echo[
          "Using a graph made from the unitized version of the adjacency matrix in the context.",
          "SDMonEvaluateSolutionsOverGraph:"
        ]
      ];

      If[ KeyExistsQ[context, "multiSiteModel"],
        model = context["multiSiteModel"],
        (*ELSE*)
        Echo["Cannot find multi-site model.", "SDMonEvaluateSolutionsOverGraph:" ];
        Return[$SDMonFailure]
      ];

      If[ KeyExistsQ[context, "solution"],
        sol = context["solution"],
        (*ELSE*)
        Echo["Cannot find solution.", "SDMonEvaluateSolutionsOverGraph:" ];
        Return[$SDMonFailure]
      ];

      res = EvaluateSolutionsOverGraph[ gr, model, populationSpec, sol, timeRangeSpec, FilterRules[ {opts}, Options[EvaluateSolutionsOverGraph] ] ];

      SDMonUnit[res, context]
    ];

SDMonEvaluateSolutionsOverGraph[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonEvaluateSolutionsOverGraph[  ]"
            <> " or SDMonEvaluateSolutionsOverGraph[OptionsPattern[]].",
        "SDMonEvaluateSolutionsOverGraph:"];
      $SDMonFailure
    ];



(**************************************************************)
(* SDMonEvaluateSolutionsOverGrid                            *)
(**************************************************************)

Clear[SDMonEvaluateSolutionsOverGrid];

SyntaxInformation[SDMonEvaluateSolutionsOverGrid] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonEvaluateSolutionsOverGrid] = Options[SDMonGetSolutionValues];

SDMonEvaluateSolutionsOverGrid[___][$SDMonFailure] := $SDMonFailure;

SDMonEvaluateSolutionsOverGrid[xs_, context_Association] := SDMonEvaluateSolutionsOverGrid[][xs, context];


SDMonEvaluateSolutionsOverGrid[ populationSpec_, timeRangeSpec_, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},

      (*      res = *)

      SDMonUnit[res, context]
    ];

SDMonEvaluateSolutionsOverGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonEvaluateSolutionsOverGrid[  ]"
            <> " or SDMonEvaluateSolutionsOverGrid[OptionsPattern[]].",
        "SDMonEvaluateSolutionsOverGrid:"];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonAssignInitialConditionsByGridAggregation             *)
(**************************************************************)

Clear[SDMonAssignInitialConditionsByGridAggregation];

SyntaxInformation[SDMonAssignInitialConditionsByGridAggregation] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[SDMonAssignInitialConditionsByGridAggregation] = { "Default" -> 0 };

SDMonAssignInitialConditionsByGridAggregation[___][$SDMonFailure] := $SDMonFailure;

SDMonAssignInitialConditionsByGridAggregation[xs_, context_Association] := SDMonAssignInitialConditionsByGridAggregation[][xs, context];

SDMonAssignInitialConditionsByGridAggregation[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "SDMonAssignInitialConditions:"];
      $SDMonFailure
    ];

SDMonAssignInitialConditionsByGridAggregation[ aCoordsToValues_Association, stockName_String, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{default, modelMultiSite, aICValues, stockSymbol},

      default = OptionValue[SDMonAssignInitialConditionsByGridAggregation, "Default"];

      If[ ! ( NumericQ[default] && default >= 0 ),
        Echo["The value of the option \"Default\" is expected to be a non-negative number.", "SDMonAssignInitialConditionsByGridAggregation:"];
        Return[$SDMonFailure]
      ];

      If[ !KeyExistsQ[ context, "multiSiteModel"],
        Echo["No multi-site model is found. (Required for this signature.)", "SDMonAssignInitialConditionsByGridAggregation:"];
        Return[$SDMonFailure]
      ];

      modelMultiSite = context["multiSiteModel"];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found. (Required for this signature.)", "SDMonAssignInitialConditionsByGridAggregation:"];
        Return[$SDMonFailure]
      ];

      If[ ! MemberQ[ Values[modelMultiSite["Stocks"]], stockName ],
        Echo["The second argument, \"" <> stockName <> "\", is expected to be one of the stock names: " <> ToString[Union[Values[modelMultiSite["Stocks"]]]],
          "SDMonAssignInitialConditions:"];
        Return[$SDMonFailure]
      ];

      If[ Length[aCoordsToValues] > 0,

        aICValues = AggregateForCellIDs[context["grid"], aCoordsToValues ];

        stockSymbol = Head@First@GetStockSymbols[ modelMultiSite, stockName];

        modelMultiSite =
            SetInitialConditions[
              modelMultiSite,
              Join[Association@Map[#[0] -> default &, GetPopulationSymbols[modelMultiSite, stockName]], Association[KeyValueMap[With[{sh = stockSymbol}, sh[#1][0] -> #2 ]&, aICValues]]]
            ],
        (* ELSE *)
        modelMultiSite =
            SetInitialConditions[
              modelMultiSite,
              Association@Map[#[0] -> default &, GetPopulationSymbols[modelMultiSite, stockName]]
            ]
      ];

      SDMonUnit[modelMultiSite, Join[context, <| "multiSiteModel" -> modelMultiSite |>]]

    ] /; CoordinatesToValuesAssociationQ[aCoordsToValues];

SDMonAssignInitialConditionsByGridAggregation[__][___] :=
    Block[{},
      Echo[
        "The expected signature is SDMonAssignInitialConditionsByGridAggregation[ coordsToValues: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, stockName_String, opts ].",
        "SDMonAssignInitialConditionsByGridAggregation:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonAssignInitialConditions                              *)
(**************************************************************)

Clear[SDMonAssignInitialConditions];

SyntaxInformation[SDMonAssignInitialConditions] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SDMonAssignInitialConditions] = Options[SDMonAssignInitialConditionsByGridAggregation];

SDMonAssignInitialConditions[___][$SDMonFailure] := $SDMonFailure;

SDMonAssignInitialConditions[xs_, context_Association] := SDMonAssignInitialConditions[][xs, context];

SDMonAssignInitialConditions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "SDMonAssignInitialConditions:"];
      $SDMonFailure
    ];

SDMonAssignInitialConditions[ stockVals : { _Rule .. }, opts : OptionsPattern[] ][xs_, context_] :=
    SDMonAssignInitialConditions[ Association[stockVals], opts ][xs, context];

SDMonAssignInitialConditions[ stockVal_Rule, opts : OptionsPattern[] ][xs_, context_] :=
    SDMonAssignInitialConditions[ <|stockVal|>, opts ][xs, context];

SDMonAssignInitialConditions[ aStockValues_Association ][xs_, context_] :=
    Block[{ model, aContextAddition },

      (* Maybe we should be able to use SDMonGetDefaultModel here. *)
      Which[

        KeyExistsQ[ context, "multiSiteModel"],
        model = context["multiSiteModel"];
        model = AssignInitialConditions[ model, aStockValues ];
        aContextAddition = <| "multiSiteModel" -> model |>,

        KeyExistsQ[ context, "singleSiteModel"],
        model = context["singleSiteModel"];
        model = AssignInitialConditions[ model, aStockValues ];
        aContextAddition = <| "singleSiteModel" -> model |>,

        True,
        Echo["Cannot find a model.", "SDMonAssignInitialConditions:"];
        Return[$SDMonFailure]
      ];

      SDMonUnit[model, Join[context, aContextAddition]]

    ];

SDMonAssignInitialConditions[ aCoordsToValues_Association, stockName_String, opts : OptionsPattern[] ][xs_, context_] :=
    SDMonAssignInitialConditionsByGridAggregation[ aCoordsToValues, stockName, opts ][xs, context];

SDMonAssignInitialConditions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of " <>
            "SDMonAssignInitialConditions[ aStockValues_Association] or " <>
            "SDMonAssignInitialConditions[ coordsToValues: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, stockName_String, opts ].",
        "SDMonAssignInitialConditions:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonAssignRateRules                                      *)
(**************************************************************)

Clear[SDMonAssignRateRules];

SyntaxInformation[SDMonAssignRateRules] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

SDMonAssignRateRules[___][$SDMonFailure] := $SDMonFailure;

SDMonAssignRateRules[xs_, context_Association] := SDMonAssignRateRules[][xs, context];

SDMonAssignRateRules[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "SDMonAssignInitialConditions:"];
      $SDMonFailure
    ];

SDMonAssignRateRules[ rateVals : { _Rule .. }, opts : OptionsPattern[] ][xs_, context_] :=
    SDMonAssignRateRules[ Association[rateVals], opts ][xs, context];

SDMonAssignRateRules[ rateVal_Rule, opts : OptionsPattern[] ][xs_, context_] :=
    SDMonAssignRateRules[ <|rateVal|>, opts ][xs, context];

SDMonAssignRateRules[ aRateRules_Association ][xs_, context_] :=
    Block[{ model, aContextAddition },

      Which[

        KeyExistsQ[ context, "multiSiteModel"],
        model = context["multiSiteModel"];
        model = AssignRateRules[ model, aRateRules ];
        aContextAddition = <| "multiSiteModel" -> model |>,

        KeyExistsQ[ context, "singleSiteModel"],
        model = context["singleSiteModel"];
        model = AssignRateRules[ model, aRateRules ];
        aContextAddition = <| "singleSiteModel" -> model |>,

        True,
        Echo["Cannot find a model.", "SDMonAssignRateRules:"];
        Return[$SDMonFailure]
      ];

      SDMonUnit[model, Join[context, aContextAddition]]

    ];

SDMonAssignRateRules[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonAssignRateRules[ aRateRules_Association].",
        "SDMonAssignRateRules:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonSimulate                                             *)
(**************************************************************)

Clear[SDMonSimulate];

SyntaxInformation[SDMonSimulate] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[SDMonSimulate] = Join[ { "MaxTime" -> 365, "TimeVariable" -> Automatic }, Options[NDSolve] ];

SDMonSimulate[___][$SDMonFailure] := $SDMonFailure;

SDMonSimulate[xs_, context_Association] := SDMonAssignInitialConditionsByGridAggregation[][xs, context];

SDMonSimulate[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{time},

      time = OptionValue[SDMonSimulate, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo["The value of the option \"MaxTime\" is expected to be a non-negative number.", "SDMonSimulate:"];
        Return[$SDMonFailure]
      ];

      SDMonSimulate[ time, opts][xs, context]
    ];

SDMonSimulate[ maxTime_?NumericQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{aSol, model, timeVar},

      If[ ! ( NumericQ[maxTime] && maxTime >= 0 ),
        Echo["The first argument is expected to be a non-negative number.", "SDMonSimulate:"];
        Return[$SDMonFailure]
      ];

      model = Fold[ SDMonBind, SDMonUnit[xs, context], {SDMonGetDefaultModel, SDMonTakeValue}];

      If[ TrueQ[ model === $SDMonFailure ],
        Echo["Cannot find a model.", "SDMonSimulate:"];
        Return[$SDMonFailure];
      ];

      timeVar = OptionValue[SDMonSimulate, "TimeVariable"];

      If[ TrueQ[ timeVar === Automatic ],
        timeVar = ReverseSortBy[Tally[Cases[Keys[model["Stocks"]], _Symbol, Infinity]], #[[2]] &][[1, 1]];
        If[ ! Developer`SymbolQ[timeVar], timeVar = Global`t ]
      ];

      If[ ! Developer`SymbolQ[timeVar],
        Echo["The value of the option \"TimeVariable\" is expected to be a symbol or Automatic.", "SDMonSimulate:"];
        Return[$SDMonFailure]
      ];

      aSol = Association @ First @ ModelNDSolve[ model, {timeVar, 0, maxTime}, FilterRules[{opts}, Options[NDSolve]] ];

      If[ !( KeyExistsQ[context, "singleSiteModel"] || KeyExistsQ[context, "multiSiteModel" ] ),
        SDMonUnit[aSol, Join[ context, <| "singleSiteModel" -> model, "solution" -> aSol |>]],
        (* ELSE *)
        SDMonUnit[aSol, Join[ context, <| "solution" -> aSol |>]]
      ]
    ];

SDMonSimulate[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonSimulate[ maxTime_?NumericQ, opts___ ].",
        "SDMonSimulate:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonGetSolutionValues                                    *)
(**************************************************************)

Clear[TimeSpecQ];
TimeSpecQ[spec_] :=
    ( NumericQ[spec] && spec >= 0 ) || MatchQ[spec, {_?NumericQ}] && spec[[1]] >= 0 ||
        ( MatchQ[spec, {_?NumericQ, _?NumericQ}] || MatchQ[spec, {_?NumericQ, _?NumericQ, _?NumericQ}] ) && spec[[1]] <= spec[[2]];

Clear[ToTimeRangeSpec];
ToTimeRangeSpec[timeSpec_?TimeSpecQ] :=
    Switch[timeSpec,

      _?NumericQ, {0, timeSpec},

      {_?NumericQ}, {timeSpec[[1]], timeSpec[[1]]},

      _, timeSpec
    ];

Clear[SDMonGetSolutionValues];

SyntaxInformation[SDMonGetSolutionValues] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SDMonGetSolutionValues] = { "Stocks" -> All, "TimeSpecification" -> 365 };

SDMonGetSolutionValues[___][$SDMonFailure] := $SDMonFailure;

SDMonGetSolutionValues[xs_, context_Association] := SDMonGetSolutionValues[][xs, context];

SDMonGetSolutionValues[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, time},

      stocks = OptionValue[SDMonGetSolutionValues, "Stocks"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _Symbol | {_Symbol..} ) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: All | ( _String | {_String..} | _Symbol | {_Symbol..} ).",
          "SDMonGetSolutionValues:"];
        Return[$SDMonFailure]
      ];

      time = OptionValue[SDMonGetSolutionValues, "TimeSpecification"];

      If[ ! TimeSpecQ[time],
        Echo[
          "The value of the option \"TimeSpecification\" is expected to be a non-negative number (corresponding to max time) or a time range specification: " <>
              "( {minTime_?NumberQ, maxTime_?NumberQ} || {minTime_?NumberQ, maxTime_?NumberQ, step_?NumberQ} ).",
          "SDMonGetSolutionValues:"];
        Return[$SDMonFailure]
      ];

      SDMonGetSolutionValues[ stocks, time, opts][xs, context]
    ];

SDMonGetSolutionValues[
  stocksSpecArg : All | ( _String | {_String..} | _StringExpression | _Symbol | {_Symbol..} ),
  timeSpec_?TimeSpecQ,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{stocksSpec = Flatten[{stocksSpecArg}], multiSiteQ, lsTimes, res},

      Which[
        KeyExistsQ[ context, "multiSiteModel"],
        multiSiteQ = True,

        KeyExistsQ[ context, "singleSiteModel"],
        multiSiteQ = False,

        True,
        Echo["Cannot find a model.", "SDMonGetSolutionValues:"];
        Return[$SDMonFailure]
      ];

      If[ ! KeyExistsQ[ context, "solution"],
        Echo["Cannot find solution.", "SDMonGetSolutionValues:"];
        Return[$SDMonFailure]
      ];

      (* Shouldn't this be handled by GetStocks? *)
      Which[
        multiSiteQ && TrueQ[stocksSpecArg === All],
        stocksSpec = Union[ Values[context["multiSiteModel"]["Stocks"]] ],

        multiSiteQ && MatchQ[stocksSpec, {_StringExpression..} | {_Symbol..}],
        stocksSpec = GetStocks[ context["multiSiteModel"], stocksSpec ] /. context["multiSiteModel"]["Stocks"],

        !multiSiteQ && TrueQ[stocksSpecArg === All],
        stocksSpec = Union[ Values[context["singleSiteModel"]["Stocks"]] ],

        !multiSiteQ && MatchQ[stocksSpec, {_StringExpression..} | {_Symbol..}],
        stocksSpec = GetStocks[ context["singleSiteModel"], stocksSpec ] /. context["singleSiteModel"]["Stocks"]
      ];

      If[ multiSiteQ,
        res = Association @ Map[ # -> EvaluateSolutionsByModelIDs[context["multiSiteModel"], #, context["solution"], ToTimeRangeSpec[timeSpec] ]&, stocksSpec],

        (*ELSE*)
        lsTimes = Range @@ ToTimeRangeSpec[timeSpec];

        stocksSpec = Union @ Flatten @ Map[ GetStockSymbols[context["singleSiteModel"], #]&, stocksSpec ];

        res = Map[ #[ lsTimes ]&, KeyTake[ context["solution"], stocksSpec ] ];
      ];

      SDMonUnit[ res, context ]
    ];

SDMonGetSolutionValues[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonGetSolutionValues[ stockSpec : All | ( _String | {_String..} | _StringExpression ), timeSpec : { _?NumericQ | {minTime_?NumberQ, maxTime_?NumberQ, step_?NumberQ} }, opts___ ]" <>
            " or SDMonGetSolutionValues[opts___].",
        "SDMonGetSolutionValues:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonBatchSimulate                                        *)
(**************************************************************)

Clear[SDMonBatchSimulate];

SyntaxInformation[SDMonBatchSimulate] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[SDMonBatchSimulate] =
    Union @ Join[
      { "Stocks" -> All, "Parameters" -> All, "MaxTime" -> 365 },
      Options[SDMonSimulate]
    ];

SDMonBatchSimulate[___][$SDMonFailure] := $SDMonFailure;

SDMonBatchSimulate[xs_, context_Association] := SDMonBatchSimulate[][xs, context];

SDMonBatchSimulate[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, params, time},

      stocks = OptionValue[SDMonBatchSimulate, "Stocks"];

      If[ ! StocksSpecQ[stocks],
        Echo[
          "The value of the option \"Stocks\" is expected to be a string, a string pattern, a list of strings, or a list of string patterns.",
          "SDMonBatchSimulate:"];
        Return[$SDMonFailure]
      ];

      params = OptionValue[SDMonBatchSimulate, "Parameters"];

      If[ ! MatchQ[ params, { _Association .. } | <| ( _ -> { _?NumberQ .. } ) .. |> ],
        Echo[
          "The value of the option \"Parameters\" is expected to be a list of associations or an association of parameter numerical lists.",
          "SDMonBatchSimulate:"];
        Return[$SDMonFailure]
      ];

      time = OptionValue[SDMonBatchSimulate, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo[
          "The value of the option \"MaxTime\" is expected to be a non-negative number.",
          "SDMonBatchSimulate:"];
        Return[$SDMonFailure]
      ];

      SDMonBatchSimulate[ stocks, params, time, opts][xs, context]
    ];

SDMonBatchSimulate[
  stockSpec_?StocksSpecQ,
  params : <| ( _ -> { _?NumericQ .. } ) .. |>,
  maxTime_?NumericQ,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{lsProcessedParams},

      (* Here we can / should are the parameters known in the model. *)
      (* Not necessary, because unknown rates are going to be ignored. *)
      lsProcessedParams = Flatten @ Outer[AssociationThread[Keys[params], List[##]] &, Sequence @@ Values[params]];

      SDMonBatchSimulate[ stockSpec, lsProcessedParams, maxTime, opts ][xs, context]

    ];

SDMonBatchSimulate[
  stockSpec_?StocksSpecQ,
  params : { _Association .. },
  maxTime_?NumericQ,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{res},

      res =
          Association @
              Map[
                Function[{p},
                  p ->
                      Fold[
                        SDMonBind,
                        SDMonUnit[xs, context],
                        {
                          SDMonAssignRateRules[ p ],
                          SDMonSimulate[maxTime, FilterRules[{opts}, Options[SDMonSimulate]] ],
                          SDMonGetSolutionValues[ stockSpec, maxTime ],
                          SDMonTakeValue
                        }
                      ]
                ],
                params
              ];

      SDMonUnit[ res, context ]
    ];

SDMonBatchSimulate[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of" <>
            " SDMonBatchSimulate[ stockSpec : ( All | _String | {_String..} | _StringExpression | {_StringExpression ..} ), params : { _Association .. } | <| ( _ -> { _?NumberQ ..} ) .. |>, maxTime_?NumericQ, opts___ ]" <>
            " or SDMonBatchSimulate[opts___].",
        "SDMonBatchSimulate:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonCalibrate                                        *)
(**************************************************************)

Clear[SDMonCalibrate];

SyntaxInformation[SDMonCalibrate] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SDMonCalibrate] =
    Join[
      { "Target" -> None, "Parameters" -> All, "StockWeights" -> Automatic, DistanceFunction -> EuclideanDistance },
      { Method -> {"NelderMead", "PostProcess" -> False} },
      Options[SDMonSimulate],
      DeleteCases[ Options[NMinimize], Method -> _ ]
    ];

SDMonCalibrate[___][$SDMonFailure] := $SDMonFailure;

SDMonCalibrate[xs_, context_Association] := SDMonCalibrate[][xs, context];

SDMonCalibrate[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{target, params},

      target = OptionValue[SDMonCalibrate, "Target"];

      If[ ! MatchQ[ target, <| ( _ -> { _?NumberQ .. } ) .. |> ],
        Echo[
          "The value of the option \"Target\" is expected to be a list of associations or an association of stocks numerical lists.",
          "SDMonBatchSimulate:"];
        Return[$SDMonFailure]
      ];

      params = OptionValue[SDMonCalibrate, "Parameters"];

      If[ ! MatchQ[ params, { _Association .. } | <| ( _ -> { _?NumberQ .. } ) .. |> ],
        Echo[
          "The value of the option \"Parameters\" is expected to be a list of associations or an association of parameter numerical lists.",
          "SDMonCalibrate:"];
        Return[$SDMonFailure]
      ];

      SDMonCalibrate[ target, params, opts][xs, context]
    ];

SDMonCalibrate[
  targetArg : <| ( _ -> _?VectorQ ) .. |>,
  params : <| ( _ -> { _?NumericQ .. } ) .. |>,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{ target = targetArg,
      model, distFunc, stockWeights,
      maxTime, ecmObj, modelEvalFunc, objFunc,
      lsProcessedParams, lsTargetStocks, res},

      (*-----------------------------------------------------*)

      distFunc = OptionValue[SDMonCalibrate, DistanceFunction];

      stockWeights = OptionValue[SDMonCalibrate, "StockWeights" ];
      If[ !( TrueQ[ stockWeights === Automatic ] || AssociationQ[stockWeights] ),
        Echo[ "The value of the options \"StockWeights\" expected to be an association or Automatic.", "SDMonCalibrate:"];
        Return[$SDMonFailure]
      ];

      (*-----------------------------------------------------*)

      model = Fold[ SDMonBind, SDMonUnit[xs, context], {SDMonGetDefaultModel, SDMonTakeValue}];

      If[ TrueQ[ model === $SDMonFailure ],
        Return[$SDMonFailure];
      ];

      target = KeyTake[ target, Join[ GetStocks[model], GetStockSymbols[model]] ];
      If[ Length[target] == 0,
        Echo[ "None of the keys of the first argument are known stocks.", "SDMonCalibrate:"];
        Return[$SDMonFailure]
      ];

      If[ Length[target] < Length[targetArg],
        Echo[ "Some of the keys of the first argument are not known stocks.", "SDMonCalibrate:"];
      ];

      If[ !( Apply[Equal, Map[ Length, Values[target] ] ] && Apply[ And, VectorQ[#, NumericQ]& /@ Values[target] ] ),
        Echo[
          "The first argument is expected to be an association of target numerical vectors that have same length.",
          "SDMonCalibrate:"
        ];
        Return[$SDMonFailure]
      ];

      (*-----------------------------------------------------*)
      If[ TrueQ[stockWeights === Automatic],
        stockWeights = <||>
      ];
      stockWeights = Join[ AssociationThread[ Keys[target], 1 ], stockWeights ];

      maxTime = Length[target[[1]]] - 1; (* Time starts from 0.*)

      ecmObj = SDMonUnit[xs, context];

      lsTargetStocks = Keys[target];

      modelEvalFunc[x_?NumberQ, args___] :=
          Block[{p},
            p = AssociationThread[ Keys[params], {x, args}];

            Fold[
              SDMonBind,
              ecmObj,
              {
                SDMonAssignRateRules[ p ],
                SDMonSimulate[maxTime, FilterRules[{opts}, Options[SDMonSimulate]] ],
                SDMonGetSolutionValues[ lsTargetStocks, maxTime ],
                SDMonTakeValue
              }
            ]
          ];

      stockWeights = KeyTake[stockWeights, Keys[target]];

      objFunc[x_?NumberQ, args___] :=
          Block[{res},
            res = modelEvalFunc[x, args];
            Dot[
              Values[stockWeights],
              MapThread[ distFunc, { Values @ KeyTake[res, Keys[target] ], Values[target] } ]
            ]
          ];

      (*-----------------------------------------------------*)

      lsProcessedParams = KeyValueMap[ Min[#2] <= #1 <= Max[#2]&, params ];

      res = NMinimize[ { objFunc[Evaluate[Sequence @@ Keys[params]]], Sequence @@ lsProcessedParams }, Keys[params], FilterRules[ {opts}, Options[NMinimize] ] ];

      SDMonUnit[res, context]
    ];


SDMonCalibrate[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of" <>
            " SDMonCalibrate[ target_Association, params : { _Association .. } | <| ( _ -> { _?NumberQ ..} ) .. |>, maxTime_?NumericQ, opts___ ]" <>
            " or SDMonCalibrate[opts___].",
        "SDMonCalibrate:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonPlotSolutions                                        *)
(**************************************************************)

Clear[SDMonPlotSolutions];

SyntaxInformation[SDMonPlotSolutions] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonPlotSolutions] =
    Join[
      { "Stocks" -> All, "MaxTime" -> Automatic, "Echo" -> True },
      Options[MultiSiteModelStocksPlot],
      Options[ParametricSolutionsPlots]
    ];

SDMonPlotSolutions[___][$SDMonFailure] := $SDMonFailure;

SDMonPlotSolutions[xs_, context_Association] := SDMonPlotSolutions[][xs, context];

SDMonPlotSolutions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, time},

      stocks = OptionValue[SDMonPlotSolutions, "Stocks"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _StringExpression) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ).",
          "SDMonPlotSolutions:"];
        Return[$SDMonFailure]
      ];

      time = OptionValue[SDMonPlotSolutions, "MaxTime"];

      If[ ! ( TrueQ[time === Automatic] || NumericQ[time] && time >= 0 ),
        Echo[
          "The value of the option \"MaxTime\" is expected to be a non-negative number otr Automatic.",
          "SDMonPlotSolutions:"
        ];
        Return[$SDMonFailure]
      ];

      SDMonPlotSolutions[ stocks, time, opts ][xs, context]
    ];

SDMonPlotSolutions[
  stocksSpecArg : ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ),
  maxTime : ( Automatic | _?NumericQ ),
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{stocksSpec = Flatten[{stocksSpecArg}], echoQ, res, stockSymbols},

      echoQ = TrueQ[ OptionValue[SDMonPlotSolutions, "Echo"] ];

      If[ ! ( TrueQ[maxTime === Automatic] || NumericQ[maxTime] && maxTime >= 0 ),
        Echo[
          "The second argument is expected to be Automatic or a non-negative number.",
          "SDMonPlotSolutions:"];
        Return[$SDMonFailure]
      ];


      Which[

        (*---------*)
        KeyExistsQ[ context, "multiSiteModel"] && KeyExistsQ[ context, "solution"],

        If[TrueQ[ stocksSpec === {All} ],
          stocksSpec = {__ ~~ ___};
        ];

        res =
            MultiSiteModelStocksPlot[ context["multiSiteModel"], stocksSpec, context["solution"], maxTime,
              FilterRules[{opts}, Options[MultiSiteModelStocksPlot]],
              ImageSize -> Medium, PlotTheme -> "Detailed"
            ],

        (*---------*)
        KeyExistsQ[ context, "singleSiteModel"] && KeyExistsQ[ context, "solution"],

        Which[
          TrueQ[ stocksSpec === {All} ],
          stockSymbols = GetStockSymbols[ context["singleSiteModel"] ],

          MatchQ[ stocksSpec, {_String ..} | {_StringExpression ..} ],
          stockSymbols = Flatten[ GetStockSymbols[ context["singleSiteModel"], # ]& /@ stocksSpec ],

          True,
          stockSymbols = GetStockSymbols[ context["singleSiteModel"] ]
        ];

        res =
            ParametricSolutionsPlots[ context["singleSiteModel"]["Stocks"], KeyTake[context["solution"], stockSymbols], None, maxTime,
              FilterRules[{opts}, Options[ParametricSolutionsPlots]],
              "Together" -> True, ImageSize -> Medium, PlotTheme -> "Detailed"
            ],

        (*---------*)
        True,

        Echo["Cannot find a model or solution.", "SDMonPlotSolutions:"];
        $SDMonFailure
      ];

      If[echoQ,
        Echo[ res, "solutions:" ]
      ];

      SDMonUnit[res, context]

    ];

SDMonPlotSolutions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonPlotSolutions[ stockSpec : ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ), maxTime : (Automatic | _?NumericQ), opts___ ]" <>
            " or SDMonPlotSolutions[opts___].",
        "SDMonPlotSolutions:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonPlotSiteSolutions                                    *)
(**************************************************************)

Clear[SDMonPlotSiteSolutions];

SyntaxInformation[SDMonPlotSiteSolutions] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[SDMonPlotSiteSolutions] =
    Join[
      { "CellIDs" -> None, "Stocks" -> All, "MaxTime" -> 365, "Echo" -> True },
      Options[MultiSiteModelStocksPlot],
      Options[ParametricSolutionsPlots]
    ];

SDMonPlotSiteSolutions[___][$SDMonFailure] := $SDMonFailure;

SDMonPlotSiteSolutions[xs_, context_Association] := SDMonAssignInitialConditionsByGridAggregation[][xs, context];

SDMonPlotSiteSolutions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{cellIDs, stocks, time},

      cellIDs = OptionValue[SDMonPlotSiteSolutions, "CellIDs"];

      If[ ! MatchQ[ cellIDs, _Integer | {_Integer..} ],
        Echo[
          "The value of the option \"CellIDs\" is expected to match the pattern: ( _Integer | {_String..} ).",
          "SDMonPlotSiteSolutions:"];
        Return[$SDMonFailure]
      ];

      stocks = OptionValue[SDMonPlotSiteSolutions, "MaxTime"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _Symbol | {_Symbol..} ) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: ( All | _String | {_String..} | _Symbol | {_Symbol..} ).",
          "SDMonPlotSiteSolutions:"];
        Return[$SDMonFailure]
      ];

      time = OptionValue[SDMonPlotSiteSolutions, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo["The value of the option \"MaxTime\" is expected to be a non-negative number.", "SDMonPlotSolutions:"];
        Return[$SDMonFailure]
      ];

      SDMonPlotSiteSolutions[ stocks, time, opts][xs, context]
    ];

SDMonPlotSiteSolutions[
  cellIDs : ( _Integer | { _Integer..} ),
  stocksSpecArg : All | ( _String | {_String..} | _StringExpression),
  maxTime : (Automatic | _?NumericQ),
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{stocksSpec = Flatten[{stocksSpecArg}], echoQ, stockSymbols, res},

      echoQ = TrueQ[ OptionValue[SDMonPlotSiteSolutions, "Echo"] ];

      If[ ! ( TrueQ[maxTime === Automatic] || NumericQ[maxTime] && maxTime >= 0 ),
        Echo["The third argument is expected to be Automatic or a non-negative number.", "SDMonPlotSolutions:"];
        Return[$SDMonFailure]
      ];

      Which[

        (*---------*)
        KeyExistsQ[ context, "multiSiteModel"] && KeyExistsQ[ context, "solution"],

        If[TrueQ[ stocksSpec === {All} ],
          stocksSpec = {__ ~~ ___};
        ];

        res = Association @ Map[ # -> SiteIndexSolutionsPlot[ #, context["multiSiteModel"], stocksSpec, context["solution"], maxTime, FilterRules[{opts}, Options[SiteIndexSolutionsPlot]] ]&, cellIDs],

        (*---------*)
        True,

        Echo["Cannot find a multi-site model or solution.", "SDMonPlotSolutions:"];
        $SDMonFailure
      ];

      If[echoQ,
        Echo[ res, "solutions:" ]
      ];

      SDMonUnit[res, context]

    ];

SDMonPlotSiteSolutions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonPlotSiteSolutions[ stockSpec : All | ( _String | {_String..} | _StringExpression ), maxTime_?NumericQ, opts___ ]" <>
            " or SDMonPlotSiteSolutions[opts___].",
        "SDMonPlotSiteSolutions:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonExportModel                                          *)
(**************************************************************)

Clear[SDMonExportModel];

SyntaxInformation[SDMonExportModel] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonExportModel] = { "FileName" -> Automatic, "Directory" -> Automatic };

SDMonExportModel[___][$SDMonFailure] := $SDMonFailure;

SDMonExportModel[][xs_, context_] := SDMonExportModel[Automatic][xs, context];

SDMonExportModel[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{fileName},

      fileName = OptionValue[SDMonExportModel, "FileName"];

      If[ ! ( TrueQ[fileName === Automatic] || StringQ[fileName] ),
        Echo[
          "The value of the option \"FileName\" is expected to be a string or Automatic.",
          "SDMonExportModel:"];
        Return[$SDMonFailure]
      ];

      SDMonExportModel[ fileName, opts][xs, context]
    ];

SDMonExportModel[ Automatic, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{fileName, dateTime},

      dateTime = StringReplace[DateString["ISODateTime"], ":" -> "."];

      fileName = "SDMon-model-" <> dateTime <> ".m";

      SDMonExportModel[fileName, opts][xs, context]
    ];

SDMonExportModel[ fileNameArg_String, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{fileName = fileNameArg, dirName, res, model, modelType},

      dirName = OptionValue[SDMonExportModel, "Directory"];

      If[ ! ( TrueQ[dirName === Automatic] || StringQ[dirName] ),
        Echo[
          "The value of the option \"Directory\" is expected to be a string or Automatic.",
          "SDMonExportModel:"];
        Return[$SDMonFailure]
      ];

      If[ MemberQ[{Automatic, None}, dirName], dirName = Directory[] ];

      If[ ! FileExistsQ[dirName],
        Echo["The directory \"" <> dirName <> "\" does not exist.", "SDMonImportModel:"];
        Return[$SDMonFailure]
      ];

      If[ ! TrueQ[ FileType[dirName] === Directory ],
        Echo["The location \"" <> dirName <> "\" is not a directory.", "SDMonImportModel:"];
        Return[$SDMonFailure]
      ];

      model = Fold[ SDMonBind, SDMonUnit[xs, context], { SDMonGetDefaultModel, SDMonTakeValue }];

      modelType =
          Which[
            KeyExistsQ[context, "multiSiteModel"], "multiSiteModel",
            KeyExistsQ[context, "singleSiteModel"], "singleSiteModel",
            True, None
          ];

      model = Append[ model, "SDMonModelType" -> modelType ];

      If[ Length[FileNameSplit[fileName]] == 1,
        fileName = FileNameJoin[{ dirName, fileName }]
      ];

      res = Export[ fileName, model ];

      SDMonUnit[res, context]
    ];

SDMonExportModel[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonExportModel[ fileName : (_String | Automatic), opts___] or SDMonExportModel[opts___].",
        "SDMonExportModel:"
      ];
      $SDMonFailure
    ];


(**************************************************************)
(* SDMonImportModel                                          *)
(**************************************************************)

Clear[SDMonImportModel];

SyntaxInformation[SDMonImportModel] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SDMonImportModel] = { "FileName" -> None };

SDMonImportModel[___][$SDMonFailure] := $SDMonFailure;

SDMonImportModel[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{fileName},

      fileName = OptionValue[SDMonImportModel, "FileName"];

      fileName =
          If[ ! StringQ[fileName],
            Echo[
              "The value of the option \"FileName\" is expected to be a string.",
              "SDMonImportModel:"];
            Return[$SDMonFailure]
          ];

      SDMonImportModel[ fileName, opts][xs, context]
    ];

SDMonImportModel[ fileName_String, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{res, model, modelType},

      If[ !FileExistsQ[fileName],
        Echo["The file \"" <> fileName <> "\" does not exist.", "SDMonImportModel:"];
        Return[$SDMonFailure]
      ];

      model = Get[ fileName ];

      If[ !EpidemiologyModelQ[model],
        Echo["The import from file \"" <> fileName <> "\" is not an SDMon model.", "SDMonImportModel:"];
        Return[$SDMonFailure]
      ];

      modelType = Lookup[model, "SDMonModelType", "singleSiteModel"];
      model = KeyDrop[ model, "SDMonModelType"];

      If[ !EpidemiologyFullModelQ[model],
        Echo["The import from file \"" <> fileName <> "\" is an SDMon model, but not a full model.", "SDMonImportModel:"];
      ];

      If[ !MemberQ[ {"singleSiteModel", "multiSiteModel"}, modelType ],
        Echo[
          "Unknown model type " <> modelType <> " in the file \"" <> fileName <> "\". " <>
              "The model type is expected to be one of \"singleSiteModel\" or \"multiSiteModel\".",
          "SDMonImportModel:"];
        Return[$SDMonFailure]
      ];

      (* Maybe it is better to SDMonSetSingleSiteModel and SDMonSetMultiSiteModel . *)
      SDMonUnit[res, Join[ context, <| modelType -> model |> ] ]
    ];

SDMonImportModel[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of SDMonImportModel[ fileName_String, opts___] or SDMonImportModel[opts___].",
        "SDMonImportModel:"
      ];
      $SDMonFailure
    ];


End[]; (* `Private` *)

EndPackage[]