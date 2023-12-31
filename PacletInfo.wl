(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "AntonAntonov/MonadicSystemDynamics",
    "Description" -> "Software monad for System Dynamics workflows",
    "Creator" -> "Anton Antonov",
    "License" -> "MIT",
    "PublisherID" -> "AntonAntonov",
    "Version" -> "1.0.1",
    "WolframVersion" -> "12.3+",
    "PrimaryContext" -> "AntonAntonov`MonadicSystemDynamics`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"AntonAntonov`MonadicSystemDynamics`"},
        "Symbols" -> {
          "AntonAntonov`MonadicSystemDynamics`$SDMonFailure",
          "AntonAntonov`MonadicSystemDynamics`BinaryOperateByKeys",
          "AntonAntonov`MonadicSystemDynamics`CoordinatesToValuesAssociationQ",
          "AntonAntonov`MonadicSystemDynamics`DeriveSusceptiblePopulation",
          "AntonAntonov`MonadicSystemDynamics`ParametricFunctionValues",
          "AntonAntonov`MonadicSystemDynamics`ParametricSolutionsListPlots",
          "AntonAntonov`MonadicSystemDynamics`ParametricSolutionsPlots",
          "AntonAntonov`MonadicSystemDynamics`SDMon",
          "AntonAntonov`MonadicSystemDynamics`SDMonAddToContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonAssignContextTo",
          "AntonAntonov`MonadicSystemDynamics`SDMonAssignInitialConditions",
          "AntonAntonov`MonadicSystemDynamics`SDMonAssignInitialConditionsByGridAggregation",
          "AntonAntonov`MonadicSystemDynamics`SDMonAssignRateRules",
          "AntonAntonov`MonadicSystemDynamics`SDMonAssignTo",
          "AntonAntonov`MonadicSystemDynamics`SDMonAssignValueTo",
          "AntonAntonov`MonadicSystemDynamics`SDMonBatchSimulate",
          "AntonAntonov`MonadicSystemDynamics`SDMonBind",
          "AntonAntonov`MonadicSystemDynamics`SDMonCalibrate",
          "AntonAntonov`MonadicSystemDynamics`SDMonContexts",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropAdjacencyMatrix",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropFromContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropGraph",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropGrid",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropMultiSiteModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropSingleSiteModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonDropSolution",
          "AntonAntonov`MonadicSystemDynamics`SDMonEcho",
          "AntonAntonov`MonadicSystemDynamics`SDMonEchoContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonEchoFunctionContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonEchoFunctionValue",
          "AntonAntonov`MonadicSystemDynamics`SDMonEchoModelGridTableForm",
          "AntonAntonov`MonadicSystemDynamics`SDMonEchoValue",
          "AntonAntonov`MonadicSystemDynamics`SDMonEvaluateSolutionsOverGraph",
          "AntonAntonov`MonadicSystemDynamics`SDMonExportModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonExtendByAdjacencyMatrix",
          "AntonAntonov`MonadicSystemDynamics`SDMonExtendByGrid",
          "AntonAntonov`MonadicSystemDynamics`SDMonFail",
          "AntonAntonov`MonadicSystemDynamics`SDMonFold",
          "AntonAntonov`MonadicSystemDynamics`SDMonGetDefaultModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonGetSolutionValues",
          "AntonAntonov`MonadicSystemDynamics`SDMonIf",
          "AntonAntonov`MonadicSystemDynamics`SDMonIfElse",
          "AntonAntonov`MonadicSystemDynamics`SDMonImportModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonIterate",
          "AntonAntonov`MonadicSystemDynamics`SDMonMakePolygonGrid",
          "AntonAntonov`MonadicSystemDynamics`SDMonMakeTravelingPatternsMatrix",
          "AntonAntonov`MonadicSystemDynamics`SDMonModifyContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonModule",
          "AntonAntonov`MonadicSystemDynamics`SDMonNest",
          "AntonAntonov`MonadicSystemDynamics`SDMonNestWhile",
          "AntonAntonov`MonadicSystemDynamics`SDMonOption",
          "AntonAntonov`MonadicSystemDynamics`SDMonPlotGrid",
          "AntonAntonov`MonadicSystemDynamics`SDMonPlotGridHistogram",
          "AntonAntonov`MonadicSystemDynamics`SDMonPlotSiteSolutions",
          "AntonAntonov`MonadicSystemDynamics`SDMonPlotSolutions",
          "AntonAntonov`MonadicSystemDynamics`SDMonPutContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonPutValue",
          "AntonAntonov`MonadicSystemDynamics`SDMonRetrieveFromContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetAdjacencyMatrix",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetGraph",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetGrid",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetMultiSiteModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetSingleSiteModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetSolution",
          "AntonAntonov`MonadicSystemDynamics`SDMonSetValue",
          "AntonAntonov`MonadicSystemDynamics`SDMonSimulate",
          "AntonAntonov`MonadicSystemDynamics`SDMonSucceed",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeAdjacencyMatrix",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeContext",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeGraph",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeGrid",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeMultiSiteModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeSingleSiteModel",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeSolution",
          "AntonAntonov`MonadicSystemDynamics`SDMonTakeValue",
          "AntonAntonov`MonadicSystemDynamics`SDMonUnit",
          "AntonAntonov`MonadicSystemDynamics`SDMonUnitQ",
          "AntonAntonov`MonadicSystemDynamics`SDMonWhen",
          "AntonAntonov`MonadicSystemDynamics`StockVariabilityPlot",
          "AntonAntonov`MonadicSystemDynamics`SubtractByKeys",
          "AntonAntonov`MonadicSystemDynamics`TrapezoidalRule",
          "AntonAntonov`MonadicSystemDynamics`TrapezoidalRuleAccumulate"
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      }
    }
  |>
]
