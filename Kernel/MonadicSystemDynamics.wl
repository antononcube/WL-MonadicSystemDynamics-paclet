BeginPackage["AntonAntonov`MonadicSystemDynamics`"];

CoordinatesToValuesAssociationQ::usage = "Checks does the argument match  <|({_?NumericQ, _?NumericQ} -> _?NumericQ) ...|>.";

BinaryOperateByKeys::usage = "BinaryOperateByKeys[a1_Association, a2_Association, op_] for each key in common applies the binary operation op.";

SubtractByKeys::usage = "SubtractByKeys[a1_Association, a2_Association] for each key in common subtracts the a2 value from the a1 value.";

DeriveSusceptiblePopulation::usage = "DeriveSusceptiblePopulation[total, infected, deceased] \
derives susceptible population from total population, infected, and deceased.";

$SDMonFailure::usage = "Failure symbol for the monad SDMon.";

SDMonGetDefaultModel::usage = "SDMonGetDefaultModel[] gets the default model in the monad object. \
The default model is determined by the following sequence of checks: \
(1) is the pipeline is an epidemiology model, \
(2) does \"multiSiteModel\" exist, \
(3) does \"singleSiteModel\" exist.";

SDMonEchoModelGridTableForm::usage = "SDMonEchoModelGridTableForm[] echoes grid table form of the default model.\
SDMonEchoModelGridTableForm[spec_String] only echoes the specified parts of the grid table form.";

SDMonMakePolygonGrid::usage = "SDMonMakePolygonGrid[points: {{_?NumberQ, _?NumberQ}..} , cellRadius_?NumberQ, opts___] \
creates a hexagonal grid (object) that covers the specified points.";

SDMonPlotGrid::usage = "SDMonPlotGrid";

SDMonPlotGridHistogram::usage = "SDMonPlotGridHistogram";

SDMonExtendByGrid::usage = "SDMonExtendByGrid";

SDMonExtendByAdjacencyMatrix::usage = "SDMonExtendByAdjacencyMatrix[ mat_?MatrixQ, factor_?NumberQ] \
extends monad's single site model into multi-site model using the numerical matrix mat.";

SDMonMakeTravelingPatternsMatrix::usage = "SDMonMakeTravelingPatternsMatrix[ aOriginDestinationToTravelers_Association, aLocationToLonLat_Association ] \
crates a contingency matrix for the travelers between the cells of grid object in the context.";

SDMonAssignInitialConditionsByGridAggregation::usage = "SDMonAssignInitialConditionsByGridAggregation";

SDMonAssignInitialConditions::usage = "SDMonAssignInitialConditions";

SDMonAssignRateRules::usage = "SDMonAssignRateRules";

SDMonSimulate::usage = "SDMonSimulate";

SDMonBatchSimulate::usage = "SDMonBatchSimulate";

SDMonCalibrate::usage = "SDMonCalibrate";

SDMonGetSolutionValues::usage = "SDMonGetSolutionValues[ stockSpec : All | ( _String | {_String..} | _StringExpression ), maxTime_?NumericQ, opts___ ] \
gets the solution values for specified stocks and maximum time.";

SDMonPlotSolutions::usage = "SDMonPlotSolutions";

SDMonPlotSiteSolutions::usage = "SDMonPlotSiteSolutions";

SDMonEvaluateSolutionsOverGraph::usage = "SDMonEvaluateSolutionsOverGraph";

SDMonExportModel::usage = "SDMonExportModel[fileName_String] exports the default monad model into file.";

SDMonImportModel::usage = "SDMonImportModel[fileName_String] imports model from a file.";

ParametricSolutionsPlots::usage = "ParametricSolutionsPlots[aStocks_Association, aSol_Association, params : (_List | None), tmax_?NumericQ, opts : OptionsPattern[]] \
uses Plot over an association of parametrized functions aSol for the stocks aStocks with function parameters params for time range {0, tmax}.";

ParametricSolutionsListPlots::usage = "ParametricSolutionsListPlots[aStocks_Association, aSol_Association, params : (_List | None), tmax_?NumericQ, opts : OptionsPattern[]] \
uses ListPlot or DateListPlot over an association of parametrized functions aSol for the stocks aStocks with function parameters params for time range {0, tmax}.";

ParametricFunctionValues::usage = "ParametricFunctionValues[pf_ParametricFunction, pars_?AssociationQ, tspec : {tmin_?NumericQ, tmax_?NumericQ, tstep_?NumericQ}] \
evaluates the parametric function pf with parameters pars over the times specified by tspec.";

TrapezoidalRule::usage = "TrapezoidalRule[pnts : {{_?NumericQ, _?NumericQ}..} ] applies the trapezoidal integration rule to list of points.";

TrapezoidalRuleAccumulate::usage = "TrapezoidalRuleAccumulate[pnts : {{_?NumericQ, _?NumericQ}..} ] \
gives accumulated integrals over a list of points.";

StockVariabilityPlot::usage = "StockVariabilityPlot[aSol_Association, stock_Symbol, aPars_Association, {fparVar_Symbol, fparVals_?VectorQ}, tspec : {tmin_?NumericQ, tmax_?NumericQ, tspep_?NumericQ}, opts___]\
makes a plot with different curves with respect to a specified parameters aPars, focus parameter values fparVals over a time grid specification tspec.";

PacletInstall["AntonAntonov/MonadMakers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/EpidemiologicalModeling", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/SSparseMatrix", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/TileStats", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/DataReshapers", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`MonadicSystemDynamics`SDMon`"];
Needs["AntonAntonov`MonadicSystemDynamics`InteractiveInterfacesFunctions`"];

End[]; (* `Private` *)

EndPackage[];