### cholera 0.8.0.9480

- add exception for Falconberg Court and Mews isolates in walkingB().


### cholera 0.8.0.9479

- temp fix for "observed" and "expected" walkingNominal(case.set).


### cholera 0.8.0.9478

- fix duplicate road segment endpoints (self-loops) in embedNodes().


### cholera 0.8.0.9477

- return 'cores' with walkingB().


### cholera 0.8.0.9476

- add code for 'area.points' and 'area.polygons' in 
  plot.walkingB(case.set = "expected").
- return 'case.pump' with walkingB(case.set = "expected").


### cholera 0.8.0.9475

- limit computation of paths to case.set == "observed"; re-enable multi.core.


### cholera 0.8.0.9474

- clean walkingB() code.


### cholera 0.8.0.9473

- add walkingB() prototype.


### cholera 0.8.0.9472

- add pump/street name to output of nearestPump(latlong = TRUE)$distance.


### cholera 0.8.0.9471

- add addNeighborhoodWalkingLatlong() prototype.


### cholera 0.8.0.9470

- amend/use index0().


### cholera 0.8.0.9469

- remove unused code in travelingSalesman().


### cholera 0.8.0.9468

- add/set nearestPumpLatlong(location = "nominal").


### cholera 0.8.0.9467

- add/set nearestPumpNominal(location = "nominal").


### cholera 0.8.0.9466

- add/set addNeighborhoodCases(latlong = FALSE).


### cholera 0.8.0.9465

- use do.call() in nearestPump().
- return data frame for nearestPumpNominal(metric = "euclidean").


### cholera 0.8.0.9464

- use lapply() for peripheryCases() and travelingSalesman() in 
  plot.walkingLatlong().


### cholera 0.8.0.9463

- add/set addKernelDensity(latlong = FALSE).


### cholera 0.8.0.9462

- add/amend methods for pumpCase().


### cholera 0.8.0.9461

- amend classes for euclideanLatlong() and walkingLatlong().


### cholera 0.8.0.9460

- add single core/Windows exception in nearestPumpLatlong().


### cholera 0.8.0.9459

- copyedit "Kernel Density Plot" vignette.


### cholera 0.8.0.9458

 - amend Windows and/or cores = 1 exception in nearestPumpNominal().


### cholera 0.8.0.9457

- amend QGIS URL and update latlong syntax in README.


### cholera 0.8.0.9456

- set addKernelDensity(multi.core = FALSE).


### cholera 0.8.0.9455

- add/set neighborhoodWalking(latlong = FALSE).
- add walkingNominal() and walkingLatlong().
- archive latlongNeighborhoodWalking().


### cholera 0.8.0.9454

- use stop(call. = FALSE) in nearestPumpNominal().


### cholera 0.8.0.9453

- return paths in nearestPump().


### cholera 0.8.0.9452

- make unitMeter() internal.


### cholera 0.8.0.9451

- update euclideanPath() argument in nearestPumpNominal().


### cholera 0.8.0.9450

- fix pumpFatalities(metric = "euclidean") and use do.call().


### cholera 0.8.0.9449

- fix pumpFatalities(latlong = FALSE).


### cholera 0.8.0.9448

- fix pumpFatalities() and use nearestPump().


### cholera 0.8.0.9447

- amend/set nearestPump(latlong = FALSE).
- archive latlongNearestPump().
- add nearestPumpNominal() and nearestPumpLatlong().


### cholera 0.8.0.9446

- set type = "roads" in plot.walking() and plot.latlong_walking().


### cholera 0.8.0.9445

- make latlongNeighborhoodData(), latlongNeighborhoodDataB() and 
  latlongNeighborhoodWalking() internal.


### cholera 0.8.0.9444

- move walkingTime() to walkingTime.R.


### cholera 0.8.0.9443

- rename latlongVoronoi.R to walkingAuxiliaryFunctions.R
- rename latlongVoronoi() to latlongVoronoiVertices().


### cholera 0.8.0.9442

- archive latlongNeighborhoodVoronoi().
- amend class name in pumpTokens() and voronoiLatlong()


### cholera 0.8.0.9441

- make latlongVoronoi() internal.


### cholera 0.8.0.9440

- archive latlongEuclideanPath().


### cholera 0.8.0.9439

- archive latlongStreetNameLocator().


### cholera 0.8.0.9438

- promote streetNumberLocatorB() to streetNumberLocator().


### cholera 0.8.0.9437

- fix/enable vector of street names in streetNameLocator().


### cholera 0.8.0.9436

- amend latlong.regular.cases and latlong.sim.ortho.proj.


### cholera 0.8.0.9435

- amend latlongSimulateFatalities() prototype.


### cholera 0.8.0.9434

- incorporate case.select in location: neighborhoodEuclidean(latlong = TRUE).


### cholera 0.8.0.9433

- incorporate case.select in location: neighborhoodEuclidean(latlong = FALSE).


### cholera 0.8.0.9432

- promote addEuclideanPathB() to addEuclideanPath().


### cholera 0.8.0.9431

- promote addWalkingPathB() to addWalkingPath().


### cholera 0.8.0.9430

- amend/update pumpPump().


### cholera 0.8.0.9429

- clean walkingPath(), casePump() and caseCase().


### cholera 0.8.0.9428

- promote/amend walkingPathB() to walkingPath().


### cholera 0.8.0.9427

- promote streetNameLocatorB() to streetNameLocator().


### cholera 0.8.0.9426

- add 'dstn.nm' argument to pumpPumpEucl().


### cholera 0.8.0.9425

- amend data.summary in pumpPumpEucl().


### cholera 0.8.0.9424

- clean code and amend output of euclideanPath().


### cholera 0.8.0.9423

- use any() in casePumpEucl()
- match caseCaseEucl() output in casePumpEucl().


### cholera 0.8.0.9422

- amend caseCaseEucl(), esp. Squares.


### cholera 0.8.0.9421

- use cholera::fatalities in validateCase().


### cholera 0.8.0.9420

- amend casePump() Square ID/name fix.


### cholera 0.8.0.9419

- add Square ID/name fix and clean code in casePump().


### cholera 0.8.0.9418

- fix pump name destination in walkingPathB().


### cholera 0.8.0.9417

- remove "outdated" landmark destination detection in walkingPathB().


### cholera 0.8.0.9416

- amend error and warning messages in validateCase() and validatePump().


### cholera 0.8.0.9415

- remove "outdated" landmark destination detection in euclideanPath().


### cholera 0.8.0.9414

- remove square.intersections argument.


### cholera 0.8.0.9413

- fix validateCase() for character cases/landmarks.


### cholera 0.8.0.9412

- remove redundant code and update variable names in in walkingPathB().


### cholera 0.8.0.9411

- fix pump selection in casePumpEucl().


### cholera 0.8.0.9410

- enable casePumpEucl(latlong = TRUE).


### cholera 0.8.0.9409

- compute missing distance 'd' in casePumpEucl().


### cholera 0.8.0.9408

- fix negtive selection and remove location argument in caseCaseEucl().


### cholera 0.8.0.9407

- enable negative selection in validateCase().


### cholera 0.8.0.9406

- clean and clarify casePumpEucl(latlong = FALSE).


### cholera 0.8.0.9405

- use geosphere::distGeo() for latlong in caseCaseEucl() and pumpPumpEucl().
- fix for cases in same stack (type = "cases").
- remove redundant code in euclideanPath(), caseCaseEucl() and pumpPumpEucl().
- update syntax for caseCaseEucl() and pumpPumpEucl() in euclideanPath().


### cholera 0.8.0.9404

- move longTitle(), mapDataRange() and validate.R functions to pathFunctions.R.


### cholera 0.8.0.9403

- use anchor case filter for all locations in casePumpEucl().


### cholera 0.8.0.9402

- amend variable names in euclideanPath().
- separate milepost arrows from direct arrow in plot.euclidean_path().
- include origin case when location %in% c("anchor", "orthogonal").


### cholera 0.8.0.9401

- remove lndmrk test.
- add dstn.nm argument to caseCase().
- return origin and location in walkingPathB().
- add x argument to longTitle().
- add anchor case filters to casePump() and caseCase().


### cholera 0.8.0.9400

- update validateCase() in walkingPathB().


### cholera 0.8.0.9399

- fix anchor orign.nm casePumpEucl().


### cholera 0.8.0.9398

- distinguish anchor and case in euclideanPath() and longTitle().


### cholera 0.8.0.9397

- archive caseLandmarks() and prior validateCase() and validatePump().


### cholera 0.8.0.9396

- fix validateCase() for character cases.


### cholera 0.8.0.9395

- add/set euclideanPath(square.intersections = FALSE); amend location argument.
- amend casePumpEucl().


### cholera 0.8.0.9394

- fix typo in validateCase().


### cholera 0.8.0.9393

- amend validateCase(); move it and validatePump() to validate.R.


### cholera 0.8.0.9392

- add exception for landmarks in casePumpEucl().
- check case.set and location arguments in euclideanPath().


### cholera 0.8.0.9391

- add/set walkingPathB(case.set = "observed", location = "nominal").
- use amended validateCase() in walkingPathB().


### cholera 0.8.0.9390

- amend validateCase(case.set = "expected").


### cholera 0.8.0.9389

- export case.set in euclideanPath().
- clean plot.euclidean_path().
- fix casePumpEucl(location = "orthogonal") with case.set == "observed".
- fix casePumpEucl(case.set == "expected").


### cholera 0.8.0.9388

- add geoCartesianSimulation() prototype.


### cholera 0.8.0.9387

- enable euclideanPath(latlong = TRUE, location = "orthogonal").


### cholera 0.8.0.9386

- add a stop() for validateCase(location) error.


### cholera 0.8.0.9385

- add/set euclideanPath(case.set = "observed", location = "nominal").


### cholera 0.8.0.9384

- add validateCase(case.set, location).


### cholera 0.8.0.9383

- archive euclideanPathB().


### cholera 0.8.0.9382

- amend latlong.regular.cases and latlong.sim.ortho.proj.


### cholera 0.8.0.9381

- use regular.cases with latlongSimulateFatalities(recompute = FALSE).


### cholera 0.8.0.9380

- restore regular.cases and sim.ortho.proj.


### cholera 0.8.0.9379

- amend regular.cases and sim.ortho.proj.


### cholera 0.8.0.9378

- promote caseLocatorB() to caseLocator().


### cholera 0.8.0.9377

- enable negative numerical selection in validatePump().


### cholera 0.8.0.9376

- add/set euclideanNominal(brute.force = FALSE).


### cholera 0.8.0.9375

- add/set neighborhoodEuclidean(case.select = "address").


### cholera 0.8.0.9374

- add/set addNeighborhoodCases(case.set = "observed", case.select = "address").


### cholera 0.8.0.9373

- add neighborhoodVoronoi(latlong = TRUE, polygon.vertices = TRUE).


### cholera 0.8.0.9372

- amend cell names for voronoiNominal(polygon.vertices = TRUE).


### cholera 0.8.0.9371

- change "case.location" argument to 'location' with values "nominal" or 
  "orthogonal" in neighborhoodVoronoi().


### cholera 0.8.0.9370

- change "case.location" argument to 'location' with values "nominal" or 
  "orthogonal" in neighborhoodEuclidean().


### cholera 0.8.0.9369

- change "case.location" argument to 'location' with values "nominal" or 
  "orthogonal" in addNeighborhoodEuclidean().
  

### cholera 0.8.0.9368

- change "case.location" argument to 'location' with values "nominal" or 
  "orthogonal" in addNeighborhoodCases().
  
  
### cholera 0.8.0.9367

- change "case.location" argument to 'location' with values "nominal" or 
  "orthogonal" in euclideanPath().


### cholera 0.8.0.9366

- use neighborhoodVoronoi() as wrapper for voronoiNominal() and 
  voronoiLatlong().
- set "nominal" and "orthogonal" as arguments for case.location and 
  pump.location.


### cholera 0.8.0.9365

- use neighborhoodEuclidean() as wrapper for euclideanNominal() and 
  euclideanLatlong().


### cholera 0.8.0.9364

- add geoCartesianCoord().


### cholera 0.8.0.9363

- add caseLocatorB() prototype.
- include case when computing xlim and ylim.


### cholera 0.8.0.9362

- add streetNumberLocatorB() prototype.


### cholera 0.8.0.9361

- fix numerical zoom in streetNameLocatorB(latlong = FALSE).


### cholera 0.8.0.9360

- add streetNameLocatorB() prototype.


### cholera 0.8.0.9359

- archive latlongSegmentLocator().
- promote segmentLocatorB() to segmentLocator().
- add "nominal" distance subtitle.


### cholera 0.8.0.9358

- temporarily allow sole selection of pump 2 (isolate) in selectPump().


### cholera 0.8.0.9357

- add segmentLocatorB() prototype.


### cholera 0.8.0.9356

- allow NULL segment.id in latlongSegmentLocator().


### cholera 0.8.0.9355

- set/fix limit for positive zoom in segmentLocator().


### cholera 0.8.0.9354

- allow NULL segment id in segmentLocator().


### cholera 0.8.0.9353

- use latlongSegmentLocator() numeric zoom in segmentLocator().


### cholera 0.8.0.9352

- show cases with numeric zoom in latlongSegmentLocator().


### cholera 0.8.0.9351

- add numeric zoom in latlongSegmentLocator().


### cholera 0.8.0.9350

- set/fix limit for positive zoom in geoCartesianStreetLocator().


### cholera 0.8.0.9349

- set plot.walking(type = "area.points").


### cholera 0.8.0.9348

- use amended meterLatLong().


### cholera 0.8.0.9347

- embed origin, topleft and bottomright in meterLatLong().


### cholera 0.8.0.9346

- add/set caseDistance(latlong = FALSE).


### cholera 0.8.0.9345

- add/set addCase(latlong = FALSE).


### cholera 0.8.0.9344

- add/set addIndexCase(latlong = FALSE).


### cholera 0.8.0.9343

- amend pump icons in plot.latlong_walking(type = "area.polygons").


### cholera 0.8.0.9342

- add latlongSegmentLocator() prototype.


### cholera 0.8.0.9341

- amend syntax in roadSegments().


### cholera 0.8.0.9340

- add/set segmentLength(latlong = FALSE).


### cholera 0.8.0.9339

- amend pump graphics for "area.points" and "area.polygons" in
  plot.latlong_walking().


### cholera 0.8.0.9338

- contextualize pumps for plot(latlongNeighborhoodWalking(case.set = "snow")).


### cholera 0.8.0.9337

- add contextual pumps in plot.latlong_walking().


### cholera 0.8.0.9336

- use pump IDs in latlongNearestPump().


### cholera 0.8.0.9335

- enable pump.select argument for plot.latlong_walking(type = "streets").


### cholera 0.8.0.9334

- enable pump.select argument in latlongNearestPump().


### cholera 0.8.0.9333

- compute whole and split segments in plot.latlong_walking(type = "streets").


### cholera 0.8.0.9332

- extract 'edges' for plot.latlong_walking(type = "streets").


### cholera 0.8.0.9331

- add plot.latlong_walking(type = "streets").


### cholera 0.8.0.9330

- return 'nearest.pump' in latlongNeighborhoodWalking().


### cholera 0.8.0.9329

- return 'cores' in latlongNeighborhoodWalking().


### cholera 0.8.0.9328

- add/set plot.latlong_walking(type = "area.points").
- add plot.latlong_walking(type = "area.polygons").


### cholera 0.8.0.9327

- remove dev.mode argument from latlongNearestPump().


### cholera 0.8.0.9326

- add latlongNeighborhoodWalking(case.set = "expected") prototype.


### cholera 0.8.0.9325

- add latlongNearestPump(case.set = "expected") prototype.


### cholera 0.8.0.9324

- set snowNeighborhood(latlong = FALSE).
- add St James Workhouse segment ("148-1") to whole.segs in snowNeighborhood().


### cholera 0.8.0.9323

- promote/rename snowNeighborhoodB() to snowNeighborhood().


### cholera 0.8.0.9322

- add/set addSnow(latlong = FALSE).


### cholera 0.8.0.9321

- amend road segments and sub-segments in snowNeighborhoodB(latlong = TRUE).


### cholera 0.8.0.9320

- use snowNeighborhoodB() in addSnow().


### cholera 0.8.0.9319

- amend/update latlong.regular.cases and latlong.sim.ortho.proj.


### cholera 0.8.0.9318

- make geoCartesianStreetLocator(zoom) logical or numeric.


### cholera 0.8.0.9317

- define more nearest segments by euclidean rather than othogonal distance in
  latlongSimulateFatalities().


### cholera 0.8.0.9316

- add/set geoCartesianStreetLocator(street.col = "gray").


### cholera 0.8.0.9315

- add/set geoCartesianStreetLocator(zoom.padding = 0).


### cholera 0.8.0.9314

- add St James Workhouse segment ("148-1") to whole.segs in snowNeighborhoodB().


### cholera 0.8.0.9313

- use vector of streets for zoom in  latlongStreetNameLocator().


### cholera 0.8.0.9312

- add street name spell-check and draw streets with lines() in 
  geoCartesianStreetLocator().


### cholera 0.8.0.9311

- add internal geoCartesianStreetLocator().


### cholera 0.8.0.9310

- add snowNeighborhoodB() prototype.


### cholera 0.8.0.9309

- add exception for single pump selection in latlong_pathData().


### cholera 0.8.0.9308

- use computed distance to identify pump isolate(s) in latlong_pathData().
- clean latlongNeighborhoodWalking().


### cholera 0.8.0.9307

- amend computation of obs.edges in plot.latlong_walking().


### cholera 0.8.0.9306

- add latlongNearestPump(case.set = "snow").


### cholera 0.8.0.9305

- amend list structure of shortest path in latlongNearestPump().


### cholera 0.8.0.9304

- replace identifyEdgesB() with auditEdge() in addNeighborhoodWalking().


### cholera 0.8.0.9303

- replace identifyEdgesB() with auditEdge() in plot.latlong_walking().


### cholera 0.8.0.9302

- fix typo in classifyCase() for snowNeighborhood().


### cholera 0.8.0.9301

- add latlongNeighborhoodWalking(case.set = "snow").


### cholera 0.8.0.9300

- add snowMap() to plot.neighborhoodWalking() for case.set = "snow".


### cholera 0.8.0.9299

- amend/restore profile2D() 'ggplot2' code.


### cholera 0.8.0.9298

- move interpolation code for "tmin" NA to plot.oxfordWeather().


### cholera 0.8.0.9297

- add/set plot.oxfordWeather(unit.observation = "month").


### cholera 0.8.0.9296

- archive profile2D() 'ggplot2' code.


### cholera 0.8.0.9295

- add addLandmarkSquares().


### cholera 0.8.0.9294

- move post.info and amend subtitle in euclideanPathB() and walkingPathB().


### cholera 0.8.0.9293

- clean addEuclideanPathB() and addWalkingPathB().


### cholera 0.8.0.9292

- add addEuclideanPathB() prototype.


### cholera 0.8.0.9291

- remove long.title argument from addWalkingPathB(); clean code.


### cholera 0.8.0.9290

- use Internet Archive links for Frerichs's UCLA website links.


### cholera 0.8.0.9289

- fix Dodson and Tobler URL for winbuilder.


### cholera 0.8.0.9288

- note Dodson and Tobler data and code on Internet Archive's Wayback Machine.


### cholera 0.8.0.9287

- amend/update oxford.weather.
- amend temperaturePlot().
- amend rainPlot().
- archive monthEndDate().
- clean plot.oxfordWeather().


### cholera 0.8.0.9286

- add/set plot.oxfordWeather(end.year = NULL); amend graph elements.


### cholera 0.8.0.9285

- add addWalkingPathB() prototype.


### cholera 0.8.0.9284

- use cholera::landmarksB in addLandmarks().


### cholera 0.8.0.9283

- fix mapDataRange() for Square labels with euclideanPathB().


### cholera 0.8.0.9282

- fix multiple origins (e.g., Squares) in euclideanPathB().
- amend argument name in sqCases().
- filter orgn and dstn in caseCaseEucl() for anchors and landmarks.
- fix multiple numeric origins in caseCaseEucl().


### cholera 0.8.0.9281

- add euclideanPathB() prototype.
- amend distance computation and function output.
- add plot.euclidean_path_B() prototype.
- add latlongEuclideanPosts() and latlongCartesian().
- amend plot.euclidean_path_B() prototype.


### cholera 0.8.0.9280

- drop selectPump() in casePump() and pumpPump() in walkingPathB().


### cholera 0.8.0.9279

- copyedit function documentation; format code.


### cholera 0.8.0.9278

- amend fix for Square cases with NULL origin/destination in caseCase().
- add amended sqCases().


### cholera 0.8.0.9277

- remove redundant code in plot.walking_path_B().
- fix duplicate Square origin label in plot.walking_path_B().


### cholera 0.8.0.9276

- add validateCase() and validatePump(); archive sqCases().
- amend variables and code in casePump(), caseCase() and pumpPump().
- amend variables in plot.walking_path_B(), longTitle() and mapDataRange().
- use validateCase() and validatePump(); amend variables in walkingPathB().


### cholera 0.8.0.9275

- fix Square cases with NULL origin/destination in caseCase().


### cholera 0.8.0.9274

- add/use sqCases() and drop/archive validateDestinationCases() in caseCase().


### cholera 0.8.0.9273

- use caseLandmarks() in validateDestinationCases().


### cholera 0.8.0.9272

- exclude other Square destination cases with a Square origin.


### cholera 0.8.0.9271

- fix landmark destination test in walkingPathB().


### cholera 0.8.0.9270

- exclude landmark destinations when origin is landmark (prototype).


### cholera 0.8.0.9269

- fix check for compound Square postfixes (e.g., "Soho Square-S1").


### cholera 0.8.0.9268

- add/use caseLandmarks() for Square entry/exit names.


### cholera 0.8.0.9267

- fix numerical landmarks (esp. Squares) in walkingPathB().


### cholera 0.8.0.9266

- amend/add walkingPathB(destination = NULL, include.landmarks = TRUE).


### cholera 0.8.0.9265

- add caseCase(include.landmarks).
.

### cholera 0.8.0.9264

- filter origin cases from destination in caseCase() for 
  walkingPathB(destination = NULL).


### cholera 0.8.0.9263

- check 'type' in walkingPathB().


### cholera 0.8.0.9262

- don't plot destination case when type = "case-pump" in plot.walking_path_B().


### cholera 0.8.0.9261

- enable origin = NULL for caseCase().
- enable origin = NULL for pumpPump().
- enable origin = NULL for walkingPathB().


### cholera 0.8.0.9260

- update/copyedit 'cholera-package' and use "_PACKAGE".


### cholera 0.8.0.9259

- temporarily round distance and time to tenth's place in plot.walking_path_B().


### cholera 0.8.0.9258

- remove roadSegmentFix(), simulateWalkingDistance() and subsetRoadsSamples() 
  from NAMESPACE.
  

### cholera 0.8.0.9257

- remove isoLines() and isoVertices() from NAMESPACE.


### cholera 0.8.0.9256

- Set `RoxygenNote` to "7.3.1".


### cholera 0.8.0.9255

- amend "Clifford Street missing segment" note.
- remove cliffordStreet() from NAMESPACE.


### cholera 0.8.0.9254

- fix multi-edge destination nodes in caseCase().


### cholera 0.8.0.9253

- draft landmarksB note.


### cholera 0.8.0.9252

- plot origin Square names in plot.walking_path_B().
- amend longTitle(type = "pumps").


### cholera 0.8.0.9251

- enable multiple origins and use walking path type functions.


### cholera 0.8.0.9250

- add pumpPump() prototype.


### cholera 0.8.0.9249

- add caseCase() prototype.


### cholera 0.8.0.9248

- add casePump() prototype.


### cholera 0.8.0.9247

- change `@docType "package"` to "_PACKAGE" in cholera.R for 'roxygen2' 7.3.0.


### cholera 0.8.0.9246

- add pumpCase.default() to NAMESPACE for 'roxygen2' 7.3.0.


### cholera 0.8.0.9245

- add pearsonResiduals() methods to NAMESPACE for 'roxygen2' 7.3.0.


### cholera 0.8.0.9244

- remove multi.core argument from walkingPathB().
- set `RoxygenNote` to "7.3.0".


### cholera 0.8.0.9243

- allow multiple landmarks in mapDataRange().


### cholera 0.8.0.9242

- fix labels for origin landmark Squares.


### cholera 0.8.0.9241

- amend mapDataRange().


### cholera 0.8.0.9240

- add/use mapDataRange() in walkingPathB().


### cholera 0.8.0.9239

- fix latlong coordinates for modelLodgingHouses().
- update landmarksB.


### cholera 0.8.0.9238

- add type exception for landmark destinations in walkingPathB().


### cholera 0.8.0.9237

- change numeric zoom to a factor >= 0 in plot.walking_path_B().


### cholera 0.8.0.9236

- increase default padding for plot.walking_path_B().


### cholera 0.8.0.9235

- remove multi.core in neighborhoodDataB() for latlongNearestPump() and 
  walkingPathB().


### cholera 0.8.0.9234

- copyedit Clifford Street missing segment description in README.


### cholera 0.8.0.9233

- use walkingPathB() in README.


### cholera 0.8.0.9232

- remove parallel code from neighborhoodDataB().


### cholera 0.8.0.9231

- remove parallel implementation of embedNodes().


### cholera 0.8.0.9230

- fix zoom for non-landmarks and tweak zoom padding for latlong = TRUE.


### cholera 0.8.0.9229

- move segmentIntersection() to landmarkDataB.R.


### cholera 0.8.0.9228

- rename geodesicMeters() to geoCartesian().


### cholera 0.8.0.9227

- fix coordinates in stLukesChurch().
- update landmarksB.


### cholera 0.8.0.9226

- change geodesic to geo.cartesian in segmentTrigonometryAddress().


### cholera 0.8.0.9225

- clean modelLodgingHouses().


### cholera 0.8.0.9224

- accommodate landmark labels in plot.walking_path_B().


### cholera 0.8.0.9223

- restore nodeData().
- fix typo for landmarksB documentation


### cholera 0.8.0.9222

- provisionally use landmarksB and landmark.squaresB.


### cholera 0.8.0.9221

- fix squareCenterB() output variable names.


### cholera 0.8.0.9220

- add landmarksB and landmark.squaresB data frames.


### cholera 0.8.0.9219

- drop .proj suffix from coordinate names in Squares() and squareCenterB().


### cholera 0.8.0.9218

- rename coordinates in functions in landmarkDataB().


### cholera 0.8.0.9217

- implement parallel version of landmarkDataB().


### cholera 0.8.0.9216

- change geodesics to geo.cartesian in landmarkB functions.


### cholera 0.8.0.9215

- amend pantheonBazaar() and stLukesChurch().


### cholera 0.8.0.9214

- use "_&_" as coordinate delimeter in pasteCoordsB().
- add/set squareExitsB(latlong = FALSE).
- amend squareCenterB() latlong variables.
- amend nominal and latlong coordinates in Squares().


### cholera 0.8.0.9213

- amend nominal and latlong coordinates in stJamesWorkhouse().


### cholera 0.8.0.9212

- amend nominal and latlong coordinates in stLukesChurch().


### cholera 0.8.0.9211

- add segmentGeoCartesian().


### cholera 0.8.0.9210

- add amended roadSegmentData().


### cholera 0.8.0.9209

- rename roadSegmentData() to roadSegEndpt().


### cholera 0.8.0.9208

- amend nominal and latlong coordinates in johnSnow().


### cholera 0.8.0.9207

- amend nominal and latlong coordinates in pantheonBazaar().


### cholera 0.8.0.9206

- amend nominal and latlong coordinates in karlMarx().


### cholera 0.8.0.9205

- amend nominal and latlong coordinates in magistratesCourt().
- fix output of magistratesCourt().


### cholera 0.8.0.9204

- amend nominal and latlong coordinates in lionBrewery().


### cholera 0.8.0.9203

- amend nominal and latlong coordinates in cravenChapel().
- add coordsCartesian() and projCartesian().


### cholera 0.8.0.9202

- amend exception for single coordinates in meterLatLong().


### cholera 0.8.0.9201

- fix exception for single coordinates in meterLatLong().


### cholera 0.8.0.9200

- set address to Hopkins Street; use proportion method in modelLodgingHouses().


### cholera 0.8.0.9199

- add exception for single coordinates (1 row) in meterLatLong().


### cholera 0.8.0.9198

- amend nominal and latlong coordinates in modelLodgingHouses().


### cholera 0.8.0.9197

- rename/amend assignLandmarkAddress() to projectLandmarkAddress().


### cholera 0.8.0.9196

- amend nominal and latlong coordinates in argyllHouse().


### cholera 0.8.0.9195

- add/set roadSegmentData(latlong = FALSE).


### cholera 0.8.0.9194

- add/set segmentTrigonometryAddress(latlong = FALSE).


### cholera 0.8.0.9193

- add landmarkPDF().


### cholera 0.8.0.9192

- amend landmarkSquares() and Squares(label.coord = TRUE).


### cholera 0.8.0.9191

- alphabetize landmarks and fix case IDs.


### cholera 0.8.0.9190

- set exit order in Squares().


### cholera 0.8.0.9189

- amend assignLandmarkAddress() and renumber landmark case IDs.


### cholera 0.8.0.9188

- use landmarkDataB() in nodeData().


### cholera 0.8.0.9187

- revert to other endpoint in stJamesWorkhouse().


### cholera 0.8.0.9186

- set case IDs for Golden and Soho Square to 20021:20022 in landmarkSquares().


### cholera 0.8.0.9185

- add/amend functions and clean code in landmarkDataB().


### cholera 0.8.0.9184

- amend modelLodgingHouses().


### cholera 0.8.0.9183

- add landmarkSquares().


### cholera 0.8.0.9182

- make pump.select first argument in addNeighborhoodWalking().


### cholera 0.8.0.9181

- amend addCase(case %in% c("anchor", "fatality", "orthogonal")).


### cholera 0.8.0.9180

- enable addSnow().


### cholera 0.8.0.9179

- add/use Squares().


### cholera 0.8.0.9178

- amend coordinates in squareCenterB() and squareExitsB().


### cholera 0.8.0.9177

- provisionally restore tanakaContourPlot().


### cholera 0.8.0.9176

- clean code for Squares in landmarkDataB().


### cholera 0.8.0.9175

- add more functions to landmarkDataB().


### cholera 0.8.0.9174

- add/use roadSegmentData().


### cholera 0.8.0.9173

- amend provisional landmarkDataB().


### cholera 0.8.0.9172

- add segmentTrignometryAddress().


### cholera 0.8.0.9171

- add addressProportion().


### cholera 0.8.0.9170

- add squareCenterB().


### cholera 0.8.0.9169

- add cravenChapel().


### cholera 0.8.0.9168

- add notes about model lodging houses.


### cholera 0.8.0.9167

- add assignLandmarkAddress().


### cholera 0.8.0.9166

- add importFrom deldir tile.list in voronoiPolygons().


### cholera 0.8.0.9165

- add stLukes().


### cholera 0.8.0.9164

- add lionBrewery().


### cholera 0.8.0.9163

- fix magistratesCourt().


### cholera 0.8.0.9162

- clean landmarkDataB().


### cholera 0.8.0.9161

- amend magistratesCourt().


### cholera 0.8.0.9160

- add roadTheta() and trignometricDelta().


### cholera 0.8.0.9159

- use isTRUE()/isFALSE() in walkingPathB().


### cholera 0.8.0.9158

- add Marlborough Street Magistrates Court and landmark street location/address.
- add/use magistratesCourt().


### cholera 0.8.0.9157

- clean DESCRIPTION Imports.


### cholera 0.8.0.9156

- check for internet connection in tanakaContourPlot().


### cholera 0.8.0.9155

- provisionally restore tanakaContourPlot().


### cholera 0.8.0.9154

- use importFrom for functions with limited package use.


### cholera 0.8.0.9153

- clean up use of double colon operator.


### cholera 0.8.0.9152

- clean DESCRIPTION Imports.


### cholera 0.8.0.9151

- use importFrom for 'sp' functions.


### cholera 0.8.0.9150

- use landmark addresses (lon.proj and lat.proj) in embedNodes().


### cholera 0.8.0.9149

- set Lion Brewery address (lon.proj and lat.proj) to Broad Street.


### cholera 0.8.0.9148

- move final "last mile" arrow and path info in plot.walking_path_B().


### cholera 0.8.0.9147

- fix Golden Square in latlongLandmarks(); amend 'landmarks'.


### cholera 0.8.0.9146

- use separate zoom padding for latlong = TRUE/FALSE in walkingPathB().


### cholera 0.8.0.9145

- set path color to "blue" for walkingPathB(path %in% c("cases", "pumps")).


### cholera 0.8.0.9144

- use separate cutpoints, latlong = TRUE/FALSE, for zero length arrows with 
  walkingPathB().


### cholera 0.8.0.9143

- archive latlongWalkingPath().


### cholera 0.8.0.9142

- add plot and print methods for walkingPathB().


### cholera 0.8.0.9141

- return "edges" and set class to "walking_path_B" for walkingPathB().


### cholera 0.8.0.9140

- fix walking.time computation and use "xy" as path coordinates in 
  walkingPathB(latlong = FALSE).
  

### cholera 0.8.0.9139

- add walkingPathB().


### cholera 0.8.0.9138

- amend graphical elements and syntax in plot.latlong_walking_path().


### cholera 0.8.0.9137

- amend syntax for plot.latlong_walking_path(long.title = TRUE).


### cholera 0.8.0.9136

- fix validateDestinationCases() for multiple landmark names.


### cholera 0.8.0.9135

- add/set plot.latlong_walking_path(long.title = TRUE).


### cholera 0.8.0.9134

- return "pmp" with latlongWalkingPath().


### cholera 0.8.0.9133

- add plot(latlongWalkingPath(type = "case-pump")) prototype.


### cholera 0.8.0.9132

- simplify computation of "ego.node" in latlongWalkingPath(destination = NULL).


### cholera 0.8.0.9131

- consolidate path data computation latlongWalkingPath().


### cholera 0.8.0.9130

- consolidate "data.summary" output in latlongWalkingPath().


### cholera 0.8.0.9129

- compute "d" for latlongWalkingPath(origin = grep("Square", origin), 
  destination = NULL, type = "case-pump").
  

### cholera 0.8.0.9128

- add additional message(s) and exclusion(s) of origin in destination for 
  latlongWalkingPath().
- add message about pump 2 isolate in 
  latlongWalkingPath(destination = NULL, type = "pumps").


### cholera 0.8.0.9127

- allow landmark and pump names in latlongWalkingPath().


### cholera 0.8.0.9126

- allow multiple Squares in validateDestinationCases().


### cholera 0.8.0.9125

- fix pump names (street names) in selectPump().


### cholera 0.8.0.9124

- add type = "cases" and type = "pumps" to latlongWalkingPath().
- allow landmark and pump names in latlongWalkingPath().


### cholera 0.8.0.9123

- allow pump names (street names) in selectPump().
- fix selectPump() using all(pump.num < 0).


### cholera 0.8.0.9122

- allow negative selection in validateDestinationCases().
- remove redundant num.chk.


### cholera 0.8.0.9121

- add exception for all(destination.chk) == TRUE in validateDestinationCases().


### cholera 0.8.0.9120

- add validateDestinationCases() prototype.


### cholera 0.8.0.9119

- archive nominalNeighborhoodData().


### cholera 0.8.0.9118

- add/set latlongNeighborhoodWalking(case.set = "observed", weighted = TRUE).
- parallelize computation of paths in plot.latlong_walking()


### cholera 0.8.0.9117

- use neighborhoodDataB() in latlongWalkingPath().


### cholera 0.8.0.9116

- use neighborhoodDataB() in latlongNearestPump().


### cholera 0.8.0.9115

- separate cases, landmarks and pumps in embedNodes().


### cholera 0.8.0.9114

- use latlong road segment endpoint for cases 286 & 369 in 
  latlongOrthoAddress().
- amend cases 286 & 369 in latlong.ortho.addr data frame.


### cholera 0.8.0.9113

- amend The Pantheon latlong coordinates in landmarks data frame.


### cholera 0.8.0.9112

- amend Golden and Soho Square(s) latlong coordinates in landmarks data frame.


### cholera 0.8.0.9111

- use georeferenced segment coordinates for Squares in latlongLandmarks().
- use landmarkDataB() in landmarksPDF() and latlongLandmarks(); exclude Squares.


### cholera 0.8.0.9110

- add code for latlongWalkingPath(weighted = FALSE).


### cholera 0.8.0.9109

- archive latlongEmbedB() and nominalEmbed().
- add neighborhoodDataB().


### cholera 0.8.0.9108

- add latlongNeighborhoodDataB() prototype.


### cholera 0.8.0.9107

- add embedNodes() prototype.


### cholera 0.8.0.9106

- add orthogonal longitude and latitude to landmarks data frame.


### cholera 0.8.0.9105

- add landmarkDataB().


### cholera 0.8.0.9104

- use vars in nominalEmbed() and latlongEmbedB().


### cholera 0.8.0.9103

- add latlongEmbedB() prototype - based on nominalEmbed().


### cholera 0.8.0.9102

- temporarily revert "case" to "anchor" for nodes created by nominalEmbed().


### cholera 0.8.0.9101

- add/set nominalEmbed(embed.pumps = TRUE).


### cholera 0.8.0.9100

- add/set nominalNeighborhoodData(embed.pumps = TRUE).


### cholera 0.8.0.9099

- add nominalNeighborhoodData() and nominalEmbed() prototypes.


### cholera 0.8.0.9098

- compute geographic distance for edges and output road.data in 
  latlongNeighborhoodData().
  
  
### cholera 0.8.0.9097

- compute edges$id2 and output road.data in latlongEmbed().


### cholera 0.8.0.9096

- add/set embed.addr = TRUE in latlongEmbed() and latlongNeighborhoodData().


### cholera 0.8.0.9095

- use selectPump() in latlongNearestPump(metric = "walking").


### cholera 0.8.0.9094

- restrict Adam and Eve Court exception to 
  latlongNearestPump(case.set = "expected").


### cholera 0.8.0.9093

- use walkingTime() in latlongNearestPump().


### cholera 0.8.0.9092

- add/set latlongNearestPump(case.set = "observed") and parallelize.
- amend exception for Adam and Eve Court.


### cholera 0.8.0.9091

- add/set latlongEmbed(case.set = "observed") and parallelize.


### cholera 0.8.0.9090

- add/set latlongNeighborhoodData(case.set = "observed").


### cholera 0.8.0.9089

- change neighborhoodData(observed = TRUE) to 
  neighborhoodData(case.set = "observed").


### cholera 0.8.0.9088

- use snowMap() and addRoads() in plot.walking().
- drop msg argument in neighborhoodWalking().


### cholera 0.8.0.9087

- clean plot.latlongEuclidean(type).


### cholera 0.8.0.9086

- remove redundant addRoads() in plot.euclidean().


### cholera 0.8.0.9085

- enable neighborhoodEuclidean(case.location = "orthogonal"); remove msg 
  argument.


### cholera 0.8.0.9084

- add/set plot.latlongVoronoi(delaunay.voronoi = "voronoi").


### cholera 0.8.0.9083

- add/set plot.voronoi(delaunay.voronoi = "voronoi").


### cholera 0.8.0.9082

- amend latlongNeighborhoodEuclidean() for amended latlongVoronoi().


### cholera 0.8.0.9081

- add/set addDelaunay(latlong = FALSE, line.width = 1).


### cholera 0.8.0.9080

- compute Voronoi cells and Delaunay triangles in latlongVoronoi().
- amend addVoronoi() for amended latlongVoronoi().


### cholera 0.8.0.9079

- use snowMap() in plot.voronoi().


### cholera 0.8.0.9078

- change latlongNeighborhoodVoronoi()'s class to "latlongVoronoi".
- use pumpTokens(); loop over pump names in plotLatlongVoronoiCases().


### cholera 0.8.0.9077

- add exceptions for "voronoi" and "latlongVoronoi" classes in pumpTokens().


### cholera 0.8.0.9076

- use pump.id for selection and pump.select for title in 
  latlongNeighborhoodVoronoi().
  

### cholera 0.8.0.9075

- add plots for latlongNeighborhoodEuclidean(case.set = "expected").


### cholera 0.8.0.9074

- add/set latlong = FALSE to pearlStringRadius(), peripheryCases() and 
  travelingSalesman().
  

### cholera 0.8.0.9073

- add/set pumpTokens(latlong = FALSE) and amend display of unselected pumps.


### cholera 0.8.0.9072

- assign pump names to list elements in latlongVoronoi().
- fix latlongVoronoi(pump.select).


### cholera 0.8.0.9071

- amend color selection in neighborhoodVoronoi().


### cholera 0.8.0.9070

- use pump.id instead of pumpID in neighborhoodWalking().


### cholera 0.8.0.9069

- amend and simplify pumpTokens().


### cholera 0.8.0.9068

- change latlongNeighborhoodEuclidean() class to "latlongEuclidean".


### cholera 0.8.0.9067

- use separate auxilliary plot functions in plot.euclidean().


### cholera 0.8.0.9066

- add zone, rd.name.short and rd.name.check variables (columns) to appendixB().


### cholera 0.8.0.9065

- add appendixB().


### cholera 0.8.0.9064

- add/set latlongNeighborhoodEuclidean(case.set = "observed").


### cholera 0.8.0.9063

- add latlong coordinates for latlong.regular.cases.


### cholera 0.8.0.9062

- add/set latlongSimulateFatalities(recompute = FALSE).


### cholera 0.8.0.9061

- use case.location %in% c("address", "orthogonal") in addNeighborhoodCases().
- use case.location %in% c("address", "orthogonal") in 
  addNeighborhoodEuclidean(). 
- set case.set = "expected" in euclideanPath() in addNeighborhoodEuclidean().
- use case.location %in% c("address", "orthogonal") in neighborhoodEuclidean().
- update parLapply() for amended addEuclideanPath().


### cholera 0.8.0.9060

- amend addEuclideanPath(case.location, case.set, mileposts, milepost.unit, 
  milepost.interval).


### cholera 0.8.0.9059

- fix plot.latlong_euclidean_path(milepost.interval).


### cholera 0.8.0.9058

- use case.location %in% c("address", "orthogonal") and case.set %in% 
  c("observed", "expected").


### cholera 0.8.0.9057

- amend pumpTokens() for negative selection.


### cholera 0.8.0.9056

- use "_&_" as coordinate delimeter in ortho.proj.pump and 
  ortho.proj.pump.vestry.


### cholera 0.8.0.9055

- fix euclideanPath() and addEuclideanPath() for revised quandrantCoordinates().


### cholera 0.8.0.9054

- temporarily archive tanakaContourPlot() for 'sp' evolution.


### cholera 0.8.0.9053

- add latlongNeighborhoodEuclidean().


### cholera 0.8.0.9052

- add title to plot.latlongNeighborhoodVoronoi().


### cholera 0.8.0.9051

- use plotLatlongVoronoiCases().


### cholera 0.8.0.9050

- use "address" or "orthogonal" for case.location and pump.location.


### cholera 0.8.0.9049

- add/set latlongNeighborhoodVoronoi(case.location = "anchor", 
  pump.location = "nominal")


### cholera 0.8.0.9048

- set neighborhoodVoronoi(case.location %in% c("address", "anchor")).
- fix pumpTokens() for negative selection in plot.voronoi().
- use only address(es) or anchor(s) for plot.voronoi(euclidean.paths = TRUE).


### cholera 0.8.0.9047

- remove add.case argument from plot.latlongNeighborhoodVoronoi().


### cholera 0.8.0.9046

- migrate from sp::spDistsN1() to geosphere::distGeo() in 
  plotLatlongEuclideanPaths().


### cholera 0.8.0.9045

- add latlong.sim.ortho.proj and latlong.regular.cases.


### cholera 0.8.0.9044

- add latlongSimulateFatalities() and latlongRegularCartesianCases().


### cholera 0.8.0.9043

- set case.location = c("address" "fatality"); use ortho.proj with 
  case.location = "address".


### cholera 0.8.0.9042

- use plotLatlongEuclideanPaths() in latlongNeighborhoodVoronoi().


### cholera 0.8.0.9041

- set col = "gray" for roads in plot.voronoi().


### cholera 0.8.0.9040

- use serialization format version 3 for fatalities, fatalities.address and 
  fatalities.unstacked.
  

### cholera 0.8.0.9039

- use "_&_" as coordinate delimeter in embedSites(), nodeData() and 
  numericNodeCoordinates().
  

### cholera 0.8.0.9038

- add/use embedSites(ortho.pump).


### cholera 0.8.0.9037

- amend latlong.ortho.addr using amended latlongOrthoAddress().


### cholera 0.8.0.9036

- choose nearest road segment endpoint for manual reclassification cases that 
  fail bisection test in latlongOrthoAddress(). 


### cholera 0.8.0.9035

- amend latlong.ortho.addr for GCP v.02.


### cholera 0.8.0.9034

- add/set latlongOrthoAddress(radius = 60).


### cholera 0.8.0.9033

- amend fatalities.unstacked for GCP v.02; add fatalitiesUnstack().


### cholera 0.8.0.9032

- amend fatalities and fatalities.address for GCP v.02.


### cholera 0.8.0.9031

- use milePosts() in plot.walking_path().


### cholera 0.8.0.9030

- rename drawPathB() and milePosts() to drawPathLatLong() and 
  milePostsLatLong(); contextualize subtitle. 


### cholera 0.8.0.9029

- add/set plot.walking_path(mileposts = TRUE).


### cholera 0.8.0.9028

- set/use quandrantCoordinatesB() as default quandrantCoordinates().


### cholera 0.8.0.9027

- plot case and pump addresses in plot.latlong_walking_path().


### cholera 0.8.0.9026

- change "pump.id" to "id" in latlongEmbed() and latlongNearestPump().


### cholera 0.8.0.9025

- amend subtitle in latlongEuclideanPath().


### cholera 0.8.0.9024

- enable latlongEuclideanPath(case.location).
- amend eucl.data and format code.
- make mileposts conditional on path distancce.


### cholera 0.8.0.9023

- change "pump.id" to "id" in latlongOrthoPump().


### cholera 0.8.0.9022

- add latlongEuclideanPath().
- add milepost graphic to latlongEuclideanPath().


### cholera 0.8.0.9021

- update latlong.ortho.addr, latlong.ortho.pump and latlong.ortho.pump.vestry 
  (GCP v.02).


### cholera 0.8.0.9020

- amend/update latlongOrthoAddress() for GCPs v.02.


### cholera 0.8.0.9019

- add note about missing Clifford Street segment.


### cholera 0.8.0.9018

- add/set segmentHighlight(latlong = FALSE, rotate.label = FALSE).


### cholera 0.8.0.9017

- add missing Clifford Street segment; use GCPs v.02.


### cholera 0.8.0.9016

- add cliffordStreet().


### cholera 0.8.0.9015

- use specific street IDs in unitMeter().


### cholera 0.8.0.9014

- add/set streetHighlight(latlong = FALSE).


### cholera 0.8.0.9013

- add latlongOrthoLandmarks() prototype.


### cholera 0.8.0.9012

- remove from NAMESPACE latlong functions that document data.


### cholera 0.8.0.9011

- remove orthogonal code from latlongLandmarks().


### cholera 0.8.0.9010

- remove unused functions from latlongCoordinates().


### cholera 0.8.0.9009

- use streetLength(latlong = TRUE) and clean code in latlongStreetNameLocator().


### cholera 0.8.0.9008

- fix distanceTime(distance.unit = "native").


### cholera 0.8.0.9007

- add/set streetLength(latlong = FALSE).


### cholera 0.8.0.9006

- add/set snowMap(add.axes_box = FALSE).


### cholera 0.8.0.9005

- reset (delete) lon-lat for recomputation in latlongFrame() and latlongRoads().
- remove ::: operators and un-export latlongRoads().


### cholera 0.8.0.9004

- use "_&_" as coordinate delimeter in latlongRoads().
- amend classification error diagnostic in latlongRoads().


### cholera 0.8.0.9003

- fix addRoads() in snowMap(add.tanaka = FALSE).


### cholera 0.8.0.9002

- add tanakaContourPlot().


### cholera 0.8.0.9001

- add latlongStreetNameLocator().


### cholera 0.8.0.9000

- enable snowMap(add.landmarks = TRUE).
- add/set addLandmarks(latlong = FALSE).
- add/set addLandmarks(text.col = "black").
- add Golden Square and Soho Square to addLandmarks(latlong = TRUE).
- add point only for St Luke's Church in addLandmarks(latlong = FALSE).
- add point and pos = 4 label for St Luke's Church in 
  addLandmarks(latlong = TRUE).
- enable addLandmarks(highlight.perimeter = TRUE, latlong = TRUE).
- add Adam and Eve Court and Falconberg Court and Mews to 
  addLandmarks(latlong = TRUE).


### cholera 0.8.0

#### New Functions

- caseDistance().
- pumpFatalities().


#### New Data

- latlong.ortho.addr, latlong.ortho.pump and latlong.ortho.pump.vestry.
- frame.data.


#### Fixes

- fix addVoronoi(color).
- fix nearestPump(metric = "euclidean", vestry = TRUE).
- fix/change unitMeter(distance.unit = "native").
- use aes_string() in profile2D().


#### Data Changes

- use Poland Street as street address for St James Workhouse.
- re-compute ortho.proj with amended unstackFatalities().
- roadSegmentFix() places cases 440 and 145 on road segment "259-1".


#### Function Changes

- add exception for observed v. expected in neighborhoodData() via embedSites().
- add/set addCase(pch = 1, cex = 1, point.lwd = 2).
- add addCase(case %in% c("all", "anchor"), col = col).
- add/set addFrame(col = "black").
- add/set latlong = FALSE in addFrame() and addRoads().
- add/set plot.euclidean(add.title = TRUE).
- add/set plot.walking_path(stacked = TRUE).
- add/set segmentLocator(cex.text = 0.67).
- add/set streetHighlight(col = "red", lwd = 3).
- add/set walkingPath(null.origin.landmark = FALSE).
- add/use duplicateNode() in neighborhoodData() via embedSites().
- amend addVoronoi().
- amend error/exception handling in walkingPath().
- amend unstackFatalities() and use roadSegmentFix().
- fix/amend milePosts() for distance and time.


#### Longitude and Latitude Prototypes

- add/set snowMap(latlong = FALSE).
- add/set addPump(latlong = FALSE).
- add/ser roadSegments(latlong = FALSE).
- add/set voronoiPolygons(latlong = FALSE).
- latlongNeighborhoodVoronoi().
- latlongNeighborhoodWalking().
- latlongWalkingPath().


#### Documentation

- note on "computing Voronoi diagrams with geographic data".


#### Archived/Deprecated Functions

- roadHighlight().


### cholera 0.7.9

#### Note

- an interim release to address code changes in 'deldir' v1.0-2.

#### New Function

- add streetNames().

#### Data Change

- amend oxford.weather data to include entire data set.

#### Fixes

- amend code for 'deldir' v1.0-2: addDelaunay().
- fix addSnow().
- fix pumpTokens() for plot.walking(type = "roads").

#### Function Changes

- add/set addPump(cex = 1).
- add/set segmentHighlight(col = "red").
- add/set plot.oxfordWeather(month = "september").
- amend plot.oxfordWeather(statistic = "rain").
- change addDelauny() to addDelaunay().
- change type = "road" to "roads" in plot.walking().
- "gray" out unobserved and unselected pumps in pumpTokens().


### cholera 0.7.5

#### New Data

- oxford.weather

#### New Functions

- isoLines()
- isoVertices()
- oxfordWeather()
- povertyLondon()
- segmentHighlight()
- winterTemperatures()

#### Function Changes

- add addCase(case = NULL).
- add and set neighborhoodVoronoi(case.location = "address").
- add and set neighborhoodVoronoi(pump.location = "nominal").
- add subtitles for selected pumps in plot.euclidean().

#### Fixes

- fix unneeded warnings in addEuclideanPath().
- fix typo, from Silver Street to Cross Street, for meter benchmark in roads
  vignette.

#### Vignettes

- add discussion of Euclidean and Voronoi "expected" neighborhoods.


### cholera 0.7.0

#### New Feature

- support for parallel computation on Windows.

#### Fixes

- fix computation of core in neighborhoodWalking().
- fix nearestPump(metric = "euclidean", case.set = ("observed", "expected")).

#### Function Changes

- rename and amend simWalkingDistance() to simulateWalkingDistance().
- deprecate case.set argument in addNeighborhoodCases().
- add nearestPump(metric = "walking", case.set = "expected").
- amend expectedCount() for summary.walking().
- add unstackAuxiliaryFunctions.R

#### Vignettes

- add Parallelization vignette.


### cholera 0.6.5

#### Fixes

- fix plot.walking_path() timeposts.
- fix plot.walking_path(observed = FALSE).
- fix St James Workhouse for walkingPath(type = "cases").
- fix St James Workhouse for euclideanPath(type = "cases").
- fix city squares for plot.euclidean_path().
- fix computed time in nearestPump() and amend sim.walking.distance.

#### New Data

- add sim.walking.distance.

#### Function Changes

- add nearestPump(metric = "euclidean").
- add distance.unit argument to addWhitehead().
- add pch and point.size arguments to addNeighborhoodCases().
- add pos argument to addCase().
- add output argument to voronoiPolygons().
- add summary.euclidean(), summary.voronoi() and summary.walking().
- add multi.core argument to simWalkingDistance().
- add case.location to addEuclideanPath().
- add 'case' argument to pumpCase().
- amend travelingSalesman() argument in addSnow().
- amend print.euclidean(), print.voronoi() and print.walking().
- amend argument 'case' to 'data' in addKernelDensity().
- amend plot.walking() titles.
- deprecate pearlString() in favor of travelingSalesman().
- deprecate statistic argument in neighborhoodVoronoi().
- enable col argument in addRoads().
- enable 'address' and 'nominal' cases in plot.voronoi().
- exclude landmarks from case.set - "expected".
- rename deldirVertices() back to voronoiPolygons().

#### Vignettes

- rename "deldirPolygons()" to "voronoiPolygons()".
- remove Pump Neighborhoods vignette.

#### DESCRIPTION

- remove 'scales' from Imports.


### cholera 0.6.0

#### Fixes

- fix title in euclideanPath(type = "case-pump").
- fix destination label for walkingPath(destination = NULL).

#### Data Changes

- add Earl of Aberdeen residence (Argyll House).
- nominal and orthogonal coordinates for landmarks.

#### Function Changes

- addNeighborhood() -> addNeighborhoodWalking()

#### Function Changes - new arguments

- addSnow(type = "perimeter", line.width = 2)
- neighborhoodData(embed = TRUE, embed.landmarks = TRUE)
- neighborhoodEuclidean(case.set = "expected")
- plot.voronoi(voronoi.cells = TRUE, delauny.triangles = FALSE)
- snowMap(...)
- streetNameLocator(add.subtitle = TRUE, token = id)
- streetNumberLocator(add.subtitle = TRUE, token = id)

#### Function Changes - polygon.method argument

- addNeighborhoodEuclidean(polygon.method = "traveling.salesman")
- plot.euclidean(polygon.method = "traveling.salesman")

- addNeighborhoodWalking(polygon.method = "pearl.string")
- plot.walking(polygon.method = "pearl.string")

#### Function Change - landmarks as origin and/or destination (treated as cases)

- euclideanPath()
- walkingPath()
- find nearest case or landmark, given pump (i.e., reverse lookup)

#### Function Changes - case.location argument: "address" or "nominal"

- addVoronoi(case.location = "nominal")
- euclideanPath(case.location = "nominal")
- neighborhoodEuclidean(case.location = "nominal")
- addNeighborhoodEuclidean(case.location = "nominal")

#### New Functions

- addCase()
- addDelauny()
- addNeighborhoodCases()
- deldirVertices()
- orthogonalProjection()
- profile2D()
- profile3D()
- streetHighlight()

#### New Exported Functions

- fixFatalities()
- landmarkData()

#### New S3 Function

- pearsonResiduals()
- plot.neighborhood_data()

#### New Vignette

- "deldirVertices(): Tiles, Triangles and Polygons"

#### Deprecated Functions

- euclideanDistance()
- walkingDistance()


### cholera 0.5.1

#### Fixes

- backward compatibility (R 3.4.4) related to base::isFALSE() & bug fix.
- fix for multiple results in walkingDistance() and walkingPath().

#### Function Changes

- enable ellipses (...) in plot.time_series() (#1).
- enable ellipses and negative selection in addPump().
- consolidate addEuclideanPath(), euclideanDistance(), euclideanPath(),
  walkingDistance() and walkingPath()

#### New Functions

- addBorder()
- addRoads()
- mapRange()


### cholera 0.5.0

#### Data Changes

- regular.cases and sim.ortho.proj:
  increase number of observations from 5K to 20K.

#### Function Changes

- "alpha.level" argument to control path transparency
    addEuclideanPath() and addWalkingPath()

- distance and time based "mileposts"
    addEuclideanPath() and addWalkingPath().
    plot.euclidean_path() and plot.walking_path().
    addMilePosts().

- "pump.subset" and "pump.select" arguments
    addCase(), addKernelDensity(), addMilePosts(), addNeighborhood(),
    neighborhoodEuclidean(), neighborhoodWalking()

- "walking.speed" argument added to:
    addMilePosts(), nearestPump(),
    addEuclideanPath(), euclideanDistance(), euclideanPath(),
    addWalkingPath(), walkingDistance(), walkingPath()

- euclideanDistance() no longer S3.
    generic S3 functionality moved to euclideanPath().

- multiCore() moved to multiCore.R.

- neighborhoodVoronoi()
    plot.voronoi() adds "euclidean.paths" argument for star graph.

- neighborhoodWalking()
    "area.polygons" related functions for plot_walking() moved to
    pearlString.R.

- simulateFatalities():
  default is now 20K observations.
  use proximate in addition to orthogonal distances to find "addresses".

- snowMap() new arguments:
  "add.cases", "add.pumps", "add.roads".

- unitMeter() default unit of measurement is now "meter".

- walkingAuxillaryFunctions.R:
    location of walking related helper functions.

- walkingDistance() no longer S3.
    generic S3 functionality moved to walkingPath().

#### New Functions

- addCase()
- addEuclideanPath()
- addMilePosts()
- addNeighborhood()
- addWalkingPath()()
- distanceTime()

#### New S3 Functions

- euclideanPath()
- walkingPath()
- neighborhoodEuclidean()

#### Vignette Changes

- Lab Notes available online and on GitHub:
  "duplicate.missing.cases.notes"
  "pump.neighborhoods.notes"
  "unstacking.bars.notes"


### cholera 0.4.0

#### Data Changes

- ortho.proj.pump and ortho.proj.pump.vestry now include node ID.

- roads and road.segments amend street names:
    "Unknown-B" to "Tent Court" (Edmund Cooper's map).
    "Unknown-D" to "St James's Market" (https://maps.nls.uk).
    "Unknown-E" to "Market Street (II)" (https://maps.nls.uk).

#### Function Changes

- addKernelDensity()
  uses "pump.subset" and "pump.select" arguments.

- addLandmarks()
  add landmarks from Edmund Cooper's map.

- classifierAudit() can return coordinates of address.

- nearestPump() now incorporates nearestPath().

- neighborhoodWalking()
  segment and sub-segment implementation.

- pumpData()
  returns node ID.

- timeSeries()
  includes day of the week.

- walkingDistance()
  add "simulated" expected cases.

#### New Functions

- addNeighborhood()

#### New S3 Implementations

- plot.walking
  type = "area.points" and type = "area-polygons".
  type = "area-polygons" via pearlString() replaces alphahull::ashape().

- print.walking() uses expectedCount().

#### Vignette Changes

- add "Kernel Density Plot".
- update "Pump Neighborhoods" with discussion of area plots.


### cholera 0.3.0

#### Data Changes

- ortho.proj:
    reclassify case 483:
      Pulteney Court (I) ("242-1") -> Little Windmill Street ("326-2").
    reclassify cases 369, 434, 11, 53, 193:
      Poland Street ("194-1") -> St James Workhouse ("148-1").

#### Function Changes

- addSnow()
    "area", "street" and "boundary" graphical annotation.

- caseLocator()
    highlight home road segment.

- neighborhoodWalking()
    "case-set" argument: "observed", "expected" and "snow".
    updated implementation and improved performance.
    pre-computed configurations from version 0.2.1 removed.

- segmentLocator(), streetNameLocator() and streetNumberLocator()
    highlight segment or street cases.
    option to plot all cases, anchor cases or no cases.

#### New S3 Implementations

- timeSeries()
- walkingDistance()
    incorporates and deprecates walkingPath().

#### New Functions

- addIndexCase()
- nearestPath()
- nearestPump()
- nodeData()
- segmentLength()
- snowNeighborhood()
- streetLength()
- unitMeter()

#### New S3 Functions

- classifierAudit()
- euclideanDistance()


### cholera 0.2.1

- Initial CRAN release.
