"
RowanBrowserService handles services that fall outside
the scope of other services.
"
Class {
	#name : 'RowanBrowserService',
	#superclass : 'RowanService',
	#instVars : [
		'projects',
		'removedMethods',
		'allClasses',
		'hierarchyServices',
		'testPackages',
		'testCount',
		'dictionaries',
		'selectedClass'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanBrowserService >> abortTransaction [

	System abortTransaction.
	self updateProjects.
	self updateDictionaries. 
	self packagesWithTests.
	self updateType: #aborted:browser:.
]

{ #category : 'client commands' }
RowanBrowserService >> allClasses [
	allClasses := SortedCollection sortBlock: [:x :y | x name < y name].
	allClasses addAll: (organizer classes collect:[:class | 
			| service |
			service := RowanClassService new name: class name. 
			service packageName: class rowanPackageName.
			service projectName: class rowanProjectName.
			service]
			).
	allClasses := allClasses asArray. 
	updateType := #classes. "#classes not used at the moment so no updates will be done"
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> classHierarchy [
	| theClasses |
	theClasses := allClasses collect:[:classService | classService theClass].
	hierarchyServices := self classHierarchy: theClasses. 
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
]

{ #category : 'private' }
RowanBrowserService >> classHierarchy: theClasses [
  | superclassChains levels services hierarchies toExpand |
  superclassChains := self superclassChainsFor: theClasses.
  hierarchies := self extendHierarchies: superclassChains.
  levels := self hierarchiesByLevel: hierarchies.
  services := Dictionary new.
  toExpand := Set new.
  self
    services: services
    from: levels
    expand: toExpand
    classes: theClasses.
  hierarchyServices := services reject: [ :array | array isEmpty ].
  hierarchyServices copy
    keysAndValuesDo: [ :key :value | 
      hierarchyServices
        at: key
        put:
          (value asSet asSortedCollection: [ :x :y | x name < y name ]) asArray ].
  ^ hierarchyServices
]

{ #category : 'client commands' }
RowanBrowserService >> classHierarchyForDictionariesNamed: dictionaryNames [
  | theClasses |
  theClasses := OrderedCollection new.
  dictionaryNames
    do: [ :dictionaryName | 
      (Rowan globalNamed: dictionaryName)
        keysAndValuesDo: [ :key :value | 
          value isClass
            ifTrue: [ theClasses add: value ] ] ].
  hierarchyServices := self classHierarchy: theClasses.
  updateType := #'classHierarchyUpdate:browser:'.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> classHierarchyForPackagesNamed: packageNames [
  | theClasses |
  theClasses := OrderedCollection new.
  packageNames
    do: [ :packageName | 
      theClasses
        addAll:
          ((Rowan image loadedPackageNamed: packageName) loadedClasses 
            collect: [ :cls | cls handle]).
      theClasses
        addAll:
          ((Rowan image loadedPackageNamed: packageName) loadedClassExtensions
            collect: [ :cls | cls handle]) ].
  hierarchyServices := self classHierarchy: theClasses.
  updateType := #'classHierarchyUpdate:browser:'.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> compileClass: definitionString [

	|  anonymousMethod |
	anonymousMethod := definitionString _compileInContext: nil 
       symbolList: Rowan image symbolList.
	UserGlobals at: #jadeiteCompileClassMethod put: anonymousMethod.
]

{ #category : 'client commands' }
RowanBrowserService >> defaultClassHierarchy [
	hierarchyServices := Dictionary new.   
	organizer hierarchy keysAndValuesDo: [:key :value |
		| classService |
		classService := key == #nil ifTrue:[#nil] ifFalse: [RowanClassService basicForClassNamed: key name].
		hierarchyServices at: classService put: (value collect:[:cls | RowanClassService basicForClassNamed: cls name]) asArray.
	].
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanBrowserService >> findRemovedServices: services [

	services do:[:service | 
		service wasDeleted ifTrue:[
				service updateType: #removed:.
				RowanCommandResult addResult: service.
		]
	].
]

{ #category : 'window registry' }
RowanBrowserService >> openWindows [

	"for testing"

	^SessionTemps current at: #rowanServicesWindowRegistry ifAbsent:[]
]

{ #category : 'client commands' }
RowanBrowserService >> packagesWithTests [
	testPackages := Set new. 
	testCount := 0. 
	TestCase allSubclasses do:[:testSubclass |
		testSubclass selectors do:[:selector |
			| packageName |
			(selector size >= 4 and:[
			(selector copyFrom: 1 to: 4) asString = 'test']) ifTrue:[
				packageName := (testSubclass compiledMethodAt: selector) rowanPackageName. 
				packageName = Rowan unpackagedName ifFalse:[
					testCount := testCount + 1. 
					testPackages add: (RowanPackageService forPackageNamed: packageName)]]]]. 
	updateType := #testPackages:. 
	testPackages := testPackages asArray. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanBrowserService >> recompileMethodsAfterClassCompilation [
  "compileClass: must be run first"

  | theClass classService packageService |
  theClass := [ (UserGlobals at: #'jadeiteCompileClassMethod') _executeInContext: nil ]
    ensure: [ UserGlobals at: #'jadeiteCompileClassMethod' put: nil ].
  classService := RowanClassService new name: theClass name.
  classService update.
  classService isNewClass: true.
  packageService := RowanPackageService
    forPackageNamed: classService packageName.
  packageService update.
  packageService selectedClass: classService.
  RowanCommandResult addResult: classService.
  selectedClass := classService.
  updateType := #none. 
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> releaseWindowHandle: integer [

	| registry |
	Rowan loggingServiceClass current logComment: 'Release window with handle: ', integer printString. 
	registry := SessionTemps current at: #rowanServicesWindowRegistry ifAbsent:[^self].
	registry removeKey: integer ifAbsent: []
]

{ #category : 'client commands' }
RowanBrowserService >> reloadProjects: projectServices andUpdateServices: services [
	| projectNames |
	projectServices do:[:projectService |
		projectService reloadProject].
	projectNames := projectServices collect: [:projectService | projectService name]. 
	services do:[:service | 
		(projectNames includes: service rowanProjectName) ifTrue:[service update]].
]

{ #category : 'client commands' }
RowanBrowserService >> removeDictionariesNamed: dictionaryNames [
	"remove from both transient & persistent symbol lists" 

	dictionaryNames do:[:dictionaryName | 
		| dictionaryNameSymbol |
		dictionaryNameSymbol := dictionaryName asSymbol.
		(Rowan image symbolList names includes: dictionaryNameSymbol) ifTrue:[
			Rowan image symbolList removeDictionaryNamed: dictionaryNameSymbol].
		(System myUserProfile symbolList names includes: dictionaryNameSymbol) ifTrue:[
			System myUserProfile symbolList removeDictionaryNamed: dictionaryNameSymbol]].
	self updateDictionaries.
	System commitTransaction.

]

{ #category : 'client commands' }
RowanBrowserService >> removeMethods: methodServices [

	| notRemoved |
	notRemoved := Array new. 
	removedMethods := Array new. 
	methodServices do: [:methodService |
		| classService |
		classService := RowanClassService forClassNamed: methodService className. 
		classService meta: methodService meta. 
		classService removeSelector: methodService selector ifAbsent:[notRemoved add: methodService].
		classService updatePackageProject.
		(notRemoved includes: methodService) ifFalse:[
			methodService updateType: #removed:.
			RowanCommandResult addResult: methodService]. 
		removedMethods add: methodService].
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', (notRemoved collect:[:ea | ea selector]) printString].
	updateType := #methodsRemoved:. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanBrowserService >> saveRootObject: oop windowHandle: integer [

	" a window has been opened on the client. Save the 
	root object of the window so it won't be recycled"
	| dictionary |
	dictionary := SessionTemps current at: #rowanServicesWindowRegistry ifAbsentPut: [Dictionary new].
	dictionary at: integer put: (Object _objectForOop: oop).
]

{ #category : 'other' }
RowanBrowserService >> selectedClass [

	^selectedClass
]

{ #category : 'perform' }
RowanBrowserService >> servicePerform: symbol withArguments: collection [
	super perform: symbol withArguments: collection.
]

{ #category : 'client commands' }
RowanBrowserService >> unloadProjectsNamed: array [
	array do:[:projectName |
		| project |
		project := Rowan image loadedProjectNamed: projectName ifAbsent:[].
		project ifNotNil: [
			Rowan projectTools delete deleteProjectNamed: projectName]]. 
	self updateProjects
]

{ #category : 'update' }
RowanBrowserService >> update [

	self updateProjects
]

{ #category : 'client commands' }
RowanBrowserService >> updateDictionaries [

	dictionaries := Rowan image symbolList names collect:[:name | RowanDictionaryService new name: name asString].
	dictionaries := dictionaries asOrderedCollection. 
	updateType := #dictionaryListUpdate:.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> updateProjects [
	| sortedProjects | 
	sortedProjects := SortedCollection sortBlock: [:a :b | a name < b name]. 
	sortedProjects addAll:  Rowan image loadedProjects.
	projects := sortedProjects collect:[:project | RowanProjectService newNamed: project name].
	updateType := #projectsUpdate:browser:.
	RowanCommandResult addResult: self
]
