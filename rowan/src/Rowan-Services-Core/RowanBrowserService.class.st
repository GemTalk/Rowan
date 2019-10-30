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
		'selectedClass',
		'newCachedSelectors',
		'newCachedClasses'
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
	allClasses := self basicAllClasses.
	updateType := #classes. "#classes not used at the moment so no updates will be done"
	RowanCommandResult addResult: self
]

{ #category : 'client commands support' }
RowanBrowserService >> basicAllClasses [
  | theClasses |
  theClasses := SortedCollection sortBlock: [ :x :y | x name < y name ].
  theClasses
    addAll:
      (organizer classes
        collect: [ :class | 
          | service |
          service := RowanClassService new name: class name.
          service packageName: class rowanPackageName.
          service projectName: class rowanProjectName.
          service ]).
  ^ theClasses asArray
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
  hierarchyServices := super classHierarchy: theClasses.
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
  | anonymousMethod |
  self confirmDuplicateName: definitionString.
  anonymousMethod := definitionString
    _compileInContext: nil
    symbolList: Rowan image symbolList.
  SessionTemps current at: #'jadeiteCompileClassMethod' put: anonymousMethod
]

{ #category : 'client commands support' }
RowanBrowserService >> confirmDuplicateName: definitionString [
  | className |
  className := (definitionString subStrings at: 3) copyWithout: $'.
  (Rowan image symbolList resolveSymbol: className asSymbol)
    ifNotNil: [ 
      className = selectedClass
        ifFalse: [ 
          (self confirm: 'Class name is already an object. Continue?')
            ifFalse: [ ^ Error signal: 'Class not compiled. Name already exists.' ] ] ]
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

{ #category : 'initialize' }
RowanBrowserService >> initialize [
  super initialize.
  newCachedSelectors := Array new.
  newCachedClasses := Array new
]

{ #category : 'accessing' }
RowanBrowserService >> newCachedClasses [
	^newCachedClasses
]

{ #category : 'accessing' }
RowanBrowserService >> newCachedClasses: object [
	newCachedClasses := object
]

{ #category : 'accessing' }
RowanBrowserService >> newCachedSelectors [
	^newCachedSelectors
]

{ #category : 'accessing' }
RowanBrowserService >> newCachedSelectors: object [
	newCachedSelectors := object
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
  TestCase allSubclasses
    do: [ :testSubclass | 
      | suite |
      suite := testSubclass buildSuite.
      suite tests
        do: [ :testCase | 
          | packageName compiledMethod |
          (testCase isKindOf: TestSuite)
            ifFalse: [ 
              compiledMethod := testCase class
                compiledMethodAt: testCase selector
                otherwise: nil.
              packageName := compiledMethod
                ifNil: [ testCase class rowanPackageName ]
                ifNotNil: [ compiledMethod rowanPackageName ].
              packageName = Rowan unpackagedName
                ifFalse: [ 
                  testCount := testCount + 1.
                  testPackages
                    add: (RowanPackageService forPackageNamed: packageName) ] ] ] ].
  updateType := #'testPackages:'.
  testPackages := testPackages asArray.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> recompileMethodsAfterClassCompilation [
  "compileClass: must be run first"

  | theClass classService packageService projectService |
  theClass := [ 
  [ (SessionTemps current at: #'jadeiteCompileClassMethod') _executeInContext: nil ]
    on: CompileWarning
    do: [ :ex | ex resume ] ]
    ensure: [ SessionTemps current at: #'jadeiteCompileClassMethod' put: nil ].
  classService := RowanClassService new name: theClass name.
  classService update.
  classService isNewClass: true.	"if nothing else, the dirty state of the package/project services
	should be updated. Would like a less heavy weight solution than this, though."
  packageService := RowanPackageService
    forPackageNamed: classService packageName.
  packageService update.
  projectService := RowanProjectService newNamed: packageService projectName.
  projectService update.
  packageService selectedClass: classService.
  RowanCommandResult addResult: classService.
  selectedClass := classService.
  updateType := #'none'.
  self updateSymbols: theClass name.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> releaseWindowHandle: integer [
  | registry |
  Rowan loggingServiceClass current
    logComment: 'Release window with handle: ' , integer printString.
  registry := SessionTemps current
    at: #'rowanServicesWindowRegistry'
    ifAbsent: [ ^ self ].
  registry removeKey: integer ifAbsent: [  ]
]

{ #category : 'client commands' }
RowanBrowserService >> reloadProjects: projectServices andUpdateServices: services [
	| projectNames |
	services do: [:service | service organizer: organizer]. 
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

{ #category : 'other' }
RowanBrowserService >> selectedClass: object [

	selectedClass := object
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

{ #category : 'update' }
RowanBrowserService >> updateSymbols: className [
  | browserService |
  browserService := RowanBrowserService new.
  browserService newCachedClasses add: className.
  browserService updateType: #'addCachedSymbols:'.
  RowanCommandResult addResult: browserService
]
