"
RowanBrowserService handles services that fall outside
the scope of other services with an emphasis on browser-
type tasks.
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
  | autoCommitService autoCommitState |
  autoCommitState := RowanService autoCommit == #'failed'
    ifTrue: [ true ]
    ifFalse: [ RowanService autoCommit ].
  System abortTransaction.
  autoCommitService := RowanAutoCommitService new.
  autoCommitService autoCommit: autoCommitState.
  self updateProjects.
  self updateDictionaries
]

{ #category : 'client commands' }
RowanBrowserService >> abortTransactionAndUpdateServices: services [
  self abortTransaction.
  services
    do: [ :service | 
      "we just updated projects, package, & dictionary services"
      (service isProjectService not
        and: [ service isDictionaryService not and: [ service isPackageService not ] ])
        ifTrue: [ 
          service
            organizer: organizer;
            updateLatest ] ]
]

{ #category : 'client commands' }
RowanBrowserService >> allClasses [
	allClasses := self basicAllClasses.
	updateType := #dontUpdate. 
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> allPackages [
	| theProjects |
	theProjects := Rowan image loadedProjects asArray.
	projects := theProjects
		collect: [ :project | 
			| projectService |
			projectService := RowanProjectService newNamed: project name.
			projectService packages: projectService packageServices.
			projectService packages
				do: [ :packageService | packageService projectName: project name ].
			projectService ].
	updateType := #'dontUpdate'.
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

{ #category : 'client commands' }
RowanBrowserService >> newProjectNamed: projectName windowHandle: handle [
	| definedProjectService project |
	definedProjectService := RowanDefinedProjectService new name: projectName.
	definedProjectService projectOop: (project := Rowan newProjectNamed: projectName) asOop.
	self saveRootObject: definedProjectService projectOop windowHandle: handle. 
	definedProjectService := definedProjectService updateType: #newProject:.
	definedProjectService specService: (RowanSpecificationService new initialize: project _loadSpecification asOop).
	RowanCommandResult addResult: definedProjectService
]

{ #category : 'window registry' }
RowanBrowserService >> openWindows [

	"for testing"

	^SessionTemps current at: #rowanServicesWindowRegistry ifAbsent:[]
]

{ #category : 'client commands' }
RowanBrowserService >> packagesWithTests [
	organizer := ClassOrganizer new.	"when we call this method, our world has changed from a reload, etc."
	testPackages := Set new.
	testCount := 0.
	testPackages := Set new.
	testCount := 0.
	(organizer allSubclassesOf: TestCase)
		do: [ :sub | 
			| packageName testMethodCount |
			testMethodCount := (sub sunitSelectors
				select: [ :each | each beginsWith: 'test' ]) size.	"sending #testSelectors was slower"
			testCount := testCount + testMethodCount.
			testMethodCount > 0
				ifTrue: [ 
					packageName := sub rowanPackageName.
					packageName = Rowan unpackagedName
						ifFalse: [ 
							testPackages
								add:
									((RowanPackageService forPackageNamed: packageName)
										updateProjectName;
										yourself) ].
					(Rowan image loadedClassExtensionsForClass: sub)
						do: [ :loadedThing | 
							testPackages
								add:
									((RowanPackageService forPackageNamed: loadedThing loadedPackage name)
										updateProjectName;
										yourself)	"don't update the entire package for performance improvement" ] ] ].
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
  self updateSymbols: (Array with: theClass name asString).
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
  | projectNames answeringService |
  services do: [ :service | service organizer: organizer ].
  projectServices do: [ :service | service organizer: organizer ].
  projectServices do: [ :projectService | projectService reloadProject ].
  projectNames := projectServices
    collect: [ :projectService | projectService name ].
  services
    do: [ :service | 
      (projectNames includes: service rowanProjectName)
        ifTrue: [ service updateLatest ] ].
  answeringService := RowanAnsweringService new organizer: organizer.
  answeringService updateAutocompleteSymbols
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
		classService updatePackageAndProject.
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
  dictionary := SessionTemps current
    at: #'rowanServicesWindowRegistry'
    ifAbsentPut: [ Dictionary new ].
  dictionary at: integer ifAbsentPut: [ Array new ].
  (dictionary at: integer) add: (Object _objectForOop: oop)

]

{ #category : 'accessing' }
RowanBrowserService >> selectedClass [

	^selectedClass
]

{ #category : 'accessing' }
RowanBrowserService >> selectedClass: object [

	selectedClass := object
]

{ #category : 'client commands' }
RowanBrowserService >> unloadProjectsNamed: projectNames [
	projectNames do: [ :projectName | (Rowan projectNamed: projectName) unload ].
	self updateProjects
]

{ #category : 'client commands' }
RowanBrowserService >> updateDictionaries [

	dictionaries := Rowan image symbolList names collect:[:name | RowanDictionaryService new name: name asString].
	dictionaries := dictionaries asOrderedCollection. 
	updateType ifNil: [updateType := OrderedCollection new]. 
	updateType add: #dictionaryListUpdate:.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanBrowserService >> updateProjects [
  | sortedProjects |
  self packagesWithTests. "make sure tests are always updated" 
  sortedProjects := SortedCollection sortBlock: [ :a :b | a name < b name ].
  sortedProjects addAll: Rowan image loadedProjects.
  projects := sortedProjects
    collect: [ :project | RowanProjectService newNamed: project name ].
  updateType := Array with: updateType with: #'projectsUpdate:browser:'. "temporary hack" 
  RowanCommandResult addResult: self
]

{ #category : 'update' }
RowanBrowserService >> updateSymbols: classNames [
  newCachedClasses addAll: classNames.
  updateType := #'addCachedSymbols:'
]
