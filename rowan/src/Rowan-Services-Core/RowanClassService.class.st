"
Most class operations done here. 

selectedMethods - client side selection. Used after a method compile.
"
Class {
	#name : 'RowanClassService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'comment',
		'instVarNames',
		'classVarNames',
		'classInstVarNames',
		'superclassName',
		'subclassType',
		'poolDictionaryNames',
		'classType',
		'meta',
		'isExtension',
		'version',
		'versions',
		'oop',
		'template',
		'filters',
		'filterType',
		'methods',
		'selectedPackageServices',
		'packageName',
		'definedPackageName',
		'selectedMethods',
		'projectName',
		'hierarchyServices',
		'variables',
		'categories',
		'isTestCase',
		'expand',
		'visibleTests',
		'isNewClass',
		'updateAfterCommand',
		'isInSymbolList',
		'dictionaryName',
		'wasRemoved',
		'renamedName'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanClassService class >> basicForClassNamed: className [ 
	"Don't get method services. Efficient for classes with many methods"
	^self new basicForClassNamed: className
]

{ #category : 'instance creation' }
RowanClassService class >> forClassNamed: className [ 

	^self new forClassNamed: className

]

{ #category : 'instance creation' }
RowanClassService class >> forClassNamed: className meta: aBoolean [

	| inst |
	inst := self forClassNamed: className subStrings first.
	inst meta: aBoolean.
	^inst

]

{ #category : 'instance creation' }
RowanClassService class >> forClassNamed: className package: packageName [

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst

]

{ #category : 'instance creation' }
RowanClassService class >> minimalForClassNamed: className [ 
	"Don't get method services. Efficient for classes with many methods"
	^self new minimalForClassNamed: className
]

{ #category : 'comparing' }
RowanClassService >> = classService [
	(classService class canUnderstand: #isClassService) ifFalse:[^false].
	^classService isClassService
			ifTrue: [  name asString = classService name asString and: [meta = classService meta]]
			ifFalse: [^false]
]

{ #category : 'client commands' }
RowanClassService >> addCategory: string [

	| theClass |

	theClass := self theClass.
	meta ifTrue:[theClass := theClass class]. 
	theClass addCategory: string.
]

{ #category : 'constants' }
RowanClassService >> addSubclassWarningString [

	^'Superclass is not packaged. Enter the desired package name'
]

{ #category : 'client commands' }
RowanClassService >> allSubclassServices [
  | subclassServices |
  subclassServices := self theClass subclasses asArray
    collect: [ :aClass | RowanClassService minimalForClassNamed: aClass name ].
  hierarchyServices := Dictionary new.
  hierarchyServices at: #'expand' put: subclassServices.
  (hierarchyServices at: #'expand')
    do: [ :classService | classService allSubclassServices ]
]

{ #category : 'Accessing' }
RowanClassService >> allTests [
  | allSelectors theClass |
  self isTestCase
    ifFalse: [ ^ Array new ].
  theClass := self theClass thisClass.
  theClass isAbstract
    ifTrue: [ ^ Array new ].
  allSelectors := self theClass thisClass allTestSelectors.
  ^ allSelectors
    collect: [ :selector | 
      | methodService |
      methodService := RowanMethodService
        forSelector: selector
        class: (theClass whichClassIncludesSelector: selector asString)
        meta: false
        organizer: organizer.
      methodService
        definedClassName: (theClass whichClassIncludesSelector: selector asString) name asString.	"may get changed in client"
      methodService ]
]

{ #category : 'testing' }
RowanClassService >> arePackageAndProjectClean [

	^self packageIsDirty not and:[self projectIsDirty not]
]

{ #category : 'initialization' }
RowanClassService >> basicForClassNamed: className [ 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	theClass isNil ifTrue:[oop := nil. ^self].
	self basicRefreshFrom: theClass.
]

{ #category : 'initialization' }
RowanClassService >> basicRefreshFrom: theClass [
	| classOrMeta theFilters |
	oop := theClass asOop.
	command := nil. 
	commandArgs := nil. 
	superclassName := theClass superClass ifNotNil:[:theSuper | theSuper name asString]. 
	comment := theClass rwComment. 
	organizer ifNil: [organizer := ClassOrganizer new]. "for Jade and tests"
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	self setComment.
	template := self classCreationTemplate.
	theFilters := SortedCollection new.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	packageName := definedPackageName := classOrMeta rowanPackageName.
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	self setIsTestCase.
	self updateIsExtension.
]

{ #category : 'Accessing' }
RowanClassService >> behavior [

	| behavior |
	behavior := self theClass. 
	meta == true ifTrue:[behavior := behavior class].
	^behavior
]

{ #category : 'client commands' }
RowanClassService >> classComment: string [
	| theClass |
	theClass := self theClass. 
	theClass rwComment: string.
]

{ #category : 'rowan' }
RowanClassService >> classCreationTemplate [
	
	^self browserTool classCreationTemplateForClass: self theClass hybridBrowser: true.
]

{ #category : 'client commands' }
RowanClassService >> classHierarchy [
	hierarchyServices := self classHierarchy: (Array with: self theClass). 
	RowanCommandResult addResult: self.
]

{ #category : 'Accessing' }
RowanClassService >> classHierarchyNames [

	| names |
	names := Array new. 
	hierarchyServices keys do:[:classService | 
		classService == #nil ifFalse:[names add: classService name]].
	^names
]

{ #category : 'Accessing' }
RowanClassService >> classInstVarNames [
	^classInstVarNames

]

{ #category : 'Updating' }
RowanClassService >> classInstVarNames: newValue [
	classInstVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassService >> classOrMeta [

	^meta 
			ifTrue:[self theClass class] 
			ifFalse: [self theClass].
]

{ #category : 'instance creation' }
RowanClassService >> classServiceFromOop: anOop [
	| theClass className classService |
	theClass := Object _objectForOop: anOop. 
	className := theClass name. 
	classService := RowanClassService new name: className.
	^className asString = name asString ifTrue:[
			className asString = 'Object' 
				ifTrue:[
					classService basicRefreshFrom: theClass]
				ifFalse:[
					classService fastRefresh]]
		ifFalse:[
			classService minimalRefreshFrom: theClass]
]

{ #category : 'Accessing' }
RowanClassService >> classType [
	^classType

]

{ #category : 'Updating' }
RowanClassService >> classType: newValue [
	classType := newValue

]

{ #category : 'Accessing' }
RowanClassService >> classVarNames [
	^classVarNames

]

{ #category : 'Updating' }
RowanClassService >> classVarNames: newValue [
	classVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassService >> comment [
	^comment

]

{ #category : 'Updating' }
RowanClassService >> comment: newValue [
	comment := newValue

]

{ #category : 'constants' }
RowanClassService >> compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol [
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	
	[ [ [ [ method := aBehavior rwCompileMethod: methodString category: categorySymbol.]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: false ]]
			on: CompileError
			do: [:ex | ^nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume]]
					on: RwPerformingUnpackagedEditNotification
					do: [:ex | ex resume ] .
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] on: Error
		do: [:ex | ex return: method -> warnings]
]

{ #category : 'client commands' }
RowanClassService >> copyClassTo: newClassName [
  | newTemplate newClass newClassService index |
  (Rowan image symbolList resolveSymbol: newClassName)
    ifNotNil: [ ^ self inform: newClassName , ' already exists' ].
  index := template findPattern: (Array with: name) startingAt: 1.
  newTemplate := template copy.
  newTemplate removeFrom: index to: index + name size - 1.
  newTemplate insertAll: newClassName at: index.
  newClass := GsCurrentSession currentSession execute: newTemplate.
  newClassService := RowanClassService new name: newClassName.
  self theClass thisClass
    methodsDo: [ :selector :gsMethod | 
      newClassService
        compileMethod: gsMethod sourceString
        behavior: newClass
        symbolList: Rowan image symbolList
        inCategory:
          (self theClass thisClass categoryOfSelector: selector) asSymbol ].
  self theClass thisClass class
    methodsDo: [ :selector :gsMethod | 
      newClassService
        compileMethod: gsMethod sourceString
        behavior: newClass class
        symbolList: Rowan image symbolList
        inCategory:
          (self theClass thisClass class categoryOfSelector: selector) asSymbol ].
  newClassService update.
  (RowanPackageService new name: newClassService packageName) update.
  (RowanDictionaryService new name: dictionaryName) update
]

{ #category : 'Updating' }
RowanClassService >> definedPackageName: newValue [

	definedPackageName := newValue

]

{ #category : 'Updating' }
RowanClassService >> expand: boolean [

	expand := boolean
]

{ #category : 'client commands' }
RowanClassService >> fastRefresh [
	"pushes less information to ston so it's faster"

	| theClass |
	theClass := self theClass. 
	self refreshFrom: theClass. 
	methods do:[:service1 |
			service1 source: nil;
				stepPoints: Array new].
	visibleTests do:[:service2 |
			service2 source: nil;
				stepPoints: Array new.
			].
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanClassService >> fileoutCategories: array [
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	array do:[:category |
		self behavior fileOutCategory: category on: ws]. 
	answeringService answer: ws contents. 
	RowanCommandResult addResult: answeringService.
]

{ #category : 'client commands' }
RowanClassService >> fileoutClass [
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	ws nextPutAll: self behavior fileOutClass. 
	answeringService answer: ws contents. 
	RowanCommandResult addResult: answeringService.
]

{ #category : 'client commands' }
RowanClassService >> fileoutMethods: array [
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	array do:[:methodService |
		self behavior fileOutMethod: methodService selector on: ws]. 
	answeringService answer: ws contents. 
	RowanCommandResult addResult: answeringService.
]

{ #category : 'Accessing' }
RowanClassService >> filters [
	^filters

]

{ #category : 'Updating' }
RowanClassService >> filters: newValue [
	filters := newValue

]

{ #category : 'initialization' }
RowanClassService >> forClassNamed: className [ 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	self refreshFrom: theClass.
]

{ #category : 'client commands' }
RowanClassService >> fullHierarchy [
  | behavior sortedSubclasses |
  behavior := self theClass.
  hierarchyServices := Dictionary new.
  hierarchyServices at: #'expand' put: Array new.
  sortedSubclasses := behavior subclasses
    asSortedCollection: [ :x :y | x name < y name ].
  RowanCommandResult addResult: self.
  sortedSubclasses
    do: [ :subclass | 
      | classService |
      classService := (self classServiceFromOop: subclass asOop) meta: meta.
      (hierarchyServices at: #'expand') add: classService.
      classService allSubclassServices ]
]

{ #category : 'comparing' }
RowanClassService >> hash [
	^self name hash bitXor: meta hash
]

{ #category : 'Accessing' }
RowanClassService >> hierarchyServices [

	^hierarchyServices
]

{ #category : 'initialization' }
RowanClassService >> initialize [

	isExtension := false.
	selectedMethods := Array new.
	meta := false. "assume most of our work is on the instance side"
	selectedPackageServices := Array new.
	isNewClass := false.
	methods := Array new.
	isInSymbolList := true.
	categories := Array new.
]

{ #category : 'initialization' }
RowanClassService >> initializeCategoriesFor: classOrMeta [

	| theFilters |
	theFilters := SortedCollection new.
	classOrMeta env: 0 categorysDo: [:category :selector | theFilters add: category asString].
	categories := theFilters asOrderedCollection.
]

{ #category : 'method history' }
RowanClassService >> initializeMethodHistoryFor: source [
  "about to compile a method. If possible, ensure it's method history is setup."

  | rowanMethodHistory methodHistory selector methodService |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  selector := [ (Rowan platform parseSelectorFrom: source) asSymbol ]
    on: CompileWarning
    do: [ :ex | ex resume ].
  selector = #'_____could_not_parse_selector_from_method_source_____'
    ifTrue: [ ^ self	"invalid source, continue and let save method fail" ]
    ifFalse: [ 
      | compiledMethod |
      compiledMethod := (Object _objectForOop: oop)
        compiledMethodAt: selector
        environmentId: 0
        otherwise: nil.
      compiledMethod
        ifNil: [ ^ self	"we'll create history after the method is compiled" ].
      methodService := RowanMethodService
        forSelector: selector
        class: self theClass
        meta: meta
        organizer: organizer.
      methodHistory := rowanMethodHistory
        at: methodService
        ifAbsentPut: [ Array new ] ]
]

{ #category : 'initialization' }
RowanClassService >> initializeTestMethodsFor: aClass [
	| testSelectors |
	(aClass inheritsFrom: TestCase) ifTrue:[
		aClass isAbstract ifTrue:[^self]. 
		testSelectors := aClass thisClass suite tests collect:[:method | method selector]. 
		methods do:[:methodService | 
			methodService isTestMethod: (testSelectors includes: methodService selector)]].
]

{ #category : 'initialization' }
RowanClassService >> initializeVariablesFor: classOrMeta [

	| theFilters |
	theFilters := SortedCollection new.
	theFilters addAll: (classOrMeta allInstVarNames collect:[:instVar | instVar asString]).
	variables := theFilters asOrderedCollection.
]

{ #category : 'Accessing' }
RowanClassService >> instVarNames [
	^instVarNames

]

{ #category : 'Updating' }
RowanClassService >> instVarNames: newValue [
	instVarNames := newValue

]

{ #category : 'testing' }
RowanClassService >> isClassService [

	^true
]

{ #category : 'Updating' }
RowanClassService >> isExtension: boolean [

	isExtension := boolean

]

{ #category : 'Updating' }
RowanClassService >> isNewClass: boolean [
	isNewClass := boolean
]

{ #category : 'testing' }
RowanClassService >> isPackageClean [

	^self packageIsDirty not
]

{ #category : 'testing' }
RowanClassService >> isProjectClean [
  ^ self projectIsDirty not
]

{ #category : 'Updating' }
RowanClassService >> isTestCase [

	^isTestCase
]

{ #category : 'Updating' }
RowanClassService >> isTestCase: aBoolean [

	isTestCase := aBoolean
]

{ #category : 'Accessing' }
RowanClassService >> meta [

	^meta

]

{ #category : 'Updating' }
RowanClassService >> meta: anObject [

	meta := anObject

]

{ #category : 'Accessing' }
RowanClassService >> methods [

	"for testing"
	^methods
]

{ #category : 'private' }
RowanClassService >> methodServiceFrom: gsNMethod in: behavior compiltationResult: compilationResult [
	| methodService |

	methodService := RowanMethodService forGsNMethod: gsNMethod organizer: organizer. 
	methodService compilationWarnings: compilationResult value.
	^methodService
]

{ #category : 'private' }
RowanClassService >> methodServicesFor: classOrMeta organizer: theOrganizer [

	methods addAll: (classOrMeta selectors collect:[:sel | 
			RowanMethodService 
				forSelector: sel class: classOrMeta thisClass meta: meta organizer: theOrganizer])
]

{ #category : 'private' }
RowanClassService >> methodsIn: theClass categories: theCategories [

	| selectors |
	selectors := Array new. 
	theCategories do:[:category |
		selectors addAll: (theClass selectorsIn: category)]. 
	^methods select:[:methodService | selectors includes: methodService selector]
]

{ #category : 'testing' }
RowanClassService >> methodsNamed: selector [
	"For testing. Multiple because class could have both instance and class methods"

	^methods select:[:methodService | methodService selector = selector]
]

{ #category : 'initialization' }
RowanClassService >> minimalForClassNamed: className [ 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	self minimalRefreshFrom: theClass.
]

{ #category : 'initialization' }
RowanClassService >> minimalRefreshFrom: theClass [
	| classOrMeta  |
	command := nil. 
	commandArgs := nil. 
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	oop := theClass asOop.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	packageName := definedPackageName := classOrMeta rowanPackageName.
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	template := self classCreationTemplate.
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	self setIsTestCase.
]

{ #category : 'client commands' }
RowanClassService >> moveMethods: methodServices to: category [
	| behavior |
	behavior := self classOrMeta.
	methodServices do: [:methodService | 
			behavior rwMoveMethod: methodService selector toCategory: category.
			methodService category: category].
	self update. 
	self selectedMethods: methodServices.
]

{ #category : 'Accessing' }
RowanClassService >> name [
	^name

]

{ #category : 'Updating' }
RowanClassService >> name: newValue [
	name := newValue asString
]

{ #category : 'private' }
RowanClassService >> objectInBaseNamed: aString [

	^Rowan image symbolList objectNamed: aString asSymbol
]

{ #category : 'client commands' }
RowanClassService >> oneLevelClassHierarchy [
  "good for expanding an existing hierarchy quickly"

  | behavior sortedSubclasses |
  behavior := self theClass.
  hierarchyServices := Dictionary new.
  hierarchyServices at: #'expand' put: Array new.
  sortedSubclasses := behavior subclasses
    asSortedCollection: [ :x :y | x name < y name ].
  sortedSubclasses
    do: [ :subclass | 
      | classService |
      classService := (self classServiceFromOop: subclass asOop) meta: meta.
      (hierarchyServices at: #'expand') add: classService ].
  RowanCommandResult addResult: self
]

{ #category : 'Accessing' }
RowanClassService >> oop [
	^oop

]

{ #category : 'Updating' }
RowanClassService >> oop: newValue [
	oop := newValue

]

{ #category : 'testing' }
RowanClassService >> packageIsDirty [

	| behavior |
	behavior := self theClass.
	behavior rowanPackageName =  Rowan unpackagedName ifTrue:[^true]. "avoid a refresh by assuming it's dirty" 
	^(RowanPackageService new name: behavior rowanPackageName) rowanDirty
]

{ #category : 'Accessing' }
RowanClassService >> packageName [
	
	^packageName

]

{ #category : 'Updating' }
RowanClassService >> packageName: pkgName [
	
	packageName := pkgName

]

{ #category : 'Accessing' }
RowanClassService >> poolDictionaryNames [
	^poolDictionaryNames

]

{ #category : 'Updating' }
RowanClassService >> poolDictionaryNames: newValue [
	poolDictionaryNames := newValue

]

{ #category : 'printing' }
RowanClassService >> printOn: aStream [

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
]

{ #category : 'testing' }
RowanClassService >> projectIsDirty [

	| behavior |
	behavior := self theClass.
	behavior rowanProjectName =  Rowan unpackagedName ifTrue:[^true]. "avoid a refresh by assuming it's dirty" 
	^(RowanProjectService new name: behavior rowanProjectName) rowanDirty
]

{ #category : 'other' }
RowanClassService >> projectName [

	^projectName
]

{ #category : 'Updating' }
RowanClassService >> projectName: newValue [
	projectName := newValue
]

{ #category : 'initialization' }
RowanClassService >> refreshFrom: theClass [
	| classOrMeta  |
	self basicRefreshFrom: theClass. 
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self refreshMethodsFor: classOrMeta

]

{ #category : 'initialization' }
RowanClassService >> refreshMethodsFor: classOrMeta [
	| gsNMethods |
	methods := SortedCollection sortBlock: [:x :y | x selector < y selector].
	self methodServicesFor: classOrMeta organizer: organizer.
	methods := methods asOrderedCollection.
	classOrMeta allInstVarNames do:[:instVar | 
			gsNMethods := organizer accessorsOf: instVar inClass: classOrMeta.
			gsNMethods do:[:gsNMethod |
				| service |
				service := methods detect:[:methodService | methodService selector = gsNMethod selector] ifNone:[].
				service ifNotNil:[
					service accessedInstVars add: instVar asString]
	]].
	self initializeTestMethodsFor: classOrMeta thisClass. 
	self setVisibleTests. "methods must be available"
]

{ #category : 'client commands' }
RowanClassService >> removeCategories: theCategories [
	| theClass  | 
	self refreshFrom: self theClass. 
	theClass := self theClass.
	meta ifTrue:[theClass := theClass class]. 
	theCategories do: [:category |
		theClass rwRemoveCategory: category.
		].
]

{ #category : 'client commands' }
RowanClassService >> removeMethods: methodsToRemove [

	| notRemoved |
	notRemoved := Array new. 
	methodsToRemove do: [:methodService |
		self removeSelector: methodService selector ifAbsent:[notRemoved add: methodService].
		(notRemoved includes: methodService) ifFalse:[
			methodService updateType: #removed:.
			RowanCommandResult addResult: methodService.
		]].
	self updateTests.
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', (notRemoved collect:[:svc | svc selector]) printString].
]

{ #category : 'rowan' }
RowanClassService >> removeSelector: selector [

	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta

]

{ #category : 'rowan' }
RowanClassService >> removeSelector: selector ifAbsent: absentBlock [
	| theClass |
	theClass := self theClass. 
	meta ifTrue: [theClass := theClass class].
	(theClass compiledMethodAt: selector otherwise: nil) isNil ifTrue:[ ^absentBlock value ].
	[self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ]
]

{ #category : 'client commands' }
RowanClassService >> renameCategoryFrom: old to: new [

	| affectedSelectors behavior |

	self update. 
	self addCategory: new. 
	behavior := self classOrMeta.
	affectedSelectors := behavior selectorsIn: old.
	methods := methods select:[:methodService | affectedSelectors includes: methodService selector].
	self moveMethods: methods to: new.
	self removeCategories: (Array with: old)
]

{ #category : 'client commands' }
RowanClassService >> renameClass: oldClassName to: newClassName [
  | references newMethods newClass oldClass |
  newMethods := Array new.
  oldClass := Rowan image resolveClassNamed: oldClassName.
  newClass := Rowan projectTools browser
    renameClassNamed: oldClassName
    to: newClassName.
  oop := newClass asOop.
  name := newClassName.
  self update.
  renamedName := oldClassName.
  self updateMethodsAfterRenameFrom: oldClassName to: newClassName.
  self updateSubclassesAfterRenameOf: newClass.
  references := organizer update referencesToObject: oldClass.
  references do: [ :method | 
    | newSource compileResult failedCompile methodService oldSource |
    failedCompile := false.
    oldSource := method sourceString.
    newSource := self
      replaceSubString: oldClassName
      in: oldSource
      with: newClassName.
    compileResult := [ 
    method inClass
      rwCompileMethod: newSource
      category: (method inClass categoryOfSelector: method selector) asSymbol ]
      on: CompileError
      do: [ :ex | 
        failedCompile := true.
        method ].
    methodService := RowanMethodService
      forGsNMethod: compileResult
      organizer: organizer.
    failedCompile
      ifTrue: [ methodService comparisonSource: oldClassName ]
      ifFalse: [ methodService comparisonSource: oldSource ].
    methodService failedCompile: failedCompile.
    methodService renamedName: oldClassName.
    newMethods add: methodService ].
  RowanCommandResult addResult: (RowanAnsweringService new answer: newMethods)
]

{ #category : 'private' }
RowanClassService >> replaceSubString: old in: string with: new [
	| offset newSource |
	newSource := string. 
	offset := 1. 	
	[(offset := newSource findString: old startingAt: offset) = 0] whileFalse:[
		newSource := newSource copyReplaceFrom: offset to: offset + old size - 1 with: new. 
		offset := offset + new size. 
	].
	^newSource
]

{ #category : 'rowan' }
RowanClassService >> rowanProjectName [

	^projectName
]

{ #category : 'client commands' }
RowanClassService >> runClassTests: classService [

	"if it errors, the client will handle the error. 
	If it passes, we return true and the client
	will display decent results." 
	| behavior |
	behavior := classService theClass. 
	self refreshFrom: behavior.
	self tests do:[:methodService |
			behavior debug: methodService selector]. 
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
]

{ #category : 'client commands' }
RowanClassService >> runMethodTests: methodServices [

	| behavior |
	behavior := self theClass.  
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior debug: methodService selector]].
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
]

{ #category : 'client commands' }
RowanClassService >> saveMethodSource: source category: category [
  | behavior compilationResult gsNMethod updatedCategory methodService |
  meta
    ifNil: [ 
      behavior := Object _objectForOop: oop.
      meta := behavior isMeta ]
    ifNotNil: [ 
      behavior := meta
        ifTrue: [ self theClass class ]
        ifFalse: [ self theClass ] ].
  oop := behavior asOop.
  self initializeMethodHistoryFor: source.
  updatedCategory := category ifNil: [ 'other' ].
  compilationResult := self
    compileMethod: source
    behavior: behavior
    symbolList: Rowan image symbolList
    inCategory: updatedCategory asSymbol.
  (gsNMethod := compilationResult key) isNil
    ifTrue: [ 
      System
        signal: 1001
        args: (Array with: compilationResult value)
        signalDictionary: GemStoneError ].
  self update.
  methodService := self
    methodServiceFrom: gsNMethod
    in: behavior
    compiltationResult: compilationResult.
  RowanCommandResult addResult: methodService.
  RowanQueryService new
    organizer: ClassOrganizer new;
    hierarchyImplementorsOf: methodService selector
      inClass: methodService className.	"this will update hierarchy method indicators for client"
  self selectedMethods: (Array with: methodService).
  self updateDirtyState.
  self updateTests.
  self
    updateSymbols:
      gsNMethod _selectorPool asArray , (Array with: methodService selector).
  methodService addToMethodHistory
]

{ #category : 'other' }
RowanClassService >> selectedMethods [
	"client side selection. Used after a method compile" 
	^selectedMethods
]

{ #category : 'Updating' }
RowanClassService >> selectedMethods: theMethods [
	selectedMethods := theMethods
]

{ #category : 'Accessing' }
RowanClassService >> selectedPackageServices [
	^selectedPackageServices
]

{ #category : 'Updating' }
RowanClassService >> selectedPackageServices: newValue [
	selectedPackageServices := newValue
]

{ #category : 'Accessing' }
RowanClassService >> selectors [

	^methods collect:[:methodService | methodService selector]
]

{ #category : 'perform' }
RowanClassService >> servicePerform: symbol withArguments: collection [
  | wasClean |
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  wasClean := self isPackageClean.
  super servicePerform: symbol withArguments: collection.
  updateAfterCommand == false
    ifFalse: [ self update ]. 
  wasClean
    ifTrue: [ self updatePackageProject ]
]

{ #category : 'Accessing' }
RowanClassService >> setComment [
  comment := self theClass thisClass comment
]

{ #category : 'private' }
RowanClassService >> setDictionary: classOrMeta [
	| dictionaryList |
		dictionaryList := Rowan image symbolList dictionariesAndSymbolsOf: classOrMeta thisClass.
		dictionaryName := dictionaryList isEmpty 
		ifTrue:[String new]
		ifFalse:[dictionaryList first first name asString].
]

{ #category : 'Updating' }
RowanClassService >> setIsTestCase [

	isTestCase := self theClass isSubclassOf: TestCase

]

{ #category : 'client commands' }
RowanClassService >> setIsTestCaseCommand [

	self setIsTestCase.
]

{ #category : 'client commands' }
RowanClassService >> setVisibleTests [
	visibleTests := SortedCollection sortBlock: [:x :y | x selector < y selector]. 
	visibleTests addAll: self allTests.
	visibleTests := visibleTests asArray.
]

{ #category : 'client commands' }
RowanClassService >> subclassCreationTemplate [
  | answerService newClassPackageName |
  answerService := RowanAnsweringService new.
  newClassPackageName := self theClass rowanPackageName = Rowan unpackagedName
    ifTrue: [ self addSubclassWarningString ]
    ifFalse: [ self theClass rowanPackageName ].
  answerService
    answer:
      (self browserTool
        classCreationTemplateForSubclassOf: name
        className: 'NewSubclass'
        category: newClassPackageName).
  RowanCommandResult addResult: answerService
]

{ #category : 'private' }
RowanClassService >> subclassServices: subclasses [

	| sortedSubclasses |

	sortedSubclasses := SortedCollection sortBlock: [:x :y | x name < y name]. 
	sortedSubclasses addAll: subclasses. 
	^(sortedSubclasses collect:[:cls | (self classServiceFromOop: cls asOop) meta: meta]) asArray.
]

{ #category : 'Accessing' }
RowanClassService >> subclassType [
	^subclassType

]

{ #category : 'Updating' }
RowanClassService >> subclassType: newValue [
	subclassType := newValue

]

{ #category : 'Accessing' }
RowanClassService >> superclassName [
	^superclassName

]

{ #category : 'Updating' }
RowanClassService >> superclassName: newValue [
	superclassName := newValue

]

{ #category : 'Accessing' }
RowanClassService >> template [
	^template

]

{ #category : 'Updating' }
RowanClassService >> template: newValue [
	template := newValue

]

{ #category : 'private' }
RowanClassService >> tests [

	^methods select:[:methodService | methodService selector asString matchPattern: #('test' $*)]
]

{ #category : 'instance creation' }
RowanClassService >> theClass [
	| theClass |
	theClass := oop ifNil:[Rowan globalNamed: name] ifNotNil: [Object _objectForOop: oop].
	theClass isMeta ifTrue:[oop := theClass thisClass asOop]. 
	(Rowan globalNamed: name) ifNil:[isInSymbolList := false]. 
	theClass ifNil: [^nil]. 
	^theClass thisClass
]

{ #category : 'updates' }
RowanClassService >> update [

	self updateClass.
]

{ #category : 'updates' }
RowanClassService >> updateClass [

	"It's possible to have a nil class. For example, if we added & selected
	a class then aborted."

	| theClass |
	theClass := self theClass. 
	theClass isNil ifTrue:[oop := nil. ^self]. 
	theClass isBehavior ifFalse:[oop := theClass asOop. ^self].
	self refreshFrom: theClass.
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanClassService >> updateDirtyState [
	| projectService | 
	selectedPackageServices do:[:packageService | 
		packageService update. 
		RowanCommandResult addResult: packageService].
	projectService := RowanProjectService newNamed: self theClass rowanProjectName. 
	RowanCommandResult addResult: projectService.
]

{ #category : 'initialization' }
RowanClassService >> updateIsExtension [
  isExtension := ((selectedPackageServices
    collect: [ :packageService | packageService name ])
    includes: definedPackageName) not
]

{ #category : 'updates' }
RowanClassService >> updateLatest [
  oop := ((Rowan image symbolList resolveSymbol: name)
    ifNil: [ 
      wasRemoved := true.
      updateType := #'removedClass:'.
      RowanCommandResult addResult: self.
      ^ self ]) value asOop.
  super updateLatest
]

{ #category : 'private' }
RowanClassService >> updateMethodsAfterRenameFrom: oldClassName to: newClassName [
  methods
    do: [ :methodService | 
      methodService
        renamedName: oldClassName;
        className: newClassName. 
      RowanCommandResult addResult: methodService ]
]

{ #category : 'updates' }
RowanClassService >> updatePackageProject [
	| packageService projectService |

	packageService := RowanPackageService new name: packageName. 
	packageService update. 
	projectService := RowanProjectService new name: projectName. 
	projectService update.
]

{ #category : 'private' }
RowanClassService >> updateSubclassesAfterRenameOf: newClass [
  organizer := ClassOrganizer new.
  (organizer allSubclassesOf: newClass)
    do: [ :subclass | 
      | subclassService |
      subclassService := RowanClassService minimalForClassNamed: subclass name.
      RowanCommandResult addResult: subclassService ]
]

{ #category : 'updates' }
RowanClassService >> updateSymbols: newSymbols [
  | browserService |
  browserService := RowanBrowserService new.
  browserService newCachedSelectors addAll: newSymbols.
  browserService updateType: #'addCachedSymbols:'. 
  RowanCommandResult addResult: browserService
]

{ #category : 'updates' }
RowanClassService >> updateTests [
  "update the test browsers on certain operations"

  RowanBrowserService new packagesWithTests.
  (RowanPackageService new
    name: packageName;
    yourself) testClasses.
  RowanCommandResult addResult: self update
]

{ #category : 'Accessing' }
RowanClassService >> version [
	^version

]

{ #category : 'Updating' }
RowanClassService >> version: newValue [
	version := newValue

]

{ #category : 'Accessing' }
RowanClassService >> versions [
	^versions

]

{ #category : 'Updating' }
RowanClassService >> versions: newValue [
	versions := newValue

]

{ #category : 'Accessing' }
RowanClassService >> visibleTests [

	^visibleTests
]

{ #category : 'testing' }
RowanClassService >> wasDeleted [
	^(Rowan globalNamed: name) isNil
]

{ #category : 'Accessing' }
RowanClassService >> wasRemoved: boolean [

	wasRemoved := boolean
]
