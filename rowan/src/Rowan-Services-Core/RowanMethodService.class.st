"
No class-specific documentation for RowanMethodService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanMethodService( oop source selector methodDefinitions classService category packageName projectName className meta hasSupers hasSubs compilationWarnings isExtension inSelectedPackage references stepPoints selectedPackageServices superDisplayString accessedInstVars breakPoints testResult definedPackage isTestMethod testRunClassName failedCompile comparisonSource firstReference renamedName isMethodForBlock homeMethodOop hasMethodHistory searchString definedClassName)

"
Class {
	#name : 'RowanMethodService',
	#superclass : 'RowanService',
	#instVars : [
		'oop',
		'source',
		'selector',
		'methodDefinitions',
		'category',
		'packageName',
		'projectName',
		'className',
		'meta',
		'hasSupers',
		'hasSubs',
		'compilationWarnings',
		'isExtension',
		'inSelectedPackage',
		'references',
		'stepPoints',
		'selectedPackageServices',
		'superDisplayString',
		'accessedInstVars',
		'breakPoints',
		'testResult',
		'definedPackage',
		'isTestMethod',
		'testRunClassName',
		'failedCompile',
		'comparisonSource',
		'firstReference',
		'renamedName',
		'isMethodForBlock',
		'homeMethodOop',
		'hasMethodHistory',
		'searchString',
		'definedClassName'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanMethodService class >> forGsNMethod: aGsNMethod organizer: anOrganizer [
	^self new
		initialize: aGsNMethod organizer: anOrganizer;
		yourself
]

{ #category : 'instance creation' }
RowanMethodService class >> forSelector: sel class: theClass meta: boolean organizer: anOrganizer [

	| service |
	service := self new. 
	service selector: sel;
		meta: boolean.
	service forClass: theClass organizer: anOrganizer.
	^service
]

{ #category : 'utility' }
RowanMethodService class >> put: string onStreamWithoutUnicode: stream [

	string do:[:char | 
			char asInteger > 255 ifTrue:[
				stream nextPutAll: '$\x', char asInteger asHexString]
			ifFalse:[
				stream nextPut: char
			]].
]

{ #category : 'utility' }
RowanMethodService class >> removeUnicode: string [

		| ws | 
		ws := WriteStream on: String new. 
		self put: string onStreamWithoutUnicode: ws. 
		^ws contents
]

{ #category : 'utility' }
RowanMethodService class >> removeUnicodeFromSource: string [

	"possibly a unicode string which is not yet 
	presentable in Jadeite."

	(string isKindOf: MultiByteString) ifTrue:[
		| ws | 
		ws := WriteStream on: String new. 
		ws nextPutAll: '''METHOD SOURCE CANNOT BE DISPLAYED
This method source contains unicode and is not displayable in Jadeite. 
An approximation of the source code is given printed below with $\x<char hex value>
replacing the actual unicode character.''';
			cr; 
			cr.
		self put: string onStreamWithoutUnicode: ws. 
		^ws contents]
	ifFalse:[
		^string].
]

{ #category : 'instance creation' }
RowanMethodService class >> source: source selector: selector category: category className: className packageName: packageName meta: boolString [

	| service |
	self rowanFixMe. "Need to include super and sub implementors"
	service := self new. 
	service 
		source: source;
		selector: selector;
		category: category asString;
		className: className;
		packageName: packageName;
		meta: boolString == true.
	^service
]

{ #category : 'comparing' }
RowanMethodService >> = methodService [
	(methodService class canUnderstand: #isMethodService) ifFalse:[^false].
	methodService isMethodService ifFalse:[^false].
	^selector = methodService selector
		and: [className asString = methodService className asString and: [meta = methodService meta]]
]

{ #category : 'Accessing' }
RowanMethodService >> accessedInstVars [
  ^ accessedInstVars
]

{ #category : 'Updating' }
RowanMethodService >> accessedInstVars: anArray [
	accessedInstVars := anArray
]

{ #category : 'method history' }
RowanMethodService >> addToMethodHistory [
  | rowanMethodHistory methodHistory |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  methodHistory := rowanMethodHistory at: self ifAbsentPut: [ Array new ].
  methodHistory add: self
]

{ #category : 'client commands' }
RowanMethodService >> allReferences [
  | methods |
  oop := self gsNMethod asOop.
  methods := organizer sendersOf: selector.
  references := methods first
    collect: [ :gsNMethod | self class forGsNMethod: gsNMethod organizer: organizer ].
  RowanCommandResult addResult: self
]

{ #category : 'Accessing' }
RowanMethodService >> breakPoints [

	^breakPoints
]

{ #category : 'Accessing' }
RowanMethodService >> breakPoints: collection [
  breakPoints := collection
]

{ #category : 'initialization' }
RowanMethodService >> breakPointsFor: aGsNMethod [
  "Answers an Array stepPoints"

  | list theMethod |
  list := OrderedCollection new.
  theMethod := aGsNMethod isMethodForBlock
    ifTrue: [ 
      isMethodForBlock := true.
      aGsNMethod homeMethod ]
    ifFalse: [ aGsNMethod ].
  homeMethodOop := theMethod asOop.
  theMethod _allBreakpoints
    ifNil: [ ^ OrderedCollection new ]
    ifNotNil: [ :anArray | 
      1 to: anArray size by: 3 do: [ :i | 
        list
          add:
            (theMethod _stepPointForMeth: (anArray at: i + 1) ip: (anArray at: i + 2)) ] ].
  ^ list asOrderedCollection
]

{ #category : 'Accessing' }
RowanMethodService >> category [
	^category

]

{ #category : 'Updating' }
RowanMethodService >> category: newValue [
	category := newValue asString
]

{ #category : 'Accessing' }
RowanMethodService >> classFromName [
  "the dictionary browser may have versions numbers in the name"

  | nameSymbol |
  nameSymbol := (className copyUpTo: Character space) asSymbol.
  ^ (System myUserProfile resolveSymbol: nameSymbol) value
]

{ #category : 'Accessing' }
RowanMethodService >> className [
	^className

]

{ #category : 'Updating' }
RowanMethodService >> className: newValue [
	className := newValue asString
]

{ #category : 'Accessing' }
RowanMethodService >> classOrMeta [

	^meta 
			ifTrue:[self classFromName class] 
			ifFalse: [self classFromName].
]

{ #category : 'client commands' }
RowanMethodService >> clearBreakAt: stepPoint [
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method clearBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanMethodService >> clearMethodBreaks [
  self update.
  breakPoints do: [ :breakPoint | self clearBreakAt: breakPoint ]
]

{ #category : 'Updating' }
RowanMethodService >> comparisonSource: string [

	comparisonSource := string
]

{ #category : 'Accessing' }
RowanMethodService >> compilationWarnings [

	^compilationWarnings
]

{ #category : 'Updating' }
RowanMethodService >> compilationWarnings: newValue [
	compilationWarnings := newValue
]

{ #category : 'client commands' }
RowanMethodService >> debugTest: testSelector inClassName: theClassName [
  testResult := 'passed'.
  [ (Rowan image objectNamed: theClassName) debug: testSelector asSymbol ]
    on: Exception
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      testResult := ex class = TestFailure
        ifTrue: [ 'failure' ]
        ifFalse: [ 
          (ex class isSubclassOf: Notification)
            ifTrue: [ 'passed' ]
            ifFalse: [ 'error' ] ].
      ex pass ].
  testRunClassName := theClassName.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMethodService >> debugTestAsFailure: testSelector inClassName: theClassName [
  testResult := 'passed'.
  [ 
  ((Rowan image objectNamed: theClassName) selector: testSelector asSymbol)
    debugAsFailure ]
    on: Exception
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      testResult := ex class = TestFailure
        ifTrue: [ 'failure' ]
        ifFalse: [ 
          (ex class isSubclassOf: Notification)
            ifTrue: [ 'passed' ]
            ifFalse: [ 'error' ] ].
      ex pass ].
  testRunClassName := theClassName.
  RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanMethodService >> definedClassName [
	^definedClassName
]

{ #category : 'accessing' }
RowanMethodService >> definedClassName: object [
	definedClassName := object
]

{ #category : 'Accessing' }
RowanMethodService >> definedPackage [

	^definedPackage
]

{ #category : 'rowan' }
RowanMethodService >> definitionClass [

	^RwMethodDefinition

]

{ #category : 'client commands' }
RowanMethodService >> disableBreakAt: stepPoint [
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method disableBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanMethodService >> disableMethodBreaks [
  self update.
  breakPoints do: [ :breakPoint | self disableBreakAt: breakPoint ]
]

{ #category : 'client commands' }
RowanMethodService >> enableBreakAt: stepPoint [
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method setBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanMethodService >> enableMethodBreaks [
  self update.
  breakPoints do: [ :breakPoint | self enableBreakAt: breakPoint ]
]

{ #category : 'Updating' }
RowanMethodService >> failedCompile: boolean [	
	
	failedCompile := boolean
]

{ #category : 'client commands' }
RowanMethodService >> fileout [
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	ws nextPutAll: (self behavior fileOutMethod: selector asString environmentId: 0).
	answeringService answer: ws contents.
	RowanCommandResult addResult: answeringService.
]

{ #category : 'Accessing' }
RowanMethodService >> firstReference: integer [

	firstReference := integer
]

{ #category : 'initialization' }
RowanMethodService >> forClass: theClass organizer: theOrganizer [
	"assume meta and selector are set"

	| classOrMeta gsNMethod |
	organizer := theOrganizer.
	classOrMeta := meta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	gsNMethod := classOrMeta compiledMethodAt: selector.
	definedPackage := gsNMethod rowanPackageName.
	oop := gsNMethod asOop.
	stepPoints := self stepPointsFor: gsNMethod.
	breakPoints := self breakPointsFor: gsNMethod.
	self updateSource: gsNMethod sourceString.
	category := (classOrMeta categoryOfSelector: selector) asString.
	className := theClass name asString.
	packageName := gsNMethod rowanPackageName.
	projectName := gsNMethod rowanProjectName.
	self setSupersAndSubsFor: classOrMeta.
	isExtension := self rowanIsExtension.
	self initializeTestMethodsFor: classOrMeta thisClass.
	self setDefinedClass: classOrMeta
]

{ #category : 'Accessing' }
RowanMethodService >> gsNMethod [

	^[self classOrMeta compiledMethodAt: selector ] on: Error do:[:ex | nil "removed method"]
]

{ #category : 'comparing' }
RowanMethodService >> hash [
  ^ (selector hash bitXor: className hash) bitXor: meta hash
]

{ #category : 'Accessing' }
RowanMethodService >> hasSubs [

	^hasSubs

]

{ #category : 'Updating' }
RowanMethodService >> hasSubs: aBoolean [

	hasSubs := aBoolean

]

{ #category : 'Accessing' }
RowanMethodService >> hasSupers [

	^hasSupers

]

{ #category : 'Updating' }
RowanMethodService >> hasSupers: aBoolean [

	hasSupers := aBoolean

]

{ #category : 'initialization' }
RowanMethodService >> initialize [

	super initialize. 
	hasSupers := false. 
	hasSubs := false.
	accessedInstVars := Array new.
	isTestMethod := false.
	failedCompile := false.
	isMethodForBlock := false.
	hasMethodHistory := true.
]

{ #category : 'initialization' }
RowanMethodService >> initialize: aGsNMethod organizer: aClassOrganizer [

	| inClass |
	oop := aGsNMethod asOop.
	definedPackage := aGsNMethod rowanPackageName. 
	selector := aGsNMethod selector.
	stepPoints := self stepPointsFor: aGsNMethod.
	breakPoints := self breakPointsFor: aGsNMethod.
	((inClass := aGsNMethod inClass) isNil or: [selector isNil]) ifTrue: [
		meta := false.
		self updateSource: aGsNMethod sourceString.
		hasSupers := false.
		hasSubs := false.
		organizer := aClassOrganizer. 
		inSelectedPackage := false.
		^self
	].
	meta := inClass isMeta.
	self 
		forClass: inClass thisClass 
		organizer: aClassOrganizer.
	self initializeTestMethodsFor: inClass thisClass.
	self setHasMethodHistory
]

{ #category : 'initialization' }
RowanMethodService >> initializeTestMethodsFor: aClass [
  | testSelectors |
  (aClass inheritsFrom: TestCase)
    ifTrue: [ 
      aClass isAbstract
        ifTrue: [ ^ self ].
      testSelectors := aClass thisClass suite tests
        collect: [ :method | method selector ].
      isTestMethod := testSelectors includes: selector ]
]

{ #category : 'testing' }
RowanMethodService >> isMethodService [

	^true
]

{ #category : 'testing' }
RowanMethodService >> isTestMethod [

	^isTestMethod
]

{ #category : 'Updating' }
RowanMethodService >> isTestMethod: boolean [

	isTestMethod := boolean
]

{ #category : 'testing' }
RowanMethodService >> isUnboundMethod [

	(className notNil and: [selector notNil]) ifTrue:[^false].
	^(Object _objectForOop: oop) isKindOf: GsNMethod
]

{ #category : 'Accessing' }
RowanMethodService >> meta [
	^meta

]

{ #category : 'Updating' }
RowanMethodService >> meta: aBoolean [
	"allow nil parameter for now" 
	meta := aBoolean == true

]

{ #category : 'Accessing' }
RowanMethodService >> method [

	^self classFromName compiledMethodAt: selector otherwise: nil
]

{ #category : 'Accessing' }
RowanMethodService >> methodDefinitions [
	^methodDefinitions

]

{ #category : 'Updating' }
RowanMethodService >> methodDefinitions: newValue [
	methodDefinitions := newValue

]

{ #category : 'Accessing' }
RowanMethodService >> name [

	^selector
]

{ #category : 'accessing' }
RowanMethodService >> oop [
	^oop
]

{ #category : 'accessing' }
RowanMethodService >> oop: object [
	oop := object
]

{ #category : 'Accessing' }
RowanMethodService >> packageName [
	^packageName

]

{ #category : 'Updating' }
RowanMethodService >> packageName: newValue [
	packageName := newValue

]

{ #category : 'printing' }
RowanMethodService >> printOn: aStream [

	super printOn: aStream. 
	aStream nextPut: $(;
				nextPutAll: (className ifNil:[nil printString]); 
				nextPutAll: '>>'; 
				nextPutAll: (selector ifNil:[nil printString]);
				nextPut: $)
]

{ #category : 'client commands' }
RowanMethodService >> reformatSource [
  source := (RBParser parseMethod: source) formattedCode.
  updateType := #'dontUpdateSystem'.	"let browser update the source"
  RowanCommandResult addResult: self
]

{ #category : 'rowan' }
RowanMethodService >> removeSelector: sel class: clsName [
	
	self rowanFixMe. "combine remove methods variants"
	self browserTool removeMethod: sel asSymbol forClassNamed: (clsName subStrings first) isMeta: (clsName subStrings size = 2)

]

{ #category : 'Accessing' }
RowanMethodService >> renamedName [
	^renamedName
]

{ #category : 'Accessing' }
RowanMethodService >> renamedName: object [
	renamedName := object
]

{ #category : 'rowan' }
RowanMethodService >> rowanIsExtension [

	^Rowan projectTools browser isExtensionMethod: selector asString forClassNamed: className asString isMeta: meta

]

{ #category : 'rowan' }
RowanMethodService >> rowanProjectName [

	^projectName
]

{ #category : 'client commands' }
RowanMethodService >> runTest: testSelector inClassName: theClassName [

	| sunitTestResult |
	sunitTestResult := (Rowan image objectNamed: theClassName) run: testSelector asSymbol.
	sunitTestResult errorCount > 0 ifTrue:[testResult := 'error']. 
	sunitTestResult failureCount > 0 ifTrue:[testResult := 'failure']. 
	sunitTestResult passedCount > 0 ifTrue:[testResult := 'passed']. 
	testRunClassName := theClassName. 
	RowanCommandResult addResult: self.
]

{ #category : 'accessing' }
RowanMethodService >> searchString [
	^searchString
]

{ #category : 'accessing' }
RowanMethodService >> searchString: object [
	searchString := object
]

{ #category : 'Accessing' }
RowanMethodService >> selectedPackageServices [

	^selectedPackageServices

]

{ #category : 'Updating' }
RowanMethodService >> selectedPackageServices: collection [

	selectedPackageServices := collection

]

{ #category : 'Accessing' }
RowanMethodService >> selector [

	^selector

]

{ #category : 'Updating' }
RowanMethodService >> selector: aSymbol [

	selector := aSymbol

]

{ #category : 'perform' }
RowanMethodService >> servicePerform: symbol withArguments: collection [
	| theCommand |
	self isUpdatingButFoundToBeDeleted
		ifTrue: [ ^ self handleDeletedService ].
	theCommand := command.
	super
		servicePerform: symbol
		withArguments: collection
		shouldUpdate: (theCommand == #'reformatSource') not
]

{ #category : 'client commands' }
RowanMethodService >> setBreakAt: stepPoint [
  | method |
  method := self isUnboundMethod
    ifTrue: [ (Object _objectForOop: oop) homeMethod ]
    ifFalse: [ self gsNMethod ].
  method setBreakAtStepPoint: stepPoint.
  self class breakPointsAreEnabled
    ifFalse: [ self disableBreakAt: stepPoint ].
  self update.
  RowanCommandResult addResult: self
]

{ #category : 'initialization' }
RowanMethodService >> setDefinedClass: aClass [
	"only used in the test browsers (for now) to identify which class the method is defined in.
	That may be different than what is displayed, say, when we run a superclass test in concrete subclass.
	Assume selector is defined in aClass hierarchy."

	definedClassName := (aClass whichClassIncludesSelector: selector asString) name
		asString
]

{ #category : 'method history' }
RowanMethodService >> setHasMethodHistory [
  | answeringService |
  answeringService := RowanAnsweringService new.
  hasMethodHistory := (answeringService basicMethodHistoryFor: self) answer size
    > 1.
]

{ #category : 'initialization' }
RowanMethodService >> setSupersAndSubsFor: theClass [

	| theSuper |
	theSuper := theClass superClass. 
	hasSupers := false. 
	[theSuper notNil and:[hasSupers not]] whileTrue:[
		hasSupers := theSuper selectors includes: selector.
		hasSupers ifTrue:[
			comparisonSource := theSuper sourceCodeAt: selector.
			superDisplayString := theSuper name, '>>', selector].
		theSuper := theSuper superClass].
	(organizer allSubclassesOf: theClass thisClass) do:[:cls |
		| aClass |
		aClass := theClass isMeta ifTrue:[cls class] ifFalse:[cls]. 
		(hasSubs := aClass includesSelector: selector) ifTrue:[
		^self]].
]

{ #category : 'Accessing' }
RowanMethodService >> source [

	^source

]

{ #category : 'Updating' }
RowanMethodService >> source: string [
	
	self updateSource: string
]

{ #category : 'Accessing' }
RowanMethodService >> stepPoints [

	"for testing"
	
	^stepPoints
]

{ #category : 'Updating' }
RowanMethodService >> stepPoints: collection [

	stepPoints := collection
]

{ #category : 'initialization' }
RowanMethodService >> stepPointsFor: aGsNMethod [
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aGsNMethod _sourceOffsetsOfSends) ifNil: [^#()].
	list := aGsNMethod homeMethod  _sourceOffsets.
	list := list collect: [:each |
		| index eachSelector |
		eachSelector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [eachSelector := selectors at: index + 1].
		each -> eachSelector.
	].
	^list
]

{ #category : 'Accessing' }
RowanMethodService >> testResult [

	^testResult
]

{ #category : 'updates' }
RowanMethodService >> update [
	super update.
	self isUnboundMethod ifFalse:[
		self wasRecycled ifTrue:[oop := self gsNMethod asOop].
		self wasDeleted ifTrue:[
			self updateType: #methodsRemoved:. 
			^RowanCommandResult addResult: self. ].  "removed method"
		oop ifNil: [oop := self gsNMethod asOop]].
	self 
		initialize: (Object _objectForOop: oop) 
		organizer: organizer.
	RowanCommandResult addResult: self.
]

{ #category : 'updates' }
RowanMethodService >> updateLatest [
  | theClass compiledMethod |
  theClass := (RowanClassService new name: className) theClass.
  theClass ifNil: [ ^ self ].
  compiledMethod := theClass compiledMethodAt: selector otherwise: nil.
  compiledMethod ifNil: [ ^ self ].
  oop := compiledMethod asOop.
  super updateLatest
]

{ #category : 'private' }
RowanMethodService >> updateSource: string [

	source := self class removeUnicodeFromSource: string
]

{ #category : 'testing' }
RowanMethodService >> wasDeleted [
	selector isNil ifTrue:[^false].
	^self gsNMethod isNil
]

{ #category : 'testing' }
RowanMethodService >> wasRecycled [
	(oop notNil and:[self gsNMethod asOop ~= oop]) ifTrue:[^true].
	^false
]
