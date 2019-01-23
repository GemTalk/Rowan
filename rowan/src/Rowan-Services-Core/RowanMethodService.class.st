Class {
	#name : 'RowanMethodService',
	#superclass : 'RowanService',
	#instVars : [
		'oop',
		'source',
		'selector',
		'methodDefinitions',
		'classService',
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
		'superSource',
		'superDisplayString',
		'accessedInstVars',
		'breakPoints',
		'testResult',
		'definedPackage',
		'isTestMethod'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanMethodService class >> forGsNMethod: aGsNMethod organizer: aClassOrganizer [

	^self new
		initialize: aGsNMethod organizer: aClassOrganizer;
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
	^accessedInstVars
]

{ #category : 'Updating' }
RowanMethodService >> accessedInstVars: anArray [
	accessedInstVars := anArray
]

{ #category : 'rowan' }
RowanMethodService >> addOrUpdateMethod [

		self browserTool  
                   addOrUpdateMethod: source
                   inProtocol: category
                   forClassNamed: self classService name
                   isMeta: meta
                   inPackageNamed: self classService packageName
]

{ #category : 'client commands' }
RowanMethodService >> allReferences [

	| methods |
	oop := self gsNMethod asOop. 
	methods := organizer sendersOf: selector.
	references := methods first collect:[:gsNMethod | 
			self class forGsNMethod: gsNMethod organizer: organizer].
	RowanCommandResult addResult: self
]

{ #category : 'initialization' }
RowanMethodService >> breakPointsFor: aGsNMethod [
	"Answers an Array stepPoints"

	| list |
	list := Array new.
	aGsNMethod _allBreakpoints ifNil: [^#()] ifNotNil: [:anArray | 
		1 to: anArray size by: 3 do: [:i | 
			list add: (aGsNMethod _stepPointForMeth: (anArray at: i + 1) ip: (anArray at: i + 2)).
		].
	].
	^list

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
	^(System myUserProfile resolveSymbol: nameSymbol) value
]

{ #category : 'Accessing' }
RowanMethodService >> className [
	^className

]

{ #category : 'Updating' }
RowanMethodService >> className: newValue [
	className := newValue

]

{ #category : 'Accessing' }
RowanMethodService >> classOrMeta [

	^meta 
			ifTrue:[self classFromName class] 
			ifFalse: [self classFromName].
]

{ #category : 'Accessing' }
RowanMethodService >> classService [

	^classService ifNil:[classService := RowanClassService forClassNamed: className package: packageName]

]

{ #category : 'Updating' }
RowanMethodService >> classService: newValue [
	classService := newValue

]

{ #category : 'client commands' }
RowanMethodService >> clearBreakAt: stepPoint [
	| method |
	method := self isUnboundMethod 
			ifTrue:[Object _objectForOop: oop] 
			ifFalse:[self gsNMethod].
	method clearBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
]

{ #category : 'Accessing' }
RowanMethodService >> compilationWarnings [

	^compilationWarnings
]

{ #category : 'Updating' }
RowanMethodService >> compilationWarnings: newValue [
	compilationWarnings := newValue
]

{ #category : 'rowan' }
RowanMethodService >> definitionClass [

	^RwMethodDefinition

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

{ #category : 'initialization' }
RowanMethodService >> forClass: theClass organizer: theOrganizer [
	"assume meta and selector are set"

	| classOrMeta gsNMethod |
	organizer := theOrganizer. 
	classOrMeta := meta ifTrue:[theClass class] ifFalse:[theClass]. 
	gsNMethod := classOrMeta compiledMethodAt: selector.
	definedPackage := gsNMethod rowanPackageName. 
	oop := gsNMethod asOop. 
	stepPoints := self stepPointsFor: gsNMethod.
	breakPoints := self breakPointsFor: gsNMethod.
	source := gsNMethod sourceString.
	category := (classOrMeta categoryOfSelector: selector) asString.
	className := theClass name. 
	packageName := gsNMethod rowanPackageName. 
	projectName := gsNMethod rowanProjectName.
	self setSupersAndSubsFor: classOrMeta.
	isExtension := self rowanIsExtension.
]

{ #category : 'Accessing' }
RowanMethodService >> gsNMethod [

	^[self classOrMeta compiledMethodAt: selector ] on: Error do:[:ex | nil "removed method"]
]

{ #category : 'comparing' }
RowanMethodService >> hash [
	^(selector hash bitXor: className hash) bitXor: meta hash
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
		source := aGsNMethod sourceString.
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
]

{ #category : 'testing' }
RowanMethodService >> isMethodService [

	^true
]

{ #category : 'Accessing' }
RowanMethodService >> isTestMethod [

	^isTestMethod
]

{ #category : 'Accessing' }
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

{ #category : 'rowan' }
RowanMethodService >> removeSelector: sel class: clsName [
	
	self rowanFixMe. "combine remove methods variants"
	self browserTool removeMethod: sel asSymbol forClassNamed: (clsName subStrings first) isMeta: (clsName subStrings size = 2)

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
	RowanCommandResult addResult: self.
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

{ #category : 'Accessing' }
RowanMethodService >> selector: aSymbol [

	selector := aSymbol

]

{ #category : 'perform' }
RowanMethodService >> servicePerform: symbol withArguments: collection [
	super servicePerform: symbol withArguments: collection.
	self update.
]

{ #category : 'client commands' }
RowanMethodService >> setBreakAt: stepPoint [
	| method |
	method := self isUnboundMethod 
			ifTrue:[Object _objectForOop: oop] 
			ifFalse:[self gsNMethod].
	method setBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
]

{ #category : 'initialization' }
RowanMethodService >> setSupersAndSubsFor: theClass [

	| theSuper |
	theSuper := theClass superClass. 
	hasSupers := false. 
	[theSuper notNil and:[hasSupers not]] whileTrue:[
		hasSupers := theSuper selectors includes: selector.
		hasSupers ifTrue:[
			superSource := theSuper sourceCodeAt: selector.
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

{ #category : 'Accessing' }
RowanMethodService >> source: aString [
	
	source := aString

]

{ #category : 'Accessing' }
RowanMethodService >> stepPoints [

	"for testing"
	
	^stepPoints
]

{ #category : 'initialization' }
RowanMethodService >> stepPointsFor: aGsNMethod [
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aGsNMethod _allDebugInfo: 10) ifNil: [^#()].
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
	
	self isUnboundMethod ifFalse:[
		self wasRecycled ifTrue:[oop := self gsNMethod asOop].
		self wasDeleted ifTrue:[
			self updateType: #methodsRemoved:. 
			^RowanCommandResult addResult: self. ].  "removed method"
		oop ifNil: [oop := self gsNMethod asOop]].
	self 
		initialize: (Object _objectForOop: oop) 
		organizer: ClassOrganizer new.
	RowanCommandResult addResult: self.
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
