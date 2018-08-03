Class {
	#name : 'RowanMethodService',
	#superclass : 'RowanService',
	#instVars : [
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
		'selectedPackageServices'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanMethodService class >> forGsNMethod: aGsNMethod organizer: aClassOrganizer [

	^self basicNew
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
		category: category;
		className: className;
		packageName: packageName;
		meta: boolString == true.
	^service

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

{ #category : 'other' }
RowanMethodService >> allReferences [

	| methods |
	methods := organizer sendersOf: selector.
	references := methods first collect:[:gsNMethod | 
			self class forSelector: gsNMethod selector class: gsNMethod inClass meta: gsNMethod inClass isMeta organizer: organizer].
	RowanCommandResult addResult: self

]

{ #category : 'rowan' }
RowanMethodService >> browserTool [

	^Rowan projectTools browser.

]

{ #category : 'Accessing' }
RowanMethodService >> category [
	^category

]

{ #category : 'Updating' }
RowanMethodService >> category: newValue [
	category := newValue

]

{ #category : 'Accessing' }
RowanMethodService >> classFromName [

	^(System myUserProfile resolveSymbol: className asSymbol) value

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
RowanMethodService >> classService [

	^classService ifNil:[classService := RowanClassService forClassNamed: className package: packageName]

]

{ #category : 'Updating' }
RowanMethodService >> classService: newValue [
	classService := newValue

]

{ #category : 'Updating' }
RowanMethodService >> compilationWarnings: newValue [
	compilationWarnings := newValue
]

{ #category : 'rowan' }
RowanMethodService >> definitionClass [

	^RwMethodDefinition

]

{ #category : 'other' }
RowanMethodService >> forClass: theClass organizer: theOrganizer [
	"assume meta and selector are set"

	| classOrMeta gsNMethod |
	organizer := theOrganizer. 
	classOrMeta := meta ifTrue:[theClass class] ifFalse:[theClass]. 
	gsNMethod := classOrMeta compiledMethodAt: selector.
	source := gsNMethod sourceString.
	category := classOrMeta categoryOfSelector: selector.
	className := theClass name. 
	packageName := gsNMethod rowanPackageName. 
	projectName := gsNMethod rowanProjectName.
	self setSupersAndSubsFor: theClass.
	isExtension := self rowanIsExtension.

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

]

{ #category : 'other' }
RowanMethodService >> initialize: aGsNMethod organizer: aClassOrganizer [

	| inClass |
	selector := aGsNMethod selector.
	stepPoints := self stepPointsFor: aGsNMethod.
	(inClass := aGsNMethod inClass) ifNil: [
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
RowanMethodService >> packageName [
	^packageName

]

{ #category : 'Updating' }
RowanMethodService >> packageName: newValue [
	packageName := newValue

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

{ #category : 'other' }
RowanMethodService >> setSupersAndSubsFor: theClass [

	| theSuper |
	theSuper := theClass superClass. 
	hasSupers := false. 
	[theSuper notNil and:[hasSupers not]] whileTrue:[
		hasSupers := theSuper canUnderstand: selector.
		theSuper := theSuper superClass].
	(organizer allSubclassesOf: theClass) do:[:cls |
		(hasSubs := cls includesSelector: selector) ifTrue:[^self]].

]

{ #category : 'Accessing' }
RowanMethodService >> source [

	^source

]

{ #category : 'Accessing' }
RowanMethodService >> source: aString [
	
	source := aString

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

{ #category : 'replication' }
RowanMethodService >> stonOn: stonWriter [
    | instanceVariableNames |
    instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
    stonWriter writeObject: self
        streamMap: 
            [:dictionary |
            instanceVariableNames do: 
                    [:each |
                    (self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
                        ifNotNil: [:value | dictionary at: each asSymbol put: value]
                        ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]

]

{ #category : 'updates' }
RowanMethodService >> update [
]
