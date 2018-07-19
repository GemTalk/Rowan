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
		'className',
		'meta',
		'hasSupers',
		'hasSubs',
		'organizer',
		'compilationWarnings',
		'isExtension'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanMethodService class >> forSelector: sel class: theClass meta: boolean organizer: organizer [

	| service |
	service := self new. 
	service selector: sel;
		meta: boolean.
	service forClass: theClass organizer: organizer.
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

{ #category : 'method compilation' }
RowanMethodService >> compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol [
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	
	[[method := aBehavior rwCompileMethod: methodString category: categorySymbol] on: CompileError
		do: [:ex | ^nil -> (ex gsArguments at: 1)]]
			on: CompileWarning
			do: 
				[:ex |
				warnings := ex gsArguments at: 1.
				ex resume].
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] on: Error
		do: [:ex | ex return: method -> warnings]

]

{ #category : 'examples' }
RowanMethodService >> createSampleMethod [
           
           |   classDefinition | 
		
			classService := RowanClassService new.
			classDefinition := classService createSampleClass. 
			source := self sampleMethodSource.
			category := 'sample'.
			meta := false. 
			^self addOrUpdateMethod

]

{ #category : 'rowan' }
RowanMethodService >> definitionClass [

	^RwMethodDefinition

]

{ #category : 'initialization' }
RowanMethodService >> forClass: theClass organizer: theOrganizer [
	"assume meta and selector are set"

	| classOrMeta method |
	organizer := theOrganizer. 
	classOrMeta := meta ifTrue:[theClass class] ifFalse:[theClass]. 
	method := classOrMeta compiledMethodAt: selector.
	source := method sourceString.
	category := classOrMeta categoryOfSelector: selector.
	className := theClass name. 
	packageName := theClass rowanPackageName. 
	self setSupersAndSubsFor: theClass.
	isExtension := self rowanIsExtension. 
	organizer := nil. "replicating the organizer with ston will fail."

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

{ #category : 'Accessing' }
RowanMethodService >> meta [
	^meta printString

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

{ #category : 'testing' }
RowanMethodService >> needsRecompile [

	| theMethod |
	theMethod := self method. 
	^theMethod isNil or:[
		theMethod sourceString ~= source or:[
		(self classFromName categoryOfSelector: selector) ~= category]]

]

{ #category : 'Accessing' }
RowanMethodService >> packageName [
	^packageName

]

{ #category : 'Updating' }
RowanMethodService >> packageName: newValue [
	packageName := newValue

]

{ #category : 'method compilation' }
RowanMethodService >> removeMethod [

	| clsService notRemoved |
	clsService := self classService. 
	notRemoved := Array new. 
	clsService removeSelector: selector ifAbsent:[notRemoved add: selector].
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', notRemoved printString]

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

{ #category : 'examples' }
RowanMethodService >> sampleClassInstance [

	^classService sampleClassInstance

]

{ #category : 'examples' }
RowanMethodService >> sampleDefinition [

	definition := self definitionClass newForSelector: 'sampleMethod' protocol: 'sampleProtocol' source: self sampleMethodSource.
	source := definition source.
	selector := definition selector.
	^definition

]

{ #category : 'examples' }
RowanMethodService >> sampleDefinitions [
	
	methodDefinitions := Array with: self sampleDefinition.
	^methodDefinitions

]

{ #category : 'examples' }
RowanMethodService >> sampleMethodSelector [

	^'sampleMethod'

]

{ #category : 'examples' }
RowanMethodService >> sampleMethodSource [

	^'sampleMethod ^''some text'''.

]

{ #category : 'method compilation' }
RowanMethodService >> saveMethod [

	| behavior compilationResult gsNMethod |
	behavior := self classFromName.
	category := category ifNil: ['other'].
	compilationResult := self		
		compileMethod: source 
		behavior: behavior 
		symbolList: System myUserProfile symbolList
		inCategory: category asSymbol.
	(gsNMethod := compilationResult key) isNil ifTrue: [
		System
			signal: 1001 
			args: (Array with: compilationResult value)
			signalDictionary: GemStoneError.
	].
	category := behavior categoryOfSelector: gsNMethod selector. 
	compilationWarnings := compilationResult value

]

{ #category : 'Accessing' }
RowanMethodService >> selector [

	^selector

]

{ #category : 'Accessing' }
RowanMethodService >> selector: aSymbol [

	selector := aSymbol

]

{ #category : 'initialization' }
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

{ #category : 'testing' }
RowanMethodService >> shouldDelete [

	| theMethod |
	theMethod := self method.
	theMethod isNil ifTrue:[^false].
	^theMethod source isNil
]

{ #category : 'Accessing' }
RowanMethodService >> source [

	^source

]

{ #category : 'Accessing' }
RowanMethodService >> source: aString [
	
	source := aString

]

{ #category : 'testing' }
RowanMethodService >> test [

	^5
]

{ #category : 'updates' }
RowanMethodService >> update [
	self shouldDelete ifTrue:[
		^self removeMethod].
	self needsRecompile ifTrue:[
		^self saveMethod].
]
