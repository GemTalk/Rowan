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
		'allFilters',
		'filters',
		'filterType',
		'methods',
		'selectedPackageServices',
		'packageName',
		'definedPackageName'
	],
	#category : 'Rowan-Services-Core'
}

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

{ #category : 'commands' }
RowanClassService >> addCategory: string [

	| theClass |

	theClass := self classFromName.
	meta ifTrue:[theClass := theClass class]. 
	theClass addCategory: string.
	self refreshFrom: self classFromName. 
	RowanCommandResult addResult: self.

]

{ #category : 'rowan' }
RowanClassService >> classCreationTemplate [
	
	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.

]

{ #category : 'private' }
RowanClassService >> classFromName [

	^Rowan globalNamed: name

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

{ #category : 'Updating' }
RowanClassService >> definedPackageName: newValue [

	definedPackageName := newValue

]

{ #category : 'Accessing' }
RowanClassService >> filters [
	^filters

]

{ #category : 'Updating' }
RowanClassService >> filters: newValue [
	filters := newValue

]

{ #category : 'Accessing' }
RowanClassService >> filterType [
	^filterType

]

{ #category : 'Updating' }
RowanClassService >> filterType: newValue [
	filterType := newValue

]

{ #category : 'instance creation' }
RowanClassService >> forClassNamed: className [ 

	| theClass |
	name := className. 
	theClass := self classFromName. 
	self refreshFrom: theClass.

]

{ #category : 'initialization' }
RowanClassService >> initialize [

	isExtension := false.

]

{ #category : 'Accessing' }
RowanClassService >> instVarNames [
	^instVarNames

]

{ #category : 'Updating' }
RowanClassService >> instVarNames: newValue [
	instVarNames := newValue

]

{ #category : 'Updating' }
RowanClassService >> isExtension: boolean [

	isExtension := boolean

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
RowanClassService >> name [
	^name

]

{ #category : 'Updating' }
RowanClassService >> name: newValue [
	name := newValue

]

{ #category : 'Accessing' }
RowanClassService >> oop [
	^oop

]

{ #category : 'Updating' }
RowanClassService >> oop: newValue [
	oop := newValue

]

{ #category : 'examples' }
RowanClassService >> packageName [
	
	^packageName

]

{ #category : 'examples' }
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

{ #category : 'updates' }
RowanClassService >> refreshFrom: theClass [
	| classOrMeta theFilters organizer |
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	template := self classCreationTemplate.
	self setComment.
	oop := theClass asOop.
	theFilters := SortedCollection new.
	filterType isNil ifTrue: [filterType := 'categories'].
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	filterType = 'categories'
		ifTrue: [classOrMeta env: 0 categorysDo: [:category :selector | theFilters add: category]].
	allFilters := theFilters asOrderedCollection.
	methods := SortedCollection sortBlock: [:x :y | x selector < y selector].
	organizer := ClassOrganizer new.
	methods addAll: (classOrMeta selectors collect:[:sel | 
			RowanMethodService 
				forSelector: sel class: theClass meta: meta organizer: organizer]).
	methods := methods asOrderedCollection

]

{ #category : 'commands' }
RowanClassService >> removeCategories: categories [
	| theClass |
	theClass := self classFromName.
	meta ifTrue:[theClass := theClass class]. 
	categories do: [:category |
		theClass removeCategory: category.
		].
	self refreshFrom: self classFromName. 
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanClassService >> removeMethods [


	| notRemoved |
	notRemoved := Array new. 
	methods do: [:methodService |
		self removeSelector: methodService selector ifAbsent:[notRemoved add: methodService selector]].
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', notRemoved printString].
	self updateClass.

]

{ #category : 'rowan' }
RowanClassService >> removeSelector: selector [

	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta

]

{ #category : 'rowan' }
RowanClassService >> removeSelector: selector ifAbsent: absentBlock [
	| theClass |
	theClass := self classFromName. 
	meta ifTrue: [theClass := theClass class].
	(theClass compiledMethodAt: selector otherwise: nil) isNil ifTrue:[ ^absentBlock value ].
	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta

]

{ #category : 'examples' }
RowanClassService >> sampleClass [
	"return the actual resolved class"
	
	^(GsSession currentSession resolveSymbol: self sampleClassName) value

]

{ #category : 'examples' }
RowanClassService >> sampleClassInstance [

	^self sampleClass new

]

{ #category : 'examples' }
RowanClassService >> sampleClassName [
	
	^'SampleClassName'

]

{ #category : 'examples' }
RowanClassService >> samplePackageName [
	
	^'SamplePackageName'

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
RowanClassService >> setComment [

	| theClass |
	theClass := self classFromName.
	(theClass canUnderstand: #comment)
		ifTrue: [comment := theClass comment]
		ifFalse: 
			[(theClass canUnderstand: #description)
					ifTrue: 
						[| description |
						description := theClass description.
						description class name = #GsClassDocumentation
							ifTrue: [comment := description detailsAboutClass]]].
	comment isNil ifTrue: [comment := String new].

]

{ #category : 'rowan' }
RowanClassService >> subclassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: name className: 'NewSubclass' category: self classFromName rowanPackageName

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

{ #category : 'updates' }
RowanClassService >> update [

	command ifNil:[ self updateClass] ifNotNil: [:symbol | self perform: symbol].
	RowanCommandResult addResult: self

]

{ #category : 'updates' }
RowanClassService >> updateClass [

	| theClass |
	theClass := self classFromName. 
	self refreshFrom: theClass.
	RowanCommandResult addResult: self
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
