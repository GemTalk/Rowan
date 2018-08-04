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
		'definedPackageName',
		'selectedMethods',
		'projectName'
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

{ #category : 'commands' }
RowanClassService >> classComment: string [

	self classFromName comment: string.
	RowanCommandResult addResult: self.

]

{ #category : 'rowan' }
RowanClassService >> classCreationTemplate [
	
	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.

]

{ #category : 'Accessing' }
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

{ #category : 'method compilation' }
RowanClassService >> compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol [
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

{ #category : 'initialization' }
RowanClassService >> forClassNamed: className [ 

	| theClass |
	name := className. 
	theClass := self classFromName. 
	self refreshFrom: theClass.

]

{ #category : 'initialization' }
RowanClassService >> initialize [

	isExtension := false.
	selectedMethods := Array new.
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

{ #category : 'private' }
RowanClassService >> methodSelectorsFor: classOrMeta organizer: theOrganizer [

	methods addAll: (classOrMeta selectors collect:[:sel | 
			RowanMethodService 
				forSelector: sel class: classOrMeta thisClass meta: meta organizer: theOrganizer])
]

{ #category : 'private' }
RowanClassService >> methodServiceFrom: gsNMethod in: behavior compiltationResult: compilationResult [
	| methodService |

	methodService := RowanMethodService forGsNMethod: gsNMethod organizer: organizer. 
	methodService compilationWarnings: compilationResult value.
	^methodService

]

{ #category : 'commands' }
RowanClassService >> moveMethods: methodServices to: category [
	| behavior |
	behavior := meta 
			ifTrue:[self classFromName class] 
			ifFalse: [self classFromName].
	methodServices do: [:methodService | 
			behavior rwMoveMethod: methodService selector toCategory: category.
			methodService category: category].
	self update. 
	self selectedMethods: methodServices. 
	RowanCommandResult addResult: self.
]

{ #category : 'Accessing' }
RowanClassService >> name [
	^name

]

{ #category : 'Updating' }
RowanClassService >> name: newValue [
	name := newValue

]

{ #category : 'private' }
RowanClassService >> objectInBaseNamed: aString [

	^System myUserProfile symbolList objectNamed: aString asSymbol
]

{ #category : 'Accessing' }
RowanClassService >> oop [
	^oop

]

{ #category : 'Updating' }
RowanClassService >> oop: newValue [
	oop := newValue

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

{ #category : 'Updating' }
RowanClassService >> projectName: newValue [
	projectName := newValue
]

{ #category : 'initialization' }
RowanClassService >> refreshFrom: theClass [
	| classOrMeta theFilters |
	command := nil. 
	commandArgs := nil. 
	comment := theClass comment. 
	organizer ifNil: [organizer := ClassOrganizer new]. "for Jade"
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	template := self classCreationTemplate.
	self setComment.
	oop := theClass asOop.
	theFilters := SortedCollection new.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	packageName := classOrMeta rowanPackageName. 
	filterType = 'variables'
		ifTrue: [^self refreshFromVariable: classOrMeta].
	self refreshFromCategory: classOrMeta
]

{ #category : 'initialization' }
RowanClassService >> refreshFromCategory: classOrMeta [
	| theFilters |
	theFilters := SortedCollection new.
	classOrMeta env: 0 categorysDo: [:category :selector | theFilters add: category].
	allFilters := theFilters asOrderedCollection.
	methods := SortedCollection sortBlock: [:x :y | x selector < y selector].
	self methodSelectorsFor: classOrMeta organizer: organizer.
	methods := methods asOrderedCollection

]

{ #category : 'initialization' }
RowanClassService >> refreshFromVariable: classOrMeta [
	| theFilters gsNmethods  | 
	theFilters := SortedCollection new.
	theFilters addAll: classOrMeta allInstVarNames.
	allFilters := theFilters asOrderedCollection.
	methods := SortedCollection sortBlock: [:x :y | x selector < y selector].
	gsNmethods := Set new. 
	filters isEmpty ifTrue:[
		self methodSelectorsFor: classOrMeta organizer: organizer]
	ifFalse:[
		filters do:[:instVar |
			gsNmethods addAll: (organizer accessorsOf: instVar inClass: classOrMeta).
			methods addAll: (gsNmethods collect:[:gsNmeth | 
				RowanMethodService 
					forSelector: gsNmeth selector class: gsNmeth inClass thisClass meta: meta organizer: organizer]).]].
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

{ #category : 'commands' }
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

{ #category : 'commands' }
RowanClassService >> saveMethodSource: source category: category [

	| behavior compilationResult gsNMethod updatedCategory methodService |
	behavior := meta ifTrue:[self classFromName class] ifFalse:[self classFromName].
	updatedCategory := category ifNil: ['other'].
	compilationResult := self		
		compileMethod: source 
		behavior: behavior 
		symbolList: System myUserProfile symbolList
		inCategory: updatedCategory asSymbol.
	(gsNMethod := compilationResult key) isNil ifTrue: [
		System
			signal: 1001  
			args: (Array with: compilationResult value) 
			signalDictionary: GemStoneError.
	]. 
	methodService := self methodServiceFrom: gsNMethod in: behavior compiltationResult: compilationResult.
	RowanCommandResult addResult: methodService.
	self update.  
	selectedMethods := (Array with: methodService).
	RowanCommandResult addResult: self.
]

{ #category : 'Updating' }
RowanClassService >> selectedMethods: newValue [
	selectedMethods := newValue
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

	self updateClass.
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
