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
		'updateAfterCommand'
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

	theClass := self classFromName.
	meta ifTrue:[theClass := theClass class]. 
	theClass addCategory: string.
]

{ #category : 'Accessing' }
RowanClassService >> allTests [
	| allSelectors theClass |
	self isTestCase ifFalse:[^Array new]. 
	theClass := self classFromName thisClass.
	theClass isAbstract ifTrue:[^Array new].
	allSelectors := self classFromName thisClass allTestSelectors.
	^allSelectors collect:[:selector | 
			RowanMethodService forSelector: selector 
										class: (theClass whichClassIncludesSelector: selector asString)
										meta: false
										organizer: organizer].
]

{ #category : 'testing' }
RowanClassService >> arePackageAndProjectClean [

	^self packageIsDirty not and:[self projectIsDirty not]
]

{ #category : 'initialization' }
RowanClassService >> basicForClassNamed: className [ 

	| theClass |
	self name: className. 
	theClass := self classFromName. 
	theClass isNil ifTrue:[oop := nil. ^self].
	self basicRefreshFrom: theClass.
]

{ #category : 'initialization' }
RowanClassService >> basicRefreshFrom: theClass [
	| classOrMeta theFilters |
	command := nil. 
	commandArgs := nil. 
	superclassName := theClass superClass ifNotNil:[:theSuper | theSuper name asString]. 
	comment := theClass rwComment. 
	organizer ifNil: [organizer := ClassOrganizer new]. "for Jade and tests"
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	template := self classCreationTemplate.
	self setComment.
	oop := theClass asOop.
	theFilters := SortedCollection new.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	packageName := definedPackageName := classOrMeta rowanPackageName.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames. 
	self setIsTestCase.
]

{ #category : 'Accessing' }
RowanClassService >> behavior [

	| behavior |
	behavior := self classFromName. 
	meta == true ifTrue:[behavior := behavior class].
	^behavior
]

{ #category : 'client commands' }
RowanClassService >> classComment: string [
	| theClass |
	theClass := self classFromName. 
	theClass rwComment: string.
]

{ #category : 'rowan' }
RowanClassService >> classCreationTemplate [
	
	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.

]

{ #category : 'Accessing' }
RowanClassService >> classFromName [

	^Rowan globalNamed: name
]

{ #category : 'client commands' }
RowanClassService >> classHierarchy [
	| behavior |
	behavior := self classFromName. 
	organizer := ClassOrganizer newWithRoot: behavior. 
	hierarchyServices := Dictionary new.   
	organizer hierarchy keysAndValuesDo: [:key :value |
		| classService |
		classService := key == #nil ifTrue:[#nil] ifFalse: [(self hierarchyClassServiceFor: key name) meta: meta].
		hierarchyServices at: classService put: (self subclassServices: value). 
	].
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
			ifTrue:[self classFromName class] 
			ifFalse: [self classFromName].
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
	
	[ [ [ method := aBehavior rwCompileMethod: methodString category: categorySymbol  ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: false ] ] 
			on: CompileError
			do: [:ex | ^nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume].
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] on: Error
		do: [:ex | ex return: method -> warnings]
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
	theClass := self classFromName. 
	self refreshFrom: theClass. 
	methods do:[:service1 |
			service1 source: nil; 
					stepPoints: Array new.
	visibleTests do:[:service2 |
			service2 source: nil; 
					stepPoints: Array new].
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
	theClass := self classFromName. 
	self refreshFrom: theClass.
]

{ #category : 'comparing' }
RowanClassService >> hash [
	^self name hash bitXor: meta hash
]

{ #category : 'private' }
RowanClassService >> hierarchyClassServiceFor: className [
	^className asString = name asString ifTrue:[
			className asString = 'Object' 
				ifTrue:[
					RowanClassService basicForClassNamed: className]
				ifFalse:[
					RowanClassService forClassNamed: className]]
		ifFalse:[
			RowanClassService minimalForClassNamed: className]
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
]

{ #category : 'initialization' }
RowanClassService >> initializeCategoriesFor: classOrMeta [

	| theFilters |
	theFilters := SortedCollection new.
	classOrMeta env: 0 categorysDo: [:category :selector | theFilters add: category asString].
	categories := theFilters asOrderedCollection.
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
	theClass := self classFromName. 
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
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames. 
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

	^System myUserProfile symbolList objectNamed: aString asSymbol
]

{ #category : 'client commands' }
RowanClassService >> oneLevelClassHierarchy [
	"good for expanding an existing hierarchy quickly"
	| behavior sortedSubclasses |
	behavior := self classFromName. 
	hierarchyServices := Dictionary new. 
	hierarchyServices at: #expand put: Array new. 
	sortedSubclasses := behavior subclasses asSortedCollection:[:x :y | x name < y name].
	sortedSubclasses do: [:subclass |
		| classService |
		classService := (self hierarchyClassServiceFor: subclass name) meta: meta.
		(hierarchyServices at: #expand) add: classService. 
	].
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
	behavior := self classFromName.
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
	behavior := self classFromName.
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
	self refreshFrom: self classFromName. 
	theClass := self classFromName.
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
			RowanCommandResult addResult: methodService
		]].
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
	theClass := self classFromName. 
	meta ifTrue: [theClass := theClass class].
	(theClass compiledMethodAt: selector otherwise: nil) isNil ifTrue:[ ^absentBlock value ].
	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta

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
	| references newMethods |
	newMethods := Array new. 
	Rowan projectTools browser renameClassNamed: oldClassName to: newClassName.
	name := newClassName. 
	organizer := ClassOrganizer new. 
	references := organizer update referencesTo: oldClassName asSymbol.
	1 to: references first size do:[:index |
		| method newSource compileResult failedCompile methodService |
		failedCompile := false. 
		method := references first at: index. 
		newSource := self replaceSubString: oldClassName in: method sourceString with: newClassName.
		compileResult := [method inClass rwCompileMethod: newSource
									category: (method inClass categoryOfSelector: method selector) asSymbol] on: CompileError do:[:ex | failedCompile := true. method].
		methodService := RowanMethodService forGsNMethod: compileResult organizer: organizer.
		methodService failedCompile: failedCompile. 
		newMethods add: methodService.
		].
	RowanCommandResult addResult: (RowanAnsweringService new answer: newMethods).
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
	behavior := classService classFromName. 
	self refreshFrom: behavior.
	self tests do:[:methodService |
			behavior debug: methodService selector]. 
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
]

{ #category : 'client commands' }
RowanClassService >> runMethodTests: methodServices [

	| behavior |
	behavior := self classFromName.  
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior debug: methodService selector]].
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
]

{ #category : 'client commands' }
RowanClassService >> saveMethodSource: source category: category [

	| behavior compilationResult gsNMethod updatedCategory methodService |        
	meta ifNil:[
				behavior := (Object _objectForOop: oop). 
				meta := behavior isMeta]
			ifNotNil:[ 
				behavior := meta ifTrue:[self classFromName class] ifFalse:[self classFromName]]. 
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
	self update.  
	methodService := self methodServiceFrom: gsNMethod in: behavior compiltationResult: compilationResult.
	RowanCommandResult addResult: methodService.
	RowanQueryService new 
			organizer: ClassOrganizer new;
			hierarchyImplementorsOf: methodService selector inClass: methodService className. "this will update hierarchy method indicators for client"
	self selectedMethods: (Array with: methodService).
	self updateDirtyState.
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
	wasClean := self arePackageAndProjectClean.
	super servicePerform: symbol withArguments: collection.
	updateAfterCommand == false ifFalse:[
		self update].
	wasClean ifTrue:[
		self updatePackageProject]
]

{ #category : 'Accessing' }
RowanClassService >> setComment [

	| theClass |
	theClass := self classFromName.
	(theClass canUnderstand: #rwComment)
		ifTrue: [comment := theClass rwComment]
		ifFalse: 
			[(theClass canUnderstand: #description)
					ifTrue: 
						[| description |
						description := theClass description.
						description class name = #GsClassDocumentation
							ifTrue: [comment := description detailsAboutClass]]].
	comment isNil ifTrue: [comment := String new].

]

{ #category : 'Updating' }
RowanClassService >> setIsTestCase [

	isTestCase := self classFromName isSubclassOf: TestCase
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
	| answerService |
	answerService := RowanAnsweringService new. 
	answerService answer: (self browserTool classCreationTemplateForSubclassOf: name 
								className: 'NewSubclass' category: self classFromName rowanPackageName).
	RowanCommandResult addResult: answerService.
]

{ #category : 'private' }
RowanClassService >> subclassServices: subclasses [

	| sortedSubclasses |

	sortedSubclasses := SortedCollection sortBlock: [:x :y | x name < y name]. 
	sortedSubclasses addAll: subclasses. 
	^(sortedSubclasses collect:[:cls | (self hierarchyClassServiceFor: cls name) meta: meta]) asArray.
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

{ #category : 'updates' }
RowanClassService >> update [

	self updateClass.
]

{ #category : 'updates' }
RowanClassService >> updateClass [

	"It's possible to have a nil class. For example, if we added & selected
	a class then aborted."

	| theClass |
	theClass := self classFromName. 
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
	projectService := RowanProjectService newNamed: self classFromName rowanProjectName. 
	RowanCommandResult addResult: projectService.
]

{ #category : 'updates' }
RowanClassService >> updatePackageProject [
	| packageService projectService |

	packageService := RowanPackageService new name: packageName. 
	packageService update. 
	RowanCommandResult addResult: packageService.

	projectService := RowanProjectService new name: projectName. 
	projectService update.
	RowanCommandResult addResult: projectService.
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

{ #category : 'testing' }
RowanClassService >> wasDeleted [
	^(Rowan globalNamed: name) isNil
]
