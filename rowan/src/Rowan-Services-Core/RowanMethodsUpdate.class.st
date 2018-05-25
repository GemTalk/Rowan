Class {
	#name : 'RowanMethodsUpdate',
	#superclass : 'RowanClassesUpdate',
	#instVars : [
		'methods',
		'methodFilters',
		'selectors',
		'selectedClass',
		'selectedSelectors',
		'inheritedClasses',
		'inheritedClass',
		'methodUpdates'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanMethodsUpdate >> inheritedClass: aString [

	inheritedClass := aString

]

{ #category : 'accessing' }
RowanMethodsUpdate >> inheritedClasses: aCollection [

	inheritedClasses := aCollection

]

{ #category : 'initialization' }
RowanMethodsUpdate >> initialize [
	super initialize.
	methodFilters := IdentitySet new.
	methodUpdates := Array new

]

{ #category : 'accessing' }
RowanMethodsUpdate >> methodFilters [

	^methodFilters

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectedClass:  aClass [

	selectedClass := aClass

]

{ #category : 'private' }
RowanMethodsUpdate >> selectedClassOverridesSelector: aSymbol [

	^selectedClass superclass notNil and: [selectedClass superclass canUnderstand: aSymbol].

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectedSelectors [

	^selectedSelectors

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectedSelectors: aCollection [

	selectedSelectors := aCollection

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selections [

	^selections

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectors [

	^selectors

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectors: aCollection [

	selectors := aCollection

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodCategories [

	classes do: [:each | 
		each 
			env: 0 
			categorysDo:[ :category :selectors | methodFilters add: category ].
	].
	methodFilters isEmpty ifTrue: [methodFilters := #(#'other')].

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodPragmas [

	| currentClass |
	currentClass := ((self objectInBaseNamed: #'Pragma') notNil and: [selections notEmpty]) ifTrue: [selections first] ifFalse: [nil].
	methodFilters := IdentitySet new.
	currentClass notNil ifTrue: [
		(currentClass methodDictForEnv: 0) do: [:each | methodFilters addAll: each pragmas].
	].
	methodFilters := methodFilters collect: [:each | each keyword].
	methodFilters := methodFilters asSortedCollection: [:a :b | a > b].

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodSelections: oldSelections hasCategoryOverride: override [

	selections := oldSelections.
	override notNil ifTrue: [selections := Array with: override].
	selections := methodFilters select: [:each | selections includes: each asString].
	selections notEmpty ifTrue: [methodFilters := selections].

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodSelections: oldSelections override: override [

	| mySelections | 
	mySelections := oldSelections.
	override notNil ifTrue: [mySelections := Array with: override].
	mySelections := methodFilters select: [:each | mySelections includes: each asString].
	self writeList: mySelections.

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodsInfo [

	selectors do:[:selector |
		| isTestClass update |

		isTestClass := false.
		selectedClass _allSuperList do: [:each | 
			isTestClass := isTestClass or: [each name = #'TestCase'].
		].
		update := RowanMethodUpdate new. 
		update selector: selector;
			isTestClass: isTestClass;
			isOverride: (self selectedClassOverridesSelector: selector).
		methods add: update. 
		]

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodVariables [

	| currentClass |
	currentClass := classes notEmpty ifTrue: [classes last] ifFalse: [nil].
	methodFilters := OrderedCollection new.
	[
		currentClass notNil.
	] whileTrue: [
		methodFilters
			addAll: currentClass instVarNames reverse;
			add: ' ' , currentClass name;
			yourself.
		currentClass := currentClass superclass.
	].

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeInheritedClassesTo: writeStream [

	inheritedClasses do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeInheritedClassTo: writeStream [

	writeStream nextPutAll: inheritedClass; lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeMethodCategoriesTo: writeStream [
	"Reverse order to be consistent with variables, where we add superclasses to the end"

	methodFilters := methodFilters asSortedCollection asArray reverse.
	self writeMethodFiltersTo: writeStream

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeMethodFiltersTo: writeStream [

	methodFilters do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeMethodsTo: writeStream [

	methods do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeSelectedSelectorsTo: writeStream [

	selectedSelectors do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeSelectionsTo: writeStream [

	selections do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeSelectorsTo: writeStream [

	| isTestClass anySatisfy |
	anySatisfy := false.
	selectedClass _allSuperList do: [:each | 
		anySatisfy := anySatisfy or: [each name = #'TestCase'].
	].
	isTestClass := anySatisfy ifTrue: [$T] ifFalse: [$F].
	selectors do: [:each | 	"JadeSystemBrowserPresenter>>#'updateMethodList' "
		writeStream 
"1"		nextPutAll: each; tab;
"2"		nextPut: ((self selectedClassOverridesSelector: each) ifTrue: [$T] ifFalse: [$F]); tab;
"3"		nextPut: isTestClass; tab;
"4"		nextPut: $F; tab;
"5"		nextPutAll: each; tab;	
		lf.
	].
	writeStream nextPut: $%; lf.

]
