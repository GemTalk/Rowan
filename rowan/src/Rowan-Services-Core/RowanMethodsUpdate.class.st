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
		'type'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'testing' }
RowanMethodsUpdate >> filterSelections [

	"intention revealing"

	^selectionNames

]

{ #category : 'testing' }
RowanMethodsUpdate >> filterSelections: aCollection [

	"intention revealing"

	selectionNames := aCollection

]

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
	methods := Array new.

]

{ #category : 'accessing' }
RowanMethodsUpdate >> methodFilters [

	^methodFilters

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectedClass [

	^self objectInBaseNamed: (selectedClass subStrings first)

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectedClass:  aString [

	selectedClass := aString

]

{ #category : 'testing' }
RowanMethodsUpdate >> selectedClassOverridesSelector: aSymbol [

	^self selectedClass superclass notNil and: [self selectedClass superclass canUnderstand: aSymbol].

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
RowanMethodsUpdate >> selectors [

	^selectors

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selectors: aCollection [

	selectors := aCollection

]

{ #category : 'accessing' }
RowanMethodsUpdate >> type: anObject [

	type := anObject

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodCategories [

	self classesFromNames do: [:each | 
		each 
			env: 0 
			categorysDo:[ :category :sels | methodFilters add: category ].
	].
	methodFilters isEmpty ifTrue: [methodFilters := #(#'other')].

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodFilterSelections: oldSelections hasCategoryOverride: override [
	self filterSelections: (oldSelections collect:[:sel | sel asSymbol]).

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodPragmas [

	| currentClass |
	currentClass := ((self objectInBaseNamed: #'Pragma') notNil and: [self selectionNames notEmpty]) ifTrue: [self objectInBaseNamed: self selectionNames first] ifFalse: [nil].
	methodFilters := IdentitySet new.
	currentClass notNil ifTrue: [
		(currentClass methodDictForEnv: 0) do: [:each | methodFilters addAll: each pragmas].
	].
	methodFilters := methodFilters collect: [:each | each keyword].
	methodFilters := (methodFilters asSortedCollection: [:a :b | a > b]) asArray.  "SortedCollections cannot be grown in Dolphin Smalltalk"

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodsInfo [

	selectors do:[:selector |
		| isTestClass update |

		isTestClass := false.
		self selectedClass _allSuperList do: [:each | 
			isTestClass := isTestClass or: [each name = #'TestCase'].
		].
		update := RowanMethodUpdate new. 
		update selector: selector;
			isTestClass: isTestClass;
			isOverride: (self selectedClassOverridesSelector: selector).
		methods add: update. 
		].

]

{ #category : 'updating' }
RowanMethodsUpdate >> updateMethodVariables [

	| currentClass | 
	currentClass := classNames notEmpty ifTrue: [self classesFromNames last] ifFalse: [nil].
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

	methods do: [:methodUpdate | 	
		writeStream 
"1"		nextPutAll: methodUpdate selector; tab;
"2"		nextPut: (methodUpdate isOverride ifTrue: [$T] ifFalse: [$F]); tab;
"3"		nextPut: (methodUpdate isTestClass ifTrue: [$T] ifFalse: [$F]); tab;
"4"		nextPut: $F; tab;
"5"		nextPutAll: methodUpdate selector; tab;	
		lf.
	].
	writeStream nextPut: $%; lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeSelectedSelectorsTo: writeStream [

	selectedSelectors do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeSelectionsTo: writeStream [

	self selectionNames do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanMethodsUpdate >> writeTypeTo: writeStream [

	writeStream 
		nextPutAll: type; tab;
		nextPutAll: '0';  "environment default"
		lf.

]
