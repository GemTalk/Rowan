Class {
	#name : 'RowanMethodsUpdate',
	#superclass : 'RowanClassesUpdate',
	#instVars : [
		'methods',
		'methodFilters'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'initialization' }
RowanMethodsUpdate >> initialize [

	methodFilters := IdentitySet new

]

{ #category : 'accessing' }
RowanMethodsUpdate >> methodFilters [

	^methodFilters

]

{ #category : 'accessing' }
RowanMethodsUpdate >> selections [

	^selections

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

{ #category : 'initialization' }
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
	mySelections notEmpty ifTrue: [methodFilters := mySelections].
	self writeList: mySelections.

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
RowanMethodsUpdate >> writeSelectionsTo: writeStream [

	selections do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]
