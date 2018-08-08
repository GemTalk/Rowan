Class {
	#name : 'RowanClassCategoryUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'categories',
		'selection'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassCategoryUpdate >> categories [

	^categories

]

{ #category : 'initialization' }
RowanClassCategoryUpdate >> initialize [

	categories := Array new.

]

{ #category : 'accessing' }
RowanClassCategoryUpdate >> selection [

	^selection

]

{ #category : 'accessing' }
RowanClassCategoryUpdate >> selection: anObject [

	selection := anObject

]

{ #category : 'jade' }
RowanClassCategoryUpdate >> writeCategoriesTo: writeStream [

	categories do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanClassCategoryUpdate >> writeSelectionTo: writeStream [

	writeStream nextPutAll: selection.
	writeStream lf.

]
