Class {
	#name : 'RowanDictionaryUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'dictionaries',
		'selections'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanDictionaryUpdate >> dictionaries [

	^dictionaries

]

{ #category : 'initialization' }
RowanDictionaryUpdate >> initialize [

	dictionaries := Array new. 
	selections := Array new.

]

{ #category : 'accessing' }
RowanDictionaryUpdate >> selections [

	^selections

]

{ #category : 'jade' }
RowanDictionaryUpdate >> writeDictionariesTo: writeStream [

	dictionaries do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanDictionaryUpdate >> writeSelectionsTo: writeStream [

	selections do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]
