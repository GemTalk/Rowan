Class {
	#name : 'RowanClassesUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'classes',
		'selections'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassesUpdate >> classes [

	^classes

]

{ #category : 'accessing' }
RowanClassesUpdate >> classes: anArray [

	classes := anArray

]

{ #category : 'private' }
RowanClassesUpdate >> historyOf: aClass [

	| history |
	(history := aClass classHistory) isNil ifTrue: [
		history := Array with: aClass.
	].
	^history

]

{ #category : 'initialization' }
RowanClassesUpdate >> initialize [

	classes := Array new.
	selections := Array new.

]

{ #category : 'jade' }
RowanClassesUpdate >> objectInBaseNamed: aString [

	^[(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].


]

{ #category : 'accessing' }
RowanClassesUpdate >> selections [

	^selections

]

{ #category : 'jade' }
RowanClassesUpdate >> writeClass: aClass to: writeStream [

	writeStream nextPutAll: aClass name.
		1 < (self historyOf: aClass) size ifTrue: [
			writeStream nextPutAll: ' ('.
			((self historyOf: aClass) indexOf: aClass) printOn: writeStream.
			writeStream nextPut: $/.
			(self historyOf: aClass) size printOn: writeStream.
			writeStream nextPut: $)].
	writeStream tab.

]

{ #category : 'jade' }
RowanClassesUpdate >> writeClassesTo: writeStream [

	classes do:[:aClass | 
		self writeClass: aClass to: writeStream
	].
	writeStream lf.

]

{ #category : 'jade' }
RowanClassesUpdate >> writeSelectionsTo: writeStream [

	selections do:[:aClass | 
		self writeClass: aClass to: writeStream].
	writeStream lf.

]

{ #category : 'jade' }
RowanClassesUpdate >> writeTestCaseInfoTo: writeStream [

	| testCaseClass | 
	testCaseClass:= self objectInBaseNamed: #'TestCase'.
	testCaseClass isNil ifTrue: [
		false printOn: writeStream.
	] ifFalse: [
		(selections allSatisfy: [:each | each  inheritsFrom: testCaseClass]) printOn: writeStream.
	].
	writeStream lf.
]
