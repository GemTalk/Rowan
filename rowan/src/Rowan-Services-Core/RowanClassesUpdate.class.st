Class {
	#name : 'RowanClassesUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'classNames',
		'selectionNames'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassesUpdate >> classesFromNames [
	| result |
	result := classNames collect:[:name | 
		| cls |
		cls := self objectInBaseNamed: (name subStrings first).
		name subStrings size = 2 "meta" ifTrue:[cls := cls class.].
		cls].
	^result

]

{ #category : 'accessing' }
RowanClassesUpdate >> classNames [

	^classNames

]

{ #category : 'accessing' }
RowanClassesUpdate >> classNamesFrom: theClasses [
	"store the class name for transport to the client"
	classNames := theClasses collect:[:class | class name]

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

	classNames := Array new.
	selectionNames := Array new.

]

{ #category : 'private' }
RowanClassesUpdate >> objectInBaseNamed: aString [

	^Rowan image symbolList objectNamed: aString asSymbol
]

{ #category : 'accessing' }
RowanClassesUpdate >> selectionNames [

	^selectionNames

]

{ #category : 'accessing' }
RowanClassesUpdate >> selectionNamesAsSymbols [

	^self selectionNames collect:[:string | string asSymbol]

]

{ #category : 'accessing' }
RowanClassesUpdate >> selections [

	^selectionNames

]

{ #category : 'accessing' }
RowanClassesUpdate >> selections: aCollection [

	selectionNames := aCollection.

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

	self classesFromNames do:[:aClass | 
		self writeClass: aClass to: writeStream
	].
	writeStream lf.

]

{ #category : 'jade' }
RowanClassesUpdate >> writeSelectionsTo: writeStream [

	self selectionNames do:[:name | 
		self writeClass: (self objectInBaseNamed: (name subStrings first)) to: writeStream].
	writeStream lf.

]

{ #category : 'jade' }
RowanClassesUpdate >> writeTestCaseInfoTo: writeStream [

	| testCaseClass | 
	testCaseClass:= self objectInBaseNamed: #'TestCase'.
	testCaseClass isNil ifTrue: [
		false printOn: writeStream.
	] ifFalse: [
		(self selectionNames allSatisfy: [:each | (self objectInBaseNamed: each)  inheritsFrom: testCaseClass]) printOn: writeStream.
	].
	writeStream lf.

]
