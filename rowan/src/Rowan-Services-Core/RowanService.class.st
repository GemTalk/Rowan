Class {
	#name : 'RowanService',
	#superclass : 'Object',
	#instVars : [
		'definition',
		'updates',
		'command'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanService class >> new [

	^super new initialize

]

{ #category : 'other' }
RowanService class >> rowanFixMe [

	"send this message to see everywhere that GS_Jade should be fixed"

]

{ #category : 'examples' }
RowanService class >> sampleService [

	^self new sampleService

]

{ #category : 'rowan' }
RowanService >> browserTool [

	^self projectTools browser

]

{ #category : 'accessing' }
RowanService >> command [

	^command

]

{ #category : 'accessing' }
RowanService >> command: aSymbol [

	command := aSymbol

]

{ #category : 'symbol dictionaries' }
RowanService >> createDefaultSymbolDictionary [

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'samples' }
RowanService >> createSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName

]

{ #category : 'symbol dictionaries' }
RowanService >> createSymbolDictionaryNamed: aName [

	| dictionary size |
	dictionary := SymbolDictionary new.
	dictionary at: aName asSymbol put: dictionary.
	size := System myUserProfile symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^ dictionary
]

{ #category : 'symbol dictionaries' }
RowanService >> defaultSymbolDictionary [

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'symbol dictionaries' }
RowanService >> defaultSymbolDictionaryName [

	^'RowanProjects'

]

{ #category : 'rowan' }
RowanService >> definitionClass [

	^self subclassResponsibility

]

{ #category : 'rowan' }
RowanService >> definitionClassName [

	^self definitionClass name

]

{ #category : 'initialization' }
RowanService >> initialize [
]

{ #category : 'rowan' }
RowanService >> projectTools [

	^Rowan projectTools

]

{ #category : 'samples' }
RowanService >> removeSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.

]

{ #category : 'symbol dictionaries' }
RowanService >> removeSymbolDictionaryNamed: aName [

	| index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]

]

{ #category : 'other' }
RowanService >> rowanFixMe [
		
	"marker for all things broken in Rowan"

]

{ #category : 'rowan' }
RowanService >> rowanLoadedPackageNames [

	| stream packages |
	self rowanFixMe.	"handle modified package display"
	stream := WriteStream on: String new.
	packages := Rowan packageNames.
	packages do: 
					[:package |
					stream
						nextPutAll: package;
						tab;
						nextPut: ((RwPackage newNamed: package) isDirty ifTrue:[$Y] ifFalse:[$N]);
						tab;
						nextPutAll: package;
						lf].
	^stream contents

]

{ #category : 'samples' }
RowanService >> sampleSymbolDictionaryName [

	^'SampleSymbolDictionaryName'

]

{ #category : 'replication' }
RowanService >> stonStringFor: anObject [

	"return a string representing a complete object structure
	suitable for replicating on the client."

	^STON toString: anObject

]

{ #category : 'symbol dictionaries' }
RowanService >> symbolDictionaryNamed: aName [

	|  index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			System myUserProfile symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].

]
