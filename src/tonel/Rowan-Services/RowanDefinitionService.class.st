Class {
	#name : 'RowanDefinitionService',
	#superclass : 'Object',
	#instVars : [
		'definition'
	],
	#category : 'Rowan-Services'
}

{ #category : 'other' }
RowanDefinitionService class >> rowanFixMe [

	"send this message to see everywhere that GS_Jade should be fixed"

]

{ #category : 'examples' }
RowanDefinitionService class >> sampleService [

	^self new sampleService

]

{ #category : 'rowan' }
RowanDefinitionService >> browserTool [

	^self projectTools browser

]

{ #category : 'symbol dictionaries' }
RowanDefinitionService >> createDefaultSymbolDictionary [

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'samples' }
RowanDefinitionService >> createSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName

]

{ #category : 'symbol dictionaries' }
RowanDefinitionService >> createSymbolDictionaryNamed: aName [

	| dictionary size | 
	dictionary := RwGsPackageSymbolDictionary new. 
	dictionary at: aName asSymbol put: dictionary. 
	size := System myUserProfile symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^dictionary

]

{ #category : 'symbol dictionaries' }
RowanDefinitionService >> defaultSymbolDictionary [

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'symbol dictionaries' }
RowanDefinitionService >> defaultSymbolDictionaryName [

	^'RowanProjects'

]

{ #category : 'rowan' }
RowanDefinitionService >> definitionClass [

	^self subclassResponsibility

]

{ #category : 'rowan' }
RowanDefinitionService >> definitionClassName [

	^self definitionClass name

]

{ #category : 'rowan' }
RowanDefinitionService >> projectTools [

	^Rowan projectTools

]

{ #category : 'samples' }
RowanDefinitionService >> removeSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.

]

{ #category : 'symbol dictionaries' }
RowanDefinitionService >> removeSymbolDictionaryNamed: aName [

	| index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]

]

{ #category : 'samples' }
RowanDefinitionService >> sampleSymbolDictionaryName [

	^'SampleSymbolDictionaryName'

]

{ #category : 'symbol dictionaries' }
RowanDefinitionService >> symbolDictionaryNamed: aName [

	|  index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			System myUserProfile symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].

]
