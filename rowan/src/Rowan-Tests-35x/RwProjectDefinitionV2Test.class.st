Class {
	#name : 'RwProjectDefinitionV2Test',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'private' }
RwProjectDefinitionV2Test class >> _sampleSymbolDictionaryName1 [

	^ #'RowanSample9_1'
]

{ #category : 'private' }
RwProjectDefinitionV2Test class >> _sampleSymbolDictionaryName2 [

	^ #'RowanSample9_2'
]

{ #category : 'private' }
RwProjectDefinitionV2Test class >> _sampleSymbolDictionaryName3 [

	^ #'RowanSample9_3'
]

{ #category : 'private' }
RwProjectDefinitionV2Test class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames
		,
			{(self _sampleSymbolDictionaryName1).
			(self _sampleSymbolDictionaryName2).
			(self _sampleSymbolDictionaryName3)}
]

{ #category : 'private' }
RwProjectDefinitionV2Test >> _sampleSymbolDictionaryName1 [

	^ self class _sampleSymbolDictionaryName1
]

{ #category : 'private' }
RwProjectDefinitionV2Test >> _sampleSymbolDictionaryName2 [

	^ self class _sampleSymbolDictionaryName2
]

{ #category : 'private' }
RwProjectDefinitionV2Test >> _sampleSymbolDictionaryName3 [

	^ self class _sampleSymbolDictionaryName3
]

{ #category : 'private' }
RwProjectDefinitionV2Test >> _specListProjectNames [
	^ {'RowanSample9'.
	'RowanSample10'.
	'RowanSample11'}
]

{ #category : 'running' }
RwProjectDefinitionV2Test >> setUp [
	super setUp.
	self _specListProjectNames
		do: [ :name | Rowan projectNamed: name ifPresent: [ :project | project unload ] ]
]

{ #category : 'tests' }
RwProjectDefinitionV2Test >> testSpecList_1 [
	self _specListProjectNames
		do: [ :name | Rowan projectNamed: name ifPresent: [ :project | project unload ] ]
]
