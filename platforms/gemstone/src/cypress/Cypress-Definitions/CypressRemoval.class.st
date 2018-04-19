"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressRemoval',
	#superclass : 'CypressPatchOperation',
	#instVars : [
		'definition'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressRemoval class >> of: aDefinition [
	^ self new definition: aDefinition

]

{ #category : 'comparing' }
CypressRemoval >> = aPatchOperation [
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]

]

{ #category : 'applying' }
CypressRemoval >> applyTo: aCypressLoader [

	aCypressLoader applyRemoval: self

]

{ #category : 'accessing' }
CypressRemoval >> definition [

	^definition

]

{ #category : 'initialization' }
CypressRemoval >> definition: aDefinition [

	definition := aDefinition

]

{ #category : 'accessing' }
CypressRemoval >> description [

	^'remove: ', self definition printString

]

{ #category : 'comparing' }
CypressRemoval >> hash [
  ^ super hash bitXor: definition hash

]

{ #category : 'loading' }
CypressRemoval >> loadClassDefinition: aDefaultSymbolDictionaryName [
  CypressError
    signal: 'inappropriate to send #loadClassDefinition to a removal operation'

]

{ #category : 'loading' }
CypressRemoval >> loadMethodDefinition [
	
	CypressError signal: 'inappropriate to send #loadMethodDefinition to a removal operation'

]

{ #category : 'loading' }
CypressRemoval >> postLoadDefinition [
	
	CypressError signal: 'inappropriate to send #postLoadDefinition to a removal operation'

]

{ #category : 'dependency' }
CypressRemoval >> provisions [
	"Answer list of global names defined by this definition"

	^#()

]

{ #category : 'dependency' }
CypressRemoval >> requirements [
	"Answer list of global names required by this definition"

	^#()

]

{ #category : 'loading' }
CypressRemoval >> unloadDefinition [

	self definition unloadDefinition.

]
