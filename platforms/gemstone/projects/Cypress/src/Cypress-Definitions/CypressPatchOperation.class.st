"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPatchOperation',
	#superclass : 'CypressObject',
	#category : 'Cypress-Definitions'
}

{ #category : 'comparing' }
CypressPatchOperation >> = aPatchOperation [
	^aPatchOperation isKindOf: self class

]

{ #category : 'applying' }
CypressPatchOperation >> applyTo: aCypressLoader [

	self subclassResponsibility: #applyTo:

]

{ #category : 'accessing' }
CypressPatchOperation >> definition [
  "answer the primary definition associated with the operation"

  self subclassResponsibility: #'definition'

]

{ #category : 'accessing' }
CypressPatchOperation >> description [

	self subclassResponsibility: #description

]

{ #category : 'comparing' }
CypressPatchOperation >> hash [
    ^ self description hash

]

{ #category : 'loading' }
CypressPatchOperation >> loadClassDefinition [
  self loadClassDefinition: self defaultSymbolDictionaryName

]

{ #category : 'loading' }
CypressPatchOperation >> loadClassDefinition: aDefaultSymbolDictionaryName [
  self subclassResponsibility: #'loadClassDefinition'

]

{ #category : 'loading' }
CypressPatchOperation >> loadMethodDefinition [

	self subclassResponsibility: #loadMethodDefinition

]

{ #category : 'loading' }
CypressPatchOperation >> postLoadDefinition [

	self subclassResponsibility: #postLoadDefinition

]

{ #category : 'printing' }
CypressPatchOperation >> printDetailsOn: aStream [

	aStream nextPutAll: self description.

]

{ #category : 'dependency' }
CypressPatchOperation >> provisions [
	"Answer list of global names defined by this definition"

	self subclassResponsibility: #provisions

]

{ #category : 'dependency' }
CypressPatchOperation >> requirements [
	"Answer list of global names required by this definition"

	self subclassResponsibility: #requirements

]

{ #category : 'loading' }
CypressPatchOperation >> unloadDefinition [

	CypressError signal: 'inappropriate to send #unloadDefinition to an addition or modification operation'

]
