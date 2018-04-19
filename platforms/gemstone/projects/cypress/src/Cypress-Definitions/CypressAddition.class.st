"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAddition',
	#superclass : 'CypressPatchOperation',
	#instVars : [
		'definition'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressAddition class >> of: aDefinition [
	^ self new definition: aDefinition

]

{ #category : 'comparing' }
CypressAddition >> = aPatchOperation [
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]

]

{ #category : 'applying' }
CypressAddition >> applyTo: aCypressLoader [

	aCypressLoader applyAddition: self

]

{ #category : 'accessing' }
CypressAddition >> definition [

	^definition

]

{ #category : 'initialization' }
CypressAddition >> definition: aDefinition [

	definition := aDefinition

]

{ #category : 'accessing' }
CypressAddition >> description [
    ^ 'add: ' , self definition printString

]

{ #category : 'comparing' }
CypressAddition >> hash [
  ^ super hash bitXor: definition hash

]

{ #category : 'loading' }
CypressAddition >> loadClassDefinition: aDefaultSymbolDictionaryName [
  self definition loadClassDefinition: aDefaultSymbolDictionaryName

]

{ #category : 'loading' }
CypressAddition >> loadMethodDefinition [
  self definition loadMethodDefinition

]

{ #category : 'loading' }
CypressAddition >> postLoadDefinition [
	self definition postLoadOver: nil

]

{ #category : 'dependency' }
CypressAddition >> provisions [
	"Answer list of global names defined by this definition"

	^self definition provisions

]

{ #category : 'dependency' }
CypressAddition >> requirements [
	"Answer list of global names required by this definition"

	^self definition requirements

]
