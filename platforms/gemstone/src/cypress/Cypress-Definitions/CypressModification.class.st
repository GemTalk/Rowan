"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressModification',
	#superclass : 'CypressPatchOperation',
	#instVars : [
		'modification',
		'obsoletion'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressModification class >> of: base to: target [
	^ self new base: base target: target

]

{ #category : 'initialization' }
CypressModification >> = aPatchOperation [
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]

]

{ #category : 'applying' }
CypressModification >> applyTo: aCypressLoader [

	aCypressLoader applyModification: self

]

{ #category : 'initialization' }
CypressModification >> base: base target: target [

	obsoletion := base.
	modification := target.

]

{ #category : 'accessing' }
CypressModification >> definition [
  "answer the primary definition associated with the operation"

  ^ self modification

]

{ #category : 'accessing' }
CypressModification >> description [
    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString

]

{ #category : 'comparing' }
CypressModification >> hash [
  ^ (super hash bitXor: modification hash) bitXor: obsoletion hash

]

{ #category : 'loading' }
CypressModification >> loadClassDefinition: aDefaultSymbolDictionaryName [
  self modification loadClassDefinition: aDefaultSymbolDictionaryName

]

{ #category : 'loading' }
CypressModification >> loadMethodDefinition [

	self modification loadMethodDefinition.

]

{ #category : 'accessing' }
CypressModification >> modification [

	^modification

]

{ #category : 'accessing' }
CypressModification >> obsoletion [

	^obsoletion

]

{ #category : 'loading' }
CypressModification >> postLoadDefinition [
	self modification postLoadOver: self obsoletion

]

{ #category : 'dependency' }
CypressModification >> provisions [
	"Answer list of global names defined by this definition"

	^self modification provisions

]

{ #category : 'dependency' }
CypressModification >> requirements [
	"Answer list of global names required by this definition"

	^self modification requirements

]
