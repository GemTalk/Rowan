"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressDefinition',
	#superclass : 'CypressObject',
	#category : 'Cypress-Definitions'
}

{ #category : 'comparing' }
CypressDefinition >> = aDefinition [

	^(aDefinition isKindOf: CypressDefinition)
		and: [aDefinition description = self description]

]

{ #category : 'loading' }
CypressDefinition >> actualClass [

	self subclassResponsibility: #actualClass

]

{ #category : 'visiting' }
CypressDefinition >> classDefinition: classBlock methodDefinition: methodBlock [
	"default is noop"

]

{ #category : 'accessing' }
CypressDefinition >> description [
	self subclassResponsibility: #description

]

{ #category : 'accessing' }
CypressDefinition >> details [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self printDetailsOn: stream.
	^stream contents

]

{ #category : 'comparing' }
CypressDefinition >> hash [
    ^ self description hash

]

{ #category : 'testing' }
CypressDefinition >> isSameRevisionAs: aDefinition [
	^ self = aDefinition

]

{ #category : 'loading' }
CypressDefinition >> loadClassDefinition [
  self loadClassDefinition: self defaultSymbolDictionaryName

]

{ #category : 'loading' }
CypressDefinition >> loadClassDefinition: aDefaultSymbolDictionaryName [
  "default is to do nothing"

]

{ #category : 'loading' }
CypressDefinition >> loadMethodDefinition [
	"default is to do nothing"

]

{ #category : 'loading' }
CypressDefinition >> postLoad [
	"noop"

]

{ #category : 'loading' }
CypressDefinition >> postLoadOver: aDefinition [

	self postLoad

]

{ #category : 'dependency' }
CypressDefinition >> provisions [
	"Answer list of global names defined by this definition"

	^#()

]

{ #category : 'dependency' }
CypressDefinition >> requirements [
	"Answer list of global names required by this definition"

	^#()

]

{ #category : 'loading' }
CypressDefinition >> unloadDefinition [

	self subclassResponsibility: #unloadDefinition

]
