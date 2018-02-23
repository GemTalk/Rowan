Class {
	#name : 'RwCypressMethodStructure',
	#superclass : 'CypressMethodStructure',
	#category : 'Rowan-Cypress'
}

{ #category : 'converting' }
RwCypressMethodStructure >> asCypressMethodDefinition [

	"Try to coerce Unicode source to simple Strings when possible."

	^ RwCypressMethodDefinition
		className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source asString
]

{ #category : 'initialization' }
RwCypressMethodStructure >> fromRwClassMethodDefinition: methodDefinition [
  self isMetaclass: true.
  self fromRwMethodDefinition: methodDefinition
]

{ #category : 'initialization' }
RwCypressMethodStructure >> fromRwInstanceMethodDefinition: methodDefinition [
  self isMetaclass: false.
  self fromRwMethodDefinition: methodDefinition
]

{ #category : 'initialization' }
RwCypressMethodStructure >> fromRwMethodDefinition: methodDefinition [
  self selector: methodDefinition selector asString.
  self category: (methodDefinition properties at: 'protocol').
  self source: methodDefinition source
]
