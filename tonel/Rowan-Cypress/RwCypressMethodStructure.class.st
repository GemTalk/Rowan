Class {
	#name : 'RwCypressMethodStructure',
	#superclass : 'CypressMethodStructure',
	#category : 'Rowan-Cypress'
}

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
