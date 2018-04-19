Class {
	#name : 'TonelParserForCypressTest',
	#superclass : 'TonelParserTest',
	#category : 'Tonel-Cypress-Tests'
}

{ #category : 'private' }
TonelParserForCypressTest >> newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  ^ TonelCypressReader
    newClassDefinitionForClassNamed: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    classInstVarNames: civarArray
    type: typeSymbol
    comment: commentString

]

{ #category : 'private' }
TonelParserForCypressTest >> newClassDefinitionFrom: anArray [
  ^ TonelCypressReader newClassDefinitionFrom: anArray

]

{ #category : 'private' }
TonelParserForCypressTest >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
  ^ TonelCypressReader
    newMethodDefinitionForClassNamed: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source

]

{ #category : 'private' }
TonelParserForCypressTest >> newTraitDefinitionFrom: anArray [
  ^ TonelCypressReader newTraitDefinitionFrom: anArray

]

{ #category : 'private' }
TonelParserForCypressTest >> newTypeDefinitionFrom: anArray [
  ^ TonelCypressReader newTypeDefinitionFrom: anArray

]
