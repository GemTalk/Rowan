Class {
	#name : 'TonelWriterForCypressTest',
	#superclass : 'TonelWriterTest',
	#category : 'Tonel-Cypress-Tests'
}

{ #category : 'private' }
TonelWriterForCypressTest >> creatClassDefinition: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  ^ CypressClassDefinition
    name: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classInstVarNames: civarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    comment: commentString
    type: typeSymbol

]

{ #category : 'private' }
TonelWriterForCypressTest >> creatClassDefinition: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  ^ CypressClassDefinition
    name: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classInstVarNames: civarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    comment: commentString
    type: typeSymbol

]

{ #category : 'private' }
TonelWriterForCypressTest >> creatMethodDefinition: classString classIsMeta: metaBoolean selector: selectorString category: catString source: sourceString [
  ^ CypressMethodDefinition
    className: classString
    classIsMeta: metaBoolean
    selector: selectorString
    category: catString
    source: sourceString



]

{ #category : 'private' }
TonelWriterForCypressTest >> defaultPackageWriter [
  ^ TonelCypressWriter new

]
