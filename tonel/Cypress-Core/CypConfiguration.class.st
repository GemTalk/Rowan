"
Configuration that guides Cypress operation.

Specific parameters:

packageInfoSource
--------------------------
A Symbol indicating where the authoritative repository of information about package membership of classes and methods is.
Acceptable values are platform-specific:

#Cypress -- the information is stored in Cypress-specific objects. Valid on all platforms, but recommended only when all code modifications are done through Cypress or tools that are Cypress-aware

#Category -- Valid for Pharo and GemStone. The category of a class is the name of its package, and a method's membership in a protocol whose name starts with $* indicates extension methods.

#SymbolDictionary -- Valid for GemStone. The SymbolDictionary in which the class's name is first found is the name of its package, and a method's membership in a protocol whose name starts with $* indicates extension methods.

#Store -- Valid for VW. Cypress package membership is Store package membership.
"
Class {
	#name : 'CypConfiguration',
	#superclass : 'Object',
	#instVars : [
		'packageInfoSource'
	],
	#category : 'Cypress-Core'
}

{ #category : 'testing' }
CypConfiguration >> needsPackageProtocolNames [
	"Answer true if image protocol names need to be *<packageName>*"

	^packageInfoSource ~~ #Cypress
]

{ #category : 'accessing' }
CypConfiguration >> packageInfoSource [
	^packageInfoSource

]

{ #category : 'accessing' }
CypConfiguration >> packageInfoSource: newValue [
	"Valid values for GemStone are #Cypress, #Category, #SymbolDictionary"

	packageInfoSource := newValue
]
