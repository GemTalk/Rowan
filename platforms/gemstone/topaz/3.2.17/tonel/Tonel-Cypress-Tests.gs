! Package: Tonel-Cypress-Tests


! Remove existing behavior from package Tonel-Cypress-Tests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Tonel-Cypress-Tests'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

doit
(TonelParserTest
	subclass: 'TonelParserForCypressTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Cypress-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelWriterTest
	subclass: 'TonelWriterForCypressTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Cypress-Tests';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for TonelParserForCypressTest

! ------------------- Instance methods for TonelParserForCypressTest

category: 'private'
method: TonelParserForCypressTest
newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
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
%

category: 'private'
method: TonelParserForCypressTest
newClassDefinitionFrom: anArray
  ^ TonelCypressReader newClassDefinitionFrom: anArray
%

category: 'private'
method: TonelParserForCypressTest
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
  ^ TonelCypressReader
    newMethodDefinitionForClassNamed: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source
%

category: 'private'
method: TonelParserForCypressTest
newTraitDefinitionFrom: anArray
  ^ TonelCypressReader newTraitDefinitionFrom: anArray
%

category: 'private'
method: TonelParserForCypressTest
newTypeDefinitionFrom: anArray
  ^ TonelCypressReader newTypeDefinitionFrom: anArray
%

! Class Implementation for TonelWriterForCypressTest

! ------------------- Instance methods for TonelWriterForCypressTest

category: 'private'
method: TonelWriterForCypressTest
creatClassDefinition: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
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
%

category: 'private'
method: TonelWriterForCypressTest
creatClassDefinition: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
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
%

category: 'private'
method: TonelWriterForCypressTest
creatMethodDefinition: classString classIsMeta: metaBoolean selector: selectorString category: catString source: sourceString
  ^ CypressMethodDefinition
    className: classString
    classIsMeta: metaBoolean
    selector: selectorString
    category: catString
    source: sourceString


%

category: 'private'
method: TonelWriterForCypressTest
defaultPackageWriter
  ^ TonelCypressWriter new
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: Tonel-Cypress-Tests


