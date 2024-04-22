Class {
	#name : 'RwGsTraitPatchV2',
	#superclass : 'RwGsPatchV2',
	#instVars : [
		'traitDefinition'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsTraitPatchV2 class >> for: aTraitDefinition inPackage: aPackageDefinition [

	^(self new)
		traitDefinition: aTraitDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'actions' }
RwGsTraitPatchV2 >> addToNewTraitsByNameSymbolList: newTraitsByNameSymbolList [
	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name symDict symDictName |
	name := traitDefinition key asSymbol.
	name ifNil: [ self error: 'Trait definition with no name.' ].
	symDictName := self symbolDictionaryName.
	symDictName = name
		ifTrue: [ 
			self
				error:
					'A trait (' , name asString , ') may not be installed in a symbol dictionary ('
						, symDictName asString , '  with the same name.' ].
	symDict := RwGsPatchSet_V2
		lookupSymbolDictName: symDictName
		in: newTraitsByNameSymbolList
		ifAbsent: [ 
			self
				error:
					'SymbolDictionary named ' , symDictName printString
						, ' not found in new traits symbol list' ].
	(symDict includesKey: name)
		ifTrue: [ 
			self
				error:
					'Duplicate new trait name ' , name printString , ' in symbol dictionary '
						, symDictName printString ,
											(RwRepositoryComponentProjectReaderVisitor
												lineNumberStringForDefinition: traitDefinition)].
	symDict at: name put: self
]

{ #category : 'actions' }
RwGsTraitPatchV2 >> basicCreateTrait [
	^ (Globals at: #'Trait')
		name: traitDefinition name
		instVars: traitDefinition instVarNames
		classVars: traitDefinition classVarNames
]

{ #category : 'actions' }
RwGsTraitPatchV2 >> createTraitFor: aPatchSet inSymDict: symDictName [
	| createdTrait |
	createdTrait := self basicCreateTrait.
	aPatchSet createdTrait: createdTrait inSymDict: symDictName.
	^ createdTrait
]

{ #category : 'Formatting' }
RwGsTraitPatchV2 >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '(', traitDefinition name, ')'
]

{ #category : 'accessing' }
RwGsTraitPatchV2 >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwGsTraitPatchV2 >> traitDefinition: newValue [
	traitDefinition := newValue .
]

{ #category : 'accessing' }
RwGsTraitPatchV2 >> traitName [

	^traitDefinition key
]
