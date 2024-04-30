Class {
	#name : 'RwGsTraitMethodAdditionSymbolDictPatchV2',
	#superclass : 'RwGsMethodAdditionSymbolDictPatchV2',
	#instVars : [
		'traitDefinition',
		'traitInstance'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'private' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> primeBehaviorNewTraitsSymbolList: createdTraits andExistingTraits: tempSymbols [
	| traitName trait symDictName |
	traitDefinition key
		ifNil: [ 
			"trait is being deleted ... we're done"
			^ self ].
	traitName := traitDefinition key asSymbol.
	symDictName := self symbolDictionaryName.
	trait := (RwGsPatchSet_V2 lookupSymbolDictName: symDictName in: createdTraits)
		at: traitName
		ifAbsent: [ 
			tempSymbols
				at: traitName
				ifAbsent: [ 
					"cannot find trait ... caller can decide whether or not that is a problem -- traitInstance will be nil"
					^ self ] ].
	traitInstance :=  trait
]

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitDefinition: object [
	traitDefinition := object
]

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitName [

	^ self traitDefinition key
]

{ #category : 'compiling' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> updateTraitMethodSourceUsingNewClassesSymbolList: createdTraits andExistingClasses: tempSymbols [
	self
		primeBehaviorNewTraitsSymbolList: createdTraits
		andExistingTraits: tempSymbols.
	traitInstance
		ifNil: [ 
			self
				error:
					'Trait ' , self traitName printString , ' not found in the symbol dictionary '
						, self symbolDictionaryName printString , ' associated with the method '
						, methodDefinition selector printString ].

	[ 
	| sourceString protocol symbolList loadedTrait loadedMethod |
	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: 'protocol') asString.

	loadedTrait := self symbolDictionaryRegistry loadedTraitForTrait: traitInstance.
	isMeta
		ifTrue: [ 
			traitInstance classSide compile: sourceString.
			loadedMethod := RwGsLoadedSymbolDictTraitMethod
				loadedClassMethodForTrait: traitInstance classSide
				source: sourceString
				protocol: protocol ]
		ifFalse: [ 
			traitInstance compile: sourceString.
			loadedMethod := RwGsLoadedSymbolDictTraitMethod
				loadedMethodForTrait: traitInstance
				source: sourceString
				protocol: protocol ].
	loadedTrait addLoadedMethod: loadedMethod ]
		on: CompileError , CompileWarning
		do: [ :ex | 
			ex
				addText:
					(RwRepositoryComponentProjectReaderVisitor
						lineNumberStringForDefinition: methodDefinition).
			ex pass ]
]
