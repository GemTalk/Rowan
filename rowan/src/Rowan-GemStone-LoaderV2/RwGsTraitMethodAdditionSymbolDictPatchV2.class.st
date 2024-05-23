Class {
	#name : 'RwGsTraitMethodAdditionSymbolDictPatchV2',
	#superclass : 'RwGsMethodAdditionSymbolDictPatchV2',
	#instVars : [
		'traitDefinition',
		'traitInstance'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitDefinition: object [
	traitDefinition := object
]

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitInstance [
	^traitInstance
]

{ #category : 'accessing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> traitInstance: object [
	traitInstance := object
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
	self traitInstance
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

	loadedTrait := self symbolDictionaryRegistry loadedTraitForTrait: self traitInstance.
	isMeta
		ifTrue: [ 
			self traitInstance classTrait compile: sourceString.
			loadedMethod := RwGsLoadedSymbolDictTraitMethod
				loadedClassMethodForTrait: self traitInstance classTrait
				source: sourceString
				protocol: protocol ]
		ifFalse: [ 
			self traitInstance compile: sourceString.
			loadedMethod := RwGsLoadedSymbolDictTraitMethod
				loadedMethodForTrait: self traitInstance
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
