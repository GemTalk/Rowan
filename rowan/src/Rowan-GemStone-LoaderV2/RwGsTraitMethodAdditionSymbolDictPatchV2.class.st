Class {
	#name : 'RwGsTraitMethodAdditionSymbolDictPatchV2',
	#superclass : 'RwGsMethodAdditionSymbolDictPatchV2',
	#instVars : [
		'traitDefinition'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsTraitMethodAdditionSymbolDictPatchV2 >> installMethod [
self halt.
	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	selector := compiledMethod selector
]

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
					"cannot find trait ... caller can decide whether or not that is a problem -- behavior will be nil"
					^ self ] ].
	behavior := isMeta
		ifTrue: [ trait class ]
		ifFalse: [ trait ]
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
	behavior
		ifNil: [ 
			self
				error:
					'Trait ' , self traitName printString , ' not found in the symbol dictionary '
						, self symbolDictionaryName printString , ' associated with the method '
						, methodDefinition selector printString ].

	[ 
	| sourceString protocol symbolList |
	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: 'protocol') asSymbol.
	compiledMethod := behavior compileMethod: sourceString ]
		on: CompileError , CompileWarning
		do: [ :ex | 
			ex
				addText:
					(RwRepositoryComponentProjectReaderVisitor
						lineNumberStringForDefinition: methodDefinition).
			ex pass ]
]
