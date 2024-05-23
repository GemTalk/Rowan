Class {
	#name : 'RwGsTraitMethodSourceSymbolDictPatchV2',
	#superclass : 'RwGsMethodSourceSymbolDictPatchV2',
	#instVars : [
		'traitDefinition',
		'traitInstance'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'accessing' }
RwGsTraitMethodSourceSymbolDictPatchV2 >> classTrait [
	^ self traitInstance classTrait
]

{ #category : 'accessing' }
RwGsTraitMethodSourceSymbolDictPatchV2 >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwGsTraitMethodSourceSymbolDictPatchV2 >> traitDefinition: object [
	traitDefinition := object
]

{ #category : 'accessing' }
RwGsTraitMethodSourceSymbolDictPatchV2 >> traitInstance [
	^traitInstance
]

{ #category : 'accessing' }
RwGsTraitMethodSourceSymbolDictPatchV2 >> traitInstance: object [
	traitInstance := object
]

{ #category : 'accessing' }
RwGsTraitMethodSourceSymbolDictPatchV2 >> traitName [

	^ self traitDefinition key
]
