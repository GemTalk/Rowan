Class {
	#name : 'RwAuditPackageTraitSymbolDictionaryDetail',
	#superclass : 'RwAuditPackageDetail',
	#instVars : [
		'loadedTrait',
		'traitSymbolDictionaryName',
		'packageSymbolDictionaryName'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> loadedPackage [
	^ self owner
]

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> loadedTrait [
	^loadedTrait
]

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> loadedTrait: object [
	loadedTrait := object
]

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> packageSymbolDictionaryName [
	^packageSymbolDictionaryName
]

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> packageSymbolDictionaryName: object [
	packageSymbolDictionaryName := object
]

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> traitSymbolDictionaryName [
	^traitSymbolDictionaryName
]

{ #category : 'accessing' }
RwAuditPackageTraitSymbolDictionaryDetail >> traitSymbolDictionaryName: object [
	traitSymbolDictionaryName := object
]
