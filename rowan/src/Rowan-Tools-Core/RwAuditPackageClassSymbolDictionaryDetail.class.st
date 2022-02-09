"
Audit failure detail for a package and class. 

Reasons:
	#differentSymbolDictionaryForClassAndPackage
	#differentSymbolDictionaryForClassExtensionAndPackage
"
Class {
	#name : 'RwAuditPackageClassSymbolDictionaryDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'loadedClass',
		'classSymbolDictionaryName',
		'packageSymbolDictionaryName'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> classSymbolDictionaryName [
	^classSymbolDictionaryName
]

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> classSymbolDictionaryName: object [
	classSymbolDictionaryName := object
]

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> loadedClass [
	^loadedClass
]

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> loadedClass: object [
	loadedClass := object
]

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> loadedPackage [
	^ self owner
]

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> packageSymbolDictionaryName [
	^packageSymbolDictionaryName
]

{ #category : 'accessing' }
RwAuditPackageClassSymbolDictionaryDetail >> packageSymbolDictionaryName: object [
	packageSymbolDictionaryName := object
]
