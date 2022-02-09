"
Audit failure detail for a class. 

Reasons:
	#rowanHybridExtensionCategoryMatchesClassPackage
	#rowanHybridExtensionCategoryFormatError
"
Class {
	#name : 'RwAuditMethodCategoryDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'behavior',
		'category'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditMethodCategoryDetail >> behavior [
	^behavior
]

{ #category : 'accessing' }
RwAuditMethodCategoryDetail >> behavior: object [
	behavior := object
]

{ #category : 'accessing' }
RwAuditMethodCategoryDetail >> category [
	^category
]

{ #category : 'accessing' }
RwAuditMethodCategoryDetail >> category: object [
	category := object
]

{ #category : 'accessing' }
RwAuditMethodCategoryDetail >> loadedClass [
	^ self owner
]
