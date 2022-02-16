"
Audit failure detail for method. 

Reasons:
	#missingCompiledMethod
	#missingLoadedMethod
	#differentMethodCategory
	#methodsNotIdentical
"
Class {
	#name : 'RwAuditMethodDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'behavior',
		'selector',
		'loadedMethod',
		'method',
		'loadedCategory',
		'category'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditMethodDetail >> behavior [
	^behavior
]

{ #category : 'accessing' }
RwAuditMethodDetail >> behavior: object [
	behavior := object
]

{ #category : 'accessing' }
RwAuditMethodDetail >> category [
	^category
]

{ #category : 'accessing' }
RwAuditMethodDetail >> category: object [
	category := object
]

{ #category : 'accessing' }
RwAuditMethodDetail >> loadedCategory [
	^ loadedCategory ifNil: [ self loadedMethod methodCategory ]
]

{ #category : 'accessing' }
RwAuditMethodDetail >> loadedCategory: object [
	loadedCategory := object
]

{ #category : 'accessing' }
RwAuditMethodDetail >> loadedClassOrClassExtension [
	^ self owner
]

{ #category : 'accessing' }
RwAuditMethodDetail >> loadedMethod [
	^loadedMethod
]

{ #category : 'accessing' }
RwAuditMethodDetail >> loadedMethod: object [
	loadedMethod := object
]

{ #category : 'accessing' }
RwAuditMethodDetail >> method [
	^method
]

{ #category : 'accessing' }
RwAuditMethodDetail >> method: object [
	method := object
]

{ #category : 'accessing' }
RwAuditMethodDetail >> selector [
	^selector ifNil: [ self loadedMethod selector ]
]

{ #category : 'accessing' }
RwAuditMethodDetail >> selector: object [
	selector := object
]
