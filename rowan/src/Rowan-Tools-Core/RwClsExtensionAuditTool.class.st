Class {
	#name : 'RwClsExtensionAuditTool',
	#superclass : 'RwClsAuditTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsExtensionAuditTool >> _auditCategory: anExtentionCategory forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension [
"if we have loaded methods but no compiled methods add error to result"
| res |
	res := super _auditCategory: anExtentionCategory selectors:  (aClassOrMeta selectorsIn: anExtentionCategory) forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension.
	aClassOrMeta isMeta 
		ifTrue: [
			(aLoadedClassExtension loadedClassMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [res  add: (RwAuditDetail for: aLoadedClassExtension 
								message:  'Missing expected class methods in the category ', ' #' , anExtentionCategory asString )   ].
	] 	ifFalse: [
			(aLoadedClassExtension loadedInstanceMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [ res  add: (RwAuditDetail for: aLoadedClassExtension 
								message: 'Missing expected instances methods in the category' , ' #' , anExtentionCategory asString)   ].
	].
	^res
]

{ #category : 'other' }
RwClsExtensionAuditTool >> auditLoadedClassExtension: aLoadedClassExtension [
"look for methods compiled into class without Rowan API"
| res   extensionCategoryName|

	res := self _result.
	extensionCategoryName :=  aLoadedClassExtension loadedPackage asExtensionName "'*' , aLoadedClassExtension loadedPackage name" .
	(Rowan globalNamed: aLoadedClassExtension name) 
		ifNil: [(RwAuditDetail for: aLoadedClassExtension message: ' Class does not exists for loaded class extension')] 
		ifNotNil: [:aBehavior ||categories | 
					
				aBehavior == aLoadedClassExtension handle
					ifFalse: [ 
						self
							errorLog: res
							add:
								(aLoadedClassExtension name , ' #' , extensionCategoryName)
									->
										(' loaded extension class (' , aLoadedClassExtension handle asOop printString
										, ') not latest version of class ('
										, aBehavior asOop printString , ') ') ].

			categories := (aBehavior _baseCategorys: 0)
				ifNil: [ #() ]
				ifNotNil: [:catDict | catDict keys ].
			(categories	
				detect: [:each | each equalsNoCase: extensionCategoryName ] ifNone: [ ])
					ifNotNil: [:aCategory | res  addAll:  (self _auditCategory: aCategory forBehavior: aBehavior loadedClass: aLoadedClassExtension)]
					ifNil: [aLoadedClassExtension loadedInstanceMethods notEmpty ifTrue: [
						res add: (RwAuditDetail for: aLoadedClassExtension message: 'Missing instance method extension category named ', extensionCategoryName) ] ].
			categories := (aBehavior class _baseCategorys: 0)
				ifNil: [ #() ]
				ifNotNil: [:catDict | catDict keys ].
			(categories
				detect: [:each | each equalsNoCase: extensionCategoryName ] ifNone: [ ])
					ifNotNil: [:aCategory | res  addAll:  (self _auditCategory: aCategory forBehavior: aBehavior class loadedClass: aLoadedClassExtension)]
					ifNil: [aLoadedClassExtension loadedClassMethods notEmpty ifTrue: [
						res add: (RwAuditDetail for: aLoadedClassExtension message: 'Missing class method extension category named ' ,extensionCategoryName) ] ].

			aLoadedClassExtension 
				loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
					(self _auditLoadedMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:x | res add: x]
				] 
				loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
					(self _auditLoadedMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass) ifNotNil: [:x | res add: x]
				]
		].
		^res
]
