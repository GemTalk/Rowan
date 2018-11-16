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
				ifTrue: [ self errorLog: res  add: ((anExtentionCategory asString -> 'expected class methods in the category are missing'))   ].
	] 	ifFalse: [
			(aLoadedClassExtension loadedInstanceMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [ self errorLog: res  add: (anExtentionCategory asString -> ('expected instances methods in the category are missing'))   ].
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
		ifNil: [self errorLog: res  add: aLoadedClassExtension name -> 'Matching compiled class does not exists '] 
		ifNotNil: [:aBehavior | 
			
			((aBehavior includesCategory: extensionCategoryName) or: [(aBehavior class includesCategory: extensionCategoryName)]) 
				ifFalse: [self errorLog: res add: extensionCategoryName -> 'Missing extension category' ].
			
			(aBehavior includesCategory: extensionCategoryName)
				ifTrue: [self errorLog: res  addAll: 	
					(self _auditCategory: extensionCategoryName forBehavior: aBehavior loadedClass: aLoadedClassExtension)].
			(aBehavior class includesCategory: extensionCategoryName)
				ifTrue: [self errorLog: res  addAll: 
					(self _auditCategory: extensionCategoryName forBehavior: aBehavior class loadedClass: aLoadedClassExtension)].

			
			aLoadedClassExtension 
				loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
					(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:x | res add: x]
				] 
				loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
					(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass) ifNotNil: [:x | res add: x]
				]
		].
		^res

]
