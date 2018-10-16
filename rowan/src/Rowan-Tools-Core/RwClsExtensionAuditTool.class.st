Class {
	#name : 'RwClsExtensionAuditTool',
	#superclass : 'RwClsAuditTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsExtensionAuditTool >> _auditCategory: anExtentionCategory forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension [
"if we have loaded methods but no compiled methods add error to result"
| res |
	res := super _auditCategory: anExtentionCategory forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension
	aClassOrMeta isMeta 
		ifTrue: [
			(aLoadedClassExtension loadedClassMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [ res add: ('expected class methods in the category ', anExtentionCategory printString, ' they are missing') -> aLoadedClassExtension name  ].
	] 	ifFalse: [
			(aLoadedClassExtension loadedMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [ res add: ('expected instances methods in the category ', anExtentionCategory printString, ' they are missing') -> aLoadedClassExtension name  ].
	].
	^res
]

{ #category : 'other' }
RwClsExtensionAuditTool >> auditLoadedClassExtenstion: aLoadedClassExtension [
"look for methods compiled into class without Rowan API"
| res   extensionCategoryName|

	res := Array new.
	extensionCategoryName := aLoadedClassExtension loadedPackage asExtensionName.
	(Rowan globalNamed: aLoadedClassExtension name) 
		ifNil: [res add: aLoadedClassExtension name -> 'Compiled class does not exists '] 
		ifNotNil: [:aBehavior | 
			
			res addAll:  ((aBehavior includesCategory: extensionCategoryName)
				ifTrue: [(self _auditCategory: extensionCategoryName forBehavior: aBehavior loadedClass: aLoadedClassExtension)]
				ifFalse: [
					(aBehavior class includesCategory: extensionCategoryName)
						ifTrue: [(self _auditCategory: extensionCategoryName forBehavior: aBehavior class loadedClass: aLoadedClassExtension)]
						ifFalse: [ { extensionCategoryName -> 'Missing extension category'} ]
			]).

			
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
