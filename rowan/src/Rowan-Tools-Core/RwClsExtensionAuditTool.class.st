Class {
	#name : 'RwClsExtensionAuditTool',
	#superclass : 'RwClsAuditTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsExtensionAuditTool >> _add: anAssoc to: aDict for: aName [

	"aDict at: aName ifAbsentPut: [aColl]."
	
	aDict add: anAssoc

]

{ #category : 'other' }
RwClsExtensionAuditTool >> _addAll: aColl to: aDict for: aName [

	"aDict at: aName ifAbsentPut: [aColl]."
	
aDict addAll: aColl

]

{ #category : 'other' }
RwClsExtensionAuditTool >> _auditCategory: extensionCategoryName forBehavior: aBehavior loadedClass: aLoadedClassExtension [
| selectors  res |
			res := Array new.
			(aBehavior includesCategory: extensionCategoryName) ifTrue: [
				selectors := aBehavior selectorsIn: extensionCategoryName.
				selectors isEmpty ifTrue: ["expected some methods"
					selectors do: [:aSelector |
							(aLoadedClassExtension includesSelector: aSelector isMeta: false) 
								ifFalse: [self _add: aSelector -> 'Missing loaded extension method' ->  aSelector  to: res for: aLoadedClassExtension name]
					]
				]
			] ifFalse: [
					(aBehavior class includesCategory: extensionCategoryName) ifTrue: [
						selectors := aBehavior class selectorsIn: aLoadedClassExtension loadedPackage asExtensionName.
							( selectors reject: [:e | aLoadedClassExtension includesSelector: e isMeta: true]) do: [:aSelector |
									self _add: aSelector -> 'Missing loaded extensions classmethod: '   to: res for: aLoadedClassExtension name.
					]						
				] ifFalse: [
					self _add: extensionCategoryName -> 'Missing extension category' to: res for: aLoadedClassExtension name.
				]
			].
			
			^res

]

{ #category : 'other' }
RwClsExtensionAuditTool >> _auditLoadedClassMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name) 
				ifNil: [aLoadedMethod -> 'Missing compiled method: ' ]
				ifNotNil: [:aGsMethod |
					aGsMethod == (aLoadedClassOrExtension loadedClassMethods at: aLoadedMethod name) handle 
						ifFalse: [aLoadedMethod name -> 'compiled class Method is not identical to loaded class Method']
		]

]

{ #category : 'other' }
RwClsExtensionAuditTool >> _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name) 
				ifNil: [aLoadedMethod -> 'Missing compiled method: ' ]
				ifNotNil: [:aGsMethod |
					aGsMethod == (aLoadedClassOrExtension loadedInstanceMethods at: aLoadedMethod name) handle 
						ifFalse: [aLoadedMethod name -> 'compiled class Method is not identical to loaded class Method']
		]

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
