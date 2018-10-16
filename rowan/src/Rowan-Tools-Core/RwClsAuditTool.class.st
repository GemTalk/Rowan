Class {
	#name : 'RwClsAuditTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsAuditTool >> _asClassPrefix: aBoolean [

	^aBoolean ifTrue: ['class' ] ifFalse: ['']

]

{ #category : 'other' }
RwClsAuditTool >> _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	|  res |
	
		res := Array new.

		(aBehavior selectorsIn: category) do: [:aSelector |
				(
					aBehavior isMeta
						ifTrue: [	self  _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass ]
						ifFalse: [ self  _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass]	
				)  ifNotNil: [:aRes | res add: aRes]
				
		].

		^res

]

{ #category : 'other' }
RwClsAuditTool >> _auditClassLoadedMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [

	^(aClassOrMeta compiledMethodAt: aLoadedMethod name) 
				ifNil: [aLoadedMethod name -> 'Missing compiled classmethod ' ]

]

{ #category : 'other' }
RwClsAuditTool >> _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit a selector. verify compiled method matches loaded method reference return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  true)
			ifNil: [aSelector -> 'Missing loaded classmethod'.]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector) == aLoadedMethod handle 
						ifFalse: [aSelector -> 'Compiled classmethod is not identical to loaded method']
			]

]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedClassMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded class method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name) 
				ifNil: [aLoadedMethod -> 'Missing compiled classmethod: ' ]

]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded instance method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name) 
				ifNil: [aLoadedMethod name -> 'Missing compiled instance method ' ]

]

{ #category : 'other' }
RwClsAuditTool >> _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit an instance selector. return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  false)
			ifNil: [aSelector -> 'Missing loaded instance method'.]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector) == aLoadedMethod handle 
							ifFalse: [aSelector -> 'Compiled instance method is not identical to loaded method']
			]

]

{ #category : 'other' }
RwClsAuditTool >> auditLoadedClass: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res  |

	res := Array new.
	(Rowan globalNamed: aLoadedClass name)  
		ifNil: [res add: aLoadedClass name -> 'Class does not exists ' ] "there is no matching Class for LoadedClass"
		ifNotNil: [:aBehavior | 
						
			aBehavior categorysDo: [:category :selectors | 
				category first == $* ifFalse: [
					res addAll: (self  _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass)]
			].
			aBehavior class categorysDo: [:category :selectors | 
				category first == $* ifFalse: [
					res addAll: (self  _auditCategory: category forBehavior: aBehavior class loadedClass: aLoadedClass)
				]
			].

	aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name) 
					ifNil: [(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:a | res add: a]]]

			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name) 
					ifNil: [(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass)  ifNotNil: [:a | res add: a]]
			]
	].
	^res

]
