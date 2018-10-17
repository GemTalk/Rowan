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
	
	^self _auditCategory: category selectors: (aBehavior selectorsIn: category)  forBehavior: aBehavior loadedClass: aLoadedClass
]

{ #category : 'other' }
RwClsAuditTool >> _auditCategory: category selectors: aSelectorSet forBehavior: aBehavior loadedClass: aLoadedClass [
	|  res |
	
		res := Array new.

		aSelectorSet do: [:aSelector |
				(
					aBehavior isMeta
						ifTrue: [	self  _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass ]
						ifFalse: [ self  _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass]	
				)  ifNotNil: [:aRes | res add: aRes]
				
		].

		^res
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
				isNil ifTrue: [aLoadedMethod -> 'Missing compiled classmethod: ' ]
]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior [
"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| res  aDict |
	res := Array new.
	((aLoadedClass propertyAt: 'superclass') isEquivalent: aBehavior superclass name ) ifFalse: [res add: aLoadedClass name -> 'Superclass is different from loaded class'].
	((aLoadedClass propertyAt: 'instvars') = (aBehavior instVarNames collect: [:e | e asString]) ) ifFalse: [res add: aLoadedClass name -> 'instVarNames changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'classvars') = ((aBehavior.classVars ifNil: [SymbolDictionary new]) keys collect: [:e | e asString])  asArray) ifFalse: [res add: aLoadedClass name -> 'ClassVars changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'pools') = ((aBehavior.poolDictionaries ifNil: [Array new]) collect: [:e | e asString]) ) ifFalse: [res add: aLoadedClass name -> 'PoolDictionaries changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'comment' ifAbsent: ['']) isEquivalent: aBehavior comment ) ifFalse: [res add: aLoadedClass name -> 'Comment has changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'category') = aBehavior category ) ifFalse: [res add: aLoadedClass name -> 'Class category has changed in compiled class v loaded class'].
	(aDict := System myUserProfile resolveSymbol: (aLoadedClass propertyAt: 'gs_SymbolDictionary') asSymbol ) 
			ifNil: [res add: 'Unable to find SymbolDictionary for LoadedClass'] 
			ifNotNil: [:smbd | smbd value at: aLoadedClass name asSymbol ifAbsent: [res add: aLoadedClass name -> 'Compiled class not found in symbol dictionary of loaded class']] .

^res
]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded instance method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name) 
				isNil ifTrue: [aLoadedMethod name -> 'Missing compiled instance method ' ]
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
			"audit class properties"
			res addAll: (self _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior).
			"audit categories"

			aBehavior categorysDo: [:category :selectors | 
				category first == $* ifFalse: [
					res addAll: (self  _auditCategory: category selectors: selectors forBehavior: aBehavior loadedClass: aLoadedClass)]
			].
			aBehavior class categorysDo: [:category :selectors | 
				category first == $* ifFalse: [
					res addAll: (self  _auditCategory: category selectors: selectors forBehavior: aBehavior class loadedClass: aLoadedClass)
				]
			].
		"audit loaded class methods"
		aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name) 
					ifNil: [(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:a | res add: a]]]

			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name) 
					ifNil: [(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass)  ifNotNil: [:a | res add: a]]
			]
	].
	^res
]
