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
				)  ifNotNil: [:aRes | self errorLog: res add: aRes]
				
		].

		^res
]

{ #category : 'other' }
RwClsAuditTool >> _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit a selector. verify compiled method matches loaded method reference return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  true)
			ifNil: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded classmethod'.]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle 
						ifFalse: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Compiled classmethod is not identical to loaded class method ']
			]
]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedClassMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded class method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name otherwise: nil) 
				isNil ifTrue: [(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled classmethod: ' ]
]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior [
"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| res  aDict |
	res := Array new.
	((aLoadedClass propertyAt: 'superclass') isEquivalent: aBehavior superclass name ) 
		ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Superclass is different from loaded class'].
	((aLoadedClass propertyAt: 'instvars') = (aBehavior instVarNames collect: [:e | e asString]) ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'instVarNames changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'classvars') = ((aBehavior.classVars ifNil: [SymbolDictionary new]) 
			keys collect: [:e | e asString])  asArray) ifFalse: [
				self errorLog: res  add: aLoadedClass name -> 'ClassVars changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'pools') = ((aBehavior.poolDictionaries ifNil: [Array new]) collect: [:e | e asString]) ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'PoolDictionaries changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'comment' ifAbsent: ['']) isEquivalent: aBehavior comment ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Comment has changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'category') = aBehavior category ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Class category has changed in compiled class v loaded class'].
	(aDict := System myUserProfile resolveSymbol: (aLoadedClass propertyAt: 'gs_SymbolDictionary') asSymbol ) 
			ifNil: [self errorLog: res  add: 'Unable to find SymbolDictionary for LoadedClass'] 
			ifNotNil: [:smbd | smbd value at: aLoadedClass name asSymbol 
					ifAbsent: [self errorLog: res  add: aLoadedClass name -> 'Compiled class not found in symbol dictionary of loaded class']] .

^res
]

{ #category : 'other' }
RwClsAuditTool >> _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded instance method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^(aClassOrMeta compiledMethodAt: aLoadedMethod name  otherwise: nil) 
				isNil ifTrue: [(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled instance method ' ]
]

{ #category : 'other' }
RwClsAuditTool >> _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit an instance selector. return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  false)
			ifNil: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded instance method'.]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle 
							ifFalse: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Compiled instance method is not identical to loaded method']
			]
]

{ #category : 'other' }
RwClsAuditTool >> auditLoadedClass: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res  |

	res := StringKeyValueDictionary new.
	(Rowan globalNamed: aLoadedClass name)  
		ifNil: [self errorLog: res add: aLoadedClass name -> 'Matching class does not exists ' ] "there is no matching Class for LoadedClass"
		ifNotNil: [:aBehavior | 
			"audit class properties"
			res addAll: (self _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior).
			"audit categories"

			aBehavior categorysDo: [:category :selectors | 
				category first == $*	ifTrue: [
						((category copyFrom: 2 to: category size) isEquivalent: aBehavior rowanPackageName) 
							ifTrue: [self errorLog: res add: category asString -> 'Extension category name can be same as class category' ]
					] ifFalse: [
						self errorLog: res addAll: (self  _auditCategory: category selectors: selectors forBehavior: aBehavior loadedClass: aLoadedClass)]
			].
			aBehavior class categorysDo: [:category :selectors | 
				category first == $* 	ifTrue: [
						((category copyFrom: 2 to: category size)  isEquivalent: aBehavior rowanPackageName) 
							ifTrue: [self errorLog: res add: category asString -> 'Extension category name can be same as class category' ]
					] ifFalse: [
					self errorLog: res  addAll: (self  _auditCategory: category selectors: selectors forBehavior: aBehavior class loadedClass: aLoadedClass)
				]
			].
		"audit loaded class methods"
		aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:a | res add: a]]]

			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass)  ifNotNil: [:a | res add: a]]
			]
	].
	^res

]

{ #category : 'other' }
RwClsAuditTool >> errorLog: aResult add: aMessage [	
"add error to results. print to file"
	aResult add: aMessage.
	GsFile gciLogServer: aMessage key asString , ' -> ' ,aMessage value asString.
]

{ #category : 'other' }
RwClsAuditTool >> errorLog: aResult addAll: aCol [	
"add all messages to result"
	aCol do: [:e | self errorLog: aResult add: e].
]
