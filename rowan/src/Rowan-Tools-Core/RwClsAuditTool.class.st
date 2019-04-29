"
This class audits individual classes
"
Class {
	#name : 'RwClsAuditTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'audit' }
RwClsAuditTool >> _asClassPrefix: aBoolean [

	^aBoolean ifTrue: ['class' ] ifFalse: ['']

]

{ #category : 'audit' }
RwClsAuditTool >> _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	| aPackage |
		"must be an extension. Do basic checks"
	aPackage := (category copyFrom: 2 to: category size).

	^category first == $*	ifTrue: ["must be an extension category. See if package exists"
		(aPackage asLowercase isEquivalent: aBehavior rowanPackageName asLowercase) 
			ifTrue: [{aLoadedClass name , ' #' , category asString -> 'Extension category name can not be same as class package' }]
			ifFalse: [
				(Rowan image loadedClassExtensionsNamed: aBehavior thisClass name 
					ifFound: [:extensions | extensions detect: [:e | e loadedPackage name asLowercase isEquivalent: aPackage] ifNone: [nil]] 
					ifAbsent: [nil]) isNil 
						ifTrue: [ | res | 
									res := self _auditCategory: category selectors: (aBehavior selectorsIn: category)  forBehavior: aBehavior loadedClass: aLoadedClass		.
									"res add: (aLoadedClass name , ' #' , category asString -> 'Class Extension is not present in the package '); 
									yourself"
						] 
						ifFalse: [{}"no basic extension problems found, class extension will be audited separately"]
			]
	] ifFalse: [
		self _auditCategory: category selectors: (aBehavior selectorsIn: category)  forBehavior: aBehavior loadedClass: aLoadedClass		
	]

]

{ #category : 'audit' }
RwClsAuditTool >> _auditCategory: category selectors: aSelectorSet forBehavior: aBehavior loadedClass: aLoadedClass [
	|  res |
	
		res := res := self _result.

		aSelectorSet do: [:aSelector |
				(
					aBehavior isMeta
						ifTrue: [	self  _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass ]
						ifFalse: [ self  _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass]	
				)  ifNotNil: [:aRes | self errorLog: res add: aRes]
				
		].

		^res

]

{ #category : 'audit' }
RwClsAuditTool >> _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit a selector. verify compiled method matches loaded method reference return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  true)
			ifNil: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded class method '.]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle
						ifTrue: [
							| expected actual |
							((expected := aLoadedMethod propertyAt: 'protocol') equalsNoCase: (actual := aBehavior categoryOfSelector:  aSelector ) ) 
								ifTrue: [nil]
								ifFalse: [aLoadedClass name , '#', (aLoadedMethod propertyAt: 'protocol') -> ('Missing class method category for loaded class (expected: ', expected printString, ' actual: ', actual printString, ')') ]
						] 
						ifFalse: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Compiled class method is not identical to loaded class method '] 
			]
]

{ #category : 'audit' }
RwClsAuditTool >> _auditLoadedClassMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded class method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^( aClassOrMeta compiledMethodAt: aLoadedMethod name  otherwise: nil) 
				ifNil: [(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled class method. ' ]
				ifNotNil: [:aMethod | aMethod == aLoadedMethod handle 
						ifTrue: [((aLoadedMethod propertyAt: 'protocol') equalsNoCase: (aClassOrMeta categoryOfSelector: aMethod selector) ) 
							ifTrue: [nil]
							ifFalse: [aLoadedClassOrExtension name , '#', (aLoadedMethod properties at: 'protocol') -> 'Missing class method category for loaded class']
						]
						ifFalse: [(aClassOrMeta name ,  ' >> ', aLoadedMethod name) -> 'Compiled class method is not identical to loaded class method. ']		
			]

]

{ #category : 'audit' }
RwClsAuditTool >> _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior [
"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| res  aDict |
	res :=  self _result.
	((aLoadedClass propertyAt: 'superclass') isEquivalent: aBehavior superclass name ) 
		ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Superclass is different from loaded class'].
	((aLoadedClass propertyAt: 'instvars') = (aBehavior instVarNames collect: [:e | e asString]) ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'instVarNames changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'classvars') = ((aBehavior.classVars ifNil: [SymbolDictionary new]) 
			keys collect: [:e | e asString]) asSortedCollection asArray) ifFalse: [
				self errorLog: res  add: aLoadedClass name -> 'ClassVars changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'pools') = ((aBehavior.poolDictionaries ifNil: [Array new]) collect: [:e | e asString]) ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'PoolDictionaries changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'comment' ifAbsent: ['']) isEquivalent: aBehavior rwComment ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Comment has changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'category') = aBehavior category ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Class category has changed in compiled class v loaded class'].
	(aDict := System myUserProfile resolveSymbol: (aLoadedClass propertyAt: 'gs_SymbolDictionary') asSymbol ) 
			ifNil: [self errorLog: res  add: aLoadedClass name -> ('Unable to find SymbolDictionary ' ,(aLoadedClass propertyAt: 'gs_SymbolDictionary'))] 
			ifNotNil: [:smbd | smbd value at: aLoadedClass name asSymbol 
					ifAbsent: [self errorLog: res  add: aLoadedClass name -> 'Compiled class not found in symbol dictionary of loaded class']] .

	^res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded instance method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^( aClassOrMeta compiledMethodAt: aLoadedMethod name  otherwise: nil) 
				ifNil: [(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled instance method. ' ]
				ifNotNil: [:aMethod | aMethod == aLoadedMethod handle 
						ifTrue: [((aLoadedMethod propertyAt: 'protocol') equalsNoCase: (aClassOrMeta categoryOfSelector: aMethod selector) ) 
							ifTrue: [nil]
							ifFalse: [aLoadedClassOrExtension name , '#', (aLoadedMethod propertyAt: 'protocol') -> 'Missing method category for loaded class. ']
						]
						ifFalse: [(aClassOrMeta name ,  ' >> ', aLoadedMethod name) -> 'Compiled instance method is not identical to loaded instance method. ']		
			]

]

{ #category : 'audit' }
RwClsAuditTool >> _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit an instance selector. return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  aBehavior isMeta)
			ifNil: [
				| notification |
				notification := (RwAuditMethodErrorNotification
					method: aSelector 
					isMeta: aBehavior isMeta 
					inClassNamed: aBehavior theNonMetaClass name
					isClassExtension: aLoadedClass isLoadedClassExtension
					intoPackageNamed: aLoadedClass packageName )
						description: 'Missing loaded instance method. ';
						yourself.
				(notification signal) 
					ifTrue: [ (aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded instance method. ' ]
					ifFalse: [ 
						"don't record audit error"
						nil ]]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle
						ifTrue: [
							| expected actual |
							((expected := aLoadedMethod propertyAt: 'protocol') equalsNoCase: (actual := aBehavior categoryOfSelector:   aSelector ) ) 
								ifTrue: [nil]
								ifFalse: [aLoadedClass name , '#', (aLoadedMethod propertyAt: 'protocol') -> ('Missing instance method category for loaded class (expected: ', expected printString, ' actual: ', actual printString, ')') ]
						] 
						ifFalse: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Compiled instance method is not identical to loaded method. ']
			]
]

{ #category : 'audit' }
RwClsAuditTool >> _result [

	^Array new.

]

{ #category : 'audit' }
RwClsAuditTool >> auditLoadedClass: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res  |

	res := self _result.
	(Rowan globalNamed: aLoadedClass name)  
		ifNil: [self errorLog: res add: aLoadedClass name -> 'Missing gemstone class for loaded class ' ] "there is no matching Class for LoadedClass"
		ifNotNil: [:aBehavior | 
			"audit class properties"
			self errorLog: res addAll:  (self _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior).
			"audit categories"

			aBehavior categorysDo: [:category :selectors | 
				self errorLog: res addAll: (self  _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass)
			].
			aBehavior class categorysDo: [:category :selectors | 
				self errorLog: res  addAll: (self  _auditCategory: category forBehavior: aBehavior class loadedClass: aLoadedClass)
			].
		"audit loaded class methods"
		aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:a | self errorLog: res add: a]]]

			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass)  ifNotNil: [:a | self errorLog: res add: a]]
			]
	].
	^res
]

{ #category : 'audit' }
RwClsAuditTool >> errorLog: aResult add: aMessage [	
"add error to results. print to file"
	aResult add: aMessage.
	GsFile gciLogServer: aMessage value asString,'  ', aMessage key asString.
	Notification signal: aMessage value asString,'  ', aMessage key asString.

]

{ #category : 'audit' }
RwClsAuditTool >> errorLog: aResult addAll: aCol [	
"add all messages to result"
	aCol do: [:e | self errorLog: aResult add: e].
]
