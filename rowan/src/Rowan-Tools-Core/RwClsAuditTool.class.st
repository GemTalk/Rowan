"
This class audits individual classes
"
Class {
	#name : 'RwClsAuditTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'audit' }
RwClsAuditTool >> _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	| aPackage |
		"must be an extension. Do basic checks"
	aPackage := (category copyFrom: 2 to: category size).

	^category first == $*	ifTrue: ["must be an extension category. See if package exists"
		(aPackage asLowercase isEquivalent: aBehavior rowanPackageName asLowercase) 
			ifTrue: [{RwAuditDetail for: aLoadedClass message: 'Extension category name <', category, '>  must not match class package name.' 
							"aLoadedClass name , ' #' , category asString -> 'Extension category name can not be same as class package' "}]
			ifFalse: [
				(Rowan image loadedClassExtensionsNamed: aBehavior thisClass name 
					ifFound: [:extensions | extensions detect: [:e | e loadedPackage name asLowercase isEquivalent: aPackage] ifNone: [nil]] 
					ifAbsent: [nil]) isNil 
						ifTrue: [ | res | 
									res := self _auditCategory: category selectors: (aBehavior selectorsIn: category)  forBehavior: aBehavior loadedClass: aLoadedClass		.
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
				( self  _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass)  
					ifNotNil: [:aRes | res add: aRes]
				
		].

		^res


]

{ #category : 'audit' }
RwClsAuditTool >> _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"audit a selector. verify compiled method matches loaded method reference return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  true)
			ifNil: [
				| notification |
				notification := (RwAuditMethodErrorNotification
					method: aSelector 
					isMeta: true
					inClassNamed: aBehavior theNonMetaClass name
					isClassExtension: aLoadedClass isLoadedClassExtension
					intoPackageNamed: aLoadedClass packageName )
						description: 'Missing loaded instance method. ';
						yourself.
				(notification signal) 
					ifTrue: [ (aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded class method. ' ]
					ifFalse: [ 
						"don't record audit error"
						nil ] ]
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
RwClsAuditTool >> _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior [
"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| res  aDict superclassName |
	res :=  self _result.
	superclassName := aBehavior superclass ifNil: [ 'nil' ] ifNotNil: [:superCls | superCls name ].
	(aLoadedClass classSuperclass isEquivalent: superclassName ) 
		ifFalse: [res  add: (RwAuditDetail for: aLoadedClass message: 'Superclass is different from loaded class')].
	(aLoadedClass classInstVarNames = (aBehavior instVarNames collect: [:e | e asString]) ) 
			ifFalse: [res  add: (RwAuditDetail for: aLoadedClass message: 'instVarNames changed in compiled class v loaded class')].
	(aLoadedClass classClassVarNames = ((aBehavior.classVars ifNil: [SymbolDictionary new]) 
			keys collect: [:e | e asString]) asSortedCollection asArray) ifFalse: [
				res  add: (RwAuditDetail for: aLoadedClass message: 'ClassVars changed in compiled class v loaded class')].
	(aLoadedClass classPoolDictionaries = ((aBehavior.poolDictionaries ifNil: [Array new]) collect: [:e | e asString]) ) 
			ifFalse: [ res  add: (RwAuditDetail for: aLoadedClass message: 'PoolDictionaries changed in compiled class v loaded class')].
	(aLoadedClass classComment isEquivalent: aBehavior rwComment ) 
			ifFalse: [res  add: (RwAuditDetail for: aLoadedClass message: 'Comment has changed in compiled class v loaded class')].
	(aLoadedClass classCategory = (aBehavior _classCategory ifNil: ['']) ) 
			ifFalse: [res  add: (RwAuditDetail for: aLoadedClass message: 'Class category has changed in compiled class v loaded class')].
	(aDict := System myUserProfile resolveSymbol: aLoadedClass classSymbolDictionaryName asSymbol ) 
			ifNil: [res  add: (RwAuditDetail for: aLoadedClass message: ('Unable to find SymbolDictionary ' , aLoadedClass classSymbolDictionaryName))] 
			ifNotNil: [:smbd | smbd value at: aLoadedClass name asSymbol 
					ifAbsent: [res  add: aLoadedClass name -> 'Compiled class not found in symbol dictionary of loaded class']] .

	^res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditLoadedMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
"verify that compiled method is present for each loaded class method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^( aClassOrMeta compiledMethodAt: aLoadedMethod name  otherwise: nil) 
				ifNil: [RwAuditDetail for: aLoadedClassOrExtension message: 'Missing compiled method>>', aLoadedMethod name 
							"(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled class method. ' "]
				ifNotNil: [:aMethod | aMethod == aLoadedMethod handle 
						ifTrue: [((aLoadedMethod methodCategory) equalsNoCase: (aClassOrMeta categoryOfSelector: aMethod selector) ) 
							ifTrue: [nil]
							ifFalse: [ RwAuditDetail for: aClassOrMeta  message: 'Missing method category: ', aLoadedMethod methodCategory, ' for loaded class' ] ]
						ifFalse: [RwAuditDetail for: aLoadedClassOrExtension 
										message:  'Compiled method is not identical to loaded class method>>' , aLoadedMethod name]	]
]

{ #category : 'audit' }
RwClsAuditTool >> _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
"#rentamed from _auidtClassSelector since functionality is same for instanance and class
 verify compiled method matches loaded method reference return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  aBehavior isMeta)
			ifNil: [
				| notification |
				notification := (RwAuditMethodErrorNotification
					method: aSelector 
					isMeta: aBehavior isMeta
					inClassNamed: aBehavior theNonMetaClass name
					isClassExtension: aLoadedClass isLoadedClassExtension
					intoPackageNamed: aLoadedClass packageName )
						description: 'Missing loaded method. ';
						yourself.
				(notification signal) 
					ifTrue: [ self halt. RwAuditDetail for: aBehavior message: 'Missing loaded method>>' , aSelector asString ]
					ifFalse: [ 
						"don't record audit error"
						nil ]  ]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle
					ifTrue: [
						| expected actual |
						((expected := aLoadedMethod methodCategory) equalsNoCase: (actual := aBehavior categoryOfSelector:  aSelector ) ) 
							ifTrue: [nil]
							ifFalse: [RwAuditDetail for: aBehavior message: (
										'Mismatched method category (expected: ', expected printString, 
										' actual: ', actual printString, ')')
						]
					] 
					ifFalse: [RwAuditDetail for: aLoadedClass message:  'Compiled method is not identical to loaded method '
				] 
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
		ifNil: [res add: (RwAuditDetail for: aLoadedClass message:  'Missing gemstone class for loaded class ') "aLoadedClass name -> 'Missing gemstone class for loaded class '" ] "there is no matching Class for LoadedClass"
		ifNotNil: [:aBehavior | 

			aBehavior == aLoadedClass handle
				ifFalse: [ 
					self
						errorLog: res
						add:
							aLoadedClass name
								->
									('loaded class (' , aLoadedClass handle asOop printString
									, ') not latest version of class ('
									, aBehavior asOop printString , ') ') ].

			"audit class properties"
			res addAll:  (self _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior).
			"audit categories"

			aBehavior categorysDo: [:category :selectors | 
				res addAll: (self  _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass)
			].
			aBehavior class categorysDo: [:category :selectors | 
				res  addAll: (self  _auditCategory: category forBehavior: aBehavior class loadedClass: aLoadedClass)
			].
		"audit loaded class methods"
		aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:a | self errorLog: res add: a]]]

			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass)  ifNotNil: [:a | self errorLog: res add: a]]
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
