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
	| packageConvention |
	packageConvention := aLoadedClass loadedProject packageConvention.

	^ packageConvention = 'RowanHybrid'
		ifTrue: [ 
			self
				_auditRowanHybridCategory: category
				forBehavior: aBehavior
				loadedClass: aLoadedClass ]
		ifFalse: [ 
			packageConvention = 'Rowan'
				ifTrue: [ 
					self
						_auditRowanCategory: category
						forBehavior: aBehavior
						loadedClass: aLoadedClass ]
				ifFalse: [ 
					self
						error:
							'package convention ' , packageConvention printString , ' not yet supported' ] ]
]

{ #category : 'audit' }
RwClsAuditTool >> _auditCategory: category selectors: aSelectorSet forBehavior: aBehavior loadedClass: aLoadedClass [
	| res |
	res := self _result.
	aSelectorSet
		do: [ :aSelector | 
			(self _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass)
				ifNotNil: [ :aRes | res addAll: aRes ] ].
	^ res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditLoadedClassProperties: aLoadedClass forBehavior: aClass [
	"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| res aDict superclassName classProperty |
	res := self _result.
	superclassName := aClass superclass
		ifNil: [ 'nil' ]
		ifNotNil: [ :superCls | superCls name ].
	(aLoadedClass classSuperclass isEquivalent: superclassName)
		ifFalse: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message: 'Superclass is different from loaded class')
						reason: #'differentSuperclass';
						loadedPropertyValue: aLoadedClass classSuperclass;
						classPropertyValue: superclassName;
						class: aClass;
						yourself) ].
	aLoadedClass classInstVarNames
		= (aClass instVarNames collect: [ :e | e asString ])
		ifFalse: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message: 'instVarNames changed in compiled class v loaded class')
						reason: #'differentClassInstVars';
						loadedPropertyValue: aLoadedClass classInstVarNames;
						classPropertyValue: aClass instVarNames;
						class: aClass;
						yourself) ].
	aLoadedClass classClassVarNames
		=
			(classProperty := ((aClass.classVars ifNil: [ SymbolDictionary new ]) keys
				collect: [ :e | e asString ]) asSortedCollection asArray)
		ifFalse: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message: 'ClassVars changed in compiled class v loaded class')
						reason: #'differentClassVars';
						loadedPropertyValue: aLoadedClass classClassVarNames;
						classPropertyValue: classProperty;
						class: aClass;
						yourself) ].
	aLoadedClass classPoolDictionaries
		=
			(classProperty := (aClass.poolDictionaries ifNil: [ Array new ])
				collect: [ :e | e asString ])
		ifFalse: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message: 'PoolDictionaries changed in compiled class v loaded class')
						reason: #'differentPoolDictionaries';
						loadedPropertyValue: aLoadedClass classPoolDictionaries;
						classPropertyValue: classProperty;
						class: aClass;
						yourself) ].
	(aLoadedClass classComment isEquivalent: aClass rwComment)
		ifFalse: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message: 'Comment has changed in compiled class v loaded class')
						reason: #'differentComment';
						loadedPropertyValue: aLoadedClass classComment;
						classPropertyValue: aClass rwComment;
						class: aClass;
						yourself) ].
	aLoadedClass classCategory
		= (classProperty := aClass _classCategory ifNil: [ '' ])
		ifFalse: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message: 'Class category has changed in compiled class v loaded class')
						reason: #'differentCategory';
						loadedPropertyValue: aLoadedClass classCategory;
						classPropertyValue: classProperty;
						class: aClass;
						yourself) ].
	(aDict := System myUserProfile
		resolveSymbol: aLoadedClass classSymbolDictionaryName asSymbol)
		ifNil: [ 
			res
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'Unable to find SymbolDictionary ' , aLoadedClass classSymbolDictionaryName)
						reason: #'missingSymbolDictionary';
						loadedPropertyValue: aLoadedClass classSymbolDictionaryName;
						class: aClass;
						yourself) ]
		ifNotNil: [ :smbd | 
			smbd value
				at: aLoadedClass name asSymbol
				ifAbsent: [ 
					res
						add:
							((RwAuditClassPropertyDetail
								for: aLoadedClass
								message: 'Class not found in symbol dictionary of loaded class')
								reason: #'missingClassInSymbolDictionary';
								loadedPropertyValue: aLoadedClass classSymbolDictionaryName;
								classPropertyValue:
										(GsCurrentSession currentSession symbolList
												dictionariesAndSymbolsOf: aClass theNonMetaClass);
								class: aClass;
								yourself) ] ].

	^ res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditLoadedMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension [
	"verify that compiled method is present for each loaded class method. return nil if no error"

	"we already check verifying selectors that compiled method matches loaded method"

	| res |
	res := self _result.
	(aClassOrMeta compiledMethodAt: aLoadedMethod name otherwise: nil)
		ifNil: [ 
			res
				add:
					((RwAuditMethodDetail
						for: aLoadedClassOrExtension
						message:
							'Missing compiled method>>' , aClassOrMeta printString , '>>'
								, aLoadedMethod selector)
						reason: #'missingCompiledMethod';
						loadedMethod: aLoadedMethod;
						behavior: aClassOrMeta;
						yourself) ]
		ifNotNil: [ :aMethod | 
			aMethod == aLoadedMethod handle
				ifTrue: [ 
					| expected actual |
					((expected := aLoadedMethod methodCategory)
						equalsNoCase:
							(actual := aClassOrMeta categoryOfSelector: aMethod selector))
						ifFalse: [ 
							res
								add:
									((RwAuditMethodDetail
										for: aClassOrMeta
										message:
											'Mismatched method category (expected: ' , expected printString , ' actual: '
												, actual printString , ')')
										reason: #'differentMethodCategory';
										loadedMethod: aLoadedMethod;
										method: aMethod;
										category: actual;
										behavior: aClassOrMeta;
										yourself) ] ]
				ifFalse: [ 
					res
						add:
							((RwAuditMethodDetail
								for: aLoadedClassOrExtension
								message:
									'Compiled method is not identical to loaded class method>>' , aLoadedMethod name)
								reason: #'methodsNotIdentical';
								loadedMethod: aLoadedMethod;
								method: aMethod;
								behavior: aClassOrMeta;
								yourself) ] ].
	^ res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditRowanCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	| res |
	res := self _result.
	(aBehavior selectorsIn: category)
		do: [ :aSelector | 
			(aLoadedClass loadedMethodAt: aSelector isMeta: aBehavior isMeta)
				ifNil: [ 
					| foundExtensionClass |
					"should be an extension method"
					foundExtensionClass := false.
					(Rowan image loadedClassExtensionsForClass: aBehavior)
						do: [ :aLoadedClassExtension | 
							(aLoadedClassExtension loadedMethodAt: aSelector isMeta: aBehavior isMeta)
								ifNotNil: [ :aLoadedMethod | foundExtensionClass := true ] ].
					foundExtensionClass
						ifFalse: [ 
							| notification |
							notification := (RwAuditMethodErrorNotification
								method: aSelector
								isMeta: aBehavior isMeta
								inClassNamed: aBehavior theNonMetaClass name
								isClassExtension: aLoadedClass isLoadedClassExtension
								intoPackageNamed: aLoadedClass packageName)
								description: 'Missing loaded method';
								reason: #'missingLoadedMethod';
								yourself.
							notification signal
								ifTrue: [ 
									res
										add:
											((RwAuditMethodDetail
												for: aLoadedClass
												message: 'Missing loaded method>>' , aSelector)
												reason: #'missingLoadedMethod';
												loadedMethod: nil;
												category: category;
												selector: aSelector;
												behavior: aBehavior;
												yourself) ] ] ]
				ifNotNil: [ :aLoadedMethod | 
					"noop ... every loaded method in the receiver is audited by auditLoadedClass:"
					 ] ].
	^ res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditRowanHybridCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	| aPackage res |
	"must be an extension. Do basic checks"
	aPackage := category copyFrom: 2 to: category size.
	res := self _result.
	(category notEmpty and: [ category first == $* ])
		ifTrue: [ 
			"must be an extension category. See if package exists"
			(aPackage asLowercase isEquivalent: aBehavior rowanPackageName asLowercase)
				ifTrue: [ 
					"aLoadedClass name , ' #' , category asString -> 'Extension category name can not be same as class package' "
					res
						add:
							((RwAuditMethodCategoryDetail
								for: aLoadedClass
								message:
									'Extension category name <' , category , '>  must not match class package name.')
								reason: #'rowanHybridExtensionCategoryMatchesClassPackage';
								category: category;
								behavior: aBehavior;
								yourself) ]
				ifFalse: [ 
					(Rowan image
						loadedClassExtensionsNamed: aBehavior thisClass name
						ifFound: [ :extensions | 
							extensions
								detect: [ :e | e loadedPackage name asLowercase isEquivalent: aPackage ]
								ifNone: [ nil ] ]
						ifAbsent: [ nil ]) isNil
						ifTrue: [ 
							res
								addAll:
									(self
										_auditCategory: category
										selectors: (aBehavior selectorsIn: category)
										forBehavior: aBehavior
										loadedClass: aLoadedClass) ]
						ifFalse: [ 
							res
								addAll:
									(self
										_auditCategory: category
										selectors: (aBehavior selectorsIn: category)
										forBehavior: aBehavior
										loadedClass: aLoadedClass) ] ] ]
		ifFalse: [ 
			aLoadedClass isLoadedClassExtension
				ifTrue: [ 
					res
						add:
							((RwAuditMethodCategoryDetail
								for: aLoadedClass
								message:
									'Extension category name <' , category , '>  must begin with a `*`.')								reason: #'rowanHybridExtensionCategoryFormatError';
								category: category;
								behavior: aBehavior;
								yourself)  ].
			res
				addAll:
					(self
						_auditCategory: category
						selectors: (aBehavior selectorsIn: category)
						forBehavior: aBehavior
						loadedClass: aLoadedClass) ].
	^ res
]

{ #category : 'audit' }
RwClsAuditTool >> _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass [
	"#rentamed from _auidtClassSelector since functionality is same for instanance and class
 verify compiled method matches loaded method reference return nil if no problem found"

	| compiledMethod |
	compiledMethod := aBehavior compiledMethodAt: aSelector otherwise: nil.
	^ (Rowan image loadedMethodForMethod: compiledMethod ifAbsent: [  ])
		ifNil: [ 
			| notification |
			notification := (RwAuditMethodErrorNotification
				method: aSelector
				isMeta: aBehavior isMeta
				inClassNamed: aBehavior theNonMetaClass name
				isClassExtension: aLoadedClass isLoadedClassExtension
				intoPackageNamed: aLoadedClass loadedPackage name)
				description: 'Missing loaded method';
				reason: #'missingLoadedMethod';
				yourself.
			notification signal
				ifTrue: [ 
					{((RwAuditMethodDetail
						for: aLoadedClass
						message: 'Missing loaded method>>' , aSelector asString)
						reason: #'missingLoadedMethod';
						loadedMethod: nil;
						method: compiledMethod;
						selector: aSelector;
						behavior: aBehavior;
						yourself)} ]
				ifFalse: [ 
					"don't record audit error"
					{} ] ]
		ifNotNil: [ :loadedMethod | 
			"noop ... every loaded method in the receiver is audited by auditLoadedClass:"
			 ]
]

{ #category : 'audit' }
RwClsAuditTool >> _result [

	^Array new.

]

{ #category : 'audit' }
RwClsAuditTool >> auditLoadedClass: aLoadedClass [
	"look for methods compiled into class without Rowan API"

	| res aBehavior |
	res := self _result.
	(Rowan globalNamed: aLoadedClass name)
		ifNil: [ 
			"there is no matching Class for LoadedClass"
			res
				add:
					(RwAuditClassDetail
						for: aLoadedClass
						reason: #'missingGemStoneClassForLoadedClass'
						message: 'Missing gemstone class for loaded class ') ].
	aBehavior := aLoadedClass handle.
	res
		addAll:
			(self _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior).
	aBehavior
		categorysDo: [ :category :selectors | 
			res
				addAll:
					(self _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass) ].
	aBehavior class
		categorysDo: [ :category :selectors | 
			res
				addAll:
					(self
						_auditCategory: category
						forBehavior: aBehavior class
						loadedClass: aLoadedClass) ].
	aLoadedClass
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
			(aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil)
				ifNil: [ 
					res
						addAll:
							(self
								_auditLoadedMethod: aLoadedMethod
								forBehavior: aBehavior
								loadedClass: loadedClass) ] ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
			(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil)
				ifNil: [ 
					res
						addAll:
							(self
								_auditLoadedMethod: aLoadedMethod
								forBehavior: aBehavior class
								loadedClass: loadedClass) ] ].
	^ res
]
