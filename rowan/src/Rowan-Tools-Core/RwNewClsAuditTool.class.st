Class {
	#name : 'RwNewClsAuditTool',
	#superclass : 'RwNewClsCommonAuditTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'audit' }
RwNewClsAuditTool >> auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	| packageConvention |
	packageConvention := aLoadedClass loadedProject packageConvention.

	^ packageConvention = 'RowanHybrid'
		ifTrue: [ 
			self
				auditRowanHybridCategory: category
				forBehavior: aBehavior
				loadedClass: aLoadedClass ]
]

{ #category : 'audit' }
RwNewClsAuditTool >> auditCompiledMethods: aLoadedClass forClass: aClass [
	"full audit of class methods. Every method should be packaged (have a loaded method)"

	aClass
		categorysDo: [ :category :selectors | 
			self auditCategory: category forBehavior: aClass loadedClass: aLoadedClass.
			selectors
				do: [ :aSelector | 
					self
						auditSelector: aSelector
						inCategory: category
						forBehavior: aClass
						loadedClass: aLoadedClass ] ].
	aClass class
		categorysDo: [ :category :selectors | 
			self auditCategory: category forBehavior: aClass class loadedClass: aLoadedClass.
			selectors
				do: [ :aSelector | 
					self
						auditSelector: aSelector
						inCategory: category
						forBehavior: aClass class
						loadedClass: aLoadedClass ] ]
]

{ #category : 'audit' }
RwNewClsAuditTool >> auditGlobalFor: aLoadedClass [
	^ (Rowan globalNamed: aLoadedClass name)
		ifNil: [ 
			"there is no matching Class for LoadedClass"
			self theAuditDetails
				add:
					(RwAuditClassDetail
						for: aLoadedClass
						reason: #'missingGemStoneClassForLoadedClass'
						message: 'Missing gemstone class for loaded class: ' , aLoadedClass name) ]
]

{ #category : 'audit' }
RwNewClsAuditTool >> auditLoadedClassProperties: aLoadedClass forClass: aClass [
	"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| aDict superclassName classProperty varNames |
	superclassName := aClass superclass
		ifNil: [ 'nil' ]
		ifNotNil: [ :superCls | superCls name ].
	(aLoadedClass classSuperclass isEquivalent: superclassName)
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name , ' superclass (' , superclassName
								, ') is different from loaded class (' , aLoadedClass classSuperclass
								, ')')
						reason: #'differentSuperclass';
						loadedPropertyValue: aLoadedClass classSuperclass;
						classPropertyValue: superclassName;
						class: aClass;
						yourself) ].
	aLoadedClass classInstVarNames
		= (varNames := aClass instVarNames collect: [ :e | e asString ])
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name , ' instVarNames changed in compiled class ('
								, varNames printString , ') v loaded class ('
								, aLoadedClass classInstVarNames printString , ')')
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
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name , ' classVars changed in compiled class ('
								, classProperty printString , ') v loaded class ('
								, aLoadedClass classClassVarNames printString , ')')
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
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name
								, ' poolDictionaries changed in compiled class v loaded class')
						reason: #'differentPoolDictionaries';
						loadedPropertyValue: aLoadedClass classPoolDictionaries;
						classPropertyValue: classProperty;
						class: aClass;
						yourself) ].
	(aLoadedClass classComment isEquivalent: aClass rwComment)
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name , ' comment has changed in compiled class ('
								, aClass rwComment printString , ') v loaded class ('
								, aLoadedClass classComment printString , ')')
						reason: #'differentComment';
						loadedPropertyValue: aLoadedClass classComment;
						classPropertyValue: aClass rwComment;
						class: aClass;
						yourself) ].
	aLoadedClass classCategory
		= (classProperty := aClass _classCategory ifNil: [ '' ])
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name
								, ' class category has changed in compiled class (' , classProperty
								, ') v loaded class (' , aLoadedClass classCategory , ')')
						reason: #'differentCategory';
						loadedPropertyValue: aLoadedClass classCategory;
						classPropertyValue: classProperty;
						class: aClass;
						yourself) ].
	(aDict := System myUserProfile
		resolveSymbol: aLoadedClass classSymbolDictionaryName asSymbol)
		ifNil: [ 
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedClass
						message:
							'For class ' , aLoadedClass name , ' unable to find SymbolDictionary '
								, aLoadedClass classSymbolDictionaryName)
						reason: #'missingSymbolDictionary';
						loadedPropertyValue: aLoadedClass classSymbolDictionaryName;
						class: aClass;
						yourself) ]
		ifNotNil: [ :smbd | 
			smbd value
				at: aLoadedClass name asSymbol
				ifAbsent: [ 
					self theAuditDetails
						add:
							((RwAuditClassPropertyDetail
								for: aLoadedClass
								message:
									'Class (' , aLoadedClass name , ') not found in symbol dictionary ('
										, aLoadedClass classSymbolDictionaryName asSymbol
										, ') of loaded class')
								reason: #'missingClassInSymbolDictionary';
								loadedPropertyValue: aLoadedClass classSymbolDictionaryName;
								classPropertyValue:
										(GsCurrentSession currentSession symbolList
												dictionariesAndSymbolsOf: aClass theNonMetaClass);
								class: aClass;
								yourself) ] ]
]
