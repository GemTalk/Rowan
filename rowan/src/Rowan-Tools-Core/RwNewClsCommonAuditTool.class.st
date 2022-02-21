Class {
	#name : 'RwNewClsCommonAuditTool',
	#superclass : 'RwClassTool',
	#instVars : [
		'theAuditDetails'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'audit' }
RwNewClsCommonAuditTool >> auditClass: aLoadedClassOrExtension [
	| aClass |
	self auditGlobalFor: aLoadedClassOrExtension.
	aClass := aLoadedClassOrExtension handle.
	self auditLoadedClassProperties: aLoadedClassOrExtension forClass: aClass.
	self auditCompiledMethods: aLoadedClassOrExtension forClass: aClass.
	aLoadedClassOrExtension
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
			self
				auditLoadedMethod: aLoadedMethod
				forBehavior: aClass
				loadedClass: loadedClass ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
			self
				auditLoadedMethod: aLoadedMethod
				forBehavior: aClass class
				loadedClass: loadedClass ].
	^ self theAuditDetails
]

{ #category : 'audit' }
RwNewClsCommonAuditTool >> auditCompiledMethods: aLoadedClassOrExtension forClass: aClass [
	self subclassResponsibility: #'auditCompiledMethods:forClass:'
]

{ #category : 'audit' }
RwNewClsCommonAuditTool >> auditLoadedMethod: aLoadedMethod forBehavior: aBehavior loadedClass: aLoadedClassOrExtension [
	(aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil)
		ifNil: [ 
			self theAuditDetails
				add:
					((RwAuditMethodDetail
						for: aLoadedClassOrExtension
						message:
							'Missing compiled method: ' , aBehavior printString , '>>'
								, aLoadedMethod selector)
						reason: #'missingCompiledMethod';
						loadedMethod: aLoadedMethod;
						behavior: aBehavior;
						yourself) ]
		ifNotNil: [ :aMethod | 
			| expected actual |
			((expected := aLoadedMethod methodCategory)
				equalsNoCase: (actual := aBehavior categoryOfSelector: aMethod selector))
				ifFalse: [ 
					self theAuditDetails
						add:
							((RwAuditMethodDetail
								for: aLoadedClassOrExtension
								message:
									'Mismatched method category (expected: ' , expected printString , ' actual: '
										, actual printString , '): ' , aBehavior printString , '>>'
										, aLoadedMethod selector)
								reason: #'differentMethodCategory';
								loadedMethod: aLoadedMethod;
								method: aMethod;
								loadedCategory: actual;
								category: expected;
								behavior: aBehavior;
								yourself) ].
			aMethod == aLoadedMethod handle
				ifFalse: [ 
					self theAuditDetails
						add:
							((RwAuditMethodDetail
								for: aLoadedClassOrExtension
								message:
									'Compiled method is not identical to loaded method: ' , aBehavior printString
										, '>>' , aLoadedMethod selector)
								reason: #'methodsNotIdentical';
								loadedMethod: aLoadedMethod;
								method: aMethod;
								behavior: aBehavior;
								yourself) ] ]
]

{ #category : 'audit' }
RwNewClsCommonAuditTool >> auditRowanHybridCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	| aPackage |
	"must be an extension. Do basic checks"
	aPackage := category copyFrom: 2 to: category size.
	(category notEmpty and: [ category first == $* ])
		ifTrue: [ 
			"must be an extension category. See if package exists"
			(aPackage asLowercase isEquivalent: aBehavior rowanPackageName asLowercase)
				ifTrue: [ 
					"aLoadedClass name , ' #' , category asString -> 'Extension category name can not be same as class package' "
					self theAuditDetails
						add:
							((RwAuditMethodCategoryDetail
								for: aLoadedClass
								message:
									'For class (' , aBehavior printString , ') extension category name <' , category
										, '>  must not match class package name ('
										, aBehavior rowanPackageName , ').')
								reason: #'rowanHybridExtensionCategoryMatchesClassPackage';
								category: category;
								behavior: aBehavior;
								yourself) ] ]
		ifFalse: [ 
			aLoadedClass isLoadedClassExtension
				ifTrue: [ 
					self theAuditDetails
						add:
							((RwAuditMethodCategoryDetail
								for: aLoadedClass
								message:
									'For class (' , aBehavior printString , ') extension category name <' , category
										, '>  must begin with a `*`.')
								reason: #'rowanHybridExtensionCategoryFormatError';
								category: category;
								behavior: aBehavior;
								yourself) ] ]
]

{ #category : 'audit' }
RwNewClsCommonAuditTool >> auditSelector: aSelector inCategory: category forBehavior: aBehavior loadedClass: aLoadedClass [
	"every method in a packaged class is expected to be packaged (i.e. have a loaded method)"

	| compiledMethod |
	(compiledMethod := aBehavior compiledMethodAt: aSelector otherwise: nil)
		ifNil: [ 
			"interesting anomaly, but not necessarily a Rowan corruption issue ... category dictionary is not correct"
			GsFile
				gciLogServer:
					'**NOTE** no method or loaded method found for selector '
						, aSelector printString , 'in category ' , category , ' for class '
						, aBehavior printString.
			^ self ].
	Rowan image
		loadedMethodForMethod: compiledMethod
		ifAbsent: [ 
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
					"record audit detail"
					self theAuditDetails
						add:
							((RwAuditMethodDetail
								for: aLoadedClass
								message:
									'Missing loaded method: ' , aBehavior printString , '>>' , aSelector)
								reason: #'missingLoadedMethod';
								loadedMethod: nil;
								method: compiledMethod;
								selector: aSelector;
								behavior: aBehavior;
								yourself) ].
			^ self ]
]

{ #category : 'accessing' }
RwNewClsCommonAuditTool >> theAuditDetails [
	^ theAuditDetails ifNil: [ theAuditDetails := Array new ]
]
