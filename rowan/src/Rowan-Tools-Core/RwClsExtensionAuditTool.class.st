Class {
	#name : 'RwClsExtensionAuditTool',
	#superclass : 'RwClsAuditTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsExtensionAuditTool >> _auditCategory: anExtentionCategory forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension [
	"if we have loaded methods but no compiled methods add error to result"

	| res |
	res := super
		_auditCategory: anExtentionCategory
		selectors: (aClassOrMeta selectorsIn: anExtentionCategory)
		forBehavior: aClassOrMeta
		loadedClass: aLoadedClassExtension.
	aClassOrMeta isMeta
		ifTrue: [ 
			(aLoadedClassExtension loadedClassMethods notEmpty
				and: [ (aClassOrMeta selectorsIn: anExtentionCategory) isEmpty ])
				ifTrue: [ 
					res
						add:
							((RwAuditDetail
								for: aLoadedClassExtension
								message:
									'Missing expected class methods in the category ' , ' #'
										, anExtentionCategory asString)
								reason: #'missingCompiledMethodsForLoadedClassExtension';
								behavior: aClassOrMeta;
								yourself) ] ]
		ifFalse: [ 
			(aLoadedClassExtension loadedInstanceMethods notEmpty
				and: [ (aClassOrMeta selectorsIn: anExtentionCategory) isEmpty ])
				ifTrue: [ 
					res
						add:
							((RwAuditDetail
								for: aLoadedClassExtension
								message:
									'Missing expected instances methods in the category' , ' #'
										, anExtentionCategory asString)
								reason: #'missingCompiledMethodsForLoadedClassExtension';
								behavior: aClassOrMeta;
								yourself) ] ].
	^ res
]

{ #category : 'other' }
RwClsExtensionAuditTool >> auditLoadedClassExtension: aLoadedClassExtension [
	"look for methods compiled into class without Rowan API"

	| res aBehavior categories |
	res := self _result.
	(Rowan globalNamed: aLoadedClassExtension name)
		ifNil: [ 
			res
				add:
					(RwAuditClassDetail
						for: aLoadedClassExtension
						reason: #'missingGemStoneClassForLoadedClassExtension'
						message: ' Class does not exists for loaded class extension') ].
	aBehavior := aLoadedClassExtension handle.

	aLoadedClassExtension loadedProject packageConvention = 'RowanHybrid'
		ifTrue: [ 
			| extensionCategoryName |
			extensionCategoryName := aLoadedClassExtension loadedPackage asExtensionName.
			categories := aBehavior rwMethodCategories
				ifNil: [ #() ]
				ifNotNil: [ :catDict | catDict keys ].
			(categories
				detect: [ :each | each asString equalsNoCase: extensionCategoryName ]
				ifNone: [  ])
				ifNotNil: [ :aCategory | 
					res
						addAll:
							(self
								_auditRowanHybridCategory: aCategory
								forBehavior: aBehavior
								loadedClass: aLoadedClassExtension) ]
				ifNil: [ 
					aLoadedClassExtension loadedInstanceMethods notEmpty
						ifTrue: [ 
							res
								add:
									((RwAuditMethodCategoryDetail
										for: aLoadedClassExtension
										message:
											'Missing instance method extension category named ' , extensionCategoryName)
										reason: #'missingExtensionCategory';
										category: extensionCategoryName;
										behavior: aBehavior;
										yourself) ] ].
			categories := aBehavior class rwMethodCategories
				ifNil: [ #() ]
				ifNotNil: [ :catDict | catDict keys ].
			(categories
				detect: [ :each | each asString equalsNoCase: extensionCategoryName ]
				ifNone: [  ])
				ifNotNil: [ :aCategory | 
					res
						addAll:
							(self
								_auditRowanHybridCategory: aCategory
								forBehavior: aBehavior class
								loadedClass: aLoadedClassExtension) ]
				ifNil: [ 
					aLoadedClassExtension loadedClassMethods notEmpty
						ifTrue: [ 
							res
								add:
									((RwAuditMethodCategoryDetail
										for: aLoadedClassExtension
										message:
											'Missing class method extension category named ' , extensionCategoryName)
										reason: #'missingExtensionCategory';
										category: extensionCategoryName;
										behavior: aBehavior;
										yourself) ] ] ]
		ifFalse: [ 
			aLoadedClassExtension loadedProject packageConvention = 'Rowan'
				ifTrue: [ 
					"extension methods may be inter-mixed with non-extension methods in the same category, so category-based audit is not useful
						but, we do want to make sure that empty loaded class extensions aren't floating around"
					aLoadedClassExtension isEmpty
						ifTrue: [ 
							res
								add:
									(RwAuditClassDetail
										for: aLoadedClassExtension
										reason: #'emptyLoadedClassExtension'
										message: 'The loaded class extension is empty') ] ]
				ifFalse: [ 
					aLoadedClassExtension loadedProject packageConvention = 'Monticello'
						ifTrue: [ self error: 'Monticlello package conventions not yet supported' ].
					self
						error:
							'Unknonwn package convention'
								, aLoadedClassExtension loadedProject packageConvention printString ] ].

	aLoadedClassExtension
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
			res
				addAll:
					(self
						_auditLoadedMethod: aLoadedMethod
						forBehavior: aBehavior
						loadedClass: loadedClass) ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
			res
				addAll:
					(self
						_auditLoadedMethod: aLoadedMethod
						forBehavior: aBehavior class
						loadedClass: loadedClass) ].
	^ res
]
