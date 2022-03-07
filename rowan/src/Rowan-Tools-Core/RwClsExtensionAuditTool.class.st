Class {
	#name : 'RwClsExtensionAuditTool',
	#superclass : 'RwClsCommonAuditTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'audit' }
RwClsExtensionAuditTool >> auditClass: aLoadedClassOrExtension [
	| aClass |
	super auditClass: aLoadedClassOrExtension.
	aClass := aLoadedClassOrExtension handle.
"stuff not covered by auditCompiledMethods:...."
	^ self theAuditDetails
]

{ #category : 'audit' }
RwClsExtensionAuditTool >> auditCompiledMethods: aLoadedClassExtension forClass: aClass [
	| categories |
	aLoadedClassExtension loadedProject packageConvention = 'RowanHybrid'
		ifTrue: [ 
			| extensionCategoryName |
			extensionCategoryName := aLoadedClassExtension loadedPackage asExtensionName.
			{aClass.
			(aClass class)}
				do: [ :aBehavior | 
					| categoryDict |
					categoryDict := aBehavior rwMethodCategories.
					categories := categoryDict
						ifNil: [ #() ]
						ifNotNil: [ :catDict | catDict keys ].
					(categories
						detect: [ :each | each asString equalsNoCase: extensionCategoryName ]
						ifNone: [  ])
						ifNotNil: [ :aCategory | 
							self
								auditRowanHybridCategory: aCategory
								forBehavior: aBehavior
								loadedClass: aLoadedClassExtension.
							aClass rowanPackageName = Rowan unpackagedName
								ifTrue: [ 
									"In an unpackaged class, we must audit the extension categories.
										Every selector in the extension category is expected to be be 
										packaged for RowanHybrid. Packaged class is covered by 
										RwNewClsAuditTool >> auditCompiledMethods:forClass:"
									(categoryDict at: aCategory)
										do: [ :aSelector | 
											self
												auditSelector: aSelector
												inCategory: aCategory
												forBehavior: aBehavior
												loadedClass: aLoadedClassExtension ] ] ]
						ifNil: [ 
							(aBehavior isMeta
								ifTrue: [ aLoadedClassExtension loadedClassMethods ]
								ifFalse: [ aLoadedClassExtension loadedInstanceMethods ]) notEmpty
								ifTrue: [ 
									self theAuditDetails
										add:
											((RwAuditMethodCategoryDetail
												for: aLoadedClassExtension
												message:
													'Missing '
														,
															(aBehavior isMeta
																ifTrue: [ 'class' ]
																ifFalse: [ 'instance' ]) , ' method extension category named '
														, extensionCategoryName , ' for: ' , aLoadedClassExtension name)
												reason: #'missingExtensionCategory';
												category: extensionCategoryName;
												behavior: aBehavior;
												yourself) ] ] ] ]
		ifFalse: [ 
			aLoadedClassExtension loadedProject packageConvention = 'Rowan'
				ifTrue: [ 
					"extension methods may be inter-mixed with non-extension methods in the same category, so category-based audit is not useful
						but, we do want to make sure that empty loaded class extensions aren't floating around"
					aLoadedClassExtension isEmpty
						ifTrue: [ 
							self theAuditDetails
								add:
									(RwAuditClassDetail
										for: aLoadedClassExtension
										reason: #'emptyLoadedClassExtension'
										message:
											'The loaded class extension is empty for: ' , aLoadedClassExtension name) ] ]
				ifFalse: [ 
					aLoadedClassExtension loadedProject packageConvention = 'Monticello'
						ifTrue: [ self error: 'Monticlello package conventions not yet supported' ].
					self
						error:
							'Unknonwn package convention'
								, aLoadedClassExtension loadedProject packageConvention printString ] ]
]

{ #category : 'audit' }
RwClsExtensionAuditTool >> auditGlobalFor: aLoadedClassExtension [
	^ (Rowan globalNamed: aLoadedClassExtension name)
		ifNil: [ 
			self theAuditDetails
				add:
					(RwAuditClassDetail
						for: aLoadedClassExtension
						reason: #'missingGemStoneClassForLoadedClassExtension'
						message:
							' Class does not exists for loaded class extension: '
								, aLoadedClassExtension name) ]
		ifNotNil: [ :theClass | 
			theClass == aLoadedClassExtension handle
				ifFalse: [ 
					"the loadedClassExtension is for a different version of the current class installed in symbol dictionaries"
					self theAuditDetails
						add:
							(RwAuditClassDetail
								for: aLoadedClassExtension
								reason: #'classesNotIdentical'
								message:
									'Installed class is not identical to the loaded class: '
										, aLoadedClassExtension name) ] ]
]

{ #category : 'audit' }
RwClsExtensionAuditTool >> auditLoadedClassProperties: aLoadedClassExtension forClass: aClass [
	"noop for loaded class extensions ... no properties to audit"
]
