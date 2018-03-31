Extension { #name : 'TDMethodTemplateDefinition' }

{ #category : '*rowan-tode' }
TDMethodTemplateDefinition >> rowanPackageName [

	^ Rowan unpackagedName
]

{ #category : '*rowan-tode' }
TDMethodTemplateDefinition >> rowanProjectName [

	^ Rowan unpackagedName
]

{ #category : '*rowan-tode' }
TDMethodTemplateDefinition >> rwCompile: source classified: methodProtocol [

	| meth |
	meth := self theBehavior rwCompileMethod: source category: methodProtocol.
	^ meth selector
]
