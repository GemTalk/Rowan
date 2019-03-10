Extension { #name : 'RwPackageDefinition' }

{ #category : '*rowan-definitions-deprecated' }
RwPackageDefinition >> addClassExtension: aClassExtensionDefinition [

	self deprecated: 'RwPackageDefinition >> addClassExtension: deprecated in Rowan 1.2.6+'.
	self addDefinition: aClassExtensionDefinition to: classExtensions
]

{ #category : '*rowan-definitions-deprecated' }
RwPackageDefinition >> removeClassExtension: aClassExtension [

	self deprecated: 'RwPackageDefinition >> removeClassExtension: deprecated in Rowan 1.2.6+'.
	self removeDefinition: aClassExtension from: classExtensions
]
