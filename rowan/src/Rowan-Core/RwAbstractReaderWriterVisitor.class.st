Class {
	#name : 'RwAbstractReaderWriterVisitor',
	#superclass : 'Object',
	#instVars : [
		'currentProjectDefinition',
		'packageConvention',
		'currentPackageDefinition',
		'currentClassDefinition',
		'currentClassExtension'
	],
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractReaderWriterVisitor class >> _readObjectFrom: aFileReference [

	aFileReference readStreamDo: [:stream |
		| reader |
		reader := STON reader on:  (ZnBufferedReadStream on:  stream).
		reader classes at: #'Package' put: Dictionary.
		^ reader next ]
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor class >> _repositoryPropertyDictFor: packagesRoot [
	| propertiesFile |
	propertiesFile := packagesRoot / 'properties' , 'st'.
	propertiesFile exists
		ifFalse: [ 
			| propertiesDict |
			propertiesFile := packagesRoot / '.filetree'.
			propertiesFile exists
				ifFalse: [ 
					propertiesFile := packagesRoot / '.properties'.
					propertiesFile exists
						ifFalse: [ ^ Dictionary new ] ].
			propertiesDict := self _readObjectFrom: propertiesFile.
			propertiesDict at: #'format' put: 'filetree'.
			^ propertiesDict ].
	^ self _readObjectFrom: propertiesFile
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor class >> tonelClassLabel [
	^ 'Class'
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor class >> tonelExtensionLabel [
	^ 'Extension'
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _packageConvention: aString [

	packageConvention := aString
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _readObjectFrom: aFileReference [

	^ self class _readObjectFrom: aFileReference
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _repositoryConventionFor: packagesRoot [

	^ (self _repositoryPropertyDictFor: packagesRoot) at: #convention ifAbsent: [ 'RowanHybrid'  ]
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _repositoryFormatFor: packagesRoot [

	^ (self _repositoryPropertyDictFor: packagesRoot) at: #format ifAbsent: [ self defaultPackageFormat ]
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _repositoryPropertyDictFor: packagesRoot [

	^ self class _repositoryPropertyDictFor: packagesRoot
]

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> _validateRowanHybridProtocolConvention:  methodDef className: className isMeta: isMeta forPackageNamed:  packageName [

	| canonProtocol expectedProtocol protocol |
	protocol := methodDef protocol.
	(protocol at: 1) = $*
		ifTrue: [
			currentClassDefinition
				ifNotNil: [
					"protocol should not start with $* for a non-extension method"
					RwExtensionProtocolNonExtensionMethodErrorNotification 
						signalWithMethodDefinition: methodDef
							className: className 
							isMeta: isMeta 
							packageName:  packageName
							packageConvention: 'RowanHybrid'.
					^ self ] ]
		ifFalse: [
			currentClassDefinition 
				ifNotNil:  [ 
					"protocol does not start with $* as expected"
					^ self ] ].			
	"validate conformance to convention for extension method"
	(protocol at: 1) = $*
		ifFalse: [
			"extension method protocol must start with a *"
			RwNonExtensionProtocolExtensionMethodErrorNotification
				signalWithMethodDefinition: methodDef
					className: className 
					isMeta: isMeta 
					packageName:  packageName
					packageConvention: 'RowanHybrid'.
			^ self  ].
	canonProtocol := protocol asLowercase.
	expectedProtocol := '*', packageName asLowercase.
	canonProtocol = expectedProtocol ifTrue: [ ^ self ].
	"protocol does not match package name"
	RwExtensionProtocolExtensionMethodPackageMismatchErrorNotification
		signalWithMethodDefinition: methodDef
			className: className 
			isMeta: isMeta 
			packageName:  packageName
			packageConvention: 'RowanHybrid'
]

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> _validateRowanMonticelloProtocolConvention: methodDef className: className isMeta: isMeta forPackageNamed: packageName [
	| canonProtocol expectedProtocol protocol |
	protocol := methodDef protocol.
	(protocol at: 1) = $*
		ifTrue: [ 
			currentClassDefinition
				ifNotNil: [ 
					"protocol should not start with $* for a non-extension method"
					RwExtensionProtocolNonExtensionMethodErrorNotification
						signalWithMethodDefinition: methodDef
						className: className
						isMeta: isMeta
						packageName: packageName
						packageConvention: 'Monticello'.
					^ self ] ]
		ifFalse: [ 
			currentClassDefinition
				ifNotNil: [ 
					"protocol does not start with $* as expected"
					^ self ] ].	"validate conformance to convention for extension method"
	(protocol at: 1) = $*
		ifFalse: [ 
			"extension method protocol must start with a *"
			RwNonExtensionProtocolExtensionMethodErrorNotification
				signalWithMethodDefinition: methodDef
				className: className
				isMeta: isMeta
				packageName: packageName
				packageConvention: 'Monticello'.
			^ self ].
	canonProtocol := protocol asLowercase.
	expectedProtocol := '*' , packageName asLowercase.
	(canonProtocol beginsWith: expectedProtocol)
		ifTrue: [ ^ self ].	"protocol does not match package name"
	RwExtensionProtocolExtensionMethodPackageMismatchErrorNotification
		signalWithMethodDefinition: methodDef
		className: className
		isMeta: isMeta
		packageName: packageName
		packageConvention: 'Monticello'
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentClassDefinition [

	^ currentClassDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentClassExtension [

	^ currentClassExtension
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentPackageDefinition [

	^ currentPackageDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentPackageDefinition: aRwPackageDefinition [

	currentPackageDefinition := aRwPackageDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentProjectDefinition [

	^ currentProjectDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentProjectDefinition: aRwComponentProjectDefinition [

	currentProjectDefinition := aRwComponentProjectDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> defaultPackageFormat [

	self subclassResponsibility: #defaultPackageFormat
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> packageConvention [

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	^ packageConvention ifNil: [ 
		| ppc dpc  |
		ppc := self currentProjectDefinition packageConvention.
		dpc := self _repositoryConventionFor: self packagesRoot.
		dpc ~= ppc
			ifTrue:  [ self error: 'Disk package convention ' , dpc printString, 
           ' does not match expected package convention ', ppc printString. ' for project ', self projectName printString ].
		packageConvention := ppc ]
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> packagesRoot [

	^ self currentProjectDefinition packagesRoot
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> projectName [

	^ self currentProjectDefinition projectName
]

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> validateClassCategory: aClassDefinition forPackageNamed: packageName [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	"signal an error if the protocol does not conform to the convention for the current project"

	(Rowan image
		validClassCategory: aClassDefinition category
		forPackageConvention: self packageConvention
		andPackageNamed: packageName)
		ifTrue: [ ^ self ].
	RwInvalidClassCategoryConventionErrorNotification
		signalWithClassDefinition: aClassDefinition
		packageName: packageName
		packageConvention: packageConvention
]

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> validateMethodDefinitionProtocol: methodDef className: className isMeta: isMeta forPackageNamed:  packageName [

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	"signal an error if the protocol does not conform to the convention for the current project"

	self packageConvention = 'RowanHybrid'
		ifTrue: [ ^ self _validateRowanHybridProtocolConvention: methodDef className: className isMeta: isMeta forPackageNamed:  packageName ].
	self packageConvention = 'Monticello'
		ifTrue: [ ^ self _validateRowanMonticelloProtocolConvention:methodDef className: className isMeta: isMeta forPackageNamed:  packageName ].
	"Rowan - no convention ... any old protocol is fine"
]
