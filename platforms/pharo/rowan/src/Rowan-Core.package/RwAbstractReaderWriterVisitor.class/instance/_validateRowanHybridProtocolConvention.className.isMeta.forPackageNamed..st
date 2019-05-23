validation
_validateRowanHybridProtocolConvention:  methodDef className: className isMeta: isMeta forPackageNamed:  packageName

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