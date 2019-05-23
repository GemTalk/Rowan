validation
_validateRowanMonticelloProtocolConvention: protocol  forPackageNamed: packageName

	| canonProtocol expectedProtocol |
	(protocol at: 1) = $*
		ifTrue: [
			currentClassDefinition
				ifNotNil: [
					"protocol should not start with $* -- an obvious violation"
					self error: 'not yet implemented']]
		ifFalse: [
			currentClassDefinition 
				ifNotNil:  [ 
					"protocol does not start with $* as expected"
					^ self ] ].			
	"validate conformance to convention for extension method"
	canonProtocol := protocol asLowercase.
	expectedProtocol := '*', packageName asLowercase.
	"off the top of my head"
	(canonProtocol beginsWith: expectedProtocol) 
		ifTrue: [
			(canonProtocol indexOf: $- startingAt: expectedProtocol size) <= 1 ifTrue: [ ^self ] ].
	"protocol does not conform to Monticello convention"
	self error: 'not yet implemented'