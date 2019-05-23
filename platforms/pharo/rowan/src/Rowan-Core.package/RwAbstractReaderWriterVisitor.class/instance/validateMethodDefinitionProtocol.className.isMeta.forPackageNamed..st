validation
validateMethodDefinitionProtocol: methodDef className: className isMeta: isMeta forPackageNamed:  packageName

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