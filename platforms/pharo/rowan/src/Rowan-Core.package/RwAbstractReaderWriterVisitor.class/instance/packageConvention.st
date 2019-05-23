accessing
packageConvention

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
			ifTrue:  [ self error: 'Disk package convention ' , dpc printString, ' does not match expected package convention ', ppc printstring. ' for project ', self projectName printString ].
		packageConvention := ppc ]