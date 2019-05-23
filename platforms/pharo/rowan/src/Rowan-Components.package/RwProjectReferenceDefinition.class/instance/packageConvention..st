accessing
packageConvention: aString

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	(#( 'RowanHybrid' 'Monticello' 'Rowan' ) includes: aString)
		ifFalse: [ self error: 'Unknown package convention ', aString printString, '. Should be one of: RowanHybrid, Monticello, or Rowan' ].
	^ self properties at: 'packageConvention' put: aString