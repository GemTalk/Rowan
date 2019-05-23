smalltalk api
adoptMethod: methodSelector inClassNamed: className  isMeta: isMeta intoPackageNamed: packageName

	"adopt the method <methodSelector> in class named <className> and it's methods into the package named <packageName>"

	| theClass theBehavior protocolString  |
	theClass := Rowan globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	protocolString := (theBehavior categoryOfSelector: methodSelector asSymbol) asString. 
	self 
		adoptMethod: methodSelector 
		protocol: protocolString 
		inClassNamed: className  
		isMeta: isMeta 
		intoPackageNamed: packageName
