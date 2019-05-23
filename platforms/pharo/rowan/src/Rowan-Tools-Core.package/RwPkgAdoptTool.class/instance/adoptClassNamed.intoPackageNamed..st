smalltalk api
adoptClassNamed: className  intoPackageNamed: packageName

	"adopt the class named <className> and it's methods into the package named <packageName>"

	"Ignore packaged instance and class methods"

	| theClass |

	theClass := Rowan globalNamed: className.
	self 
		adoptClassNamed: className 
		instanceSelectors: theClass selectors 
		classSelectors: theClass class selectors 
		intoPackageNamed: packageName
