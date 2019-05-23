smalltalk api
adoptClassNamed: className  instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName

	"adopt the class named <className> and it's methods into the package named <packageName>"

	"Ignore packaged instance and class methods"
	
	self 
		adoptClassNamed: className 
		classExtension: false 
		instanceSelectors: instanceSelectors 
		classSelectors: classSelectors 
		intoPackageNamed: packageName
