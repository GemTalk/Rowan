smalltalk api
adoptClassExtensionNamed: className  instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName

	"adopt extension methods for the class named <className> into the package named <packageName>"

	self 
		adoptClassNamed: className 
		classExtension: true 
		instanceSelectors: instanceSelectors 
		classSelectors: classSelectors 
		intoPackageNamed: packageName
