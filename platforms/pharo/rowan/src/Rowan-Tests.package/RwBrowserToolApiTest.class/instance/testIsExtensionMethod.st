tests
testIsExtensionMethod

	| projectName packageNames className packageName1 packageName2   browserTool |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	browserTool
		addOrUpdateMethod: 'bar ^1'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: true
		inPackageNamed: packageName1.	"add method in the package of the class"

	self deny: (browserTool isExtensionMethod: 'ivar1' forClassNamed: className isMeta: false).
	self deny: (browserTool isExtensionMethod: 'bar' forClassNamed: className isMeta: true).

	browserTool
		addOrUpdateMethod: 'foo ^1'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	browserTool
		addOrUpdateMethod: 'baz ^1'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: true
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	self assert: (browserTool isExtensionMethod: 'foo' forClassNamed: className isMeta: false).
	self assert: (browserTool isExtensionMethod: 'baz' forClassNamed: className isMeta: true).

	self deny: (browserTool isExtensionMethod: 'yourself' forClassNamed: 'Object' isMeta: false). "not rowanized at moment"
