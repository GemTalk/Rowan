tests
testHybridClassCreationWithClassCreationTemplateB

	"https://github.com/dalehenrich/Rowan/issues/17"

	"make sure that we actually evaluate the class creation templates"

	| category byteClass variableClass projectName packageNames packageName1 packageName2 browserTool template |
	category := 'Category'.
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	browserTool := Rowan projectTools browser.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridByte1'
		type: 'bytes'
		category: packageName1
		packageName: nil.
	byteClass := template evaluate.
	self assert: byteClass rowanPackageName = packageName1.
	self assert: byteClass category = packageName1.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridIndexable1'
		type: 'variable'
		category: category
		packageName: packageName1.
	variableClass := template evaluate.
	self assert: variableClass rowanPackageName = packageName1.
	self assert: variableClass category = category