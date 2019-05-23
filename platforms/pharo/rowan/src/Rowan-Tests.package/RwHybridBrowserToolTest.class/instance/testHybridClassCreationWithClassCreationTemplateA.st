tests
testHybridClassCreationWithClassCreationTemplateA

	"https://github.com/dalehenrich/Rowan/issues/17"

	"make sure that we actually evaluate the class creation templates"

	| category normalClass projectName packageNames packageName1 packageName2 browserTool template |
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
		className: 'SimpleHybridNormal1'
		category: packageName1.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridNormal1'
		category: category
		packageName: packageName1.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = category.

	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: false.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = category.

	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: true.	" packageName does not equal category, so hyrbridBrowser form cannot be used"
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = category.

	normalClass category: packageName1.	"now we can use hybridBrowser form again"
	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: true.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1