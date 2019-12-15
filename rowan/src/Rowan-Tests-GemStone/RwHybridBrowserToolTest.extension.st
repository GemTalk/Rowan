Extension { #name : 'RwHybridBrowserToolTest' }

{ #category : '*rowan-tests-gemstone' }
RwHybridBrowserToolTest >> testHybridClassCreationWithClassCreationTemplate_292 [

	"https://github.com/dalehenrich/Rowan/issues/292"

	|  class projectName packageNames packageName1 template expectedTemplate oldClass |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
"1 create class - indexable"
	class := Object
		rwIndexableSubclass: 'TestVariableClass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'Object rwIndexableSubclass: ''TestVariableClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"2 create subclass - indexable"
	oldClass := class.
	class := oldClass
		rwIndexableSubclass: 'TestVariableSubclass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'TestVariableClass rwSubclass: ''TestVariableSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"1 create class - bytes"
	class := Object
		rwByteSubclass: 'TestByteClass'
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'Object rwByteSubclass: ''TestByteClass''
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"2 create subclass - byteSubclass"
	oldClass := class.
	class := oldClass
		rwSubclass: 'TestByteSubclass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'TestByteClass rwSubclass: ''TestByteSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"1 create class - disallowGciStore"
	class := Error
		rwSubclass: 'TestErrorClass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'Error rwSubclass: ''TestErrorClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"1 create class - traverseByCallback"
	class := RcIdentityBag
		rwSubclass: 'TestRcIdentityBagClass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'RcIdentityBag rwSubclass: ''TestRcIdentityBagClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.
]
