tests-issue 198
testIssue198A

	"https://github.com/dalehenrich/Rowan/issues/198"

	"in attempting to reproduce issue #198, hit this issue. See https://github.com/dalehenrich/Rowan/issues/198#issuecomment-391831224
		for characterization."

	"subclass of class with new version, which is referenced by a method in a third class, is moved to a different package ... 
		failure occurs when the new superclass version and move to different package occurs in one atomic load. "

	| projectName projectNames packageName1 packageName2 className1 className2 theClass 
		theSubclass projectDefinition theClassDefinition theSubclassDefinition 
		packageDefinition1 packageDefinition2 |

	projectName := 'Issue198Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue198-Core'.
	packageName2 := 'Issue198-Deprecated'.
	className1 := 'Issue198Class'.
	className2 := 'Issue198Subclass'.

	self
		_loadProjectDefinition: projectName
		packageNames: {packageName1. packageName2}
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: ''.

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSubclass := theClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.

	theClassDefinition := packageDefinition1 classDefinitions at: className1.
	theClassDefinition instVarNames: #( 'ivar1').																	"force new version of className1 and className2"

	theSubclassDefinition := packageDefinition1 classDefinitions at: className2.
	packageDefinition1 classDefinitions removeKey: theSubclassDefinition key.
	packageDefinition2 classDefinitions at: theSubclassDefinition key put: theSubclassDefinition.	"move subclass to different package"

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	self assert: theSubclass rowanPackageName = packageName2.
