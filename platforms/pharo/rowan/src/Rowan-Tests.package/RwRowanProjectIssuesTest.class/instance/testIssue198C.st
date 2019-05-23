tests-issue 198
testIssue198C

	"https://github.com/dalehenrich/Rowan/issues/198"

	"Need to make sure that classes moved between packages (unrelated to new class versions) are processed correctly"

	| projectName projectNames packageName1 packageName2 className1 className2 className3 theClass 
		theSubclass theUnrelatedMovedClass projectDefinition theClassDefinition 
		packageDefinition1 packageDefinition2 theUnrelatedMovedClassDefinition |

	projectName := 'Issue198Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue198-Core'.
	packageName2 := 'Issue198-Deprecated'.
	className1 := 'Issue198Class'.
	className2 := 'Issue198Subclass'.
	className3 := 'Issue198UnrelatedMovedClass'.

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

	theUnrelatedMovedClass := Object
		rwSubclass: className3
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

	theUnrelatedMovedClassDefinition := packageDefinition1 classDefinitions at: className3.
	packageDefinition1 classDefinitions removeKey: theUnrelatedMovedClassDefinition key.
	packageDefinition2 classDefinitions at: theUnrelatedMovedClassDefinition key put: theUnrelatedMovedClassDefinition.	"move unrelated moved class to different package"

	Rowan projectTools load loadProjectDefinition: projectDefinition.
