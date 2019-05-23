tests-issue 199
testIssue199

	"https://github.com/dalehenrich/Rowan/issues/199"

	"rename superclass of a class"

	| projectName projectNames packageName1  className1 className2 className3 theClass theSubclass projectDefinition theClassDefinition 
		theSubclassDefinition packageDefinition1 |

	projectName := 'Issue199Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue199-Core'.
	className1 := 'Issue199Class'.
	className2 := 'Issue199Subclass'.
	className3 := 'Issue199ClassNew'.

	self
		_loadProjectDefinition: projectName
		packageNames: {packageName1}
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

	theClassDefinition := packageDefinition1 classDefinitions at: className1.
	packageDefinition1 classDefinitions removeKey: theClassDefinition key.
	theClassDefinition name: className3.
	packageDefinition1 addClassDefinition: theClassDefinition.										"rename className1 to className3"

	theSubclassDefinition := packageDefinition1 classDefinitions at: className2.
	theSubclassDefinition superclassName: className3.												"change className2 superclass to className3"

	Rowan projectTools load loadProjectDefinition: projectDefinition.
