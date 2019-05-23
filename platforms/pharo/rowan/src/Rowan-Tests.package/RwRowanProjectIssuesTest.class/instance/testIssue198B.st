tests-issue 198
testIssue198B

	"https://github.com/dalehenrich/Rowan/issues/198"

	"subclass of class with new version, which is referenced by a method in a third class, is moved to a different package ... 
		failure occurs when the new superclass version and move to different package occurs in one atomic load. "

	| projectName projectNames packageName1 packageName2 className1 className2 className3 theClass 
		theSubclass theReferenceClass projectDefinition theClassDefinition theSubclassDefinition 
		packageDefinition1 packageDefinition2 theReferenceClassDefinition |

	projectName := 'Issue198Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue198-Core'.
	packageName2 := 'Issue198-Deprecated'.
	className1 := 'Issue198Class'.
	className2 := 'Issue198Subclass'.
	className3 := 'Issue198ReferenceClass'.

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

	theReferenceClass := Object
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

	theSubclassDefinition := packageDefinition1 classDefinitions at: className2.
	packageDefinition1 classDefinitions removeKey: theSubclassDefinition key.
	packageDefinition2 classDefinitions at: theSubclassDefinition key put: theSubclassDefinition.	"move subclass to different package"

	theReferenceClassDefinition := packageDefinition1 classDefinitions at: className3.
	theReferenceClassDefinition addInstanceMethodDefinition: 
		(RwMethodDefinition
						newForSelector: #'boom'
						protocol: 'accessing'
						source: 'boom ^ ', className2).																"add method referencing moved class"

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	self assert: (Rowan globalNamed: className3) new boom == (Rowan globalNamed: className2).
	self assert: theSubclass rowanPackageName = packageName2.
