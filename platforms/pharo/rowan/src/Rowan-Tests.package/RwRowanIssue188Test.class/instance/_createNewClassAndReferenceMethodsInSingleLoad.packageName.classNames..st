private
_createNewClassAndReferenceMethodsInSingleLoad: projectName packageName: packageName classNames: classNames

	"
	Setup new class and new class reference:
		In a single load:
			1. Create new class C
			2. add method x in B to return class C
	"

	| projectDefinition packageDefinition classDefinition |

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames at: 3)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition addClassDefinition: classDefinition.

	(packageDefinition classDefinitions at: (classNames at: 2))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'x'
						protocol: 'accessing'
						source: 'x ^ ', (classNames at: 3)).

	Rowan projectTools load loadProjectDefinition: projectDefinition
