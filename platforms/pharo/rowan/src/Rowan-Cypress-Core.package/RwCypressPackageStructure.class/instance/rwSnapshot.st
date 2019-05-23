snapshotting
rwSnapshot

	| rwPackageDefinition |
	rwPackageDefinition := RwPackageDefinition new
		properties:
			(Dictionary new
				at: 'name' put: self packageName;
				yourself).
	self classes
		do: [ :classStructure | 
			| rwClassDef |
			rwClassDef := RwClassDefinition fromCypressClassStructure: classStructure.
			rwPackageDefinition addClassDefinition: rwClassDef ].
	self extensions
		do: [ :classStructure | 
			| rwClassExtensionDef |
			rwClassExtensionDef := RwClassExtensionDefinition
				newForClassNamed: classStructure className.
			(classStructure instanceMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					rwClassExtensionDef
						addInstanceMethodDefinition:
							(RwMethodDefinition fromCypressMethod: methodStructure) ].
			(classStructure classMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					rwClassExtensionDef
						addClassMethodDefinition:
							(RwMethodDefinition fromCypressMethod: methodStructure) ].
			rwPackageDefinition addClassExtensionDefinition: rwClassExtensionDef ].
	^ rwPackageDefinition