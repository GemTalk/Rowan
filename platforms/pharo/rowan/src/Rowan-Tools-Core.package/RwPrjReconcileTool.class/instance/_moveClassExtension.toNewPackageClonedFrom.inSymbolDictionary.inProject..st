private
_moveClassExtension: theClassExtension toNewPackageClonedFrom: packageDef inSymbolDictionary: expectedSymDictName inProject: projectDef

	| newPackageName newPackageDef |
	newPackageName := packageDef name, '-ext-', expectedSymDictName.
	newPackageDef := projectDef 
		packageNamed: newPackageName 
		ifAbsent: [ 
			| pDef |
			pDef := RwPackageDefinition newNamed: newPackageName.
			projectDef addPackage: pDef.
			pDef ].
	packageDef removeClassExtensionDefinition: theClassExtension.
	newPackageDef addClassExtensionDefinition: theClassExtension.
	"update the method categories --- hybrid only ... but that is what we're doing"
	theClassExtension instanceMethodDefinitions values do: [:methodDef |
		methodDef protocol: '*', newPackageName ].
	theClassExtension classMethodDefinitions values do: [:methodDef |
		methodDef protocol: '*', newPackageName ].
	^ newPackageDef