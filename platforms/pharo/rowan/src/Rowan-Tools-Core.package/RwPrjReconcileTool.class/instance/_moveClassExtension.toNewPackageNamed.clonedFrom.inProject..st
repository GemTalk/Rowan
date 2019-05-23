private
_moveClassExtension: theClassExtension toNewPackageNamed: newPackageName clonedFrom: packageDef inProject: projectDef

	| newPackageDef |
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