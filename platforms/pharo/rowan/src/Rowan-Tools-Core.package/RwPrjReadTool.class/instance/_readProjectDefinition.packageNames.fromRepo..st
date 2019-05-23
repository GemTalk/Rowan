private
_readProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo

	projectDefinition packageNames do: [:packageName | 
		(packageNames includes: packageName) 
			ifFalse: [ 
				"deleted package"
				projectDefinition removePackageNamed: packageName ] ].
	packageNames
		do: [ :packageName | 
			| structureName reader packageDefinition |
			structureName := packageName , repo packageExtension.
			reader := repo reader.
			reader
				packageStructure: (RwCypressPackageStructure named: structureName);
				packageDirectory:
						(reader fileUtils
								directoryFromPath: structureName
								relativeTo: repo directoryPath);
				readPackageStructure.
			packageDefinition := reader packageStructure rwSnapshot.
			projectDefinition addOrUpdatePackage: packageDefinition ].
	projectDefinition propertyAt: RwLoadedProject _projectDefinitionSourceKey put: RwLoadedProject _projectDiskDefinitionSourceValue.
	^ projectDefinition