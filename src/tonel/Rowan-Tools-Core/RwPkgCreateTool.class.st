Class {
	#name : 'RwPkgCreateTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPkgCreateTool >> _createLoadedPackagesNamed: packageNames [
  packageNames
    do: [ :packageName | RwGsLoadedPackage existingOrNewPackageNamed: packageName ]
]

{ #category : 'smalltalk api' }
RwPkgCreateTool >> createPackagesNamed: packageNames [
  "no project, no repository can only work on loaded packages"

  ^ self _createLoadedPackagesNamed: packageNames
]
