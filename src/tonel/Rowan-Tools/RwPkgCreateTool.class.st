Class {
	#name : 'RwPkgCreateTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
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

{ #category : 'smalltalk api' }
RwPkgCreateTool >> createPackagesNamed: packageNames forProjectNamed: projectName doCommit: doCommit commitMsg: commitMsg [
  | projectSpec |
  projectSpec := Rowan
    projectNamed: projectName
    ifAbsent: [ 
      self
        error:
          'No project named ' , projectName printString , ' found in the registry' ].
  ^ self
    createPackagesNamed: packageNames
    forSpecification: projectSpec
    doCommit: doCommit
    commitMsg: commitMsg
]

{ #category : 'smalltalk api' }
RwPkgCreateTool >> createPackagesNamed: packageNames forSpecification: projectSpec doCommit: doCommit commitMsg: commitMsg [

	| originalPackageNames newPackageNames newPackageSet projectName |
	projectName := projectSpec specName.
	self _createLoadedPackagesNamed: packageNames.
	originalPackageNames := projectSpec packageNamesInLoadOrder.
	projectSpec packageNamesInLoadOrder: packageNames.
	newPackageNames := packageNames
		reject: [ :each | originalPackageNames includes: each ].
	newPackageSet := RwPackageSetDefinition new.
	newPackageNames
		do: [ :packageName | 
			"write empty packages to disk for each of new packages"
			newPackageSet
				addPackage:
					(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: Dictionary new
						classExtensions: Dictionary new) ].
	Rowan projectTools write
		writePackageSet: newPackageSet
		specification: projectSpec.
	doCommit
		ifFalse: [ ^ projectSpec ].
	^ Rowan repositoryTools commit
		commitRepositoryForSpecification: projectSpec
		message: commitMsg
]

{ #category : 'smalltalk api' }
RwPkgCreateTool >> createToLoadPackagesNamed: toLoadPackageNames forProjectNamed: projectName [

	| projectSpec existingPackageNames toLoad |
	projectSpec := Rowan
		projectNamed: projectName
		ifAbsent: [ 
			self
				error:
					'No project named ' , projectName printString , ' found in the registry' ].
	existingPackageNames := projectSpec packageNamesInLoadOrder.
	toLoad := toLoadPackageNames
		select: [ :each | existingPackageNames includes: each ].
	projectSpec toLoadPackageNames: toLoad
]
