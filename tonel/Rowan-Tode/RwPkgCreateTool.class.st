Class {
	#name : 'RwPkgCreateTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tode'
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
  originalPackageNames := projectSpec packageNames.
  projectSpec packageNames: packageNames.
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
  existingPackageNames := projectSpec packageNames.
  toLoad := toLoadPackageNames
    select: [ :each | existingPackageNames includes: each ].
  projectSpec toLoadPackageNames: toLoad
]

{ #category : 'man page' }
RwPkgCreateTool >> manPage [
  ^ TDManPage
    commandName: 'create'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  pkg create - Create packages
SYNOPSIS
  pkg create [( --commit[=<commit-message>] | [--toLoad] ) --project=<project-name>]/
             <package-name>...

DESCRIPTION
  If the --project option is not specified, then the packages will be created in the image
  as an empty Loaded Package, but will not be connected to any project or repository.

  If the --project is specified, the list of package names REPLACES the list of package names
  in the project spec. The list must always be in package load order. The newly created 
  packages are saved to the project repository. If the --commit option is specified a repository
  commit is performed. The --commit option only applies when the --project option has been 
  specified.

  If the --toLoad option is specified then the packageNamesToLoad list is updated and each of
  named packages must already exist. The --commit option does not apply when the --toLoad option
  is used as the only changes are made to the in-image project spec. To save changes to a project
  spec, use the `prj spec` command.

EXAMPLES
  pkg create --commit=`update` --project=Foo Foo-Core Foo-Tests
  pkg create --commit=`update` --toLoad --project=Foo Foo-Core

SEE ALSO
  prj spec --export Foo
  prj --help spec
'
]
