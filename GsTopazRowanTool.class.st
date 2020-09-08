Class {
	#name : 'GsTopazRowanTool',
	#superclass : 'GemStoneRowanTool',
	#category : 'GemStone-Rowan-Tools'
}

{ #category : 'private' }
GsTopazRowanTool >> _currentTopazPackageKey [
	^ #'RowanTopazCurrentPackageName'
]

{ #category : 'private' }
GsTopazRowanTool >> _currentTopazProjectKey [
	^#'RowanTopazCurrentProjectName'
]

{ #category : 'private' }
GsTopazRowanTool >> _defaultComponentCondition [
	^ 'common'
]

{ #category : 'private' }
GsTopazRowanTool >> _defaultComponentName [
	^ 'Core'
]

{ #category : 'private' }
GsTopazRowanTool >> _defaultDefaultSymbolDictionaryName [
	^ 'UserGlobals'
]

{ #category : 'private' }
GsTopazRowanTool >> _defaultPackageConvention [
	^ 'Rowan'
]

{ #category : 'private' }
GsTopazRowanTool >> _defaultPackageFormat [
	^ 'tonel'
]

{ #category : 'private' }
GsTopazRowanTool >> _defaultProjectType [
	^ #'disk'
]

{ #category : 'reports' }
GsTopazRowanTool >> _unmanagedClassesAndMethodsReportForClass: aClass details: details packaged: packagedReport unpackaged: unpackagedReport [
	| unpackagedName packageName methodDetails unpackagedClassLogged packagedClassLogged |
	unpackagedName := Rowan unpackagedName.

	details at: 'class' put: aClass.
	packageName := aClass rowanPackageName.
	details at: 'packageName' put: packageName.
	packagedClassLogged := unpackagedClassLogged := false.
	packageName = unpackagedName
		ifTrue: [ 
			unpackagedClassLogged := true.
			unpackagedReport
				nextPutAll: '	Class: ' , aClass name asString;
				nextPutAll: ' unpackaged';
				lf ]
		ifFalse: [ 
			packagedClassLogged := true.
			packagedReport
				nextPutAll: '	Class: ' , aClass name asString;
				nextPutAll: ' packaged in ' , packageName;
				lf ].
	methodDetails := details
		at: 'instanceMethodDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	aClass
		methodsDo: [ :selector :method | 
			| methodDetail |
			methodDetail := methodDetails
				at: selector asString
				put: StringKeyValueDictionary new.
			packageName := method rowanPackageName.
			methodDetail
				at: 'method' put: method;
				at: 'packageName' put: packageName.
			packageName = unpackagedName
				ifTrue: [ 
					unpackagedClassLogged
						ifFalse: [ 
							unpackagedClassLogged := true.
							unpackagedReport
								nextPutAll: '	Class: ' , aClass name asString;
								lf ].
					unpackagedReport
						nextPutAll: '		Method: ' , selector asString;
						nextPutAll: ' unpackaged';
						lf ]
				ifFalse: [ 
					packagedClassLogged
						ifFalse: [ 
							packagedClassLogged := true.
							packagedReport
								nextPutAll: '	Class: ' , aClass name asString;
								lf ].
					packagedReport
						nextPutAll: '		Method: ' , selector asString;
						nextPutAll: ' packaged in ' , packageName;
						lf ] ].
	methodDetails := details
		at: 'classMethodDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	aClass class
		methodsDo: [ :selector :method | 
			| methodDetail |
			methodDetail := methodDetails
				at: selector asString
				put: StringKeyValueDictionary new.
			packageName := method rowanPackageName.
			methodDetail
				at: 'method' put: method;
				at: 'packageName' put: packageName.
			packageName = unpackagedName
				ifTrue: [ 
					unpackagedReport
						nextPutAll: '		Class method: ' , selector asString;
						nextPutAll: ' unpackaged';
						lf ]
				ifFalse: [ 
					packagedReport
						nextPutAll: '		Class method: ' , selector asString;
						nextPutAll: ' packaged in ' , packageName;
						lf ] ]
]

{ #category : 'reports' }
GsTopazRowanTool >> _unmanagedClassesAndMethodsReportForSymbolDictionaries: symbolDictionaries details: details packaged: packagedReport unpackaged: unpackagedReport [
	| unpackagedName |
	unpackagedName := Rowan unpackagedName.
	symbolDictionaries
		do: [ :symbolDictionary | 
			| symbolDictionaryDetails |
			symbolDictionaryDetails := details
				at: symbolDictionary name asString
				ifAbsentPut: [ StringKeyValueDictionary new ].
			symbolDictionaryDetails at: 'symbolDictionary' put: symbolDictionary.
			self
				_unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary
				details: symbolDictionaryDetails
				packaged: packagedReport
				unpackaged: unpackagedReport ]
]

{ #category : 'reports' }
GsTopazRowanTool >> _unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary details: details packaged: packagedReport unpackaged: unpackagedReport [
	| unpackagedName classDetails |
	unpackagedName := Rowan unpackagedName.
	classDetails := details
		at: 'classDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	self
		_classesIn: symbolDictionary
		do: [ :aClass | 
			| theClassDetails |
			theClassDetails := classDetails
				at: aClass name
				ifAbsentPut: [ StringKeyValueDictionary new ].
			self
				_unmanagedClassesAndMethodsReportForClass: aClass
				details: theClassDetails
				packaged: packagedReport
				unpackaged: unpackagedReport ]
]

{ #category : 'packages' }
GsTopazRowanTool >> addNewPackageNamed: packageName forProjectNamed: projectName [
	"add a new package to the named loaded project"

	^ self
		addNewPackageNamed: packageName
		forProjectNamed: projectName
		toComponentNamed: self _defaultComponentName
]

{ #category : 'packages' }
GsTopazRowanTool >> addNewPackageNamed: packageName forProjectNamed: projectName inSybolDictionaryNamed: symbolDictionaryName toComponentNamed: componentName [
	"add a new package to the named loaded project"

	^ (Rowan projectNamed: projectName)
		addNewPackageNamed: packageName
		inSybolDictionaryNamed: symbolDictionaryName
		toComponentNamed: componentName
]

{ #category : 'packages' }
GsTopazRowanTool >> addNewPackageNamed: packageName forProjectNamed: projectName toComponentNamed: componentName [
	"add a new package to the named loaded project"

	^ (Rowan projectNamed: projectName)
		addNewPackageNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'components' }
GsTopazRowanTool >> addTopLevelComponentNamed: componentName forProjectNamed: projectName [
	"Add the named component to the named project and add the component name to the load specification"

	self
		addTopLevelComponentNamed: componentName
		forProjectNamed: projectName
		condition: self _defaultComponentCondition
]

{ #category : 'components' }
GsTopazRowanTool >> addTopLevelComponentNamed: componentName forProjectNamed: projectName condition: condition [
	"Add the named component to the named project and add the component name to the load specification
		with the given condition"

	(Rowan projectNamed: projectName)
		addTopLevelComponentNamed: componentName
		condition: condition
]

{ #category : 'projects' }
GsTopazRowanTool >> createNewLoadedProject: projectName in: parentDirectory componentName: componentName packageNames: packageNames defaultSymbolDictionaryName: defaultSymbolDictionaryName [
	"Create a new loaded project with the given attributes, using the default project type, packageFormat and packageConvention.
		The project is created, written to disk, loaded into the image, and the project is set as the current topaz project. 

	Return the newly created project (instance of RwProject)"

	^ self
		createNewLoadedProject: projectName
		in: parentDirectory
		type: self _defaultProjectType
		packageFormat: self _defaultPackageFormat
		packageConvention: self _defaultPackageConvention
		componentName: componentName
		packageNames: packageNames
		defaultSymbolDictionaryName: defaultSymbolDictionaryName
]

{ #category : 'projects' }
GsTopazRowanTool >> createNewLoadedProject: projectName in: parentDirectory packageNames: packageNames [
	"Create a new loaded project with the given attributes, using the default project type, packageFormat, packageConvention,
		componentName and defaultSymbolDictionaryName.
		The project is created, written to disk, loaded into the image, and the project is set as the current topaz project. 

	Return the newly created project (instance of RwProject)"

	^ self
		createNewLoadedProject: projectName
		in: parentDirectory
		type: self _defaultProjectType
		packageFormat: self _defaultPackageFormat
		packageConvention: self _defaultPackageConvention
		componentName: self _defaultComponentName
		packageNames: packageNames
		defaultSymbolDictionaryName: self _defaultDefaultSymbolDictionaryName
]

{ #category : 'projects' }
GsTopazRowanTool >> createNewLoadedProject: projectName in: projectsHome type: repoType packageFormat: packageFormat packageConvention: packageConvention componentName: componentName packageNames: packageNames defaultSymbolDictionaryName: defaultSymbolDictionaryName [
	"Create a new loaded project with the given attributes.
		The project is created, written to disk, loaded into the image, and the project is set as the current topaz project. 

	Return the newly created project (instance of RwProject)"

	| project |
	project := (Rowan newProjectNamed: projectName)
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictionaryName;
		repoType: repoType;
		packageFormat: packageFormat;
		packageConvention: packageConvention;
		addTopLevelComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself.
	self currentTopazProjectName: projectName.
	^ project resolve write load at: 1
]

{ #category : 'packages' }
GsTopazRowanTool >> currentTopazPackageName [
	"Return the current topaz package name or nil"

	^ SessionTemps current at: self _currentTopazPackageKey otherwise: nil
]

{ #category : 'packages' }
GsTopazRowanTool >> currentTopazPackageName: packageNameOrNil [
	"Set the current topaz package name. New methods and classes created in the topaz session 
		will be added to the named package. If nil, new methods and classes will be unmanaged."

	SessionTemps current at: self _currentTopazPackageKey put: packageNameOrNil
]

{ #category : 'projects' }
GsTopazRowanTool >> currentTopazProjectName [
	"answer the name of the current project"

	^ SessionTemps current at: self _currentTopazProjectKey otherwise: nil
]

{ #category : 'projects' }
GsTopazRowanTool >> currentTopazProjectName: projectNameOrNil [
	"set the name of the current project"

	SessionTemps current at: self _currentTopazProjectKey put: projectNameOrNil
]

{ #category : 'components' }
GsTopazRowanTool >> exportComponentsForProject: projectName [
	"save the current components for the named project to disk"

	^ (Rowan projectNamed: projectName) defined resolve exportComponents
]

{ #category : 'load specs' }
GsTopazRowanTool >> exportLoadSpecificationForProject: projectName [
	"save the current load specification for the named project to disk"

	^ (Rowan projectNamed: projectName) defined resolve exportLoadSpecification
]

{ #category : 'packages' }
GsTopazRowanTool >> exportPackagesForProject: projectName [
	"save the currently modified packages in the named project to disk"

	^ (Rowan projectNamed: projectName) defined resolve exportPackages
]

{ #category : 'projects' }
GsTopazRowanTool >> exportProjectNamed: projectName inTopazFormatTo: filePath [
	"export the loaded packages in the named project to a topaz format file named filePath"

	^ (Rowan projectNamed: projectName) exportTopazFormatTo: filePath logClassCreation: true
]

{ #category : 'git support' }
GsTopazRowanTool >> gitCheckoutProject: projectName branchOrSHA: branchOrSHA [
	"do a git checkout of the given branchOrCommit for the given project"

	^ (Rowan projectNamed: projectName) gitCheckout: branchOrSHA
]

{ #category : 'git support' }
GsTopazRowanTool >> gitCommitProject: projectName commitComment: comment [
	"do a git commit of the given project with the given commit comment"

	^ (Rowan projectNamed: projectName) gitCommit: comment
]

{ #category : 'git support' }
GsTopazRowanTool >> gitCreateBranchProject: projectName branchName: branchName [
	"do a git checkout of the given branchOrCommit for the given project"

	^ (Rowan projectNamed: projectName) gitCreateBranch: branchName
]

{ #category : 'git support' }
GsTopazRowanTool >> gitLogProject: projectName [
	"Return the git log of the project"

	^ self gitLogProject: projectName logLimit: 25
]

{ #category : 'git support' }
GsTopazRowanTool >> gitLogProject: projectName logLimit: logLimit [
	"Return the git log of the project"

	^ (Rowan projectNamed: projectName) gitLog: logLimit
]

{ #category : 'git support' }
GsTopazRowanTool >> gitPullProject: projectName remote: remoteName branch: branchName [
	"do a git pull for the given projectl remote and branch"

	^ (Rowan projectNamed: projectName)
		gitPullRemote: remoteName
		branch: branchName
]

{ #category : 'git support' }
GsTopazRowanTool >> gitPushProject: projectName remote: remoteName branch: branchName [
	"do a git push for the given projectl remote and branch"

	^ (Rowan projectNamed: projectName)
		gitPushRemote: remoteName
		branch: branchName
]

{ #category : 'git support' }
GsTopazRowanTool >> gitShortStatusProject: projectName [
	"Return the git short status of the project; should be empty unless the working directory has been modified"

	^ (Rowan projectNamed: projectName) gitShortStatus
]

{ #category : 'git support' }
GsTopazRowanTool >> gitStatusProject: projectName [
	"Return the git status of the project"

	^ (Rowan projectNamed: projectName) gitStatus
]

{ #category : 'reports' }
GsTopazRowanTool >> listPackagesForProjectNamed: projectName [
	"Return a list of the currently visible projects"

	^ (Rowan projectNamed: projectName) packageNames
]

{ #category : 'reports' }
GsTopazRowanTool >> listProjects [
	"Return a list of the currently visible projects"

	^ Rowan projectNames
]

{ #category : 'projects' }
GsTopazRowanTool >> loadProjectFromUrl: loadSpecUrl projectsHome: projectsHome [
	"read the load specification from the given url; resolve the spec to clone the project (if needed) and
		read the packages from disk based on the default component names and default conditional attributes."

	^ self
		loadProjectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: nil
		customConditionalAttributes: nil
]

{ #category : 'projects' }
GsTopazRowanTool >> loadProjectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNamesOrNil customConditionalAttributes: customConditionalAttributesOrNil [
	"read the load specification from the given url; resolve the spec to clone the project (if needed) and
		read the packages from disk based on the listed component names (if nil, use the component names
		defined in load spec) and conditional attributes (if nil, use the conditional attributes defined in the
		load spec)."

	| loadSpec resolvedProject rwResolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		yourself.
	componentNamesOrNil ifNotNil: [ loadSpec componentNames: componentNamesOrNil ].
	customConditionalAttributesOrNil
		ifNotNil: [ loadSpec customConditionalAttributes: customConditionalAttributesOrNil ].
	resolvedProject := loadSpec resolve.
	rwResolvedProject := (RwResolvedProject newNamed: resolvedProject name)
		_resolvedProject: resolvedProject resolve;
		yourself.
	^ rwResolvedProject load at: 1
]

{ #category : 'classes' }
GsTopazRowanTool >> moveClass: class toPackageNamed: packageName [
	"Move class to <packageName>, whether or not it has been packaged. The methods in the class that are in the
		original package of the class are also moved to the new package. If the class was originally unpackaged,
		then only unpackaged methods (class and instance side) are moved to the new package."

	| loadedPackage |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package ' , packageName printString , ' does not exist' ].
	class rwMoveClassToPackage: packageName
]

{ #category : 'methods' }
GsTopazRowanTool >> moveMethod: method toPackageNamed: packageName [
	"Move the method into <packageName>, whether or not it has been packaged"

	| loadedPackage theBehavior |
	(theBehavior := method inClass)
		ifNil: [ self error: 'An anonymous method cannot be packaged' ].
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package ' , packageName printString , ' does not exist' ].
	theBehavior rwMoveMethod: method selector toPackage: packageName
]

{ #category : 'projects' }
GsTopazRowanTool >> reloadProjectNamed: projectName [
	"reload the named project and dependent projects from disk. The components and packages are reread from disk based on the 
		settings in the loaded load specification and then loaded into the image.

		Return the list of projects (RwProject) that were loaded."

	^ (Rowan projectNamed: projectName) loadProjectSet
]

{ #category : 'packages' }
GsTopazRowanTool >> removePackageNamed: packageName [
	"remove the package from the loaded project associated with the package"

	| loadedPackage projectName |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package ' , packageName printString , ' was not found' ].
	projectName := loadedPackage loadedProject name.
	^ self removePackageNamed: packageName fromProjectNamed: projectName
]

{ #category : 'packages' }
GsTopazRowanTool >> removePackageNamed: packageName fromProjectNamed: projectName [
	"remove the package from the named project"

	^ (Rowan projectNamed: projectName) removePackageNamed: packageName
]

{ #category : 'projects' }
GsTopazRowanTool >> unloadProjectNamed: projectName [
	"unload the named project"

	^ (Rowan projectNamed: projectName) unload
]

{ #category : 'reports' }
GsTopazRowanTool >> unmanagedClassesAndMethodsReportForClass: aClass [
	| packagedReport unpackagedReport details classDetails theClassDetails |
	packagedReport := WriteStream on: String new.
	unpackagedReport := WriteStream on: String new.
	details := GsUnmanagedClassReport new.
	classDetails := details
		at: 'classDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	theClassDetails := classDetails
		at: aClass name
		ifAbsentPut: [ StringKeyValueDictionary new ].
	self
		_unmanagedClassesAndMethodsReportForClass: aClass
		details: theClassDetails
		packaged: packagedReport
		unpackaged: unpackagedReport.
	details
		at: 'packagedReport' put: packagedReport contents;
		at: 'unpackagedReport' put: unpackagedReport contents;
		yourself.
	^ details
]

{ #category : 'reports' }
GsTopazRowanTool >> unmanagedClassesAndMethodsReportForSymbolDictionaries: symbolDictionaries [
	| packagedReport unpackagedReport details symbolDictionariesDetails |
	packagedReport := WriteStream on: String new.
	unpackagedReport := WriteStream on: String new.
	details := GsUnmanagedSymbolDictionariesReport new.
	symbolDictionariesDetails := details
		at: 'symbolDictionaryDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	self
		_unmanagedClassesAndMethodsReportForSymbolDictionaries: symbolDictionaries
		details: symbolDictionariesDetails
		packaged: packagedReport
		unpackaged: unpackagedReport.
	details
		at: 'packagedReport' put: packagedReport contents;
		at: 'unpackagedReport' put: unpackagedReport contents;
		yourself.
	^ details
]

{ #category : 'reports' }
GsTopazRowanTool >> unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary [
	| packagedReport unpackagedReport details symbolDictionariesDetails symbolDictionaryDetails |
	packagedReport := WriteStream on: String new.
	unpackagedReport := WriteStream on: String new.
	details := GsUnmanagedSymbolDictionariesReport new.
	symbolDictionariesDetails := details
		at: 'symbolDictionaryDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	symbolDictionaryDetails := symbolDictionariesDetails
		at: symbolDictionary name asString
		ifAbsentPut: [ StringKeyValueDictionary new ].
	symbolDictionaryDetails at: 'symbolDictionary' put: symbolDictionary.
	self
		_unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary
		details: symbolDictionaryDetails
		packaged: packagedReport
		unpackaged: unpackagedReport.
	details
		at: 'packagedReport' put: packagedReport contents;
		at: 'unpackagedReport' put: unpackagedReport contents;
		yourself.
	^ details
]

{ #category : 'classes' }
GsTopazRowanTool >> unpackageClass: class [
	"unpackage the the given class and all of the methods in the class (instance and class side) that are in the 
		same package as the class definition. Do nothing if the class definition is not packaged."

	Rowan projectTools browser unpackageClass: class
]

{ #category : 'methods' }
GsTopazRowanTool >> unpackageMethod: method [
	"unpackage the given method, while leaving the method installed in the image"

	| theBehavior |
	(theBehavior := method inClass)
		ifNil: [ self error: 'An anonymous method cannot be unpackaged' ].
	Rowan projectTools browser unpackageMethod: method
]
