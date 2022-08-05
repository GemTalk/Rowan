Class {
	#name : 'RowanBootstrapGsWriter',
	#superclass : 'Object',
	#instVars : [
		'logStream'
	],
	#category : 'RowanBootstrapGsWriter-Core'
}

{ #category : 'instance creation' }
RowanBootstrapGsWriter class >> logStream: aStream [
	^ self new
		logStream: aStream;
		yourself
]

{ #category : 'logging' }
RowanBootstrapGsWriter >> log: aMessage [
	self logStream
		nextPutAll: aMessage;
		lf
]

{ #category : 'accessing' }
RowanBootstrapGsWriter >> logStream [
	^logStream
]

{ #category : 'accessing' }
RowanBootstrapGsWriter >> logStream: object [
	logStream := object
]

{ #category : 'writing' }
RowanBootstrapGsWriter >> write: loadSpecUrl projectsHome: projectsHome platformConditionalAttributes: platformConditionalAttributes theRepositoryRoot: theRepositoryRoot gsFileName: gsFileName excludedProjectNames: excludedProjectNames specialCaseDict: specialCaseDict logCreation: logCreation [
	"Read project and packages from disk."

	| resolvedProject theProjectSetDefinition loadSpec excludedPackages topazFileNameMap projectSetModification visitor |
	loadSpec := RwSpecification fromUrl: loadSpecUrl.
	resolvedProject := loadSpec
		projectsHome: projectsHome;
		resolveProject.
	theProjectSetDefinition := resolvedProject
		readProjectSet: loadSpec customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	theProjectSetDefinition
		do: [ :projectDefinition | 
			self log: '	Project: ' , projectDefinition name.
			projectDefinition packageNames sort
				do: [ :pkgName | self log: '		' , pkgName ] ].
	topazFileNameMap := Dictionary new.
	topazFileNameMap at: gsFileName put: {}.
	excludedPackages := Set new.
	theProjectSetDefinition keys
		do: [ :projectName | 
			| projectDefinition |
			"compute excludedPackages for all projects"
			projectDefinition := theProjectSetDefinition projectNamed: projectName.
			specialCaseDict
				at: projectName
				ifPresent: [ :assocList | 
					assocList
						do: [ :assoc | 
							excludedPackages addAll: assoc value.
							(topazFileNameMap
								at: gsFileName , '_' , assoc key
								ifAbsentPut: [ Set new ]) addAll: assoc value.
							self log: '	---' , assoc key , ' packages ' , assoc value printString ] ] ].
	self log: '	Excluded packages: ' , excludedPackages asArray printString.
	theProjectSetDefinition keys
		do: [ :projectName | 
			(excludedProjectNames includes: projectName)
				ifFalse: [ 
					| projectDefinition thePackages |
					"define contents of RowanV2.gs"
					projectDefinition := theProjectSetDefinition projectNamed: projectName.
					thePackages := projectDefinition packageNames.
					thePackages removeAllPresent: excludedPackages.
					thePackages
						do: [ :packageName | (topazFileNameMap at: gsFileName) add: packageName ] ] ].
	GsFile
		gciLogServer:
			'LoadSpec: ' , gsFileName , '.gs ('
				, (platformConditionalAttributes at: 4) printString , ')'.

	projectSetModification := theProjectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		logCreation: logCreation;
		repositoryRootPath: theRepositoryRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		yourself.
	visitor visit: projectSetModification
]

{ #category : 'writing' }
RowanBootstrapGsWriter >> write: loadSpecUrl projectsHome: projectsHome platformConditionalAttributes: platformConditionalAttributes theRepositoryRoot: theRepositoryRoot gsFileName: gsFileName specialCaseDict: specialCaseDict logCreation: logCreation [
	self
		write: loadSpecUrl
		projectsHome: projectsHome
		platformConditionalAttributes: platformConditionalAttributes
		theRepositoryRoot: theRepositoryRoot
		gsFileName: gsFileName
		excludedProjectNames: #()
		specialCaseDict: specialCaseDict
		logCreation: logCreation
]
