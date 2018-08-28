  set u SystemUser p swordfish
  login

# set rowanCompile to true 
#
  run
  UserGlobals at: #rowanCompile put: true.
  System commit
%

# Bootstrap Rowan into image
#		Create Rowan Symbol Dictionaries
  run
	| session symbolList |
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	#( #RowanKernel #RowanLoader #RowanTools)
		do: [:symbolName | 
			| newDict size |
			newDict := SymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			size := System myUserProfile symbolList size.
			System myUserProfile insertDictionary: newDict at: size + 1 ].
%
  commit

# Bootstrap Rowan into image
#		Use Rowan to install Rowan
  run
  UserGlobals 
    at: #CypressBootstrapRowanBlock 
    put: [:symbolDictName :packageNames  |
    | packageManager repo |
    packageManager := CypressPackageManager3 new.
    repo := CypressAbstractRepository
      onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_PROJECTS_HOME/Rowan/rowan/src/'  )
      alias: ''.
    packageManager
      defaultSymbolDictionaryName: symbolDictName asSymbol.
    packageNames
      do: [ :packageName | 
        packageManager
          addResolvedReference:
            (CypressResolvedReference name: packageName repository: repo) ].
    packageManager loadResolvedReferences ].
%
  commit

# Define the classes needed by definitions, configurations and specifications
  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('Rowan-Kernel' 'Rowan-Url-Core').
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanTools'
    value: #('Rowan-Definitions' 'Rowan-Configurations' 'Rowan-Specifications' 
	).	"Rowan Definitions, Configurations and Specifications"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('GemStone-Interactions-Core' 'Rowan-Url-3215' 
      'Rowan-Core' 'Rowan-GemStone-Core' 'Rowan-Cypress-Core' 'Rowan-Core-Definitions-Extensions'
      'Rowan-Services-Core' 'Rowan-Url-Extensions'
      'Rowan-Services-Extensions'
	).	"Populate with Rowan implementation classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanTools'
    value: #('Rowan-Tools-Core' 'Rowan-Tools-Deprecated' 'Rowan-Definitions-Deprecated'
	'Rowan-Cypress-Definitions' 'Rowan-GemStone-Definitions' 
	'Rowan-GemStone-Specifications'
	).	"Rowan Tools"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('Rowan-Tools-Extensions' 'Rowan-Deprecated' 'Rowan-Tests' 'Rowan-Services-Tests'	
	).	"Tool extension methods for RowanKernel classes "
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanLoader'
    value: #('Rowan-GemStone-Loader'
	).		"GemStone Rowan loader classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('Rowan-GemStone-Loader-Extensions'
	).	"Extension methods in non-loader classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'Globals'
    value: #('GemStone-Interactions-Kernel' 'Rowan-GemStone-Kernel' 'Rowan-Cypress-Kernel' 
      'Rowan-Tools-Kernel' 
      'Rowan-GemStone-3215'
	).		"Extension methods for GemStone kernel classes"
%
  commit

  run
  CypressBootstrapRowanBlock
    value: 'UserGlobals'
    value: #( 'Rowan-JadeServer').           "install JadeServer classes"
%
  commit


  run
  UserGlobals removeKey: #CypressBootstrapRowanBlock.
%
  commit

# Install Rowan, Cypress, STON, and Tonel using Rowan to adopt the existing classes and extension
#  methods into the correct package structure
  run
 	| projectSetDefinition gitRepoPath packageCreateTool projectLoadTool loadedProjectInfo |
	projectSetDefinition := RwProjectSetDefinition new.
	gitRepoPath := '$ROWAN_PROJECTS_HOME/Rowan'.
	{
		{'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/Rowan.ston'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/cypress/specs/Cypress_SystemUser.ston'. 'Default'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/ston/specs/STON_SystemUser.ston'. 'Bootstrap'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/tonel/specs/Tonel_SystemUser.ston'. 'Bootstrap'}.
	} 
	do: [:ar |
		"Read project and packages from disk, creating a projectSetDefinition with all 4 projects"
		| specification specUrl readTool |
		specUrl := ar at: 1.
		specification := RwSpecification fromUrl: specUrl.
		specification
			repositoryRootPath: gitRepoPath;
			repositoryUrl: 'cypress:' , gitRepoPath , '/' , specification repoPath , '/';
			register. "Create each of the loaded projects"
		readTool := Rowan projectTools read.
		ar size = 1
			ifTrue: [
				| theProjectSetDefinition |
				theProjectSetDefinition := readTool readProjectSetForProjectNamed: specification specName.
				theProjectSetDefinition
					do: [:projectDefinition |
						projectSetDefinition addProject: projectDefinition ].
				projectSetDefinition properties: theProjectSetDefinition properties ]
			ifFalse: [
				| configName |
				configName := ar at: 2.
				(readTool readProjectSetForProjectNamed: specification specName withConfiguration: configName)
					do: [:projectDefinition |
						projectSetDefinition addProject: projectDefinition ] ] ].

	loadedProjectInfo := projectSetDefinition properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			projectName = 'Rowan'
				ifTrue: [ 
					"install the packageMapSpecs for this load into the specification prior to the load"
					| projectDefinition spec gemstoneSpec thePackageMapSpecs |
					projectDefinition := projectSetDefinition projectNamed: projectName ifAbsent: [].
					spec := projectDefinition specification.
					thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
					gemstoneSpec := spec platformSpec at: 'gemstone'.
					(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
						ifNotNil: [:name | gemstoneSpec defaultSymbolDictName: name ].
					(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
						ifNotNil: [:boolean | 
							gemstoneSpec defaultUseSessionMethodsForExtensions: boolean  ].
					(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
						ifNotNil: [:map | gemstoneSpec packageNameToPlatformPropertiesMap: map] ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: 'RowanKernel'.
	Rowan image newOrExistingSymbolDictionaryNamed: 'RowanLoader'.
	Rowan image newOrExistingSymbolDictionaryNamed: 'RowanTools'.

	packageCreateTool := Rowan packageTools create.
	projectSetDefinition projects 
		do: [:projectDefinition |
			"The loaded project was created by the earlier #register,
				traverse the package definitions and create loaded packages for each"
			| specification projectName |
			projectName := projectDefinition name.
			specification := (Rowan image loadedProjectNamed: projectName) specification.
			projectDefinition packageNames
				do: [:packageName |
					packageCreateTool createLoadedPackageNamed: packageName inProjectNamed: projectName ] ].

	"Adopt the project set definition"
	Rowan projectTools adopt adoptProjectSetDefinition: projectSetDefinition.

	projectLoadTool := Rowan projectTools load.

	projectSetDefinition projects 
		do: [:projectDefinition |
			"make sure that the loaded SHA is set for each project"
			projectLoadTool specification: projectDefinition specification.
			projectDefinition specification updateLoadedCommitIdForTool: projectLoadTool.
			projectDefinition name = 'Rowan'
				ifTrue: [
					(loadedProjectInfo at: projectDefinition name ifAbsent: [])
						ifNotNil: [:map |
							projectDefinition specification imageSpec
								loadedConfigurationNames: (map at: 'loadedConfigurationNames');
								loadedGroupNames: (map at: 'loadedGroupNames') ] ] ].

	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		"mark projects and packages not dirty"
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].
	

  true
    ifTrue: [
      | incorrectlyPackaged |
      "quick and dirty validation that Rowan loaded with Rowan is correct"
      incorrectlyPackaged := (ClassOrganizer new classes 
	select: [:cl | 
		(cl name asString beginsWith: 'Rw') 
			or: [cl name asString beginsWith: 'Rowan' ]])
	select: [:cl | 
		(Rowan image loadedClassNamed: cl name asString ifAbsent: [])
			handle ~~ cl ].
      incorrectlyPackaged isEmpty ifFalse: [ self error: 'Rowan is not correctly packaged' ] ].
%
  commit

# Install Rowan class in Published symbol dict, so it is availailable to all users
# 
   run
  | rowanAssoc |
  rowanAssoc := RowanKernel associationAt: #Rowan.
  Published add: rowanAssoc.
%
  commit

  logout
  set u DataCurator p swordfish
  login

# set rowanCompile to true for DataCurator
#
run
UserGlobals at: #rowanCompile put: true.
System commit
%
  commit
  logout

