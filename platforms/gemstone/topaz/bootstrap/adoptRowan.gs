# Install Rowan, Cypress, STON, and Tonel using Rowan to adopt the existing classes and extension
#  methods into the correct package structure
  run
 	| projectSetDefinition gitRepoPath packageCreateTool projectLoadTool loadedProjectInfo |
	projectSetDefinition := RwProjectSetDefinition new.
	gitRepoPath := '$ROWAN_PROJECTS_HOME/Rowan'.
	{
		{'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/Rowan.ston'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/Cypress/specs/Cypress_SystemUser.ston'. 'Default'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/STON/specs/STON_SystemUser.ston'. 'Bootstrap'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/Tonel/specs/Tonel_SystemUser.ston'. 'Bootstrap'}.
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

	projectSetDefinition projects
		do: [:projectDefinition |
			| audit projectName |
			projectName := projectDefinition name.
			GsFile gciLogServer: '---Auditing project: ', projectName printString.
			audit := Rowan projectTools audit auditForProjectNamed: projectName.
			GsFile gciLogServer: '	-- audit finished '. 
			audit isEmpty ifFalse: [ self error: 'Post load Rowan audit failed for project ', projectName printString ] ]
	
%
  commit

run
	"reload the projects, so that the image source matches the packaged source exactly ... 
		defer reconciling the added lines in .gs files relative to tonel source to a later
		date"
	#( 'Rowan' 'STON' 'Cypress' 'Tonel' )
		do: [:projectName |
		"make sure test group is loaded ... include deprecated packages for now"
		[ 
		| groupNames |
		groupNames := #('tests' 'deprecated' 'jadeServer').
		Rowan projectTools load
			loadProjectNamed: projectName
			withGroupNames: groupNames ]
				on: CompileWarning do: [:ex | ex resume ] ].
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

# set rowanCompile to true 
#
run
UserGlobals at: #rowanCompile put: true.
System commit
%
  commit
  logout
