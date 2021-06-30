#!//usr/local/bin/smalltalk/gemstone/topaz
#
#	read project source from rowan/source and then write project source for specified gemstone versions
#
# expect to be logged in ... no state changes are made by this script
#

	display classoops

  set user SystemUser p swordfish
  login

expectvalue true
run
	| platformConditionalAttributes350 platformConditionalAttributes3215 projectSetModification visitor 
		topazFileNameMap repositoryRoot350 packageCreateTool projectLoadTool platformConditionalAttributes360 
		platformConditionalAttributes361 auditFailures gsFileName repositoryRoot3215 repositoryRoot360 
		platformConditionalAttributes370 repositoryRoot370 repositoryRoot361 dbo_ROWAN_PROJECTS_HOME |

false ifTrue: [ 
	Rowan projectTools trace startTracing 
].
  GsFile gciLogServer: '--- read and write project source RowanV2'.
	platformConditionalAttributes361 := {
		'common'.
		'gemstone'.
		'gemstone-kernel'.
		'3.6.1' asRwGemStoneVersionNumber.	"want to read 3.6.1 packages"
	}.
	dbo_ROWAN_PROJECTS_HOME := '/home/dhenrich/rogue/_homes/rogue/_home/server/stones/dbo_anon_3215/git'.
	repositoryRoot361 := (dbo_ROWAN_PROJECTS_HOME, '/Rowan/platforms/gemstone/topaz/bootstrap/3.6.1') asFileReference.
	repositoryRoot361 ensureCreateDirectory.
	{
		{
			'file:',dbo_ROWAN_PROJECTS_HOME, '/Rowan/rowanV3/specs/Rowan.ston'.
			dbo_ROWAN_PROJECTS_HOME.
			platformConditionalAttributes361.
			repositoryRoot361.
			'RowanV2'.
			(Dictionary new 
				at: 'Rowan' 
					put: 
						{
							('stubs' -> {'Rowan-GemStone-Kernel-Stubs-36x' }) .
							('tonel' -> 
								{ 
									'Rowan-Tonel-Core' .
									'Rowan-Tonel-GemStone-Kernel-32-5' .
									'Rowan-Tonel-GemStone-Kernel' .
								}) .
						}; 
				at: 'STON' 
					put: 
						{
							('tonel' -> 
								{ 
									'STON-Core' .
									'STON-GemStone-Kernel'.
									'STON-GemStoneBase' .
									'STON-GemStoneCommon' .
									'STON-GemStone-Kernel36x' .
								}) .
						}; 
				yourself).
			true.
		}.
	} 
	do: [:ar |
		"Read project and packages from disk."
		| configNames loadSpecs theProjectSetDefinition loadSpecUrl 
			loadSpec projectsHome platformConditionalAttributes theRepositoryRoot 
			specialCaseDict excludedPackages logCreation |
		loadSpecUrl := ar at: 1.
		projectsHome := ar at: 2.
		platformConditionalAttributes := (ar at: 3).
		theRepositoryRoot := ar at: 4.
		gsFileName := ar at: 5.
		specialCaseDict := ar at: 6.
		logCreation := ar at: 7.
		loadSpec := RwSpecification fromUrl: loadSpecUrl.
		loadSpecs := loadSpec
			projectsHome: projectsHome;
			produce.
		loadSpecs do: [:ls | ls addCustomConditionalAttributes: #('tests' 'deprecated' 'jadeServer') ].
		theProjectSetDefinition := loadSpecs read: platformConditionalAttributes.
		theProjectSetDefinition
			do: [:projectDefinition |	
				GsFile gciLogServer: '	Project: ', projectDefinition name.
				projectDefinition packageNames sort do: [:pkgName | GsFile gciLogServer: '		', pkgName ] ].
		topazFileNameMap := Dictionary new.
		topazFileNameMap at: gsFileName put: {}.
		excludedPackages := Set new.
		theProjectSetDefinition keys do: [:projectName |
			| projectDefinition thePackages |
			"compute excludedPackages for all projects"
			projectDefinition := theProjectSetDefinition projectNamed: projectName.
			specialCaseDict 
				at: projectName
				ifPresent: [:assocList |
					assocList do: [:assoc | 
						excludedPackages addAll: assoc value.
						(topazFileNameMap at: gsFileName, '_', assoc key ifAbsentPut: [ Set new ])
							addAll: assoc value.
						GsFile gciLogServer: '	---', assoc key, ' packages ', assoc value printString ] ] ].
		GsFile gciLogServer: '	Excluded packages: ', excludedPackages asArray printString.
		theProjectSetDefinition keys do: [:projectName |
			| projectDefinition thePackages |
			"define contents of RowanV2.gs"
			projectDefinition := theProjectSetDefinition projectNamed: projectName.
			thePackages := projectDefinition packageNames.
			thePackages removeAllPresent: excludedPackages.
			thePackages do: [:packageName |
				(topazFileNameMap at: gsFileName) add: packageName ] ].
		GsFile gciLogServer: 'LoadSpec: ', gsFileName, '.gs (', (platformConditionalAttributes at: 4) printString, ')'.
		projectSetModification := theProjectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
		visitor := RwGsModificationTopazWriterVisitorV2 new
			logCreation: logCreation;
			repositoryRootPath: theRepositoryRoot;
			topazFilenamePackageNamesMap: topazFileNameMap;
			yourself.
		visitor visit: projectSetModification ].
	true
%