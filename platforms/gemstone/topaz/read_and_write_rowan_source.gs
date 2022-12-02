#!//usr/local/bin/smalltalk/gemstone/topaz
#
#	read project source from rowan/source and then write project source for loaded projects in a 3.5.0 image
#
#	expect to be logged in ... no state changes are made by this script 
#

expectvalue true
run
	| projectSetModification visitor topazFileNameMap repositoryRoot packageCreateTool projectLoadTool auditFailures gsFileName platformConditionalAttributes370 repositoryRoot370 |

false ifTrue: [ 
	Rowan projectTools trace startTracing 
].
  GsFile gciLogServer: '--- read and write project source RowanV2'.
	platformConditionalAttributes370 := {
		'common'.
		'gemstone'.
		'gemstone-kernel'.
		'3.7.0' asRwGemStoneVersionNumber.	"want to read 3.7.0 packages"
	}.
	repositoryRoot370 := '$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/3.7.0/project_src_v2' asFileReference.
	repositoryRoot370 ensureCreateDirectory.
false ifTrue: [ 
	repositoryRoot370 deleteAllChildren.
 ].
	{
		{
		'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/Rowan.ston'.
			'$ROWAN_PROJECTS_HOME'.
			platformConditionalAttributes370.
			repositoryRoot370.
			'RowanV2'.
			(Dictionary new 
				at: 'Rowan' 
					put: 
						{
							('stubs' -> {'Rowan-GemStone-Kernel-Stubs-36x' }) .
							('tonel' -> 
								{ 
									'Rowan-Tonel-Core' .
									'Rowan-Tonel-GemStone-Kernel-32x' .
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
		| configNames groupNames resolvedProject theProjectSetDefinition loadSpecUrl loadSpec projectsHome conditionalAttributes theRepositoryRoot specialCaseDict excludedPackages logCreation |
		loadSpecUrl := ar at: 1.
		projectsHome := ar at: 2.
		conditionalAttributes := (ar at: 3) copy.
		theRepositoryRoot := ar at: 4.
		gsFileName := ar at: 5.
		specialCaseDict := ar at: 6.
		logCreation := ar at: 7.
		loadSpec := RwSpecification fromUrl: loadSpecUrl.
		conditionalAttributes addAll: loadSpec customConditionalAttributes.
		resolvedProject := loadSpec
			projectsHome: projectsHome;
			resolve.
		theProjectSetDefinition :=  resolvedProject readProjectSet: conditionalAttributes.
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
		GsFile gciLogServer: 'LoadSpec: ', gsFileName, '.gs (', (conditionalAttributes at: 4) printString, ')'.

		projectSetModification := theProjectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
		visitor := RwGsModificationTopazWriterVisitorV2 new
			logCreation: logCreation;
			repositoryRootPath: theRepositoryRoot;
			topazFilenamePackageNamesMap: topazFileNameMap;
			yourself.
		visitor visit: projectSetModification ].
	true
%
