Extension { #name : 'RwPrjAdoptTool' }

{ #category : '*rowan-tools-corev2' }
RwPrjAdoptTool >> _adoptProjectProjectsInProjectSet: projectSetDefinition [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	| auditFailures reAudit theProjectSetDefinition tracer wasTracing adoptErrors |
	theProjectSetDefinition := RwProjectSetDefinition new.
	tracer := Rowan projectTools trace.
	wasTracing := tracer isTracing.
	tracer startTracing.
	projectSetDefinition projects
		do: [ :resolvedProject | 
			| resolvedProject_copy  |
			"make a copy of the resolvedProject so we can safely nuke all of the project packages"
			resolvedProject_copy := resolvedProject copy.
			tracer trace: 'Project: ' , resolvedProject_copy name.
			resolvedProject_copy packageNames
				do: [ :packageName | 
					"wipe out package contents, so we can load *empty* project and packages, that will be adopted in next step"
					tracer trace: '	' , packageName.
					(resolvedProject_copy packageNamed: packageName)
						classDefinitions: Dictionary new;
						classExtensions: Dictionary new;
						yourself ].
			theProjectSetDefinition addProject: resolvedProject_copy ].
	wasTracing
		ifFalse: [ 
			"reduce noise if tracing not already set"
			tracer stopTracing ].
	Rowan projectTools loadV2 loadProjectSetDefinition: theProjectSetDefinition.	"Load the project shell -- project and empty packages"
	wasTracing
		ifFalse: [ tracer startTracing ].
	[ 
	"Adopt the project set definition ... 
		Log and ignore any missing method or missing classes encountered as they may not be
		present in the .gs bootstrap file for the proejct ... The will be created when we
		reload the project a little bit later on."
	adoptErrors := false.
	Rowan projectTools adopt adoptProjectSetDefinition: projectSetDefinition ]
		on:
			RwAdoptMissingMethodErrorNotification , RwAdoptMissingClassErrorNotification
		do: [ :ex | 
			ex
				methodErrorDo: [ 
					adoptErrors := true.
					tracer
						trace:
							'Missing loaded method ' , ex methodPrintString
								, ' encountered during adopt' ]
				classErrorDo: [ 
					adoptErrors := true.
					tracer
						trace:
							'Missing loaded class ' , ex className , ' encountered during adopt' ].
			ex resume: nil ].

	projectSetDefinition deriveLoadedThings
		do: [ :loadedProject | 
			"mark projects and packages not dirty"
			loadedProject markNotDirty.
			loadedProject loadedPackages
				valuesDo: [ :loadedPackage | loadedPackage markNotDirty ] ].

	System commit.	"save pre-audit state, so that audit errors can be examined"

	reAudit := true.	"kick off the first audit"
	[ reAudit ]
		whileTrue: [ 
			auditFailures := {}.
			reAudit := false.
			projectSetDefinition projects
				do: [ :projectDefinition | 
					| audit projectName |
					projectName := projectDefinition name.
					audit := Rowan projectTools audit auditForProjectNamed: projectName.
					tracer trace: '	-- audit finished '.
					audit isEmpty
						ifFalse: [ 
							"we must have had an audit failure that was not handled"
							reAudit := false.
							tracer trace: 'FAILED AUDIT: ' , projectName.
							auditFailures add: projectName ] ].
			reAudit
				ifTrue: [ tracer trace: 'RERUN AUDIT' ] ].
	wasTracing
		ifFalse: [ tracer stopTracing ].
	auditFailures isEmpty
		ifFalse: [ 
			self
				error:
					'Post load Rowan audit failed for projects ' , auditFailures printString ].
	adoptErrors
		ifTrue: [ self error: 'Missing methods during adopt step, Check log for details' ]
]

{ #category : '*rowan-tools-corev2' }
RwPrjAdoptTool >> adoptProjectFromUrl: specUrl diskUrl: diskUrl projectsHome: projectsHome [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	| loadSpec projectSetDefinition |
	loadSpec := RwSpecification fromUrl: specUrl.
	projectSetDefinition := loadSpec
		diskUrl: diskUrl;
		projectsHome: projectsHome;
		readProjectSet.

	Rowan projectTools adopt
		_adoptProjectProjectsInProjectSet: projectSetDefinition
]

{ #category : '*rowan-tools-corev2' }
RwPrjAdoptTool >> adoptProjectFromUrl: specUrl projectsHome: projectsHome [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	| loadSpec projectSetDefinition |
	loadSpec := RwSpecification fromUrl: specUrl.
	projectSetDefinition := loadSpec
		projectsHome: projectsHome;
		readProjectSet.

	self _adoptProjectProjectsInProjectSet: projectSetDefinition
]

{ #category : '*rowan-tools-corev2' }
RwPrjAdoptTool >> adoptProjectFromUrl: specUrl readonlyDiskUrl: diskUrl projectsHome: projectsHome [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	| loadSpec projectSetDefinition |
	loadSpec := RwSpecification fromUrl: specUrl.
	projectSetDefinition := loadSpec
		readonlyDiskUrl: diskUrl;
		projectsHome: projectsHome;
		readProjectSet.

	Rowan projectTools adopt
		_adoptProjectProjectsInProjectSet: projectSetDefinition
]
