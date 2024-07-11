Class {
	#name : 'RwPrjInstallToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'bootstrap installation' }
RwPrjInstallToolV2 >> install_3_RowanV2 [
	| rowanSpecPath rowanProjectsHome |
	rowanSpecPath := '$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/Rowan.ston'.
	rowanProjectsHome := '$ROWAN_PROJECTS_HOME'.
	^ self
		install_3_RowanV2: rowanSpecPath rowanProjectsHome: rowanProjectsHome;
		install_4_RowanV2: rowanSpecPath rowanProjectsHome: rowanProjectsHome
]

{ #category : 'bootstrap installation' }
RwPrjInstallToolV2 >> install_3_RowanV2: rowanSpecPath rowanProjectsHome: rowanProjectsHome [
	"
		Install FileSystem, Rowan, Cypress, STON, and Tonel using Rowan to adopt the existing classes and extension
		methods into the correct package structure
	"

	| projectSetDefinition auditFailures reAudit theProjectSetDefinition |
	Rowan projectTools trace
		startTracing;
		trace: '--- installing install_3_RowanV2.tpz'.
	{{('file:' , rowanSpecPath).
	rowanProjectsHome}}
		do: [ :ar | 
			| specUrl "Load project and packages from disk" projectsHome loadSpec resolvedProject |
			specUrl := ar at: 1.
			projectsHome := ar at: 2.
			loadSpec := RwSpecification fromUrl: specUrl.
			resolvedProject := loadSpec
				projectsHome: projectsHome;
				resolveProject.
			projectSetDefinition := resolvedProject readProjectSet ].

	theProjectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition projects
		do: [ :resolvedProject | 
			| resolvedProject_copy |
			"make a copy of the resolvedProject so we can safely nuke all of the project packages"
			resolvedProject_copy := resolvedProject copy.
			GsFile stdout
				nextPutAll: 'Project: ' , resolvedProject_copy name;
				lf.
			resolvedProject_copy packageNames
				do: [ :packageName | 
					"wipe out package contents, so we can load *empty* project and packages, that will be adopted in next step"
					GsFile stdout
						nextPutAll: '	' , packageName;
						lf.
					(resolvedProject_copy packageNamed: packageName)
						classDefinitions: Dictionary new;
						classExtensions: Dictionary new;
						yourself ].
			theProjectSetDefinition addProject: resolvedProject_copy ].	"Load the project shell -- project and empty packages"
	Rowan projectTools loadV2 loadProjectSetDefinition: theProjectSetDefinition.
	SessionTemps current removeKey: #'ROWAN_TRACE' ifAbsent: [  ].	"end logging to topaz output file"
	[ 
	"Adopt the project set definition ... 
		Log and ignore any missing method or missing classes encountered as they may not be
		present in the .gs bootstrap file for the proejct ... The will be created when we
		reload the project a little bit later on."
	Rowan projectTools adopt adoptProjectSetDefinition: projectSetDefinition ]
		on:
			RwAdoptMissingMethodErrorNotification , RwAdoptMissingClassErrorNotification
		do: [ :ex | 
			ex
				methodErrorDo: [ 
					GsFile
						gciLogServer:
							'Missing loaded method ' , ex methodPrintString
								, ' encountered during adopt ... IGNORED' ]
				classErrorDo: [ 
					GsFile
						gciLogServer:
							'Missing loaded class ' , ex className , ' encountered during adopt ... IGNORED' ]
				traitErrorDo: [ 
					GsFile
						gciLogServer:
							'Missing loaded Trait ' , ex traitName, ' encountered during adopt ... IGNORED' ].
			ex resume: nil ].

	projectSetDefinition deriveLoadedThings
		do: [ :loadedProject | 
			"mark projects and packages not dirty"
			loadedProject markNotDirty.
			loadedProject loadedPackages
				valuesDo: [ :loadedPackage | loadedPackage markNotDirty ] ].

	reAudit := true.	"kick off the first audit"
	[ reAudit ]
		whileTrue: [ 
			auditFailures := {}.
			reAudit := false.
			projectSetDefinition projects
				do: [ :projectDefinition | 
					| audit projectName |
					projectName := projectDefinition name.
					[ audit := Rowan projectTools audit auditForProjectNamed: projectName ]
						on: RwAuditMethodErrorNotification
						do: [ :ex | 
							| beh |
							GsFile
								gciLogServer:
									'extra unpackaged method ' , ex methodPrintString
										, ' encountered during audit ... REMOVED (audit will be rerun)'.	"method is not present in the current package structure, so it should be removed"
							beh := Rowan globalNamed: ex className.
							ex isMetaclass
								ifTrue: [ beh := beh class ].
							beh removeSelector: ex selector.
							reAudit := true.	"don't record as an audit error, but make sure that we rerun the audit"
							ex resume: false ].
					GsFile gciLogServer: '	-- audit finished '.
					audit isEmpty
						ifFalse: [ 
							reAudit := false.	"we must have had an audit failure that was not handled"
							GsFile gciLogServer: 'FAILED AUDIT: ' , projectName.
							auditFailures add: projectName ] ].
			reAudit
				ifTrue: [ GsFile gciLogServer: 'RERUN AUDIT' ] ].
	auditFailures isEmpty
		ifFalse: [ 
			self
				error:
					'Post load Rowan audit failed for projects ' , auditFailures printString ]
]

{ #category : 'bootstrap installation' }
RwPrjInstallToolV2 >> install_4_RowanV2: rowanSpecPath rowanProjectsHome: rowanProjectsHome [
	"
		Load project and packages from disk to pick up any modifications not included in the bootstrap files; 
		fail if there are CompileWarnings; 
		audit after load;
		return true on success
	"

	| resolvedProject loadSpecUrl loadSpec projectsHome loadedProjects warnings |
	Rowan projectTools trace
		startTracing;
		trace: '--- installing install_4.tpz'.
	loadSpecUrl := 'file:' , rowanSpecPath.
	projectsHome := rowanProjectsHome.
	loadSpec := RwSpecification fromUrl: loadSpecUrl.
	resolvedProject := loadSpec
		projectsHome: projectsHome;
		resolveProject.
	warnings := {}.
	[ loadedProjects := resolvedProject load ]
		on: CompileWarning
		do: [ :ex | 
			(ex description includesString: 'not optimized')
				ifFalse: [ warnings add: ex asString printString ].
			ex resume ].

	System commit.

	warnings isEmpty
		ifFalse: [ 
			GsFile gciLogServer: 'COMPILE WARNINGS: '.
			warnings do: [ :warning | GsFile gciLogServer: '	' , warning ].
			self error: 'Warnings during project reload' ].

	loadedProjects
		do: [ :loadedProject | 
			| audit |
			"audit after load"
			audit := loadedProject audit.
			audit isEmpty
				ifFalse: [ 
					self
						error:
							'Post load Rowan audit failed for project ' , loadedProject name printString ] ].
	^ true
]
