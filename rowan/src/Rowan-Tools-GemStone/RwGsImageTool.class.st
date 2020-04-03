Class {
	#name : 'RwGsImageTool',
	#superclass : 'RwGemStoneTool',
	#category : 'Rowan-Tools-GemStone'
}

{ #category : 'bootstrap' }
RwGsImageTool >> adoptGemStone64: specUrl projectsHome: projectsHome [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	| loadSpec projectSetDefinition auditFailures reAudit theProjectSetDefinition tracer wasTracing |
	loadSpec := RwSpecification fromUrl: specUrl.
	projectSetDefinition := loadSpec
		diskUrl: 'file://' , projectsHome;
		projectsHome: projectsHome;
		resolveProjectSet.

	theProjectSetDefinition := RwProjectSetDefinition new.
	tracer := Rowan projectTools trace.
	wasTracing := tracer isTracing.
	tracer startTracing.
	projectSetDefinition projects
		do: [ :resolvedProject | 
			| resolvedProject_copy projectDefinition |
			"make a copy of the resolvedProject (and repair it for now, since copyForLoadedProject is somewhat destructive"
			resolvedProject_copy := resolvedProject copyForLoadedProject.
			projectDefinition := resolvedProject _projectDefinition copy.
			projectDefinition components: resolvedProject_copy _projectStructure.
			resolvedProject_copy
				_projectDefinition: projectDefinition;
				_projectStructure: nil;
				yourself.
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
	Rowan projectTools loadV2 loadProjectSetDefinition: theProjectSetDefinition.	"Load the project shell -- project and empty packages"
	SessionTemps current removeKey: #'ROWAN_TRACE' ifAbsent: [  ].
	[ 
	"end logging to topaz output file"
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
					tracer
						trace:
							'Missing loaded method ' , ex methodPrintString
								, ' encountered during adopt ... IGNORED' ]
				classErrorDo: [ 
					tracer
						trace:
							'Missing loaded class ' , ex className , ' encountered during adopt ... IGNORED' ].
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
					'Post load Rowan audit failed for projects ' , auditFailures printString ]
]

{ #category : 'repository' }
RwGsImageTool >> newRepositoryRoot: repositoryRoot forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project repositoryRoot: repositoryRoot
]

{ #category : 'repository' }
RwGsImageTool >> newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'repository' }
RwGsImageTool >> newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes instanceMigrator: instanceMigrator forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
		instanceMigrator: instanceMigrator
]
