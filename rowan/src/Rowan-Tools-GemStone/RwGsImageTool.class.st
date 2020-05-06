Class {
	#name : 'RwGsImageTool',
	#superclass : 'RwGemStoneTool',
	#category : 'Rowan-Tools-GemStone'
}

{ #category : 'bootstrap' }
RwGsImageTool >> adoptGemStone64: specUrl projectsHome: projectsHome [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	| loadSpec projectSetDefinition auditFailures reAudit theProjectSetDefinition tracer wasTracing adoptErrors |
	loadSpec := RwSpecification fromUrl: specUrl.
	projectSetDefinition := loadSpec
		diskUrl: 'file:' , projectsHome;
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
								, ' encountered during adopt ... IGNORED' ]
				classErrorDo: [ 
					adoptErrors := true.
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
					[ audit := Rowan projectTools audit auditForProjectNamed: projectName ]
						on: RwAdoptAuditErrorNotification
						do: [ :ex | 
							false
								ifTrue: [ self halt ].
							ex resume: true ].
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

{ #category : 'packages' }
RwGsImageTool >> readRewriteGemStone64Packages: archBase [
	"
		Rowan gemstoneTools image readRewriteGemStone64Packages: '/home/dhenrich/work/j_36x/'
	"

	| repositoryRoot platformConfigurationAttributes specUrl loadSpec resolvedProject |
	SessionTemps current at: #'ROWAN_TRACE' put: nil.	"#gciLogServer "
	repositoryRoot := archBase , '/image'.

	platformConfigurationAttributes := {'common'.
	'gemstone'.
	'bootstraponly'}.

	specUrl := repositoryRoot asFileReference / 'rowan' / 'specs'
		/ 'GemStone64.ston'.
	(loadSpec := RwSpecification fromUrl: 'file:' , specUrl pathString)
		projectsHome: repositoryRoot;
		diskUrl: 'file:' , repositoryRoot;
		yourself.
	resolvedProject := loadSpec resolve.

	[ resolvedProject read: platformConfigurationAttributes ]
		on: CompileWarning
		do: [ :ex | 
			| str |
			((str := ex asString) includesString: 'not optimized')
				ifTrue: [ ex resume ]
				ifFalse: [ 
					GsFile gciLogServer: str.
					ex pass ] ].

	resolvedProject packages
		do: [ :packageDef | 
			| classExtensions |
			"merge class extensions into class definitions and remove class dextension"
			classExtensions := packageDef classExtensions.
			packageDef classDefinitions
				do: [ :classDef | 
					classExtensions
						at: classDef name
						ifPresent: [ :classExtension | 
							classExtension instanceMethodDefinitions
								do: [ :meth | classDef addInstanceMethodDefinition: meth ].
							classExtension classMethodDefinitions
								do: [ :meth | classDef addClassMethodDefinition: meth ].
							packageDef removeClassExtensionDefinition: classExtension ] ] ].

	resolvedProject exportPackages
]
