Class {
	#name : 'RwGsImageTool',
	#superclass : 'RwGemStoneTool',
	#category : 'Rowan-Tools-GemStone'
}

{ #category : 'bootstrap' }
RwGsImageTool >> adoptGemStone64: specUrl projectsHome: projectsHome [
	"Create loaded project (if needed), traverse the package definitions and 
				create loaded packages for each"

	self deprecated: 'Use Rowan projectTools acopt adoptProjectFromUrl:diskUrl:projectsHome:, instead'.
	Rowan projectTools adopt adoptProjectFromUrl: specUrl diskUrl: 'file:' , projectsHome projectsHome: projectsHome
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
