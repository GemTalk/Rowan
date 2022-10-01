Class {
	#name : 'RwProjectTopazDeltaWriterTest',
	#superclass : 'RwProjectReaderWriterTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests' }
RwProjectTopazDeltaWriterTest >> testDeltaCreationForGemstoneBaseImage [
	| loadSpecFrom loadSpecTo projectName   
		fromProjectSetDefinition toProjectSetDefinition projectSetModification visitor 
		deltaFile projectsHome repositoryRootPath customConditionalAttributes |

	projectName := 'gemstoneBaseImage'.
	projectsHome := self _testRowanProjectsSandbox.

	customConditionalAttributes := {'common'.
	'gemstone'.
	'bootstraponly'.
	'compilerClasses'.
	'filein3'.
	'filein4'.
	'obsoleteClasses'}.

	loadSpecFrom := (RwSpecification fromUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.6.5-x86_64.Linux/upgrade/projects/gemstoneBaseImage/rowan/specs/gemstoneBaseImage.ston')
		customConditionalAttributes: customConditionalAttributes;
		diskUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.6.5-x86_64.Linux/upgrade/projects/gemstoneBaseImage';
		yourself.
	loadSpecTo := (RwSpecification fromUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.7.0_l-x86_64.Linux/upgrade/projects/gemstoneBaseImage/rowan/specs/gemstoneBaseImage.ston')
		customConditionalAttributes: customConditionalAttributes;
		diskUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.7.0_l-x86_64.Linux/upgrade/projects/gemstoneBaseImage/';
		projectsHome: projectsHome;
		yourself.

"read projects from disk"
	fromProjectSetDefinition := loadSpecFrom resolve readProjectSet: customConditionalAttributes.
	toProjectSetDefinition := loadSpecTo resolve readProjectSet: customConditionalAttributes.

"create projectSetModification"
	projectSetModification := fromProjectSetDefinition
		compareAgainstBase: toProjectSetDefinition.

	repositoryRootPath := projectsHome.
	deltaFile := 'topazDelta'.
	visitor := RwGsModificationTopazDeltaWriterVisitorV2 new
		repositoryRootPath: repositoryRootPath;
		deltaFile: deltaFile;
		yourself.
	visitor visit: projectSetModification.
]
