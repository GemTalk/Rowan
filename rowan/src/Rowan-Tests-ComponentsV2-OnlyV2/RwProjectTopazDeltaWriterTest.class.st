Class {
	#name : 'RwProjectTopazDeltaWriterTest',
	#superclass : 'RwProjectReaderWriterTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests' }
RwProjectTopazDeltaWriterTest >> testDeltaCreationForGemstoneBaseImage [
	| loadSpecFrom loadSpecTo projectName   
		fromProjectSetDefinition toProjectSetDefinition projectSetModification visitor 
		projectsHome deltaRoot customConditionalAttributes header headerStream |

	projectName := 'gemstoneBaseImage'.
	projectsHome := self _testRowanProjectsSandbox.

	customConditionalAttributes := {'common'.
	'gemstone'.
	'bootstraponly'.
	'compilerClasses'.
	'filein3'.
	'filein4'.
	'obsoleteClasses'}.

	loadSpecTo := (RwSpecification fromUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.7.0_l-x86_64.Linux/upgrade/projects/gemstoneBaseImage/rowan/specs/gemstoneBaseImage.ston')
		customConditionalAttributes: customConditionalAttributes;
		diskUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.7.0_l-x86_64.Linux/upgrade/projects/gemstoneBaseImage/';
		projectsHome: projectsHome;
		yourself.

	{
		{ 
			'3.6.5' . 
			'/home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.6.5-x86_64.Linux/' .
			'upgrade/projects/gemstoneBaseImage/' .
			'rowan/specs/gemstoneBaseImage.ston' .
		}
	} do: [:ar |
		| gemstoneVersion productTreeRoot repositoryRoot gemstoneSpec |
		gemstoneVersion := ar at: 1.
		productTreeRoot := ar at: 2.
		repositoryRoot := ar at: 3.
		gemstoneSpec := ar at: 4.
		loadSpecFrom := (RwSpecification fromUrl: 'file://', productTreeRoot, '/', repositoryRoot, '/', gemstoneSpec)
			customConditionalAttributes: customConditionalAttributes;
			diskUrl: 'file://', productTreeRoot, '/', repositoryRoot;
			yourself.

		header := String new.
		headerStream := (productTreeRoot, '/version.txt') asFileReference contents readStream.
		[ headerStream atEnd ] whileFalse: [ header add: '!**	', headerStream nextLine; lf ].

	"read projects from disk"
		fromProjectSetDefinition := loadSpecFrom resolve readProjectSet: customConditionalAttributes.
		toProjectSetDefinition := loadSpecTo resolve readProjectSet: customConditionalAttributes.

	"create projectSetModification"
		projectSetModification := fromProjectSetDefinition
			compareAgainstBase: toProjectSetDefinition.

		deltaRoot := ('/home/dhenrich/work/l_37x/slow50/bootstrap/deltas/' , gemstoneVersion)asFileReference.
		deltaRoot ensureCreateDirectory.
		deltaRoot ensureDeleteAllChildren.
		visitor := RwGsModificationTopazDeltaWriterVisitorV2 new
			topazFileHeader: header;
			repositoryRootPath: deltaRoot;
			yourself.
		visitor visit: projectSetModification ].
]
