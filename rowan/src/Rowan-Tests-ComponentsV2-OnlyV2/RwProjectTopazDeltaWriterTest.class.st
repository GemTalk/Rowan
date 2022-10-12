Class {
	#name : 'RwProjectTopazDeltaWriterTest',
	#superclass : 'RwProjectReaderWriterTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests' }
RwProjectTopazDeltaWriterTest >> testDeltaCreationForGemstoneBaseImage [
	| loadSpecBootstrapOnly loadSpecFrom loadSpecTo projectName bootstrapProjectSetDefinition
		bootstrapProjectDef fromProjectSetDefinition toProjectSetDefinition 
		projectSetModification visitor projectsHome deltaRoot customConditionalAttributes header 
		headerStream bootStrapMethodsMap |

	projectName := 'gemstoneBaseImage'.
	projectsHome := self _testRowanProjectsSandbox.

	customConditionalAttributes := 
	{
		'common'. 'bootstraponly'.
	}.
	loadSpecBootstrapOnly := (RwSpecification fromUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.7.0_l-x86_64.Linux/upgrade/projects/gemstoneBaseImage/rowan/specs/gemstoneBaseImage.ston')
		customConditionalAttributes: customConditionalAttributes;
		diskUrl: 'file:///home/dhenrich/_homes/rogue/_home/shared/downloads/products/GemStone64Bit3.7.0_l-x86_64.Linux/upgrade/projects/gemstoneBaseImage/';
		projectsHome: projectsHome;
		yourself.
	loadSpecBootstrapOnly projectName: 'gemstoneBaseImageBootstrapOnly'.
	bootstrapProjectSetDefinition := loadSpecBootstrapOnly resolve readProjectSet: customConditionalAttributes.
	bootstrapProjectDef := (bootstrapProjectSetDefinition projectNamed: loadSpecBootstrapOnly projectName) _projectDefinition.

	bootStrapMethodsMap := Dictionary new.
	{ 'Filein1A-BootstrapOnly' . 'Filein1C-BootstrapOnly' . 'Filein3B-BootstrapOnly' }
		do: [:packageName |
			(bootstrapProjectDef packageNamed: packageName)
				classExtensions keysAndValuesDo: [:className :cDef |
					| ar |
					ar := (bootStrapMethodsMap at: className ifAbsentPut: [ Array new: 2 ]).
					((ar at: 1 put: Set new) 
							addAll: (cDef instanceMethodDefinitions collect: [:each | each selector ])).
					((ar at: 2 put: Set new) 
							addAll: (cDef classMethodDefinitions collect: [:each | each selector ])).
				] ].
	((bootStrapMethodsMap at: 'Object') at: 1)
		removeAll: #(#'_unsafeSet11Oop:' #'_unsafeSetOop:'). "explicitly deleted during upgrade"
	((bootStrapMethodsMap at: 'Class') at: 1)
		removeAll: #(#'_rwCategory:' #'_rwCreateSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:newVersionOf:description:options:ifUnpackagedDo:'). "overridden by Rowan"
	((bootStrapMethodsMap at: 'Behavior') at: 1)
		removeAll: #(#'_rwRemoveAllMethods:enironmentId:' #'_rwMoveMethod:toCategory:' #'_rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:'). "overridden by Rowan"
	((bootStrapMethodsMap at: 'GsFileIn') at: 1)
		removeAll: #(#'currentProject:' #'currentPackage:'). "overridden by Rowan"

	customConditionalAttributes := 
	{
		'common'.
		'gemstone'.
		'compilerClasses'.
		'filein3'.
		'filein4'.
		'obsoleteClasses'.
		'upgradeImage'.
	}.

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
			{  'filein3' . 'filein4' . 'x509' . 'ANSI_PortableStreams' } .
		}
	} do: [:ar |
		| gemstoneVersion productTreeRoot repositoryRoot gemstoneSpec fromCustomConditionalAttributes topazFileNameMap |
		gemstoneVersion := ar at: 1.
		productTreeRoot := ar at: 2.
		repositoryRoot := ar at: 3.
		gemstoneSpec := ar at: 4.
		fromCustomConditionalAttributes := ar at: 5.
		loadSpecFrom := (RwSpecification fromUrl: 'file://', productTreeRoot, '/', repositoryRoot, '/', gemstoneSpec)
			customConditionalAttributes: fromCustomConditionalAttributes;
			diskUrl: 'file://', productTreeRoot, '/', repositoryRoot;
			yourself.

		header := String new.
		headerStream := (productTreeRoot, '/version.txt') asFileReference contents readStream.
		[ headerStream atEnd ] whileFalse: [ header add: '!**	', headerStream nextLine; lf ].

	"read projects from disk"
		fromProjectSetDefinition := loadSpecFrom resolve readProjectSet: customConditionalAttributes. 
		toProjectSetDefinition := loadSpecTo resolve readProjectSet: customConditionalAttributes.

	"create empty files to cover off packages with no modifications"
		deltaRoot := ('/home/dhenrich/junk/generateGs/bootstrap/deltas/' , gemstoneVersion) asFileReference.
		deltaRoot ensureCreateDirectory.
		deltaRoot ensureDeleteAllChildren.
		topazFileNameMap := RwGsModificationTopazDeltaWriterVisitorV2 _topazFileNameMap: (toProjectSetDefinition projectNamed: projectName).
	 topazFileNameMap keys 
		do: [:fileName |
			(deltaRoot / fileName , 'gs') 
				writeStreamDo: [:fileStream | 
					fileStream 
						nextPutAll:  '! DELTA ' , fileName , '.gs'; lf ] ].

fromProjectSetDefinition
	definitions keysAndValuesDo: [:pName :resolvedProject |
		resolvedProject packages keysAndValuesDo: [:packageName :packageDef |
			packageDef classDefinitions keysAndValuesDo: [:className :classDef | 
				(bootStrapMethodsMap at: className ifAbsent: [])
					ifNotNil: [:methodArray | 
						| methodDefs mDef |
						((methodDefs := classDef instanceMethodDefinitions) collect: [:each | each selector]) do: [:selector |
							((methodArray at: 1) includes: selector)
								ifTrue: [ mDef := methodDefs at: selector. mDef source: mDef source, 'XXX'. (methodArray at: 1) remove: selector ] ].
						((methodDefs := classDef classMethodDefinitions) collect: [:each | each selector]) do: [:selector |
							((methodArray at: 2) includes: selector)
								ifTrue: [ mDef := methodDefs at: selector. mDef source: mDef source, 'XXX'. (methodArray at: 2) remove: selector ] ] ] ].
			packageDef classExtensions keysAndValuesDo: [:className :classDef | 
				(bootStrapMethodsMap at: className ifAbsent: [])
					ifNotNil: [:methodArray | 
						| methodDefs mDef |
						((methodDefs := classDef instanceMethodDefinitions) collect: [:each | each selector]) do: [:selector |
							((methodArray at: 1) includes: selector)
								ifTrue: [ mDef := methodDefs at: selector. mDef source: mDef source, 'XXX' . (methodArray at: 1) remove: selector ] ].
						((methodDefs := classDef classMethodDefinitions) collect: [:each | each selector]) do: [:selector |
							((methodArray at: 2) includes: selector)
								ifTrue: [ mDef := methodDefs at: selector. mDef source: mDef source, 'XXX'. (methodArray at: 2) remove: selector ] ] ] ].
			packageName = 'Filein2A'
				ifTrue: [
					(packageDef classDefinitions at: 'InstVarMappingArray' ifAbsent: [])
						ifNotNil: [:classDef |
							"Class changes shape across boundary, force all methods to be recompiled"
							classDef 
								classMethodDefinitions: Dictionary new;
								instanceMethodDefinitions: Dictionary new ] ].			
			packageName = 'Filein3B'
				ifTrue: [
					{ { #'unsafeTerminate' .'unsafeTerminate xxx'} . { #'terminateTries:eachTimeoutMs:' . 'terminateTries: a eachTimeoutMs: b xxx' } }
						do: [:mar |
								(packageDef classExtensions at: 'GsProcess')
									addInstanceMethod: (mar at: 2) protocol: 'XXX' .
								((bootStrapMethodsMap at: 'GsProcess' ifAbsent: []) at: 1) remove: (mar at: 1) ].
					(packageDef classDefinitions at: 'CDeclaration' ifAbsent: [])
						ifNotNil: [:classDef |
							"All methods removed from CDeclaration during upgradeImage"
							classDef 
								classMethodDefinitions: Dictionary new;
								instanceMethodDefinitions: Dictionary new ].
					(packageDef classDefinitions at: 'JsonParser' ifAbsent: [])
						ifNotNil: [:classDef | 
							classDef superclassName = 'PPCompositeParser'
								ifTrue: [ 
									"JSonParser renamed to JsonPetitParser during upgradeImage"
									classDef name: 'JsonPetitParser'.
									packageDef classDefinitions removeKey: 'JsonParser'.
									packageDef classDefinitions at: classDef name put: classDef ] ] ] ] ].

bootStrapMethodsMap keysAndValuesDo: [:className :methodArray | 
	(methodArray at: 1) isEmpty ifFalse: [ self halt ].
	(methodArray at: 2) isEmpty ifFalse: [ self halt ] ].

	"create projectSetModification"
		projectSetModification := toProjectSetDefinition
			compareAgainstBaseForLoader: fromProjectSetDefinition.
projectSetModification halt.

		visitor := RwGsModificationTopazDeltaWriterVisitorV2 new
			topazFileHeader: header;
			repositoryRootPath: deltaRoot;
			yourself.
		visitor visit: projectSetModification ].
]
