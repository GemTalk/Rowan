"
| commitComment |
RwRowanSample9V3Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9V4') ensureDeleteAll.

commitComment := 'GemTalk/Rowan#953 - generating inital contents for RowanSample9V4'.
RwTestProjectLibraryGeneratorV4 new
	projectName: 'RowanSample9V4';
	projectUrl: 'git@github.com:dalehenrich/RowanSample9V4.git';
	preserveRowanSHA: false;
	primeIndexCardMap;
	preserveChangesOnGithub: false;
	genSpec_0000: commitComment;
	genSpec_0001: commitComment;
	genSpec_0002: commitComment;
	genSpec_0003: commitComment;
	genSpec_0004: commitComment;
	genSpec_0005: commitComment;
	genSpec_0006: commitComment;
	genSpec_0007: commitComment;
	genSpec_0008: commitComment;
	genSpec_0009: commitComment;
	genSpec_0010: commitComment;
	genSpec_0011: commitComment;
	genSpec_0012: commitComment;
	genSpec_0013: commitComment;
	genSpec_0014: commitComment;
	genSpec_0015: commitComment;
	genSpec_0016: commitComment;
	genSpec_0017: commitComment;
	genSpec_0018: commitComment;
	genSpec_0019: commitComment;
	genSpec_0020: commitComment;
	genSpec_0021: commitComment;
	genSpec_0022: commitComment;
	genSpec_0023: commitComment;
	genSpec_0024: commitComment;
	genSpec_0025: commitComment;
	genSpec_0026: commitComment;
	genSpec_0027: commitComment;
	genSpec_0028: commitComment;
	genSpec_0029: commitComment;
	genSpec_0030: commitComment;
	genSpec_0031: commitComment;
	genSpec_0032: commitComment;
	genSpec_0033: commitComment;
	genSpec_0034: commitComment;
	genSpec_0035: commitComment;
	genSpec_0036: commitComment;
	genSpec_0037: commitComment;
	genSpec_0038: commitComment;
	genSpec_0039: commitComment;
	genSpec_0040: commitComment;
	genSpec_0041: commitComment;
	genSpec_0042: commitComment;
	genSpec_0043: commitComment;
	genSpec_0044: commitComment;
	genSpec_0045: commitComment;
	genSpec_0046: commitComment;
	genSpec_0047: commitComment;
	genSpec_0048: commitComment;
	genSpec_0049: commitComment;
	genSpec_0050: commitComment;
	genSpec_0051: commitComment;
	genSpec_0052: commitComment;
	genSpec_0053: commitComment;
	genSpec_0054: commitComment;
	genSpec_0055: commitComment;
	genSpec_0056: commitComment;
	genSpec_0057: commitComment;
	genSpec_0058: commitComment;
	genSpec_0059: commitComment;
	genSpec_0060: commitComment;
	genSpec_0061: commitComment;
	genSpec_0062: commitComment;
	genSpec_0063: commitComment;
	genSpec_0064: commitComment;
	genSpec_0065: commitComment;
	genSpec_0066: commitComment;
	genSpec_0067: commitComment;
	genSpec_0068: commitComment;
	genSpec_0069: commitComment;
	genSpec_0070: commitComment;
	genSpec_0071: commitComment;
	genSpec_0074: commitComment;
	genSpec_0075: commitComment;
	genSpec_0072: commitComment;
	genSpec_0073: commitComment;
	genSpec_0076: commitComment;
	genSpec_0077: commitComment;
	genSpec_0078: commitComment;
	genSpec_0079: commitComment;
	genSpec_0080: commitComment;
	genSpec_0081: commitComment;
	genSpec_0082: commitComment;
	genSpec_0083: commitComment;
	genSpec_0084: commitComment;
	genSpec_0085: commitComment;
	genSpec_0089: commitComment;
	genSpec_0090: commitComment;
	yourself
"
Class {
	#name : 'RwTestProjectLibraryGeneratorV4',
	#superclass : 'RwTestProjectLibraryGeneratorV3',
	#category : 'Rowan-TestsV4'
}

{ #category : 'private' }
RwTestProjectLibraryGeneratorV4 >> _addDefinitionsFor_0068: resolvedProject [
	"Start with spec_0067 (RowanSample9V4) and spec_0006 (RowanSample10V3, RowanSample11, RowanSample12); recursive required project chain; RowanSample9V4 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V4; class hierarchy runs RowanSample9V4Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V4Class1"

	self _addDefinitionsFor_0068: resolvedProject revision: 'spec_0006'
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV4 >> _addDefinitionsFor_0069_Bridge: resolvedProject [
	"Start with spec_0000 (RowanSample9V4) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0007 requires RowanSample9V4:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0007 that requires RowanSample9V3:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9V3:spec_0069 has a corresponding test case that confirms the class structure."

	| componentName1 superclassName1 packageName1 className1 componentName2 superclassName2 packageName2 className2 requiredLoadSpec1 requiredRevision1 requiredProjectName1 requiredLoadSpec2 requiredRevision2 requiredProjectName2 requiredSpecName1 requiredSpecName2 |
	componentName1 := 'Bridge'.

	requiredProjectName1 := 'RowanSample11'.
	requiredSpecName1 := requiredProjectName1 , '_' , componentName1.
	requiredRevision1 := 'spec_0005'.
	requiredProjectName2 := 'RowanSample12'.
	requiredSpecName2 := requiredProjectName2 , '_' , componentName1.
	requiredRevision2 := 'spec_0007'.

	packageName1 := projectName , '-' , componentName1.

	className1 := 'Abstract' , projectName , componentName1 , 'Class1'.
	superclassName1 := 'AbstractRowanSample11', componentName1, 'Class1'.

	resolvedProject specName: componentName1.

	(resolvedProject addLoadComponentNamed: componentName1 comment: '')
		addProjectNamed: requiredSpecName1;
		addProjectNamed: requiredSpecName2.
	resolvedProject
		removeComponentNames:
			{'Child'.
			'Parent'}.	"this is dependent upon the order that this method is called"
	resolvedProject addPackageNamed: packageName1 toComponentNamed: componentName1.
	((resolvedProject packageNamed: packageName1)
		addClassNamed: className1
		super: superclassName1
		category: packageName1)
		addInstanceMethod: 'foo ^ 1' protocol: 'accessing';
		yourself.

	componentName2 := 'tests/' , componentName1.
	packageName2 := projectName , '-' , componentName1 , '-Tests'.
	className2 := projectName , 'Tests'.
	superclassName2 := 'TestCase'.

	resolvedProject addSubcomponentNamed: componentName2 condition: 'tests'.
	resolvedProject
		addComponentNamed: componentName2
		toComponentNamed: componentName1.
	resolvedProject addPackageNamed: packageName2 toComponentNamed: componentName2.
	((resolvedProject packageNamed: packageName2)
		addClassNamed: className2
		super: superclassName2
		category: packageName2)
		addInstanceMethod: 'testClass1 self assert: RowanSample12ConcreteClass1 new foo = 1'
			protocol: 'tests';
		yourself.

	requiredLoadSpec1 := RwLoadSpecificationV2 new
		specName: requiredSpecName1;
		projectName: requiredProjectName1;
		revision: requiredRevision1;
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName1 , '.git';
		componentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	requiredLoadSpec1 exportTo: resolvedProject projectsRoot.

	requiredLoadSpec2 := RwLoadSpecificationV2 new
		specName: requiredSpecName2;
		projectName: requiredProjectName2;
		revision: requiredRevision2;
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName2 , '.git';
		componentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	requiredLoadSpec2 exportTo: resolvedProject projectsRoot
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV4 >> _addDefinitionsFor_0069_Parent: resolvedProject [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0007 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0007. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0007 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure."

	| componentName1 superclassName1 packageName1 className1 componentName2 superclassName2 packageName2 className2 requiredLoadSpec requiredRevision requiredProjectName requiredSpecName |

	componentName1 := 'Parent'.

	requiredProjectName := 'RowanSample12'.
	requiredSpecName := requiredProjectName, '_', componentName1.
	requiredRevision := 'spec_0008'.

	packageName1 := projectName , '-' , componentName1.

	className1 := 'Abstract' , projectName , componentName1, 'Class1'.
	superclassName1 := 'Object'.

	resolvedProject specName: componentName1.

	(resolvedProject addLoadComponentNamed: componentName1 comment: '')
		addProjectNamed: requiredSpecName.
	resolvedProject
		removeComponentNames:
			{'Child'.
			'Bridge'}.	"make sure that only Parent component is listed in component names"
	resolvedProject addPackageNamed: packageName1 toComponentNamed: componentName1.
	((resolvedProject packageNamed: packageName1)
		addClassNamed: className1
		super: superclassName1
		category: packageName1)
		addInstanceMethod: 'foo self subclassResponsibility: #foo'
			protocol: 'accessing';
		yourself.

	componentName2 := 'tests/' , componentName1.
	packageName2 := projectName , '-' , componentName1 , '-Tests'.
	className2 := projectName , 'Tests'.
	superclassName2 := 'TestCase'.

	resolvedProject addSubcomponentNamed: componentName2 condition: 'tests'.
	resolvedProject
		addComponentNamed: componentName2
		toComponentNamed: componentName1.
	resolvedProject addPackageNamed: packageName2 toComponentNamed: componentName2.
	((resolvedProject packageNamed: packageName2)
		addClassNamed: className2
		super: superclassName2
		category: packageName2)
		addInstanceMethod:
				'testClass1 self assert: RowanSample12ConcreteClass1 new foo = 1'
			protocol: 'tests';
		yourself.

	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredSpecName;
		projectName: requiredProjectName;
		revision: requiredRevision;
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName , '.git';
		componentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.

	^ resolvedProject loadSpecification copy "canonical load spec for spec_0069"
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV4 >> _addDefinitionsFor_0090: resolvedProject [
	"Start with spec_0089 to create the project using Monticello package convention ... add a BaselineOf that can be used by Pharo to read and write the project ... add a foreign property (#package) to the classes to be compatible out of the box with Pharo."

	| packageName1 packageName2 packageName3 packageName4 className1 className2 |
	resolvedProject packageConvention: 'Monticello'.
	
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := 'BaselineOf', projectName.
	packageName3 := projectName , '-' , 'Tests'.
	packageName4 := projectName , '-' , 'Extensions'.
	
	resolvedProject componentNames: {}.
	resolvedProject
		removeComponentNamed: 'Core';
		addLoadComponentNamed: 'Core'
			comment: 'Top-level component for loading the application';
		addComponentStructureFor: 'Core'
			startingAtComponentNamed: 'Core'
			pathNameArray:
				{'platform'.
				'pharo'}
			conditionPathArray:
				{'common'.
				{'pharo'}}
			comment: 'Component for loading pharo-specific packages';
		addComponentNamed: 'tests/Tests' toComponentNamed: 'Core';
		addPackageNamed: packageName3 toComponentNamed: 'tests/Tests';
		addPackageNamed: packageName2 toComponentNamed: 'platform/pharo/Core';
		addPackageNamed: packageName1 toComponentNamed: 'Core';
		addPackageNamed: packageName4 toComponentNamed: 'Core';
		yourself.
	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: className1)
		addInstanceMethod:
				'extensionMethod ^ self'
			protocol: '*' , packageName4 asLowercase, '-core';
			yourself.
	"add #package foreign properties to the two classes to be compatible with Pharo"
	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className1)
			gs_foreignKeys: { #package -> packageName1 };
			yourself.
	((resolvedProject packageNamed: packageName3)
		classDefinitionNamed: className2)
			gs_foreignKeys: { #package -> packageName3 };
			yourself.
	"add BaselineOf so Pharo and GemStone can share the project"
	((resolvedProject packageNamed: packageName2)
		addClassNamed: 'BaselineOf', projectName super: 'BaselineOf' category: packageName2)
		addInstanceMethod:
				'baseline: spec
	<baseline>
	spec for: #pharo do: [
		spec
			package: ''RowanSample9V4-Core'';
			yourself ]'
			protocol: 'baselines';
			yourself.
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV4 >> _createNewProject: aString [
	^ Rowan newV4ProjectNamed: aString
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV4 >> genSpec_0069: commitMessage [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0007 requires RowanSample9V4:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0007 that requires RowanSample9V4:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9V4:spec_0069 has a corresponding test case that confirms the class structure."

	"https://github.com/GemTalk/Rowan/issues/705"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title canonicalLoadSpec |
	postfix := '0069'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0007 requires RowanSample9V4:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0007 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9V4:spec_0069 has a corresponding test case that confirms the class structure.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 69
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(705);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		packageConvention: 'Rowan';
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | canonicalLoadSpec := self _addDefinitionsFor_0069: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: canonicalLoadSpec
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV4 >> genSpec_0090: commitMessage [
	"Start with spec_0089 to create the project using Monticello package convention ... add a BaselineOf that can be used by Pharo to read and write the project ... add a foreign property (#package) to the classes to be compatible out of the box with Pharo."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title srcDir |
	postfix := '0090'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0089'.
	title := 'Start with spec_0089 to create the project using Monticello package convention ... add a BaselineOf that can be used by Pharo to read and write the project ... add a foreign property (#package) to the classes to be compatible out of the box with Pharo.
### Example Metacello load expression
```
Metacello new
  repository: ''tonel:///export/bosch1/users/dhenrich/_stones/37x/stones/rowan3_dev_g/sandbox/RowanSample9V4/src'';
  baseline: ''RowanSample9V4'';
  load: #().
```
'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 90
		derivedFrom: derivedFrom
		comment: 'Intended for internal Monticello package convention testing ... the presence of the BaselineOf is to make testing with Pharo possible')
		rowanIssues: #(829 959);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0090: resolvedProject ].

	"Create Pharo meta data files: .project and src/.properties"
	resolvedRepository gitRoot / '.project'
		writeStreamDo: [ :fileStream | 
			fileStream truncate.
			fileStream nextPutAll: '{ ''srcDirectory'' :  ''src'' }' ].
	srcDir := resolvedRepository gitRoot / 'src'.
	srcDir ensureCreateDirectory.
	srcDir / '.properties'
		writeStreamDo: [ :fileStream | 
			fileStream truncate.
			fileStream nextPutAll: '{ #format : #tonel }' ].		

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV4 >> primaryProjectName [
	^ 'RowanSample9V4'
]
