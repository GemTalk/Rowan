"
| commitComment |
RwRowanSample9V3Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9V3') ensureDeleteAll.

commitComment := ''.
RwTestProjectLibraryGeneratorV3 new
	projectName: 'RowanSample9V3';
	projectUrl: 'git@github.com:dalehenrich/RowanSample9V3.git';
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
	yourself
"
Class {
	#name : 'RwTestProjectLibraryGeneratorV3',
	#superclass : 'RwTestProjectLibraryGenerator',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0065: resolvedProject [
	"Start with spec_0060; replace external project RowanSample10 spec_0001 with spec_0002"

	| requiredProjectName requiredLoadSpec componentName |
	requiredProjectName := 'RowanSample10'.
	componentName := 'Core'.
	
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		revision: 'spec_0002';
		gitUrl: 'git@github.com:dalehenrich/RowanSample10.git';
		componentNames: {componentName};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'mytests' }
		yourself.

	resolvedProject customConditionalAttributes: #('tests' 'mytests').

	resolvedProject projectsRoot ensureCreateDirectory.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0066: resolvedProject [
	"Start with spec_0001; add RowanSample10, RowanSampleProjects11 and RowanSample12 as required projects"

	| classNameIndex packageName |
	resolvedProject projectsRoot ensureCreateDirectory.
	classNameIndex := 2.
	packageName := projectName , '-Core'.
	{'RowanSample10'.
	'RowanSample11'.
	'RowanSample12'}
		do: [ :rpn | 
			((resolvedProject packageNamed: packageName)
				addClassNamed: projectName , 'Class' , classNameIndex asString
				super: 'Object'
				instvars: #('ivar1')
				category: packageName
				comment: 'I am an example class whose superclass is from a required project')
				addInstanceMethod: 'foo ^1' protocol: 'accessing';
				yourself.
			self
				_addDefinitionsFor_0060: resolvedProject
				classNameIndex: classNameIndex asString
				requiredProjectName: rpn.
			classNameIndex := classNameIndex + 1 ]
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0067: resolvedProject [
	"Start with spec_0001 (RowanSample9V3) and spec_0003 (RowanSample10V3, RowanSample11, RowanSample12); linear required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1"

	self
		_addDefinitionsFor_0067: resolvedProject
		requiredProjectName: 'RowanSample10V3'
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0067: resolvedProject requiredProjectName: requiredProjectName [
	"Start with spec_0001 (RowanSample9V3) and spec_0003 (RowanSample10V3, RowanSample11, RowanSample12); linear required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1"

	^ self
		_addDefinitionsFor_0067: resolvedProject
		specName: 'spec_0003'
		requiredProjectName: requiredProjectName
		superClassBaseName: 'Class1'
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0067: resolvedProject specName: specName requiredProjectName: requiredProjectName superClassBaseName: superClassBaseName [
	"Start with spec_0001 (RowanSample9V3) and spec_0003 (RowanSample10V3, RowanSample11, RowanSample12); linear required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1"

	| requiredLoadSpec componentName superclassName packageName className  |
	resolvedProject projectsRoot ensureCreateDirectory.
	packageName := projectName , '-Core'.
	componentName := 'Core'.
	className := projectName, 'Class1'.
	superclassName := requiredProjectName , superClassBaseName.
	packageName := projectName , '-Core'.
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		revision: specName;
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName , '.git';
		componentNames: {componentName};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: {'tests'};
		yourself.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.
	(resolvedProject componentNamed: componentName)
		addProjectNamed: requiredProjectName;
		yourself.
	((resolvedProject packageNamed: packageName) classDefinitionNamed: className)
		superclassName: superclassName;
		instVarNames: #();
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0068: resolvedProject [
	"Start with spec_0067 (RowanSample9V3) and spec_0004 (RowanSample10V3, RowanSample11, RowanSample12); recursive required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V3; class hierarchy runs RowanSample9V3Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1"

	| className packageName requiredLoadSpec requiredProjectName componentName |
	resolvedProject projectsRoot ensureCreateDirectory.
	packageName := projectName , '-Core'.
	componentName := 'Core'.
	className := projectName , 'Class2'.
	requiredProjectName := 'RowanSample10V3'.

	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		revision: 'spec_0004';
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName , '.git';
		componentNames: {componentName};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: {'tests'};
		yourself.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.

	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #()
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0068: resolvedProject requiredProjectName: requiredProjectName [
	"Start with spec_0003; part of recursive required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V3; class hierarchy runs RowanSample9V3Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1"

	"only expected to be sent from the required projects as the name of the branch is the only change (spec_0004)"

	| requiredLoadSpec componentName |
	resolvedProject projectsRoot ensureCreateDirectory.
	componentName := 'Core'.
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		revision: 'spec_0004';
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName , '.git';
		componentNames: {componentName};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: {'tests'};
		yourself.
	requiredLoadSpec exportTo: resolvedProject projectsRoot
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0069_Bridge: resolvedProject [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0005 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0006 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure."

	| componentName1 superclassName1 packageName1 className1 componentName2 superclassName2 packageName2 className2 requiredLoadSpec1 requiredRevision1 requiredProjectName1 requiredLoadSpec2 requiredRevision2 requiredProjectName2 requiredSpecName1 requiredSpecName2 |
	componentName1 := 'Bridge'.

	requiredProjectName1 := 'RowanSample11'.
	requiredSpecName1 := requiredProjectName1 , '_' , componentName1.
	requiredRevision1 := 'spec_0005'.
	requiredProjectName2 := 'RowanSample12'.
	requiredSpecName2 := requiredProjectName2 , '_' , componentName1.
	requiredRevision2 := 'spec_0006'.

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
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0069_Child: resolvedProject [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0005 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0006 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure."

	| componentName1 superclassName1 packageName1 className1 componentName2 superclassName2 packageName2 className2 requiredLoadSpec1 requiredRevision1 requiredProjectName1 requiredSpecName1 |
	componentName1 := 'Child'.

	requiredProjectName1 := 'RowanSample10V3'.
	requiredSpecName1 := requiredProjectName1 , '_' , componentName1.
	requiredRevision1 := 'spec_0001'.

	packageName1 := projectName , '-' , componentName1.

	className1 := projectName , componentName1 , 'Class1'.
	superclassName1 := 'RowanSample10V3Class1'.

	resolvedProject specName: componentName1.

	(resolvedProject addLoadComponentNamed: componentName1 comment: '')
		addProjectNamed: requiredSpecName1.
	resolvedProject
		removeComponentNames:
			{'Bridge'.
			'Parent'}.	"this is dependent upon the order that this method is called"
	resolvedProject addPackageNamed: packageName1 toComponentNamed: componentName1.
	(resolvedProject packageNamed: packageName1)
		addClassNamed: className1
		super: superclassName1
		category: packageName1.

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
		addInstanceMethod: 'testClass1 self assert: ' , className1 , ' new foo = 1'
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
	requiredLoadSpec1 exportTo: resolvedProject projectsRoot
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0069_Parent: resolvedProject [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0005 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0006 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure."

	| componentName1 superclassName1 packageName1 className1 componentName2 superclassName2 packageName2 className2 requiredLoadSpec requiredRevision requiredProjectName requiredSpecName |

	componentName1 := 'Parent'.

	requiredProjectName := 'RowanSample12'.
	requiredSpecName := requiredProjectName, '_', componentName1.
	requiredRevision := 'spec_0005'.

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
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0069: resolvedProject [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0005 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0006 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure."

	| canonicalLoadSpec |
	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedProject specsRoot ensureCreateDirectory.
	resolvedProject
		removeComponentNames: {'Core'};
		removeComponentNamed: 'Core';
		removeComponentNamed: 'tests/Tests'.
	canonicalLoadSpec := self _addDefinitionsFor_0069_Parent: resolvedProject.
	resolvedProject loadSpecification gitUrl: self projectUrl.
	resolvedProject
		exportLoadSpecification.
	canonicalLoadSpec specName: 'spec_0069'.

	self _addDefinitionsFor_0069_Child: resolvedProject.
	resolvedProject
		exportLoadSpecification.

	self
		_addDefinitionsFor_0069_Bridge: resolvedProject.	"load spec will be exported by caller"
	^ canonicalLoadSpec
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0070: resolvedProject [
	"Start with spec_0001 (RowanSample9) add 2 more components: attr1/Core1 that adds the class RowanSample9Class2 as a subclass of RowanSample9Class1 and attr2/Core2 that adds the class RowanSample9Class3 as a subclass of RowanSampleClass1. Tests are included by attr1/tests/Core1 and attr2/tests/Core2 The custom conditional attributes attr1 and attr2 are used to control  whether or not the classes are loaded. By default the attribute list includes only tests."

	"https://github.com/GemTalk/Rowan/issues/711"

	| packageName1 packageName2 packageName3 packageName4 componentName componentName1 componentName2 componentName3 componentName4 |
	packageName1 := projectName , '-' , 'Core1'.
	packageName2 := projectName , '-' , 'Core2'.
	packageName3 := projectName , '-Core1-Tests'.
	packageName4 := projectName , '-Core2-Tests'.
	componentName := 'Core'.
	componentName1 := 'attr1/Core'.
	componentName2 := 'attr2/Core'.
	componentName3 := 'attr1/tests/Core'.
	componentName4 := 'attr2/tests/Core'.

	resolvedProject
		packageConvention: 'Rowan';
		addSubcomponentNamed: componentName1
			condition: 'attr1';
		addSubcomponentNamed: componentName2
			condition: 'attr2';
		addSubcomponentStructureFor: componentName
			startingAtComponentNamed: componentName
			conditionPathArray: #('attr1' 'tests');
		addSubcomponentStructureFor: componentName
			startingAtComponentNamed: componentName
			conditionPathArray: #('attr2' 'tests');
		addPackageNamed: packageName1
			toComponentNamed: componentName1;
		addPackageNamed: packageName2
			toComponentNamed: componentName2;
		addPackageNamed: packageName3
			toComponentNamed: componentName3;
		addPackageNamed: packageName4
			toComponentNamed: componentName4;
		addComponentNamed: componentName1
			toComponentNamed: componentName;
		addComponentNamed: componentName2
			toComponentNamed: componentName;
		yourself.

	((resolvedProject packageNamed: packageName1)
		addClassNamed:  projectName , 'Class2' super: projectName , 'Class1' category: 'attribute 1')
		addInstanceMethod: 'attr1 ^''attr1''' protocol: 'attribute 1';
		yourself.
	((resolvedProject packageNamed: packageName2)
		addClassNamed:  projectName , 'Class3' super: projectName , 'Class1' category: 'attribute 2')
		addInstanceMethod: 'attr2 ^''attr2''' protocol: 'attribute 2';
		yourself.
	((resolvedProject packageNamed: packageName3)
		addClassExtensionNamed: projectName , 'TestCase')
		addInstanceMethod:
				'testAttr1 self assert: ' , projectName , 'Class2 new attr1 = ''attr1'''
			protocol: 'tests'.
	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: projectName , 'TestCase')
		addInstanceMethod:
				'testAttr2 self assert: ' , projectName , 'Class3 new attr2 = ''attr2'''
			protocol: 'tests'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0072: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0053; change the project spec so that none of the directories are using the default path"

	| resolvedEmbeddedProject |
	resolvedEmbeddedProject := embeddedProject resolveProject.

	(resolvedEmbeddedProject repositoryRoot / 'rowan') ensureDeleteAll.	"old directory"

	resolvedEmbeddedProject
		projectsHome: self projectsHome;
		gitUrl: self projectUrl;
		packageFormat: 'tonel';
		componentsPath: 'embed_rowan/components';
		packagesPath: 'embed_rowan/src';
		projectsPath: 'embed_rowan/projects';
		specsPath: 'embed_rowan/specs';
		projectSpecFile: 'embed_rowan/project.ston';
		export;
		exportLoadSpecification.	"write the embedded project directory structure - REQUIRED"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo:
			resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0073: resolvedProject_0073 [
	"Start with  spec_0001, add 3 additional projects, using relativeRepositoryRoot to distinquish between projects"

	"https://github.com/GemTalk/Rowan/issues/571"

	"https://github.com/GemTalk/Rowan/issues/714"

	| loadSpecification_0073 packageNames |
	loadSpecification_0073 := resolvedProject_0073 loadSpecification.
	packageNames := resolvedProject_0073 packageNames.

	loadSpecification_0073 customConditionalAttributes: {'tests'}.

	resolvedProject_0073
		removeComponentNamed: 'Core';
		componentNames: {};
		addLoadComponentNamed: 'Core' comment: 'the component';
		addComponentNamed: 'tests/Tests' toComponentNamed: 'Core';
		yourself.
	packageNames
		do: [ :packageName | 
			packageName ~= (projectName , '-Tests')
				ifTrue: [ resolvedProject_0073 addPackageNamed: packageName toComponentNamed: 'Core' ] ].

	1 to: 3 do: [ :index | 
		| resolvedProjectName suffix loadSpecification specName resolvedProject baseName |
		suffix := '_' , index asString.
		specName := loadSpecification_0073 specName , suffix.
		resolvedProjectName := resolvedProject_0073 name , suffix.
		baseName := 'rowan' , suffix.

		loadSpecification := RwLoadSpecificationV2 new
			projectName: resolvedProjectName;
			projectAlias: resolvedProject_0073 name;
			projectsHome: self projectsHome;
			gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
			yourself.

		loadSpecification
			specName: resolvedProjectName;
			revision: loadSpecification_0073 revision;
			gitUrl: self projectUrl;
			relativeRepositoryRoot: baseName;
			componentNames: {};
			customConditionalAttributes: {'tests'};
			comment: loadSpecification_0073 comment;
			yourself.

		resolvedProject := loadSpecification resolveProject.

		resolvedProject
			addLoadComponentNamed: 'Core' comment: 'the component';
			addSubcomponentNamed: 'tests/Tests'
				condition: 'tests'
				comment: 'the test component'
				toComponentNamed: 'Core';
			yourself.

		resolvedProject projectSpecification
			componentsPath: 'components';
			packagesPath: 'src';
			projectsPath: 'projects';
			specsPath: 'specs';
			projectSpecPath: nil;
			packageFormat: 'tonel';
			yourself.

		self
			_addSimpleDefinitionsFor: resolvedProject
			projectName: resolvedProject projectName.

		resolvedProject repositoryRoot ensureDeleteAll.

		resolvedProject
			gitUrl: self projectUrl;
			export;
			exportLoadSpecification.	"write the rowan directory structure"

		resolvedProject_0073
			addProjectNamed: resolvedProjectName
			toComponentNamed: 'Core'.

		resolvedProject_0073 projectsRoot ensureCreateDirectory.
		resolvedProject specsRoot / resolvedProjectName , 'ston'
			copyTo: resolvedProject_0073 projectsRoot / resolvedProjectName , 'ston' ].

	^ resolvedProject_0073
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0074: resolvedProject [
	"Start with spec_0001; add an additional project using relativeRepositoryRoot that is basically a copy of spec_001; and adjust class in primary project be a subclass of class in additional project"

	"https://github.com/GemTalk/Rowan/issues/714"

	| requiredProjectName requiredLoadSpec requiredProject componentName requiredClassName1 className1 packageName1 relativeRepositoryRoot |
	requiredProjectName := projectName , '_embedded_1'.
	relativeRepositoryRoot := 'embeddedProjects/', requiredProjectName.
	componentName := 'Core'.
	requiredClassName1 :=  requiredProjectName , 'Class1'.
	className1 :=  projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		projectAlias: projectName;
		revision: 'spec_0074';
		gitUrl: self projectUrl;
		relativeRepositoryRoot: relativeRepositoryRoot;
		componentNames: {};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	requiredProject := (requiredLoadSpec projectsHome: resolvedProject projectsHome) resolveProject.
	requiredProject projectSpecification
		specsPath: 'rowan/specs';
		projectsPath: 'rowan/projects';
		packagesPath: 'rowan/src';
		componentsPath: 'rowan/components';
		packageFormat: 'tonel';
		yourself.
	requiredProject
		projectSpecFile: 'rowan/project.ston';
		addLoadComponentNamed: componentName;
		addSubcomponentStructureFor: 'Tests'
			startingAtComponentNamed: componentName
			conditionPathArray: #('tests');
		yourself.
	self _addDefinitionsFor: requiredProject projectName: requiredProjectName.

	requiredProject
		projectsHome: self projectsHome;
		gitUrl: self projectUrl;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	requiredProject specsRoot / requiredLoadSpec specName , 'ston'
		copyTo: resolvedProject projectsRoot / requiredLoadSpec specName , 'ston'.

	(resolvedProject componentNamed: componentName)
		addProjectNamed: requiredProjectName;
		yourself.

	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className1)
			superclassName: requiredClassName1;
			instVarNames: #();
			yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0075: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0074; add instance side method #ivar1 referencing inherited ivar to class in primary project; add instance method #ivar1_embedded to class in embedded project"

	| componentName className1 packageName1 className2 packageName2 resolvedEmbeddedProject embeddedProjectName className3 className4 packageName3 packageName4 |
	embeddedProjectName := embeddedProject projectName.
	componentName := 'Core'.
	className1 :=  projectName , 'Class1'.
	className2 :=  projectName , 'TestCase'.
	className3 := embeddedProjectName, 'Class1'.
	className4 := embeddedProjectName, 'TestCase'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	packageName3 := embeddedProjectName , '-Core'.
	packageName4 := embeddedProjectName , '-Tests'.
	
	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className1)
			addInstanceMethod: 'ivar1 ^ ivar1'
			protocol: 'accessing';
			yourself.

	((resolvedProject packageNamed: packageName2)
		classDefinitionNamed: className2)
		addInstanceMethod: 'testInstVars_', className1, '
	| x |
	self assert: (x := ', className1, ' new ivar1) isNil.
'
			protocol: 'tests';
		addInstanceMethod: 'testInstVars_', className3, '
	| x |
	self assert: (x := ', className1, ' new ivar1_embedded) isNil.
'
			protocol: 'tests';
		yourself.

	resolvedEmbeddedProject := embeddedProject resolveProject.
	((resolvedEmbeddedProject packageNamed: packageName3)
		classDefinitionNamed: className3)
			addInstanceMethod: 'ivar1_embedded ^ ivar1'
			protocol: 'accessing';
			yourself.
	((resolvedEmbeddedProject packageNamed: packageName4)
		classDefinitionNamed: className4)
			addInstanceMethod: 'testInstVars_', className3, '
	| x |
	self assert: (x := ', className3, ' new ivar1_embedded) isNil.'
			protocol: 'tests';
			yourself.
	resolvedEmbeddedProject
		projectsHome: self projectsHome;
		packageFormat: 'tonel';
		gitUrl: self projectUrl;
		export;
		exportLoadSpecification.	"write the embedded project directory structure - REQUIRED"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0076: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0074; add platform conditional packages to the embedded project (using relativeRepositoryRoot) and the primary project."

	| resolvedEmbeddedProject |

	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject  condition: 'gnu' packageLabel: 'Gnu'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gnu' packageLabel: 'Gnu'.

	(resolvedEmbeddedProject := embeddedProject resolveProject)
		projectsHome: self projectsHome;
		gitUrl: self projectUrl;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0077: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0074; add platform conditional packages (GemStone version 2.4.0) to the embedded project (using relativeRepositoryRoot) and the primary project."

	| resolvedEmbeddedProject |
	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject condition: 'gs2.[4-]' packageLabel: 'GemStone24x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gs2.[4-]'  packageLabel: 'GemStone24x'.

	(resolvedEmbeddedProject := embeddedProject resolveProject)
		projectsHome: self projectsHome;
		gitUrl: self projectUrl;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0078: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0074; add platform conditional packages (GemStone version 3.6.x, 3.5.x, and 3.2.15) to the embedded project (using relativeRepositoryRoot) and the primary project. This is expected to reproduce issue #594 for those three patforms."

	| resolvedEmbeddedProject |
	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject condition: 'gs2.[4-]' packageLabel: 'GemStone24x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gs2.[4-]'  packageLabel: 'GemStone24x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject condition: 'gs3.[2-]' packageLabel: 'GemStone3x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gs3.[2-]'  packageLabel: 'GemStone3x'.

	(resolvedEmbeddedProject := embeddedProject resolveProject)
		projectsHome: self projectsHome;
		gitUrl: self projectUrl;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0082: resolvedProject [
	"Start with spec_0001; add classes that use the full complement of class types: normal (inherit from spec_0001); byteSubclass; variable and write out using topaz package format. Spice things up with extension methods for each of the classes."

	"https://github.com/GemTalk/Rowan/issues/820"

	| componentName packageName1 packageName2 packageName3 className1 className2 className3 className4 package1 package2 package3 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Tests'.
	packageName3 := projectName , '-' , 'Extensions'.	"new"
	className1 := projectName , 'Class1'.
	className2 := projectName , 'ByteClass1'.
	className3 := projectName , 'IndexableClass1'.
	className4 := projectName , 'TestCase'.

	resolvedProject addLoadComponentNamed: componentName.

	package1 := resolvedProject
		addPackageNamed: packageName1
		toComponentNamed: componentName.
	package2 := resolvedProject
		addPackageNamed: packageName2
		toComponentNamed: componentName.
	package3 := resolvedProject
		addPackageNamed: packageName3
		toComponentNamed: componentName.

	package1 addClassNamed: className1 super: 'Object' category: packageName1.
	(package1
		addClassNamed: className2
		super: 'Object'
		category: packageName1
		type: 'byteSubclass')
		addClassMethod:
			'with: aValue with: val2 | inst | inst := self new: 2. inst at: 1 put: aValue; at: 2 put: val2. ^inst'
		protocol: 'instance creation'.
	(package1
		addClassNamed: className3
		super: 'Object'
		category: packageName1
		type: 'variable')
		addClassMethod:
			'with: aValue | inst | inst := self new: 1. inst at: 1 put: aValue. ^inst'
		protocol: 'instance creation'.

	package2 addClassNamed: className4 super: 'TestCase' category: packageName2.

	(package3 addClassExtensionNamed: className1)
		addClassMethod: 'bar ^''bar'''
		protocol: '*' , packageName3 asLowercase.
	(package3 addClassExtensionNamed: className2)
		addClassMethod: 'bar ^''bar'''
		protocol: '*' , packageName3 asLowercase.
	(package3 addClassExtensionNamed: className3)
		addClassMethod: 'bar ^''bar'''
		protocol: '*' , packageName3 asLowercase.

	((resolvedProject packageNamed: packageName2) classDefinitionNamed: className4)
		addInstanceMethod: 'test1  self assert: ' , className1 , ' bar = ''bar'''
			protocol: 'tests';
		addInstanceMethod: 'test2  self assert: ' , className2 , ' bar = ''bar'''
			protocol: 'tests';
		addInstanceMethod: 'test3  self assert: ' , className3 , ' bar = ''bar'''
			protocol: 'tests';
		addInstanceMethod:
				'test4  self assert: ((' , className2 , ' with: 1 with: 255) at: 1) equals: 1'
			protocol: 'tests';
		addInstanceMethod:
				'test5  self assert: ((' , className3 , ' with: #abc) at: 1) equals: #abc'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0084: resolvedProject [
	"Start with spec_0001; add and use Traits"

	| packageName1 packageName2 packageName3 traitName className1 className2 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Traits'.
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.
	resolvedProject
		addPackagesNamed: {packageName2} toComponentNamed: 'Core';
		yourself.
	traitName := projectName , 'Trait1'.
	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		instVarNames: #('iv1');
		classInstVarNames: #('civ1');
		classVarNames: #('Cv1');
		traits: traitName;
		classTraits: traitName , ' classTrait';
		yourself.
	((resolvedProject packageNamed: packageName2)
		addTraitNamed: traitName
		instvars: #('iv1')
		classinstvars: #('civ1')
		classvars: #('Cv1'))
		addInstanceMethod: 'iv1 ^ iv1' protocol: 'accessing';
		addInstanceMethod: 'cv1 ^ Cv1' protocol: 'accessing';
		addClassMethod: 'civ1 ^ civ1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		addInstanceMethod: 'testIv1  self assert: ' , className1 , ' new iv1 = nil'
			protocol: 'tests';
		addInstanceMethod: 'testCiv1  self assert: ' , className1 , ' civ1 = nil'
			protocol: 'tests';
		addInstanceMethod: 'testCv1  self assert: ' , className1 , ' new cv1 = nil'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0085_A: resolvedProject [
"Start from scratch and build a package/component structure for testing different Class/Trait combinations. Classes are in separate C* packages. Traits are in separate Tr* packages. Tests  are in separate T* packages. Each C* package is in a C* conditional (C*) component. Each Tr* package is in a Tr* conditional (Tr*) component. Each Test* package is in a Test* conditional (C*-T*) component. There is a Test package for Core/Trait combo. The idea is that we can test a significant number of Class/Trait combinations in a single project using conditions to control what is loaded.

List of condition tuples intended to work together, since they were built together:
	1. C01, Tr01, T01

Interesting things could happen when different conditions are mixed together, but transitions between built tuples should always work.
"

	| componentName1 componentName2 componentName3 componentName4 componentName5 
		packageName1 packageName2 packageName3 packageName4 packageName5 traitName 
		className1 className2 index |
	traitName := projectName , 'Trait1'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	index := '01'.
	componentName1 := 'C' , index.
	componentName2 := 'Tr' , index.
	componentName3 := 'T' , index.
	componentName4 := 'T' , index, '_class'.
	componentName5 := 'T' , index, '_trait'.
	resolvedProject
		addSubcomponentNamed: componentName1
			condition: 'C' , index
			comment: 'Class package'
			toComponentNamed: 'Core';
		addSubcomponentNamed: componentName2
			condition: 'Tr' , index
			comment: 'Trait package'
			toComponentNamed: 'Core';
		addSubcomponentNamed: componentName3
			condition: 'T' , index
			comment: 'Test package'
			toComponentNamed: 'Core';
		addSubcomponentNamed: componentName4
			condition: 'C' , index
			comment: 'Class Test package'
			toComponentNamed: componentName3;
		addSubcomponentNamed: componentName5
			condition: 'Tr' , index
			comment: 'Trait Test package'
			toComponentNamed: componentName3;
		yourself.


	packageName1 := projectName , '-' , 'C' , index.
	packageName2 := projectName , '-' , 'Tr' , index.
	packageName3 := projectName , '-' , 'T' , index.
	packageName4 := projectName , '-' , 'C', '-' , 'T' , index.
	packageName5 := projectName , '-' , 'Tr', '-' , 'T'  , index.
	resolvedProject
		addPackagesNamed: {packageName1} toComponentNamed: componentName1;
		addPackagesNamed: {packageName2} toComponentNamed: componentName2;
		addPackagesNamed: {packageName3} toComponentNamed: componentName3;
		addPackagesNamed: {packageName4} toComponentNamed: componentName4;
		addPackagesNamed: {packageName5} toComponentNamed: componentName5;
		yourself.
	((resolvedProject packageNamed: packageName1)
		addClassNamed: className1
		super: 'Object'
		instvars: #('iv1')
		classinstvars: #('civ1')
		classvars: #('Cv1')
		category: packageName1
		comment: 'I am an example class')
		traits: traitName;
		classTraits: traitName , ' classTrait';
		addInstanceMethod: 'iv1 ^ iv1' protocol: 'accessing';
		addInstanceMethod: 'cv1 ^ Cv1' protocol: 'accessing';
		addClassMethod: 'civ1 ^ civ1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName2)
		addTraitNamed: traitName
		instvars: #('iv1')
		classinstvars: #('civ1')
		classvars: #('Cv1')
		category: packageName2)
			addInstanceMethod: 'iv1_t ^ iv1' protocol: 'accessing';
			addInstanceMethod: 'cv1_t ^ Cv1' protocol: 'accessing';
			addClassMethod: 'civ1_t ^ civ1' protocol: 'accessing';
			yourself.
	(resolvedProject packageNamed: packageName3)
		addClassNamed: className2
		super: 'TestCase'
		category: packageName3
		comment: 'I test the example class'.
	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: className2)
			addInstanceMethod: 'testIv1  self assert: ' , className1 , ' new iv1 = nil'
				protocol: 'tests';
			addInstanceMethod: 'testCiv1  self assert: ' , className1 , ' civ1 = nil'
				protocol: 'tests';
			addInstanceMethod: 'testCv1  self assert: ' , className1 , ' new cv1 = nil'
				protocol: 'tests';
			yourself.
	((resolvedProject packageNamed: packageName5)
		addClassExtensionNamed: className2)
			addInstanceMethod:
					'testIv1_t  self assert: ' , className1 , ' new iv1_t = nil'
				protocol: 'tests';
			addInstanceMethod: 'testCiv1_t  self assert: ' , className1 , ' civ1_t = nil'
				protocol: 'tests';
			addInstanceMethod:
					'testCv1_t  self assert: ' , className1 , ' new cv1_t = nil'
				protocol: 'tests';
			addInstanceMethod:
					'testTrait  self assert: ((ClassOrganizer new traits) includes: ', traitName, ')'
				protocol: 'tests';
			yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0085_B: resolvedProject [
"Start from scratch and build a package/component structure for testing different Class/Trait combinations. Classes are in separate C* packages. Traits are in separate Tr* packages. Tests  are in separate T* packages. Each C* package is in a C* conditional (C*) component. Each Tr* package is in a Tr* conditional (Tr*) component. Each Test* package is in a Test* conditional (C*-T*) component. There is a Test package for Core/Trait combo. The idea is that we can test a significant number of Class/Trait combinations in a single project using conditions to control what is loaded.

List of condition tuples intended to work together, since they were built together:
	1. C01, Tr01, T01

Interesting things could happen when different conditions are mixed together, but transitions between built tuples should always work.
"

	| packageName1 packageName2 packageName3 traitName className1 className2 |
	traitName := projectName , 'Trait1'.
	packageName1 := projectName , '-' , 'Core1'.
	packageName2 := projectName , '-' , 'Trait1'.
	packageName3 := projectName , '-' , 'Test1'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.
	resolvedProject
		addPackagesNamed: {packageName2} toComponentNamed: 'Core';
		yourself.
	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		instVarNames: #('iv1');
		classInstVarNames: #('civ1');
		classVarNames: #('Cv1');
		yourself.
	((resolvedProject packageNamed: packageName2)
		addTraitNamed: traitName
		instvars: #('iv1')
		classinstvars: #('civ1')
		classvars: #('Cv1'))
		addInstanceMethod: 'iv1 ^ iv1' protocol: 'accessing';
		addInstanceMethod: 'cv1 ^ Cv1' protocol: 'accessing';
		addClassMethod: 'civ1 ^ civ1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		addInstanceMethod: 'testIv1  self assert: ' , className1 , ' new iv1 = nil'
			protocol: 'tests';
		addInstanceMethod: 'testCiv1  self assert: ' , className1 , ' civ1 = nil'
			protocol: 'tests';
		addInstanceMethod: 'testCv1  self assert: ' , className1 , ' new cv1 = nil'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0085_C: resolvedProject [
"Start from scratch and build a package/component structure for testing different Class/Trait combinations. Classes are in separate C* packages. Traits are in separate Tr* packages. Tests  are in separate T* packages. Each C* package is in a C* conditional (C*) component. Each Tr* package is in a Tr* conditional (Tr*) component. Each Test* package is in a Test* conditional (C*-T*) component. There is a Test package for Core/Trait combo. The idea is that we can test a significant number of Class/Trait combinations in a single project using conditions to control what is loaded.

List of condition tuples intended to work together, since they were built together:
	1. C01, Tr01, T01

Interesting things could happen when different conditions are mixed together, but transitions between built tuples should always work.
"

	| packageName1 packageName2 packageName3 traitName className1 className2 |
	traitName := projectName , 'Trait1'.
	packageName1 := projectName , '-' , 'Core1'.
	packageName2 := projectName , '-' , 'Trait1'.
	packageName3 := projectName , '-' , 'Test1'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.
	resolvedProject
		addPackagesNamed: {packageName2} toComponentNamed: 'Core';
		yourself.
	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		instVarNames: #('iv1');
		classInstVarNames: #('civ1');
		classVarNames: #('Cv1');
		yourself.
	((resolvedProject packageNamed: packageName2)
		addTraitNamed: traitName
		instvars: #('iv1')
		classinstvars: #('civ1')
		classvars: #('Cv1'))
		addInstanceMethod: 'iv1 ^ iv1' protocol: 'accessing';
		addInstanceMethod: 'cv1 ^ Cv1' protocol: 'accessing';
		addClassMethod: 'civ1 ^ civ1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		addInstanceMethod: 'testIv1  self assert: ' , className1 , ' new iv1 = nil'
			protocol: 'tests';
		addInstanceMethod: 'testCiv1  self assert: ' , className1 , ' civ1 = nil'
			protocol: 'tests';
		addInstanceMethod: 'testCv1  self assert: ' , className1 , ' new cv1 = nil'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _addDefinitionsFor_0085: resolvedProject [
"Start from scratch and build a package/component structure for testing different Class/Trait combinations. Classes are in separate C* packages. Traits are in separate Tr* packages. Tests  are in separate T* packages. Each C* package is in a C* conditional (C*) component. Each Tr* package is in a Tr* conditional (Tr*) component. Each Test* package is in a Test* conditional (C*-T*) component. There is a Test package for Core/Trait combo. The idea is that we can test a significant number of Class/Trait combinations in a single project using conditions to control what is loaded.

List of condition tuples intended to work together, since they were built together:
	1. C01, Tr01, T01

Interesting things could happen when different conditions are mixed together, but transitions between built tuples should always work.
"

	resolvedProject
		addTopLevelComponentNamed: 'Core'
		comment: 'Single top-level component, that lists all of conditional components that can be loaded. The packages that are actually loaded, will be dictated by the conditional attributes'.
	self _addDefinitionsFor_0085_A: resolvedProject.
true ifTrue: [ ^ resolvedProject ].
	self _addDefinitionsFor_0085_B: resolvedProject.
	self _addDefinitionsFor_0085_C: resolvedProject.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _createNewProject: aString [
	^ Rowan newProjectNamed: aString
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> _genSpecFor_0071: specName definedProject: definedProject addDefinitions: addDefinitionsBlock [
	"Issue #714, so fiddling with directory structure"

	| resolvedRepository resolvedProject |
	resolvedProject := definedProject resolveStrict.

	resolvedRepository := resolvedProject repository.
	(resolvedRepository branches includes: specName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			resolvedRepository checkout: specName.
			resolvedRepository pull: resolvedRepository remote branch: specName ]
		ifFalse: [ 
			"create the branch"
			resolvedRepository createBranch: specName ].

	resolvedProject revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"

	resolvedRepository repositoryRoot ensureDeleteAll.

	addDefinitionsBlock value: resolvedProject defined.

	resolvedProject
		gitUrl: self projectUrl;
		export;
		exportLoadSpecification.	"write the rowan directory structure"

	^ resolvedRepository
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0065: commitMessage [
	"Start with spec_0060; replace external project RowanSample10 spec_0001 with spec_0002"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0065'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0060'.
	title := 'Start with spec_0060; replace external project RowanSample10 spec_0001 with spec_0002'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 65
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(700);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0065: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0066: commitMessage [
	"Start with spec_0001; add RowanSample10, RowanSampleProjects11 and RowanSample12 as required projects"

	"https://github.com/GemTalk/Rowan/issues/701"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0066'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001; add RowanSample10, RowanSampleProjects11 and RowanSample12 as required projects'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 66
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(701);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0066: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0067: commitMessage [
	"Start with spec_0001 (RowanSample9) and spec_0003 (RowanSample10, RowanSample11, RowanSample12); linear required project chain; RowanSample9 requires RowanSample10; RowanSample10 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10Class1>RowanSample9Class1"

	"https://github.com/GemTalk/Rowan/issues/701"
	"https://github.com/GemTalk/Rowan/issues/861"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0067'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001 (RowanSample9) and spec_0003 (RowanSample10, RowanSample11, RowanSample12); linear required project chain; RowanSample9 requires RowanSample10; RowanSample10 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10Class1>RowanSample9Class1'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 67
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(701 861);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0067: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0068: commitMessage [
	"Start with spec_0067 (RowanSample9V3) and spec_0004 (RowanSample10V3, RowanSample11, RowanSample12); recursive required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V3; class hierarchy runs RowanSample9V3Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1"

	"https://github.com/GemTalk/Rowan/issues/701"
	"https://github.com/GemTalk/Rowan/issues/861"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0068'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0067'.
	title := 'Start with spec_0067 (RowanSample9V3) and spec_0004 (RowanSample10V3, RowanSample11, RowanSample12); recursive required project chain; RowanSample9V3 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V3; class hierarchy runs RowanSample9V3Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V3Class1'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 68
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(701 861);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0068: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0069: commitMessage [
	"Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0005 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0006 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure."

	"https://github.com/GemTalk/Rowan/issues/705"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title canonicalLoadSpec |
	postfix := '0069'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Start with spec_0000 (RowanSample9) and add Parent, Child and Bridge top-level components with corresponding packages. The Bridge component is not expected to be loaded with the Parent component, because it has a required project conflict.

The Parent package has an AbstractRowanSample9ParentClass1 class. RowanSample12:spec_0005 requires RowanSample9:spec_0069 and has a RowanSample12ConcreteClass1 class that suclasses AbstractRowanSample9ParentClass1. 

The Child package has a concrete class that subclasses RowanSample10Class1, making RowanSample10:spec0001 a required project. 

The Bridge package has an AbstractRowanSample9BridgeClass1 class that is a subclass of AbstractRowanSample11ParentClass1 from RowanSample11:spec_0005. The class AbstractRowanSample9BridgeClass1 has subclass RowanSample12ConcreteClass1 from the project RowanSample12:spec_0006 that requires RowanSample9:spec_0069. AbstractRowanSample9BridgeClass1 has a method #foo that is implemented with subclassResponsibility and #foo is defined in RowanSample12ConcreteClass1.

Each of the components in RowanSample9:spec_0069 has a corresponding test case that confirms the class structure.'.

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
RwTestProjectLibraryGeneratorV3 >> genSpec_0070: commitMessage [
	"Start with spec_0001 (RowanSample9) add 2 more components: attr1/Core1 that adds the class RowanSample9Class2 as a subclass of RowanSample9Class1 and attr2/Core2 that adds the class RowanSample9Class3 as a subclass of RowanSampleClass1. Tests are included by attr1/tests/Core1 and attr2/tests/Core2 The custom conditional attributes attr1 and attr2 are used to control  whether or not the classes are loaded. By default the attribute list includes only tests."

	"https://github.com/GemTalk/Rowan/issues/711"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0070'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001 (RowanSample9) add 2 more components: attr1/Core1 that adds the class RowanSample9Class2 as a subclass of RowanSample9Class1 and attr2/Core2 that adds the class RowanSample9Class3 as a subclass of RowanSampleClass1. Tests are included by attr1/tests/Core1 and attr2/tests/Core2 The custom conditional attributes attr1 and attr2 are used to control  whether or not the classes are loaded. By default the attribute list includes only tests.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 70
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(711);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0070: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0071: commitMessage [
	"Start with spec_0000 and create a directory structure that is offset by one directory deeper from git root. Enable me to play games with differences between git root and repository root."

	"https://github.com/GemTalk/Rowan/issues/714"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0071'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Start with spec_0000 and create a directory structure that is offset by one directory deeper from git root. Enable me to play games with differences between git root and repository root.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 71
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(714);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		relativeRepositoryRoot: 'offset';
		addLoadComponentNamed: 'Core';
		addSubcomponentNamed: 'tests/Tests'
			condition: 'tests'
			toComponentNamed: 'Core';
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor_0071: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | 
			self _addDefinitionsFor_0001: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0072: commitMessage [
	"Start with spec_0075; change the project spec so that none of the directories are using the default path"

	"https://github.com/GemTalk/Rowan/issues/724"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embeddedProjectDefinition requiredProjectName relativeRepositoryRoot |
	postfix := '0072'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0075'.
	title := 'Start with spec_0053; change the project spec so that none of the directories are using the default path'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 72
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(724);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	relativeRepositoryRoot := 'embeddedProjects/' , requiredProjectName.
	embeddedProjectDefinition := (self _createNewProject: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: 'rowan/project.ston';
		revision: derivedFrom;
		gitUrl: self projectUrl;
		relativeRepositoryRoot: relativeRepositoryRoot;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: {'tests'};
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embeddedProjectDefinition
		addDefinitions: [ :resolvedProject :embeddedProject | self _addDefinitionsFor_0072: resolvedProject embeddedProject: embeddedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0073: commitMessage [
	"Start with  spec_0001, add 3 additional projects, using relativeRepositoryRoot to distinquish between projects"

	"https://github.com/GemTalk/Rowan/issues/571"

	"https://github.com/GemTalk/Rowan/issues/714"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0073'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0001, add 3 additional projects, using relativeRepositoryRoot to distinquish between projects'
		index: 73
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(571 714);
		yourself.

	definedProject := (self _createNewProject: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0073: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0074: commitMessage [
	"Start with spec_0001; add an additional project using relativeRepositoryRoot that is basically a copy of spec_001; and adjust class in primary project be a subclass of class in additional project"

	"https://github.com/GemTalk/Rowan/issues/714"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0074'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001; add an additional project using relativeRepositoryRoot that is basically a copy of spec_001; and adjust class in primary project be a subclass of class in additional project'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 74
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(591 571);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0074: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0075: commitMessage [
	"Start with spec_0074; add instance side method #ivar1 referencing inherited ivar to class in primary project; add instance method #ivar1_embedded to class in embedded project"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embeddedProjectDefinition requiredProjectName relativeRepositoryRoot |
	postfix := '0075'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0074'.
	title := 'Start with spec_0074; add instance side method #ivar1 referencing inherited ivar to class in primary project; add instance method #ivar1_embedded to class in embedded project'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 75
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(591);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	relativeRepositoryRoot := 'embeddedProjects/', requiredProjectName.
	embeddedProjectDefinition := (self _createNewProject: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: 'rowan/project.ston';
		revision: derivedFrom;
		gitUrl: self projectUrl;
		relativeRepositoryRoot: relativeRepositoryRoot;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: {'tests'} yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embeddedProjectDefinition
		addDefinitions: [ :resolvedProject :embeddedProject | self _addDefinitionsFor_0075: resolvedProject embeddedProject: embeddedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0076: commitMessage [
	"Start with spec_0074; add platform conditional packages to the embedded project (using relativeRepositoryRoot) and the primary project."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embeddedProjectDefinition requiredProjectName relativeRepositoryRoot |
	postfix := '0076'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0074'.
	title := 'Start with spec_0074; add platform conditional packages to the embedded project (using relativeRepositoryRoot) and the primary project.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 76
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(594);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	relativeRepositoryRoot := 'embeddedProjects/', requiredProjectName.
	embeddedProjectDefinition :=  (self _createNewProject: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: 'rowan/project.ston';
		revision: derivedFrom;
		gitUrl: self projectUrl;
		relativeRepositoryRoot: relativeRepositoryRoot;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embeddedProjectDefinition
		addDefinitions: [ :resolvedProject :embeddedProject | self _addDefinitionsFor_0076: resolvedProject embeddedProject: embeddedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0077: commitMessage [
	"Start with spec_0074; add platform conditional packages (GemStone version 2.4.0) to the embedded project (using relativeRepositoryRoot) and the primary project."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embeddedProjectDefinition requiredProjectName relativeRepositoryRoot |
	postfix := '0077'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0074'.
	title := 'Start with spec_0074; add platform conditional packages (GemStone version 2.4.0) to the embedded project (using relativeRepositoryRoot) and the primary project.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 77
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(594);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	relativeRepositoryRoot := 'embeddedProjects/', requiredProjectName.
	embeddedProjectDefinition :=  (self _createNewProject: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: 'rowan/project.ston';
		revision: derivedFrom;
		gitUrl: self projectUrl;
		relativeRepositoryRoot: relativeRepositoryRoot;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embeddedProjectDefinition
		addDefinitions: [ :resolvedProject :embeddedProject | self _addDefinitionsFor_0077: resolvedProject embeddedProject: embeddedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0078: commitMessage [
	"Start with spec_0074; add platform conditional packages (GemStone version 3.6.x, 3.5.x, and 3.2.15) to the embedded project (using relativeRepositoryRoot) and the primary project. This is expected to reproduce issue #594 for those three patforms."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embeddedProjectDefinition requiredProjectName relativeRepositoryRoot |
	postfix := '0078'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0074'.
	title := 'Start with spec_0074; add platform conditional packages (GemStone version 3.6.x, 3.5.x, and 3.2.15) to the embedded project (using relativeRepositoryRoot) and the primary project. This is expected to reproduce issue #594 for those three patforms.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 78
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(594);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	relativeRepositoryRoot := 'embeddedProjects/', requiredProjectName.

	embeddedProjectDefinition :=  (self _createNewProject: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: 'rowan/project.ston';
		revision: derivedFrom;
		gitUrl: self projectUrl;
		relativeRepositoryRoot: relativeRepositoryRoot;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embeddedProjectDefinition
		addDefinitions: [ :resolvedProject :embededProject | self _addDefinitionsFor_0078: resolvedProject embeddedProject: embededProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0082: commitMessage [
	"Start with master; add Core, Extension, and Test packages. Add classes to Core that use the full complement of class types: normal, byteSubclass, and variable. Add extension methods for each of the classes. Add tests for each class. Use topaz package format."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0082'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.
	title := 'Start with master; add Core, Extension, and Test packages. Add classes to Core that use the full complement of class types: normal, byteSubclass, and variable. Add extension methods for each of the classes. Add tests for each class. Use topaz package format.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 82
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #( 820);
		yourself.
	definedProject := (Rowan newProjectNamed: self projectName)
		projectsHome: self projectsHome;
		packageFormat: 'topaz';
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		packagesPath: 'src';
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0082: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0084: commitMessage [
	"Start with spec_0001 (RowanSample9V3) and add a Trait definition. Update RowanSample9V3Class1 to use the Trait, instance and class size ..  update tests to test the trait methods."

	"https://github.com/GemTalk/Rowan/issues/783"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0084'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001 (RowanSample9V3) and add a Trait definition. Update RowanSample9V3Class1to use the Trait, instance and class size ..  update tests to test the trait methods'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 84
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(783);
		yourself.

	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0084: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGeneratorV3 >> genSpec_0085: commitMessage [
"Start from scratch and build a package/component structure for testing different Class/Trait combinations. Classes are in separate C* packages. Traits are in separate Tr* packages. Tests  are in separate T* packages. Each C* package is in a C* conditional (C*) component. Each Tr* package is in a Tr* conditional (Tr*) component. Each Test* package is in a Test* conditional (C*-T*) component. There is a Test package for Core/Trait combo. The idea is that we can test a significant number of Class/Trait combinations in a single project using conditions to control what is loaded.

List of condition tuples intended to work together, since they were built together:
	1. C01, Tr01, T01

Interesting things could happen when different conditions are mixed together, but transitions between built tuples should always work.
"

	"https://github.com/GemTalk/Rowan/issues/783"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0085'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.
	title := 'Start from scratch and build a package/component structure for testing different Class/Trait combinations. Classes are in separate C* packages. Traits are in separate Tr* packages. Tests  are in separate T* packages. Each C* package is in a C* conditional (C*) component. Each Tr* package is in a Tr* conditional (Tr*) component. Each Test* package is in a Test* conditional (C*-T*) component. There is a Test package for Core/Trait combo. The idea is that we can test a significant number of Class/Trait combinations in a single project using conditions to control what is loaded.

List of condition tuples intended to work together, since they were built together:

C01, Tr01, T01

Interesting things could happen when different conditions are mixed together, but transitions between built tuples should always work.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 85
		derivedFrom:derivedFrom
		comment: '')
		rowanIssues: #(783);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		packageConvention: 'Rowan';
		specComponentNames: {'Core'};
		customConditionalAttributes: {'C01' .  'T01' . 'Tr01' };
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0085: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorV3 >> primaryProjectName [
	^ 'RowanSample9V3'
]

{ #category : 'accessing' }
RwTestProjectLibraryGeneratorV3 >> projectReadmeIntro [
	"Intro in README -- change this to reflect that this is for V3.0 not V2.0 and that it is a v3 variant of RowanSample9 ... do the change when you are prepared to push changes to masterV3.2"

	^ 'Sample project that is being used to develop the **final** version of the V2.0 project handling API.'
]
