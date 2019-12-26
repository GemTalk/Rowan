Extension { #name : 'RwProjectLoadComponentVisitorV2Test' }

{ #category : '*rowan-testsV2' }
RwProjectLoadComponentVisitorV2Test >> testBasicResolvedProjectVisit [
	"test of RwProjectLoadComponentVisitorV2 as it is used in the RwPrjReadToolV2."

	| platformAttributes groupNames componentsRoot basicProject visitor componentNamesToLoad projectName loadSpec projectAlias projectPath projectsHome |
	platformAttributes := {'common'.
	'gemstone'.
	('3.5.0' asRwGemStoneVersionNumber)}.
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_DiskConfig_Test'.
	componentNamesToLoad := #('Core').

	projectsHome := RwRowanSample9Test _testRowanProjectsSandbox asFileReference.
	projectPath := projectsHome / projectAlias.
	projectPath exists
		ifTrue: [ projectPath deleteAll ].

	loadSpec := RwLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		specName: projectName;
		projectsHome: projectsHome;
		componentNames: componentNamesToLoad;
		groupNames: #('core');
		projectSpecFile: 'rowan/project.ston';
		gitUrl: 'https://github.com/dalehenrich/RowanSample9';
		revision: 'spec_0008';
		yourself.
	basicProject := RwResolvedProjectV2 basicLoadSpecification: loadSpec.
	basicProject _projectRepository resolve.	"create clone"
	groupNames := loadSpec groupNames.
	componentsRoot := basicProject componentsRoot.

	self assert: basicProject packageNames isEmpty.

	visitor := self _visitorClass new
		platformAttributes: platformAttributes;
		groupNames: groupNames;
		componentsRoot: componentsRoot;
		projectsRoot: basicProject projectsRoot;
		resolvedProject: basicProject;
		yourself.
	projectName := basicProject projectAlias.

	self assert: basicProject packageNames isEmpty.

	componentNamesToLoad
		do: [ :componentName | 
			| component url |
			url := 'file:' , (componentsRoot / componentName , 'ston') pathString.
			component := RwAbstractProjectConfiguration fromUrl: url.
			component projectName: projectName.

			visitor visit: component ].
	self
		assert:
			basicProject packageNames sort
				=
					#('RowanSample9-Core' 'RowanSample9-Extensions' 'RowanSample9-GemStone') sort.
	self assert: visitor projectLoadSpecs isEmpty
]
