Class {
	#name : 'RwComponentDefinitionTest',
	#superclass : 'RwAbstractComponentDefinitionTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwComponentDefinitionTest >> _createRepositoryRoot [

	repositoryRoot := FileSystem memory * 'componentDefinitionTestRoot'
]

{ #category : 'tests' }
RwComponentDefinitionTest >> testBasicComponentDefinitionCreation [

	| componentDef  componentName1 subComponentName1 subComponentName2 packageName0 packageName1 packageName2 
		className0 className1 className2 |
	componentName1 := 'Example'.
	subComponentName1 := 'Core'.
	subComponentName2 := 'Tests'.
	packageName0 := 'Example-Common'.
	packageName1 := 'Example-Core'.
	packageName2 := 'Example-Tests'.
	className0 := 'ExampleCoreClass'.
	className1 := 'ExampleApplicationClass'.
	className2 := 'ExampleTests'.

	componentDef := RwComponentDefinition newNamed: componentName1.
	(componentDef addPackageNamed: packageName0)
			addClassDefinition: 
				((RwClassDefinition 
					newForClassNamed: className0
					super: 'Object'
					category: packageName0)
						addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'bar ^ 1' protocol: 'accessing');
						addClassMethodDefinition: (RwMethodDefinition newForSource: 'bar ^ 1' protocol: 'accessing');
						yourself);
			yourself.
	((componentDef addSubComponentNamed: subComponentName1)
		addPackageNamed: packageName1)
			addClassDefinition: 
				((RwClassDefinition 
					newForClassNamed: className1
					super: className0
					category: packageName1)
						addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'foo ^ 1' protocol: 'accessing');
						addClassMethodDefinition: (RwMethodDefinition newForSource: 'foo ^ 1' protocol: 'accessing');
						yourself);
			addClassExtensionDefinition: 
				((RwClassExtensionDefinition 
					newForClassNamed: className0)
						addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionMethod ^ 1' protocol: '*', packageName1 asLowercase);
						yourself);
			yourself.
	((componentDef addSubComponentNamed: subComponentName2)
		addPackageNamed: packageName2)
			addClassDefinition: 
				((RwClassDefinition 
					newForClassNamed: className2
					super: 'TestCase'
					category: packageName2)
						addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'test' protocol: 'testing');
						yourself);
			addClassExtensionDefinition: 
				((RwClassExtensionDefinition 
					newForClassNamed: className0)
						addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'testExtensionMethod ^ 1' protocol: '*', packageName2 asLowercase);
						yourself);
			yourself.
]
