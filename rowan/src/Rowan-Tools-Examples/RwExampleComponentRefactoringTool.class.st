"
This is an example tool that was used to refactor the Rowan component structure that was present 
in commit ebdabb9d9 of the Rowan project. See https://github.com/GemTalk/Rowan/issues/617 for
additional information.

The goal of the restructuring was to move the tests, classes and methods that were managed by 
conditional components with the condition v2Only and v2 into packages where possible and new
component structure otherwise. The process involved basically folding the v2 and v2Only 
components into the existing common component structure.

If you inspect the result of the following expression:

	Rowan projectTools examples componentRefactoring forProjectNamed: 'Rowan' componentNamed: 'Rowan'

you see that there are 6 instance variables:
	project					- the loaded project for Rowan (instance of RwProject)
	components			- a dictionary of the components on disk for the current Rowan project
	packages				- a dictionary that maps the packageNames to the component that manages
									the package
	conditions				- a dictionary that maps the subcomponent conditions to a list of the
									subcomponents that specify that condition
	platformConditions	- a dictionary that maps the platformComponent condition arrays to a list of
									the plaformComponents that specifies that condition array

Inspecting these fields can give you an overview of the composition of the given project and load spec.

The method #refactorComponentsWithCondition: does folds the classes and methods into existing 
packages or creates new components to manage the existing packages when an appropriate existing
package cannot be found.

	Rowan projectTools examples componentRefactoring componentRefactoring forProjectNamed: 'Rowan' componentNamed: 'Rowan'
		refactorComponentsWithCondition: 'v2Only';
		refactorComponentsWithCondition: 'v2';
		export
"
Class {
	#name : 'RwExampleComponentRefactoringTool',
	#superclass : 'RwExamplesTool',
	#instVars : [
		'definedProject',
		'components',
		'packages',
		'conditions',
		'platformConditions'
	],
	#category : 'Rowan-Tools-Examples'
}

{ #category : 'instance creation' }
RwExampleComponentRefactoringTool class >> forProjectNamed: projectName componentNamed: componentName [
	^ self new forProjectNamed: projectName componentNamed: componentName
]

{ #category : 'instance creation' }
RwExampleComponentRefactoringTool class >> new [
	^ self basicNew initialize
]

{ #category : 'accessing' }
RwExampleComponentRefactoringTool >> components [
	^ components
]

{ #category : 'accessing' }
RwExampleComponentRefactoringTool >> conditions [
	^ conditions
]

{ #category : 'accessing' }
RwExampleComponentRefactoringTool >> definedProject [
	^ definedProject ifNil: [ definedProject := self project defined ]
]

{ #category : 'initialization' }
RwExampleComponentRefactoringTool >> forProjectNamed: projectName componentNamed: componentName [
	| notFoundBlock |
	definedProject := (Rowan projectNamed: projectName) defined.
	components := Dictionary new.
	packages := Dictionary new.
	conditions := Dictionary new.
	platformConditions := Dictionary new.
	notFoundBlock := [ :missingComponentName | 
	| missingComponent |
	missingComponent := RwAbstractComponent
		fromComponentsDirectory: definedProject componentsRoot
		named: missingComponentName.
	components at: missingComponentName put: missingComponent packageNames.
	missingComponent ].
	definedProject _concreteProject
		allComponentsIn: componentName
		matchBlock: [ :ignored | true ]
		notFound: notFoundBlock
		do: [ :component | 
			| condition |
			condition := component condition.
			condition _isArray
				ifTrue: [ 
					| dict |
					dict := platformConditions at: condition ifAbsentPut: [ Dictionary new ].
					(dict at: 'packages' ifAbsentPut: [ Set new ])
						addAll: component packageNames.
					(dict at: 'components' ifAbsentPut: [ IdentitySet new ]) add: component ]
				ifFalse: [ 
					| dict |
					dict := conditions at: condition ifAbsentPut: [ Dictionary new ].
					(dict at: 'packages' ifAbsentPut: [ Set new ])
						addAll: component packageNames.
					(dict at: 'components' ifAbsentPut: [ IdentitySet new ]) add: component.
					(dict at: 'allPackageNames' ifAbsentPut: [ Set new ])
						addAll:
							(definedProject _resolvedProject
								allPackageNamesIn: component name
								matchBlock: [ :ignored | true ]
								notFound: notFoundBlock) ].
			components at: component name put: component.
			component packageNames
				do: [ :packageName | 
					packages at: packageName ifPresent: [ self halt: 'package duplication' ].
					packages at: packageName put: component ] ]
]

{ #category : 'initialization' }
RwExampleComponentRefactoringTool >> initialize [
	components := Dictionary new.
	packages := Dictionary new.
	conditions := Dictionary new.
	platformConditions := Dictionary new
]

{ #category : 'accessing' }
RwExampleComponentRefactoringTool >> packages [
	^ packages
]

{ #category : 'accessing' }
RwExampleComponentRefactoringTool >> platformConditions [
	^ platformConditions
]

{ #category : 'refactoring' }
RwExampleComponentRefactoringTool >> refactorComponentsWithCondition: targetCondition [
	"This refactoring code is based on the assumption that the component structure was 
		created using the addComponentStructureFor:... family of methods or were 
		created following the addComponentStructureFor:... algorithm manually"

	^ self
		refactorComponentsWithCondition: targetCondition
		packageMap: Dictionary new
		ifAbsent: [ :packageName | 
			self
				error:
					'No package name for ' , packageName printString , ' found in package map' ]
]

{ #category : 'refactoring' }
RwExampleComponentRefactoringTool >> refactorComponentsWithCondition: targetCondition packageMap: packageMap ifAbsent: absentBlock [
	"This refactoring code is based on the assumption that the component structure was 
		created using the addComponentStructureFor:... family of methods or were 
		created following the addComponentStructureFor:... algorithm manually"

	((conditions at: targetCondition) at: 'components')
		do: [ :component | 
			| componentName path basename destinationComponentName x y |
			componentName := component name.
			path := FileSystem * componentName.
			basename := path basename.
			path parent basename = targetCondition
				ifTrue: [ 
					destinationComponentName := path parent parent.
					component packageNames
						do: [ :packageName | 
							| destinationPackageName package |
							destinationPackageName := packageMap
								at: packageName
								ifAbsent: [ absentBlock cull: packageName ].
							package := self definedProject
								packageNamed: packageName
								ifAbsent: [ 
									self
										error:
											'Cannot find package named ' , packageName printString , ' in defined project.' ].
							(x := package classDefinitions keys copy)
								do: [ :className | definedProject moveClassNamed: className toPackageNamed: destinationPackageName ].
							(y := package classExtensions keys copy)
								do: [ :className | 
									definedProject
										moveClassExtensionNamed: className
										fromPackageNamed: packageName
										toPackageNamed: destinationPackageName ].
							self halt: 'remove the package' ] ]
				ifFalse: [ 
					(x := path parent basename) = 'platforms'
						ifFalse: [ self error: 'expected platforms' ].
					(x := path parent parent basename) = targetCondition
						ifTrue: [ destinationComponentName := path parent parent parent ]
						ifFalse: [ self error: 'expected ' , targetCondition ].
					self halt ].
			self halt ]
]
