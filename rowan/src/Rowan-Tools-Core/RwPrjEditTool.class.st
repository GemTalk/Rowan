Class {
	#name : 'RwPrjEditTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'class editing' }
RwPrjEditTool >> addClass: classDefinition inPackageNamed: packageName inProject: projectDefinition [
	"Add the given class definition to the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassDefinition: classDefinition.
	^ projectDefinition
]

{ #category : 'class editing' }
RwPrjEditTool >> addClass: classDefinition inPackageNamed: packageName inProjectNamed: projectName [

	"Add the given class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition

]

{ #category : 'class extension editting' }
RwPrjEditTool >> addClassExtension: classExtensionDefinition inPackageNamed: packageName inProject: projectDefinition [
	"Add the given class extension definition to the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.
	^ projectDefinition
]

{ #category : 'class extension editting' }
RwPrjEditTool >> addClassExtension: classExtensionDefinition inPackageNamed: packageName inProjectNamed: projectName [

	"Add the given class extension definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		addClassExtension: classExtensionDefinition
		inPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition

]

{ #category : 'class editing' }
RwPrjEditTool >> removeClass: classDefinition fromPackageNamed: packageName inProject: projectDefinition [
	"Remove the given class definition from the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition removeClassDefinition: classDefinition.
	^ projectDefinition
]

{ #category : 'class editing' }
RwPrjEditTool >> removeClass: classDefinition fromPackageNamed: packageName inProjectNamed: projectName [

	"Remove the given class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		removeClass: classDefinition
		fromPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition

]

{ #category : 'class editing' }
RwPrjEditTool >> removeClassNamed: className fromPackageNamed: packageName inProject: projectDefinition [
	"Remove the named class definition from the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition removeClassNamed: className.
	^ projectDefinition
]

{ #category : 'class editing' }
RwPrjEditTool >> removeClassNamed: className fromPackageNamed: packageName inProjectNamed: projectName [

	"Remove the named class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		removeClassNamed: className
		fromPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition

]

{ #category : 'class editing' }
RwPrjEditTool >> updateOrAddClass: classDefinition inPackageNamed: packageName inProject: projectDefinition [
	"Update or add the given class definition to the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition updateClassDefinition: classDefinition.
	^ projectDefinition
]

{ #category : 'class editing' }
RwPrjEditTool >> updateOrAddClass: classDefinition inPackageNamed: packageName inProjectNamed: projectName [

	"Update or add the given class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		updateOrAddClass: classDefinition
		inPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition

]
