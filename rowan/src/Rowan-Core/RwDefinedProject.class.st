Class {
	#name : 'RwDefinedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwDefinedProject >> _resolvedProject [
	^ resolvedProject
		ifNil: [ 
			resolvedProject := RwResolvedProjectV2 new
				projectName: self name;
				yourself ]
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: componentName [
	^ self _resolvedProject addNewComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName toComponentNamed: componentName [
	^ self _resolvedProject
		addPackageNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackagesNamed: packageName toComponentNamed: componentName [
	^ self _resolvedProject
		addPackagesNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> packageNamed: aString [
	^ self _resolvedProject packageNamed: aString
]

{ #category : 'accessing' }
RwDefinedProject >> repoType: aSymbol [
	"#disk, #git or #none"

	self _resolvedProject repoType: aSymbol
]

{ #category : 'transitions' }
RwDefinedProject >> resolve [
	^ RwResolvedProject fromDefinedProject: self
]
