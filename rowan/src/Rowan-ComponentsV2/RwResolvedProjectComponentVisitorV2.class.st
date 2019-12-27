Class {
	#name : 'RwResolvedProjectComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'projectDefinition',
		'resolvedProject'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'instance creation' }
RwResolvedProjectComponentVisitorV2 class >> resolvedProject: resolvedProject platformAttributes: platformConfigurationAttributes groupNames: groupNames [
	^ self new
		platformAttributes: platformConfigurationAttributes;
		groupNames: groupNames;
		resolvedProject: resolvedProject;
		yourself
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self projectDefinition addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> componentsRoot [
	^ self resolvedProject componentsRoot
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> packageNames [
	^ self resolvedProject packageNames
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectDefinition [
	^ self resolvedProject _projectDefinition
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectsRoot [
	^ self resolvedProject projectsRoot
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> resolvedProject [
	^ resolvedProject
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> resolvedProject: aResolvedProject [
	resolvedProject := aResolvedProject
]
