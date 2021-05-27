Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'to be removed' }
RwPrjReadToolV2 >> readLoadedProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
		useLoadedProjects: true
]

{ #category : 'to be removed' }
RwPrjReadToolV2 >> readProjectForResolvedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	RwResolvedProjectComponentVisitorV2
		readProjectForResolvedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject readPackageNames: resolvedProject packageNames.
	^ resolvedProject
]

{ #category : 'read produced projects' }
RwPrjReadToolV2 >> readProjectSetForProducedProject: producedProject [
	^ self
		readProjectSetForProducedProject: producedProject
		platformConditionalAttributes: producedProject platformConditionalAttributes
]

{ #category : 'read produced projects' }
RwPrjReadToolV2 >> readProjectSetForProducedProject: producedProject platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForProducedProject: producedProject
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName [
	| project |
	project := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ project readLoadedProjectSet
]

{ #category : 'to be removed' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName customConditionalAttributes: customConditionalAttributes [
	| project |
	project := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ project
		readLoadedProjectSet: customConditionalAttributes
		platformConditionalAttributes: project platformConditionalAttributes
]

{ #category : 'to be removed' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ (Rowan image loadedProjectNamed: projectName) asDefinition
		readLoadedProjectSet: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'to be removed' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'to be removed' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]
