Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'read produced projects' }
RwPrjReadToolV2 >> readProjectForProducedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	RwResolvedProjectComponentVisitorV2
		readProjectForProducedProject: resolvedProject
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

{ #category : 'read produced projects' }
RwPrjReadToolV2 >> readProjectSetForProducedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForProducedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName [
	| project |
	project := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ project readLoadedProjectSet
]
