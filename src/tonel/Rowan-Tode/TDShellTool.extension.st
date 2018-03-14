Extension { #name : 'TDShellTool' }

{ #category : '*rowan-tode' }
TDShellTool >> browseRowanClassHierarchy: aClass label: windowLabel [

	^ (aClass asTDClassDefinition
		appEnv: self applicationEnvironment;
		yourself)
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'rowanHierarchyList')
				windowName: #'hierarchy';
				windowLabel: windowLabel;
				yourself)
]
