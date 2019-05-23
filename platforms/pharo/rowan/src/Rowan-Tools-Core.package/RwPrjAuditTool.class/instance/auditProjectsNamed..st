other
auditProjectsNamed: aCol
"audit all named projects"
	
	| res |
	res := Array new.
		aCol do: [:prjName | res addAll: (self auditForProjectNamed: prjName )	].
	^res
