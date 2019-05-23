*rowan-core-definitions-extensions
compareSourceAgainstBaseForNewClassVersion: aDefinition

	"unconditional modification for new class versions"

	| modification before after |
	modification := RwSourceModification new.
	before := aDefinition source.
	after := self source.
	modification
		addElementModification:
			(RwUnconditionalPropertyModification
				key: 'source'
				oldValue: before
				newValue: after).
	^ modification