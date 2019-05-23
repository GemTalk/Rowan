private
_assert: foundBlock forClassName: className ifAbsent: absentBlock

	"validate loaded class registry structure"

	| projectTools |
	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassNamed: className
		ifFound: foundBlock
		ifAbsent: absentBlock