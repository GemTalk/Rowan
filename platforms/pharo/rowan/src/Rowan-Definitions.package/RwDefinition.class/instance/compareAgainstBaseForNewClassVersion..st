comparing
compareAgainstBaseForNewClassVersion: aBaseDefinition

	"Diff myself for changes against the given base definition. 
	Answer a Modification, which might be empty if there are no changes."

	"Filter the definition to include changes that are only applicable to new class versions"

	self subclassResponsibility