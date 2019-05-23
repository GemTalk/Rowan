initialization
initialize

	packageNames := Set new.
	visitedConfigurationNames := Set new.
	configurationNames := Set new.
	platformAttributes := #().
	groupNames := {}.
	configurationBasePath := ''.
	packageMapSpecs := Dictionary new.
