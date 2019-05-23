private
_visited: aConfiguration

	super _visited: aConfiguration.
	visitedConfigurationMap at:  aConfiguration name put: aConfiguration.
