Updating - digests
updateDigestsFromAllRepositories

	self repositories do: [:each | self updateDigestsFromRepository: each]