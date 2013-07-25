loading
postLoad
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].
