private
handleInformDuring: aBlock interactionBlock: interactionBlock

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: (self gsInteractionInformHandler: interactionBlock)) ]