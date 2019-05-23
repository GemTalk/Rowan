converting
asRwRepository
  "return a platform appropriate repository for the repository located in the directory referenced by the receiver"

  ^ self createRwTonelRepositoryForPath: self pathForDirectory
