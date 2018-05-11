private
materialize: string
  ^ (STON091Reader on: string decodeFromUTF8 readStream) next