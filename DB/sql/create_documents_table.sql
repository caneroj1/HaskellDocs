CREATE TABLE Documents (
  textSearchableColumn tsvector DEFAULT null,
  docID serial PRIMARY KEY,
  title VARCHAR(255) DEFAULT null,
  createdAt TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  indexed BOOLEAN DEFAULT false,
  indexFailure BOOLEAN DEFAULT false,
  lastIndexedAt TIMESTAMP WITH TIME zone DEFAULT null
);
