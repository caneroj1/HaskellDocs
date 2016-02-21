CREATE TABLE DocumentComponents (
  componentID serial PRIMARY KEY,
  docID integer references Documents,
  filename VARCHAR(255),
  createdAt TIMESTAMP WITH TIME zone DEFAULT CURRENT_TIMESTAMP,
  indexed BOOLEAN DEFAULT false,
  indexFailure BOOLEAN DEFAULT false,
  lastIndexedAt TIMESTAMP WITH TIME zone DEFAULT null
);
