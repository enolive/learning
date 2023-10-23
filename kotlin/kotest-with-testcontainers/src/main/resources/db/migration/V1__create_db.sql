create table beers (
    beer_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    brand TEXT NOT NULL,
    name TEXT NOT NULL,
    strength DECIMAL NOT NULL
)