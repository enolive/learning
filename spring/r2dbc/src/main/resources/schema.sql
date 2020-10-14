CREATE TABLE product
(
    id          integer generated always as identity,
    description character varying(255),
    price       numeric,
    PRIMARY KEY (id)
);
