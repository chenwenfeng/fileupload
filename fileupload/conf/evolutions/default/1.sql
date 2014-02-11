# Users schema
 
# --- !Ups
 
CREATE TABLE User (
    email varchar(255) NOT NULL,
    password varchar(255) NOT NULL,
    PRIMARY KEY (email)
);
 
# --- !Downs