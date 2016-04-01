create table users (
   id serial primary key,
   login varchar(255) not null
);

create table posts(
   id serial primary key,
   header varchar(255) not null,
   text text not null,
   author_id integer references users(id) not null
);

create table comments(
   id serial primary key,
   text text not null,
   author_id integer references users(id) not null,
   post_id integer references posts(id) not null,
   meta text -- json meta
);

insert into users(login) values
('Sam'),
('Mike'),
('Joe'),
('Elis');

insert into posts(header, text, author_id) values
('About sql', 'foo', 1),
('About Erlang', 'bar', 1),
('My cookies', 'Ooops', 4);

insert into comments(text, author_id, post_id) values
('Sql is greate thing', 2, 1),
('What is sql?', 4, 1),
('Teasty?', 1, 3),
('Great!', 4, 3);
