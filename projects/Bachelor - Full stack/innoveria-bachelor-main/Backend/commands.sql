
CREATE TABLE `session` (
  `token` varchar(255) PRIMARY KEY,
  `user_id` integer,
  `created_at` datetime,
  `expires_at` datetime
);

CREATE TABLE `users` (
  `id` integer PRIMARY KEY AUTO_INCREMENT,
  `email` varchar(255),
  `first_name` varchar(255),
  `last_name` varchar(255),
  `password` varchar(255),
  `permission` integer,
  `company_id` integer
);

CREATE TABLE `company` (
  `id` integer PRIMARY KEY AUTO_INCREMENT,
  `name` varchar(255)
);


CREATE TABLE `building` (
  `id` integer PRIMARY KEY AUTO_INCREMENT,
  `company_id` integer,
  `name` varchar(255)
);


CREATE TABLE `department` (
  `id` integer PRIMARY KEY AUTO_INCREMENT,
  `building_id` integer,
  `name` varchar(255)
);


CREATE TABLE `machine` (
  `eui` varchar(255) PRIMARY KEY,
  `name` varchar(255),
  `expected_use` integer,
  `machineNr` varchar(255),
  `building_id` integer,
  `department_id` integer,
  `voltage` integer
);


CREATE TABLE `enoek_suggestion_measures` (
  `id` integer PRIMARY KEY AUTO_INCREMENT,
  `header` varchar(255),
  `description` text,
  `author` integer,
  `start_date` date,
  `stop_date` date,
  `active` bool,
  `approved` bool,
  `process_id` integer
);

CREATE TABLE `processes` (
  `id` integer PRIMARY KEY AUTO_INCREMENT,
  `name` varchar(255),
  `description` text,
  `company_id` integer
);

CREATE TABLE `machine_processes` (
  `machine_id` varchar(255),
  `processes_id` integer
);


CREATE TABLE `sensorData` (
  `eui` varchar(255),
  `Acumulation` double,
  `AVG_current` double,
  `Offset_max` double,
  `Offset_min` double,
  `Voltage` double,
  `Temprature` double,
  `date_time` datetime
);

CREATE TABLE gateway (
  EUI VARCHAR(255) PRIMARY KEY,
  company_id INTEGER,
  name VARCHAR(255),
  FOREIGN KEY (company_id) REFERENCES company (id)
);

ALTER TABLE `session` ADD FOREIGN KEY (`user_id`) REFERENCES `users` (`id`);

ALTER TABLE `users` ADD FOREIGN KEY (`company_id`) REFERENCES `company` (`id`);

ALTER TABLE `building` ADD FOREIGN KEY (`company_id`) REFERENCES `company` (`id`);

ALTER TABLE `department` ADD FOREIGN KEY (`building_id`) REFERENCES `building` (`id`);

ALTER TABLE `machine` ADD FOREIGN KEY (`building_id`) REFERENCES `building` (`id`);

ALTER TABLE `machine` ADD FOREIGN KEY (`department_id`) REFERENCES `department` (`id`);

ALTER TABLE `enoek_suggestion_measures` ADD FOREIGN KEY (`author`) REFERENCES `users` (`id`);

ALTER TABLE `enoek_suggestion_measures` ADD FOREIGN KEY (`process_id`) REFERENCES `processes` (`id`);

ALTER TABLE `machine_processes` ADD FOREIGN KEY (`machine_id`) REFERENCES `machine` (`eui`);

ALTER TABLE `machine_processes` ADD FOREIGN KEY (`processes_id`) REFERENCES `processes` (`id`);

ALTER TABLE `sensorData` ADD FOREIGN KEY (`eui`) REFERENCES `machine` (`eui`);

ALTER TABLE `processes` ADD FOREIGN KEY (`company_id`) REFERENCES `company` (`id`);

INSERT INTO company (`name`) VALUES ('Vyrk');
INSERT INTO company (`name`) VALUES ('Innoveria');
INSERT INTO company (`name`) VALUES ('Monitor');
INSERT INTO users (`email`, `first_name`, `last_name`, `password`, `permission`, `company_id`)VALUES ('admin@admin.no', 'admin', 'admin', 'admin', 0, 1);
INSERT INTO session VALUES ("0321412", 1, '2024-02-01 15:30:45', '2024-07-09 15:30:45');
INSERT INTO building (`company_id`, `name`) VALUES (1, 'A');
INSERT INTO building (`company_id`, `name`) VALUES (1, 'B');
INSERT INTO building (`company_id`, `name`) VALUES (1, 'C');
INSERT INTO department (`building_id`, `name`) VALUES (1, 'AA');
INSERT INTO department (`building_id`, `name`) VALUES (1, 'AB');
INSERT INTO department (`building_id`, `name`) VALUES (1, 'AC');
INSERT INTO department (`building_id`, `name`) VALUES (2, 'BA');
INSERT INTO department (`building_id`, `name`) VALUES (2, 'BB');
INSERT INTO department (`building_id`, `name`) VALUES (2, 'BC');
INSERT INTO department (`building_id`, `name`) VALUES (3, 'CA');
INSERT INTO department (`building_id`, `name`) VALUES (3, 'CB');
INSERT INTO department (`building_id`, `name`) VALUES (3, 'CC');
INSERT INTO machine (`eui`, `name`, `expected_use`, `machineNr`, `building_id`, `department_id`) VALUES ("E890E1CC8CF44B49", "press", 5, "4727AD", 1, 1);
INSERT INTO processes (`name`, `description`, `company_id`) VALUES ("WINDOW", "Make window for houses", 1);
INSERT INTO enoek_suggestion_measures (`header`, `description`, `author`, `start_date`, `stop_date`, `active`, `approved`, `process_id`) VALUES ("Oven", "Oven burn more stuff", 1, '2024-02-01 15:30:45', '2024-07-09 15:30:45', 0, NULL, 1);
INSERT INTO machine_processes (`machine_id`, `processes_id`) VALUES ("E890E1CC8CF44B49", 1);
INSERT INTO sensorData (`eui`, `Acumulation`, `AVG_current`, `Offset_max`, `Offset_min`, `Voltage`, `Temprature`, `date_time`) VALUES
  ("E890E1CC8CF44B49", 0, 0, 0, 0, 0, 0, '2024-02-11 15:30:45');