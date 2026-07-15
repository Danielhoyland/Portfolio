-- phpMyAdmin SQL Dump
-- version 5.2.0
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Feb 25, 2023 at 03:56 AM
-- Server version: 10.4.27-MariaDB
-- PHP Version: 8.2.0

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `assignment3`
--

-- --------------------------------------------------------

--
-- Table structure for table `contracts`
--

CREATE TABLE `contracts` (
  `Company_name` varchar(255) NOT NULL,
  `Pharmacy_phone` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `contracts`
--

INSERT INTO `contracts` (`Company_name`, `Pharmacy_phone`) VALUES
('Janson & Janson', '54327612'),
('Janson & Janson', '54327612'),
('Pfizer', '54327612'),
('Bayer', '87340213'),
('Roche', '35446281'),
('CSL', '98463251'),
('Abbott', '87340213'),
('Vertex Pharmaceuticals', '87340213'),
('Allergan', '98463251'),
('Allergan', '35446281');

-- --------------------------------------------------------

--
-- Table structure for table `drug`
--

CREATE TABLE `drug` (
  `trademark` varchar(255) NOT NULL,
  `formula` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `drug`
--

INSERT INTO `drug` (`trademark`, `formula`) VALUES
('Advil', 'ibuprofen'),
('Aleve', 'naproxen'),
('Bayer Aspirin', 'aspirin'),
('Irenka', 'duloxetine'),
('Myoflex', 'Trolamine salicylate'),
('Ultram', 'tramadol'),
('Zipsor', 'diclofenac');

-- --------------------------------------------------------

--
-- Table structure for table `pharmacy`
--

CREATE TABLE `pharmacy` (
  `phone` varchar(255) NOT NULL,
  `name` varchar(255) NOT NULL,
  `address` varchar(255) NOT NULL,
  `street` varchar(255) NOT NULL,
  `city` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `pharmacy`
--

INSERT INTO `pharmacy` (`phone`, `name`, `address`, `street`, `city`) VALUES
('35446281', 'Absolute Care', '39 Spruce Drive', 'Charlottesville', 'VA 22901'),
('45362819', 'Pharma Best', '15 Williams Drive', 'Elgin', 'IL 60120'),
('54327612', 'City Drug', '36 South Cherry', 'Starkville', 'MS 39759'),
('87340213', 'Be Well', '790 Clay Road', 'Ooltewah', 'TN 37363'),
('87435217', 'Pill Pack', '29 E. Pine Lane', 'Stuart', 'FL 34997'),
('98463251', 'Better Life', '8004 Eagle St.', 'Sarasota', 'FL 34231');

-- --------------------------------------------------------

--
-- Table structure for table `pharmacycompany`
--

CREATE TABLE `pharmacycompany` (
  `name` varchar(255) NOT NULL,
  `phone` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `pharmacycompany`
--

INSERT INTO `pharmacycompany` (`name`, `phone`) VALUES
('Abbott', '66392014'),
('Allergan', '47639201'),
('Bayer', '88374291'),
('CSL', '93462918'),
('Janson & Janson', '23749912'),
('Pfizer', '45732810'),
('Roche', '66372910'),
('Vertex Pharmaceuticals', '91228345');

-- --------------------------------------------------------

--
-- Table structure for table `sells`
--

CREATE TABLE `sells` (
  `Pharmacy_phone` varchar(255) NOT NULL,
  `trademark` varchar(255) NOT NULL,
  `Company_name` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `sells`
--

INSERT INTO `sells` (`Pharmacy_phone`, `trademark`, `Company_name`) VALUES
('54327612', 'Ultram', 'Pfizer'),
('54327612', 'Aleve', 'Abbott'),
('87340213', 'Aleve', 'Abbott'),
('35446281', 'Advil', 'Allergan'),
('98463251', 'Advil', 'Allergan'),
('35446281', 'Irenka', 'Vertex Pharmaceuticals');

-- --------------------------------------------------------

--
-- Table structure for table `sold_by`
--

CREATE TABLE `sold_by` (
  `Company_name` varchar(255) NOT NULL,
  `trademark` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `sold_by`
--

INSERT INTO `sold_by` (`Company_name`, `trademark`) VALUES
('Abbott', 'Aleve'),
('Allergan', 'Advil'),
('Bayer', 'Bayer Aspirin'),
('CSL', 'Ultram'),
('Janson & Janson', 'Aleve'),
('Janson & Janson', 'Myoflex'),
('Janson & Janson', 'Ultram'),
('Janson & Janson', 'Zipsor'),
('Pfizer', 'Ultram'),
('Pfizer', 'Zipsor'),
('Roche', 'Irenka'),
('Vertex Pharmaceuticals', 'Irenka');

--
-- Indexes for dumped tables
--

--
-- Indexes for table `contracts`
--
ALTER TABLE `contracts`
  ADD KEY `Company_name` (`Company_name`),
  ADD KEY `Pharmacy_phone` (`Pharmacy_phone`);

--
-- Indexes for table `drug`
--
ALTER TABLE `drug`
  ADD PRIMARY KEY (`trademark`);

--
-- Indexes for table `pharmacy`
--
ALTER TABLE `pharmacy`
  ADD PRIMARY KEY (`phone`);

--
-- Indexes for table `pharmacycompany`
--
ALTER TABLE `pharmacycompany`
  ADD PRIMARY KEY (`name`);

--
-- Indexes for table `sells`
--
ALTER TABLE `sells`
  ADD KEY `Pharmacy_phone` (`Pharmacy_phone`),
  ADD KEY `trademark` (`trademark`);

--
-- Indexes for table `sold_by`
--
ALTER TABLE `sold_by`
  ADD PRIMARY KEY (`Company_name`,`trademark`),
  ADD KEY `trademark` (`trademark`);

--
-- Constraints for dumped tables
--

--
-- Constraints for table `contracts`
--
ALTER TABLE `contracts`
  ADD CONSTRAINT `contracts_ibfk_1` FOREIGN KEY (`Company_name`) REFERENCES `pharmacycompany` (`name`),
  ADD CONSTRAINT `contracts_ibfk_2` FOREIGN KEY (`Pharmacy_phone`) REFERENCES `pharmacy` (`phone`);

--
-- Constraints for table `sells`
--
ALTER TABLE `sells`
  ADD CONSTRAINT `sells_ibfk_1` FOREIGN KEY (`Pharmacy_phone`) REFERENCES `pharmacy` (`phone`),
  ADD CONSTRAINT `sells_ibfk_2` FOREIGN KEY (`trademark`) REFERENCES `drug` (`trademark`);

--
-- Constraints for table `sold_by`
--
ALTER TABLE `sold_by`
  ADD CONSTRAINT `sold_by_ibfk_1` FOREIGN KEY (`Company_name`) REFERENCES `pharmacycompany` (`name`),
  ADD CONSTRAINT `sold_by_ibfk_2` FOREIGN KEY (`trademark`) REFERENCES `drug` (`trademark`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
