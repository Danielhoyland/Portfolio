-- phpMyAdmin SQL Dump
-- version 5.2.0
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Sep 19, 2023 at 02:10 PM
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
-- Database: `fridgeio`
--

-- --------------------------------------------------------

--
-- Table structure for table `products`
--

CREATE TABLE `products` (
  `ID` int(11) NOT NULL,
  `Name` varchar(255) NOT NULL,
  `Base_measure` varchar(255) NOT NULL,
  `Type_food` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `products`
--

INSERT INTO `products` (`ID`, `Name`, `Base_measure`, `Type_food`) VALUES
(101, 'Milk', 'Liter', 'Dairy'),
(102, 'Flour', 'Grams', 'dry goods'),
(103, 'Eggs', 'Dozen', 'Animal Products');

-- --------------------------------------------------------

--
-- Table structure for table `recipe`
--

CREATE TABLE `recipe` (
  `ID` int(11) NOT NULL,
  `Name` varchar(255) NOT NULL,
  `Description` text DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `recipe`
--

INSERT INTO `recipe` (`ID`, `Name`, `Description`) VALUES
(1, 'Spaghetti Carbonara', 'Classic Italian pasta dish with eggs, cheese, and pancetta.'),
(2, 'Chicken Alfredo', 'Creamy pasta dish with chicken and Parmesan cheese.'),
(3, 'Vegetable Stir-Fry', 'Stir-fried vegetables with soy sauce and tofu.');

-- --------------------------------------------------------

--
-- Table structure for table `recipe_products`
--

CREATE TABLE `recipe_products` (
  `Recipe_ID` int(11) DEFAULT NULL,
  `Product_ID` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `recipe_products`
--

INSERT INTO `recipe_products` (`Recipe_ID`, `Product_ID`) VALUES
(1, 101),
(1, 102),
(2, 101),
(2, 103),
(3, 102),
(3, 103);

-- --------------------------------------------------------

--
-- Table structure for table `users`
--

CREATE TABLE `users` (
  `Username` varchar(255) NOT NULL,
  `Password` varchar(255) NOT NULL,
  `Email` varchar(255) NOT NULL UNIQUE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `users`
--

INSERT INTO `users` (`Username`, `Password`, `Email`) VALUES
('Daniel', 'hashed_password_1', 'Daniehoy@stud.ntnu.no'),
('Ofin', 'Fart', 'odinaa@stud.ntnu.no'),
('Elfungo', 'hashed_password_3', 'ferdinws@stud.NTNU.no');

-- --------------------------------------------------------

--
-- Table structure for table `users_lists`
--

CREATE TABLE `users_lists` (
  `User_ID` varchar(255) DEFAULT NULL,
  `Product_ID` int(11) DEFAULT NULL,
  `Expiration_date` date DEFAULT NULL,
  `Amount` float(11,2) DEFAULT NULL,
  `Image_ID` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `users_lists`
--

INSERT INTO `users_lists` (`User_ID`, `Product_ID`, `Expiration_date`, `Amount`, `Image_ID`) VALUES
('Daniehoy@stud.ntnu.no', 101, '2023-10-01', 2.5, 0),
('Daniehoy@stud.ntnu.no', 102, '2023-09-01', 3400, 0),
('odinaa@stud.ntnu.no', 103, '2023-09-06', 2, 0),
('ferdinws@stud.NTNU.no', 101, '2023-09-07', 11, 0),
('odinaa@stud.ntnu.no', 101, '2023-09-07', 1.5, 0);


-- --------------------------------------------------------

--
-- Table structure for table `users_recipe`
--

CREATE TABLE `users_recipe` (
  `User_ID` varchar(255) DEFAULT NULL,
  `Recipe_ID` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `users_recipe`
--

INSERT INTO `users_recipe` (`User_ID`, `Recipe_ID`) VALUES
('Daniehoy@stud.ntnu.no', 1),
('Daniehoy@stud.ntnu.no', 3),
('Daniehoy@stud.ntnu.no', 2),
('ferdinws@stud.NTNU.no', 1),
('ferdinws@stud.NTNU.no', 2),
('odinaa@stud.ntnu.no', 3);

--
-- Indexes for dumped tables
--

--
-- Indexes for table `products`
--
ALTER TABLE `products`
  ADD PRIMARY KEY (`ID`);

--
-- Indexes for table `recipe`
--
ALTER TABLE `recipe`
  ADD PRIMARY KEY (`ID`);

--
-- Indexes for table `recipe_products`
--
ALTER TABLE `recipe_products`
  ADD KEY `Recipe_ID` (`Recipe_ID`),
  ADD KEY `Product_ID` (`Product_ID`);

--
-- Indexes for table `users`
--
ALTER TABLE `users`
  ADD PRIMARY KEY (`Email`);

--
-- Indexes for table `users_lists`
--
ALTER TABLE `users_lists`
  ADD KEY `User_ID` (`User_ID`),
  ADD KEY `Product_ID` (`Product_ID`);

--
-- Indexes for table `users_recipe`
--
ALTER TABLE `users_recipe`
  ADD KEY `User_ID` (`User_ID`),
  ADD KEY `Recipe_ID` (`Recipe_ID`);

--
-- Constraints for table `recipe_products`
--
ALTER TABLE `recipe_products`
  ADD CONSTRAINT `recipe_products_ibfk_1` FOREIGN KEY (`Recipe_ID`) REFERENCES `recipe` (`ID`),
  ADD CONSTRAINT `recipe_products_ibfk_2` FOREIGN KEY (`Product_ID`) REFERENCES `products` (`ID`);

--
-- Constraints for table `users_lists`
--
ALTER TABLE `users_lists`
  ADD CONSTRAINT `users_lists_ibfk_1` FOREIGN KEY (`User_ID`) REFERENCES `users` (`Email`),
  ADD CONSTRAINT `users_lists_ibfk_2` FOREIGN KEY (`Product_ID`) REFERENCES `products` (`ID`);

--
-- Constraints for table `users_recipe`
--
ALTER TABLE `users_recipe`
  ADD CONSTRAINT `users_recipe_ibfk_1` FOREIGN KEY (`User_ID`) REFERENCES `users` (`Email`),
  ADD CONSTRAINT `users_recipe_ibfk_2` FOREIGN KEY (`Recipe_ID`) REFERENCES `recipe` (`ID`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
