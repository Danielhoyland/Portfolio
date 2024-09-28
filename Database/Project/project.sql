-- phpMyAdmin SQL Dump
-- version 5.2.0
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: May 06, 2023 at 01:44 PM
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
-- Database: `timetableDB`
--

-- --------------------------------------------------------

--
-- Table structure for table `booking`
--

CREATE TABLE `booking` (
  `Person_ID` int(11) DEFAULT NULL,
  `Description` text DEFAULT NULL,
  `Start_time` time NOT NULL,
  `End_time` time NOT NULL,
  `Booking_Date` date NOT NULL,
  `Room_Nr` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `booking`
--

INSERT INTO `booking` (`Person_ID`, `Description`, `Start_time`, `End_time`, `Booking_Date`, `Room_Nr`) VALUES
(1, 'Meeting with project team', '10:00:00', '11:00:00', '2023-04-20', 101),
(2, 'Bacheloer work', '14:00:00', '15:00:00', '2023-04-22', 103),
(3, 'Dnd :)', '18:00:00', '22:00:00', '2023-04-22', 101),
(7, 'lofi beats to study and chill to', '12:00:00', '14:00:00', '2023-04-22', 104);

-- --------------------------------------------------------

--
-- Table structure for table `course`
--

CREATE TABLE `course` (
  `ID` int(11) NOT NULL,
  `Description` text DEFAULT NULL,
  `Semester_ID` int(11) DEFAULT NULL,
  `nr_students` int(11) DEFAULT NULL,
  `Lecturer` int(11) DEFAULT NULL,
  `Institute` varchar(255) DEFAULT NULL,
  `Faculty` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `course`
--

INSERT INTO `course` (`ID`, `Description`, `Semester_ID`, `nr_students`, `Lecturer`, `Institute`, `Faculty`) VALUES
(1, 'Introduction to Computer Science', 1, 50, NULL, 'Department of Computer Science', 'School of Engineering'),
(2, 'Database Systems', 2, 75, 4, 'Department of Computer Science', 'School of Engineering'),
(3, 'Web Development', 1, 35, 6, 'Department of Computer Science', 'School of Engineering');

-- --------------------------------------------------------

--
-- Table structure for table `lesson`
--

CREATE TABLE `lesson` (
  `Course_ID` int(11) NOT NULL,
  `Lesson_Date` date NOT NULL,
  `Start_time` time NOT NULL,
  `End_time` time NOT NULL,
  `Activity` text DEFAULT NULL,
  `Room` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `lesson`
--

INSERT INTO `lesson` (`Course_ID`, `Lesson_Date`, `Start_time`, `End_time`, `Activity`, `Room`) VALUES
(1, '2022-09-06', '08:00:00', '10:00:00', 'Lecture', 101),
(1, '2022-09-08', '10:00:00', '12:00:00', 'Lab', 103),
(2, '2023-02-13', '14:00:00', '16:00:00', 'Lecture', 102),
(2, '2023-02-15', '16:00:00', '18:00:00', 'Lab', 101),
(3, '2022-10-20', '12:00:00', '14:00:00', 'Lecture', 103),
(3, '2022-10-22', '14:00:00', '16:00:00', 'Lab', 102),
(1, '2023-05-01', '08:00:00', '10:00:00', 'Lecture', 104),
(1, '2023-04-08', '10:00:00', '12:00:00', 'Lab', 106),
(2, '2023-03-21', '14:00:00', '16:00:00', 'Lecture', 105),
(2, '2023-05-17', '11:00:00', '18:00:00', 'Lab', 104),
(3, '2023-04-20', '10:00:00', '12:00:00', 'Lecture', 106),
(3, '2023-03-22', '13:00:00', '15:00:00', 'Lab', 105);

-- --------------------------------------------------------

--
-- Table structure for table `room`
--

CREATE TABLE `room` (
  `Number` int(11) NOT NULL,
  `Size` int(11) DEFAULT NULL,
  `Type` text DEFAULT NULL,
  `Floor` int(11) DEFAULT NULL,
  `Building` text DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `room`
--

INSERT INTO `room` (`Number`, `Size`, `Type`, `Floor`, `Building`) VALUES
(101, 20, 'Group', 1, 'Main'),
(102, 30, 'Lab', 1, 'Main'),
(103, 25, 'Auditorium', 2, 'Main'),
(104, 40, 'Auditorium', 2, 'Main'),
(105, 35, 'Auditorium', 3, 'Main'),
(106, 8, 'Group', 1, 'Main');

-- --------------------------------------------------------

--
-- Table structure for table `semester`
--

CREATE TABLE `semester` (
  `ID` int(11) NOT NULL,
  `Season` varchar(50) NOT NULL,
  `Year` int(11) NOT NULL,
  `Weeknr_start` int(11) NOT NULL,
  `Weeknr_end` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `semester`
--

INSERT INTO `semester` (`ID`, `Season`, `Year`, `Weeknr_start`, `Weeknr_end`) VALUES
(1, 'Fall', 2022, 36, 50),
(2, 'Spring', 2023, 1, 15),
(3, 'Summer', 2023, 20, 28),
(4, 'Fall', 2023, 32, 50),
(5, 'Spring', 2024, 2, 16);

-- --------------------------------------------------------

--
-- Table structure for table `student`
--

CREATE TABLE `student` (
  `User_ID` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `student`
--

INSERT INTO `student` (`User_ID`) VALUES
(1),
(2),
(3),
(7),
(8);

-- --------------------------------------------------------

--
-- Table structure for table `studied_by`
--

CREATE TABLE `studied_by` (
  `Student_ID` int(11) NOT NULL,
  `Course_ID` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `studied_by`
--

INSERT INTO `studied_by` (`Student_ID`, `Course_ID`) VALUES
(1, 2),
(2, 1),
(3, 3),
(7, 2),
(8, 3);

-- --------------------------------------------------------

--
-- Table structure for table `taught_by`
--

CREATE TABLE `taught_by` (
  `Teacher_ID` int(11) NOT NULL,
  `Course_ID` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `taught_by`
--

INSERT INTO `taught_by` (`Teacher_ID`, `Course_ID`) VALUES
(5, 3),
(4, 2),
(6, 1),
(9, 3);

-- --------------------------------------------------------

--
-- Table structure for table `teacher`
--

CREATE TABLE `teacher` (
  `User_ID` int(11) NOT NULL,
  `Title` text DEFAULT NULL,
  `Institute` text DEFAULT NULL,
  `Faculty` text DEFAULT NULL,
  `Location` text DEFAULT NULL,
  `Office` text DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `teacher`
--

INSERT INTO `teacher` (`User_ID`, `Title`, `Institute`, `Faculty`, `Location`, `Office`) VALUES
(4, 'Dr.', 'Department of Computer Science', 'Faculty of Engineering', 'Gjøvik', '101'),
(5, 'Prof.', 'Department of Physics', 'Faculty of Science', 'Gjøvik', '202'),
(6, 'Assoc. Prof.', 'Department of History', 'Faculty of Humanities', 'Gjøvik', '303'),
(9, 'Assoc. Prof.', 'Department of Horror', 'Faculty of Monsters', 'Gjøvik', '404');

-- --------------------------------------------------------

--
-- Table structure for table `users`
--

CREATE TABLE `users` (
  `ID` int(11) NOT NULL,
  `First_name` text DEFAULT NULL,
  `Last_name` text DEFAULT NULL,
  `Email` text DEFAULT NULL,
  `Username` varchar(255) DEFAULT NULL,
  `User_Password` blob DEFAULT NULL,
  `Access_Level` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `users`
--

INSERT INTO `users` (`ID`, `First_name`, `Last_name`, `Email`, `Username`, `User_Password`, `Access_Level`) VALUES
(1, 'John', 'Doe', 'johndoe@example.com', 'johndoe', 0x70617373776f7264313233, 2),
(2, 'Jane', 'Doe', 'janedoe@example.com', 'janedoe', 0x70617373776f7264343536, 2),
(3, 'Bob', 'Smith', 'bobsmith@example.com', 'bobsmith', 0x70617373776f7264373839, 2),
(4, 'Alice', 'Johnson', 'alicejohnson@example.com', 'alicejohnson', 0x70617373776f7264333231, 3),
(5, 'Michael', 'Brown', 'michaelbrown@example.com', 'michaelbrown', 0x70617373776f7264363534, 3),
(6, 'Emily', 'Davis', 'emilydavis@example.com', 'emilydavis', 0x70617373776f7264393837, 3),
(7, 'David', 'Williams', 'davidwilliams@example.com', 'davidwilliams', 0x70617373776f7264313336, 2),
(8, 'Avada', 'Kedavra', 'avadakedavra@example.com', 'avadakedavra', 0x70617373776f7264323437, 2),
(9, 'Mike', 'Wazowski', 'mike wazowski@example.com', 'mikewazowski', 0x70617373776f7264333639, 3);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
