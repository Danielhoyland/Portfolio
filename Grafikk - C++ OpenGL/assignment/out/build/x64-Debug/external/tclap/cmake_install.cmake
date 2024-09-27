# Install script for directory: C:/Users/Ukule Martin/source/repos/assignment/external/tclap

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/tclap" TYPE FILE FILES "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/tclap/TCLAPConfig.h")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xdocx" OR NOT CMAKE_INSTALL_COMPONENT)
  list(APPEND CMAKE_ABSOLUTE_DESTINATION_FILES
   "C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug/doc/html")
  if(CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(WARNING "ABSOLUTE path INSTALL DESTINATION : ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  if(CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(FATAL_ERROR "ABSOLUTE path INSTALL DESTINATION forbidden (by caller): ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  file(INSTALL DESTINATION "C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug/doc" TYPE DIRECTORY FILES "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/docs/html")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xdocx" OR NOT CMAKE_INSTALL_COMPONENT)
  list(APPEND CMAKE_ABSOLUTE_DESTINATION_FILES
   "C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug/doc/index.html;C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug/doc/manual.html;C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug/doc/style.css")
  if(CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(WARNING "ABSOLUTE path INSTALL DESTINATION : ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  if(CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(FATAL_ERROR "ABSOLUTE path INSTALL DESTINATION forbidden (by caller): ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  file(INSTALL DESTINATION "C:/Users/Ukule Martin/Source/Repos/assignment/out/install/x64-Debug/doc" TYPE FILE FILES
    "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/docs/index.html"
    "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/docs/manual.html"
    "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/docs/style.css"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/tclap.pc")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/TCLAP/TCLAPConfig.cmake")
    file(DIFFERENT EXPORT_FILE_CHANGED FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/TCLAP/TCLAPConfig.cmake"
         "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/CMakeFiles/Export/lib/cmake/TCLAP/TCLAPConfig.cmake")
    if(EXPORT_FILE_CHANGED)
      file(GLOB OLD_CONFIG_FILES "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/TCLAP/TCLAPConfig-*.cmake")
      if(OLD_CONFIG_FILES)
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/TCLAP/TCLAPConfig.cmake\" will be replaced.  Removing files [${OLD_CONFIG_FILES}].")
        file(REMOVE ${OLD_CONFIG_FILES})
      endif()
    endif()
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/TCLAP" TYPE FILE FILES "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/CMakeFiles/Export/lib/cmake/TCLAP/TCLAPConfig.cmake")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/TCLAP" TYPE FILE FILES "C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/TCLAPConfig-version.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/docs/cmake_install.cmake")
  include("C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/examples/cmake_install.cmake")
  include("C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/tests/cmake_install.cmake")
  include("C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/unittests/cmake_install.cmake")
  include("C:/Users/Ukule Martin/source/repos/assignment/out/build/x64-Debug/external/tclap/include/tclap/cmake_install.cmake")

endif()

