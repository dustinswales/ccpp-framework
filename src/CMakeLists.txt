#------------------------------------------------------------------------------
# Set the sources
set(SOURCES_F90
    ccpp_types.F90
    ccpp_hashable.F90
    ccpp_hash_table.F90
    ccpp_constituent_prop_mod.F90
    ccpp_scheme_utils.F90
)

# Generate list of Fortran modules from defined sources
foreach(source_f90 ${SOURCES_F90})
    string(REGEX REPLACE ".F90" ".mod" module_f90 ${source_f90})
    list(APPEND MODULES_F90 ${CMAKE_CURRENT_BINARY_DIR}/${module_f90})
endforeach()

# Set the sources: kinds file, created by capgen
set(KINDS $ENV{CCPP_KINDS})
if(KINDS)
  message(STATUS "Got CCPP KINDS from environment variable")
else(KINDS)
  include(${CMAKE_CURRENT_BINARY_DIR}/../../physics/CCPP_KINDS.cmake)
  message(STATUS "Got CCPP KINDS from cmakefile include file")
endif(KINDS)

#------------------------------------------------------------------------------
# Add the toplevel source directory to our include directoies (for .h)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Add the toplevel binary directory to our include directoies (for .mod)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Set a cached variable containing the includes, so schemes can use them
set(${PACKAGE}_INCLUDE_DIRS
    "${CMAKE_CURRENT_SOURCE_DIR}$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}"
     CACHE FILEPATH "${PACKAGE} include directories")
set(${PACKAGE}_LIB_DIRS
    "${CMAKE_CURRENT_BINARY_DIR}"
     CACHE FILEPATH "${PACKAGE} library directories")

#------------------------------------------------------------------------------
# Define the executable and what to link
add_library(ccpp_framework STATIC ${KINDS} ${SOURCES_F90})
target_link_libraries(ccpp_framework PUBLIC MPI::MPI_Fortran)
set_target_properties(ccpp_framework PROPERTIES VERSION ${PROJECT_VERSION}
                                      SOVERSION ${PROJECT_VERSION_MAJOR}
                                      LINK_FLAGS ${CMAKE_Fortran_FLAGS})

#------------------------------------------------------------------------------
# Installation
#
target_include_directories(ccpp_framework PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    $<INSTALL_INTERFACE:include>
)

# Define where to install the library
install(TARGETS ccpp_framework
        EXPORT ccpp_framework-targets
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION lib
)

# Export our configuration
install(EXPORT ccpp_framework-targets
        FILE ccpp_framework-config.cmake
        DESTINATION lib/cmake
)

# Define where to install the Fortran modules
install(FILES ${MODULES_F90} DESTINATION include)
