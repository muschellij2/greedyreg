# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /opt/local/bin/cmake

# The command to remove a file.
RM = /opt/local/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/johnmuschelli/Dropbox/Packages/greedy/greedy

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/johnmuschelli/Dropbox/Packages/greedy/src

# Include any dependencies generated for this target.
include CMakeFiles/test_accum.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/test_accum.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/test_accum.dir/flags.make

CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o: CMakeFiles/test_accum.dir/flags.make
CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o: /Users/johnmuschelli/Dropbox/Packages/greedy/greedy/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/johnmuschelli/Dropbox/Packages/greedy/src/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o -c /Users/johnmuschelli/Dropbox/Packages/greedy/greedy/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx

CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/johnmuschelli/Dropbox/Packages/greedy/greedy/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx > CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.i

CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/johnmuschelli/Dropbox/Packages/greedy/greedy/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx -o CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.s

CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.requires:

.PHONY : CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.requires

CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.provides: CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.requires
	$(MAKE) -f CMakeFiles/test_accum.dir/build.make CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.provides.build
.PHONY : CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.provides

CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.provides.build: CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o


# Object files for target test_accum
test_accum_OBJECTS = \
"CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o"

# External object files for target test_accum
test_accum_EXTERNAL_OBJECTS =

test_accum: CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o
test_accum: CMakeFiles/test_accum.dir/build.make
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkdouble-conversion-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitksys-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkvnl_algo-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkvnl-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkv3p_netlib-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitknetlib-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkvcl-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKCommon-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkNetlibSlatec-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKStatistics-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKTransform-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKLabelMap-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKMesh-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkzlib-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKMetaIO-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKSpatialObjects-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKPath-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKQuadEdgeMesh-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOImageBase-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKOptimizers-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKPolynomials-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKBiasCorrection-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKBioCell-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKEXPAT-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOXML-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOSpatialObjects-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKFEM-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmDICT-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmMSFF-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKznz-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKniftiio-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKgiftiio-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkhdf5_cpp.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkhdf5.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOBMP-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOBioRad-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOCSV-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOGDCM-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOIPL-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOGE-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOGIPL-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOHDF5-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkjpeg-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOJPEG-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitktiff-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTIFF-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOLSM-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOMRC-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOMesh-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOMeta-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIONIFTI-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKNrrdIO-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIONRRD-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkpng-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOPNG-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOSiemens-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOStimulate-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformBase-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformHDF5-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformInsightLegacy-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformMatlab-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOVTK-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKKLMRegionGrowing-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKOptimizersv4-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkopenjpeg-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKVTK-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKWatersheds-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKReview-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkMGHIO-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKgiftiio-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKLabelMap-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKQuadEdgeMesh-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKPolynomials-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKBiasCorrection-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKBioCell-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOSpatialObjects-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOXML-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKFEM-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKOptimizers-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOBMP-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOBioRad-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOGDCM-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmMSFF-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmDICT-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmIOD-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKEXPAT-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmDSED-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmCommon-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmjpeg8-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmjpeg12-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmjpeg16-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmopenjpeg-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmcharls-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkgdcmuuid-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOGE-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOGIPL-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOJPEG-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTIFF-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitktiff-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkjpeg-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOMeta-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKMetaIO-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIONIFTI-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKniftiio-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKznz-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIONRRD-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKNrrdIO-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOPNG-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkpng-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOSiemens-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOIPL-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOStimulate-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformHDF5-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkhdf5_cpp.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkhdf5.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformInsightLegacy-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformMatlab-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOTransformBase-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOVTK-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKKLMRegionGrowing-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkopenjpeg-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKVTK-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKWatersheds-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKStatistics-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkNetlibSlatec-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKSpatialObjects-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKMesh-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKTransform-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKPath-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkzlib-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKIOImageBase-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKCommon-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkdouble-conversion-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitksys-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libITKVNLInstantiation-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkvnl_algo-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkvnl-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkv3p_netlib-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitknetlib-4.11.a
test_accum: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/ITKR/libs/lib/libitkvcl-4.11.a
test_accum: CMakeFiles/test_accum.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/johnmuschelli/Dropbox/Packages/greedy/src/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable test_accum"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/test_accum.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/test_accum.dir/build: test_accum

.PHONY : CMakeFiles/test_accum.dir/build

CMakeFiles/test_accum.dir/requires: CMakeFiles/test_accum.dir/testing/src/TestOneDimensionalInPlaceAccumulateFilter.cxx.o.requires

.PHONY : CMakeFiles/test_accum.dir/requires

CMakeFiles/test_accum.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/test_accum.dir/cmake_clean.cmake
.PHONY : CMakeFiles/test_accum.dir/clean

CMakeFiles/test_accum.dir/depend:
	cd /Users/johnmuschelli/Dropbox/Packages/greedy/src && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/johnmuschelli/Dropbox/Packages/greedy/greedy /Users/johnmuschelli/Dropbox/Packages/greedy/greedy /Users/johnmuschelli/Dropbox/Packages/greedy/src /Users/johnmuschelli/Dropbox/Packages/greedy/src /Users/johnmuschelli/Dropbox/Packages/greedy/src/CMakeFiles/test_accum.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/test_accum.dir/depend

