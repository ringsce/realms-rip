cmake_minimum_required(VERSION 3.10)
project(ekron-rpi)

# Set C++ standard
set(CMAKE_CXX_STANDARD 11)

# Find FreePascal (FPC)
find_program(FPC fpc)

# Add C++ source files
set(CPP_SOURCES
    src/main.cpp
    src/utils.cpp
)

# Add assembly source files (Raspberry Pi 3+)
set(ASM_SOURCES
    src/rpi3.asm
)

# Add FreePascal source files
set(FPC_SOURCES
    src/main.pas
)

# Set output directory for FreePascal
set(FPC_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/fpc_output)

# Set output directory for C++
set(CPP_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/cpp_output)

# Set output directory for ASM
set(ASM_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/asm_output)

# Compile FreePascal files
foreach(FPC_FILE ${FPC_SOURCES})
    get_filename_component(FPC_BASENAME ${FPC_FILE} NAME_WE)
    add_custom_command(
        OUTPUT ${FPC_OUTPUT_DIRECTORY}/${FPC_BASENAME}.o
        COMMAND ${FPC} -o${FPC_OUTPUT_DIRECTORY}/${FPC_BASENAME}.o ${FPC_FILE}
        DEPENDS ${FPC_FILE}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Compiling ${FPC_FILE} with FreePascal"
    )
    list(APPEND FPC_OBJECTS ${FPC_OUTPUT_DIRECTORY}/${FPC_BASENAME}.o)
endforeach()

# Assemble ASM files
foreach(ASM_FILE ${ASM_SOURCES})
    get_filename_component(ASM_BASENAME ${ASM_FILE} NAME_WE)
    add_custom_command(
        OUTPUT ${ASM_OUTPUT_DIRECTORY}/${ASM_BASENAME}.o
        COMMAND as -o ${ASM_OUTPUT_DIRECTORY}/${ASM_BASENAME}.o ${ASM_FILE}
        DEPENDS ${ASM_FILE}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Assembling ${ASM_FILE}"
    )
    list(APPEND ASM_OBJECTS ${ASM_OUTPUT_DIRECTORY}/${ASM_BASENAME}.o)
endforeach()

# Compile C++ files
foreach(CPP_FILE ${CPP_SOURCES})
    get_filename_component(CPP_BASENAME ${CPP_FILE} NAME_WE)
    add_executable(${CPP_BASENAME} ${CPP_FILE})
    target_compile_options(${CPP_BASENAME} PRIVATE -O3) # Optimization level, adjust as needed
    target_link_libraries(${CPP_BASENAME} ${FPC_OBJECTS} ${ASM_OBJECTS})
    set_target_properties(${CPP_BASENAME} PROPERTIES
                          RUNTIME_OUTPUT_DIRECTORY ${CPP_OUTPUT_DIRECTORY})
endforeach()
