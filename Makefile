ROOT_DIR = $(shell pwd)

CMAKE_PROG ?= $(shell (command -v cmake3 || echo cmake))
BUILD_TOOL = ninja
BUILD_TYPE = Ninja

BUILD_CMD = $(BUILD_TOOL) -v

all: pakmen

pakmen: build/.ran-cmake
	+$(BUILD_CMD) -C build

build/.ran-cmake:
	mkdir -p build
	cd build && $(CMAKE_PROG) -G '$(BUILD_TYPE)' $(ROOT_DIR)
