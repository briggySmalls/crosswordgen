BUILD_DIR=build/
WEB_DIR=web/
LIBRARY_SRC="library/target/scala-3.1.3/mycrossword-fastopt/main.js"

# This is a comment
build-web: build-library
	cp -r ${WEB_DIR} ${BUILD_DIR}
	cp ${LIBRARY_SRC} ${BUILD_DIR}

build-library:
	cd library && sbt fastLinkJS

clean:
	rm -rf ${BUILD_DIR}
