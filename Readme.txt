To package JavaFX Scala Project using Simple Build Tool:

Follow the sbt settings in here.

Must first publish the no.vedaadata sbt plugin to local ivy2 repository using the following command :

sbt publish-local

Then must build project and package using sbt package-javafx. This calls an ant build script to do everything like it should. The jar should then just work! Don't know how to make an App bundle for Mac yet though.

Not tested on PC or Mac without Java 1.7.