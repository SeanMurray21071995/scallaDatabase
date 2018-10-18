name := "dataBaseSlick"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq( "com.typesafe.slick" %% "slick" % "3.2.1",  
"org.slf4j" % "slf4j-nop" % "1.6.4", 
"com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",  
"mysql" % "mysql-connector-java" % "6.0.6")
