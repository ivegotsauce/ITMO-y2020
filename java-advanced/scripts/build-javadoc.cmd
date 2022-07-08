SET a=info\kgeorgiy\java\advanced\implementor
SET b=info\kgeorgiy\ja\sharipov\implementor
SET link=https://docs.oracle.com/en/java/javase/11/docs/api/
SET c=java-advanced-2022\modules\info.kgeorgiy.java.advanced.implementor
javadoc -d ..\javadoc -link %link% -classpath ..\java-solutions -private ..\java-solutions\%b%\Implementor.java ..\..\%c%\%a%\Impler.java ..\..\%c%\%a%\JarImpler.java ..\..\%c%\%a%\ImplerException.java