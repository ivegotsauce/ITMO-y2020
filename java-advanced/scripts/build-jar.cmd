SET module=java-advanced-2022\modules\info.kgeorgiy.java.advanced.implementor
SET package=info\kgeorgiy\java\advanced\implementor
SET a=%module%\%package%
SET b=info\kgeorgiy\ja\sharipov\implementor
cd ..\java-solutions
javac -d ..\scripts %b%\Implementor.java -cp ..\..\%module%
cd ..\..\
javac -d java-advanced\scripts %a%\JarImpler.java -cp %module%
javac -d java-advanced\scripts %a%\Impler.java -cp %module%
javac -d java-advanced\scripts %a%\ImplerException.java -cp %module%
cd java-advanced\scripts
jar cfm impler.jar Manifest.txt %b%\Implementor.class %package%\JarImpler.class %package%\Impler.class %b%\Implementor$1.class %package%\ImplerException.class
rm -r info