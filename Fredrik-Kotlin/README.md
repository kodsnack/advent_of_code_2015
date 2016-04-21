**Lösningar skrivna i Kotlin**

För att kompilera en Kotlin-fil med tester:

`kotlinc -cp ../../../lib/junit-4.10.jar  day01.kt -include-runtime -d day01.jar`

Kör sedan testerna med:

`kotlin -cp day01.jar:../../../lib/junit-4.10.jar  org.junit.runner.JUnitCore DayOne`
