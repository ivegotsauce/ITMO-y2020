package info.kgeorgiy.ja.sharipov.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class StudentDB implements StudentQuery {

    private <T extends Collection<E>, E> T mapStudents(Collection<Student> students, Function<Student, E> mapFunc,
                                                       Supplier<T> supplier) {
        return students.stream().map(mapFunc).collect(Collectors.toCollection(supplier));
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return mapStudents(students, Student::getFirstName, ArrayList::new);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return mapStudents(students, Student::getLastName, ArrayList::new);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return mapStudents(students, Student::getGroup, ArrayList::new);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return mapStudents(students, s -> s.getFirstName() + " " + s.getLastName(), ArrayList::new);
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return mapStudents(students, Student::getFirstName, TreeSet::new);
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Comparator.comparingInt(Student::getId))
                .map(Student::getFirstName).orElse("");
    }

    private List<Student> getSortedStudents(Collection<Student> students, Comparator<Student> cmp) {
        return students.stream().sorted(cmp).toList();
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return getSortedStudents(students, Comparator.comparingInt(Student::getId));
    }

    private static final Comparator<Student> studentComparator = Comparator.comparing(Student::getLastName)
            .thenComparing(Student::getFirstName).reversed()
            .thenComparingInt(Student::getId);

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return getSortedStudents(students, studentComparator);
    }

    private <T, E> T findStudentByE
            (Collection<Student> students, Function<Student, E> getE,
             Collector<? super Student, ?, T> collector, E other) {
        return students.stream().filter(s -> other.equals(getE.apply(s))).sorted(studentComparator).collect(collector);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentByE(students, Student::getFirstName, Collectors.toList(), name);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentByE(students, Student::getLastName, Collectors.toList(), name);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudentByE(students, Student::getGroup, Collectors.toList(), group);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return findStudentByE(students, Student::getGroup, Collectors
                .toMap(Student::getLastName, Student::getFirstName,
                        (existing, replacement) ->
                                existing.compareTo(replacement) < 0 ? existing : replacement), group);
    }
}