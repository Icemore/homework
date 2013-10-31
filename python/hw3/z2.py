
class Student:
    def __init__(self, name):
        self.name = name
        self.marks = []

    def __str__(self):
        return "%s: %s" % (self.name, ' '.join(map(str, self.marks)))

    def GPA(self):
        return float(sum(self.marks)) / len(self.marks)

    def AddMarks(self, marks):
        self.marks.extend(marks)

class Database:
    def __init__(self, path):
        self.students = []

        f = open(path)
        f.readline()
        for line in f:
            name, marks = line.split(':')
            curStudent = Student(name)
            curStudent.AddMarks(map(int, marks[1:].split()))
            self.students.append(curStudent)
        f.close()

    def AddStudent(self, student):
        self.students.append(student)

    def RemoveStudent(self, student):
        self.students.remove(student)

    def WriteToFile(self, path):
        f = open(path, "w")
        f.write("%d\n" % len(self.students))
        f.writelines(["%s\n" % str(st) for st in self.students])
        f.close()

    def GetStudentWithHighestGPA(self):
        return max(self.students, key=(lambda student: student.GPA()))

if __name__ == "__main__":
    db = Database("db.txt")
    print db.GetStudentWithHighestGPA().name
    db.WriteToFile("out.txt")