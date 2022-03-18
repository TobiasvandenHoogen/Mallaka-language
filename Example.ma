

process intSum {a, b}->(
    intSum = num1 + num2)<-

Main {integer}->
    num1 = 8
    num2 = 10
    Main = intSum num1 num2<-

