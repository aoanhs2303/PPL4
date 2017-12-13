import org.scalatest.FunSuite

/**
  * Created by nhphung on 4/30/17.
  */
class CodeGenSuite extends FunSuite with TestCodeGen {
  test("1. Built In: putIntLn") {
    val input = """
      void main(){
        putIntLn(100);
      }
      """
    val expected = "100"
    assert(checkCode(input, expected, 501))
  }
  test("2. Built In: putFloatLn") {
    val input = """
      void main(){
        putFloatLn(1.1);
      }
      """
    val expected = "1.1"
    assert(checkCode(input, expected, 502))
  }
  test("3. Built In: putStringLn") {
    val input = """
      void main(){
        putStringLn("PPL");
      }
      """
    val expected = "PPL"
    assert(checkCode(input, expected, 503))
  }
  test("4. Built In: putBoolLn") {
    val input = """
      void main(){
        putBoolLn(true);
      }
      """
    val expected = "true"
    assert(checkCode(input, expected, 504))
  }
  test("5. Built In: putIntLn with expression as parameter") {
    val input = """
      void main(){
        putIntLn(1+2+3);
      }"""
    val expected = "6"
    assert(checkCode(input, expected, 505))
  }
  test("6. myArray of int type") {
    val input = """
      int a[10]; 
      void main(){
        //nothing
      }"""
    val expected = ""
    assert(checkCode(input, expected, 506))
  }
  test("7. myArray of String type") {
    val input = """
      string str[10]; 
      void main(){
        str[0]="hello";
        putStringLn(str[0]);
      }"""
    val expected = "hello"
    assert(checkCode(input, expected, 507))
  }
  test("8. Int variable") {
    val input = """
      int a;
      void main(){
        //nothing
      }"""
    val expected = ""
    assert(checkCode(input, expected, 508))
  }
  test("9. Multiple declaration in global") {
    val input = """
      int a,b[2];
      void main(){
        //nothing
      }"""
    val expected = ""
    assert(checkCode(input, expected, 509))
  }
  test("10. myArray of Boolean and myArray of Float") {
    val input = """
      boolean b[2]; 
      float f[2];
      void main(){
        putBoolLn(b[1]);putFloatLn(f[0]);
      }
      """
    val expected = "false0.0"
    assert(checkCode(input, expected, 510))
  }
  test("11. Assign stmt float ") {
    val input = """
      void main () {
        float a;
        a = 0; 
        a=a+1.0; 
        putFloatLn(a);
      }
      """
    val expected = "1.0"
    assert(checkCode(input,expected,511))
  }
  test("12. Assign stmt int ") {
    val input = """
      void main () {
        float a; 
        a=(a=2)+1; 
        putFloatLn(a);
      }
      """
    val expected = "3.0"
    assert(checkCode(input,expected,512))
  }
  test("13. Assign stmt int negative number ") {
    val input = """
      void main () {
        float a; 
        a = -1 ; 
        putFloatLn(a);
      }
      """
    val expected = "-1.0"
    assert(checkCode(input,expected,513))
  }
  test("14. Assign a string number to variable of string") {
    val input =
      """
        void main(){
          string s[2];
          s[1]="hello";
          putStringLn(s[1]);
        }
      """
    val expected = "hello"
    assert(checkCode(input, expected, 514))
  }
  test("15. Assign a integer number to variable of float") {
    val input =
      """
        void main(){
          int a;
          float b;
          a=2;
          b=a;
          putFloatLn(b);
        }
      """
    val expected = "2.0"
    assert(checkCode(input, expected, 515))
  }
  test("16. Assign an expression of int to variable of int") {
    val input =
      """
        int myFun(boolean check){
          if(check) return 11;
          return 10;
        }
        void main(){
          int a,b,c,d,e,f,g,h[2];
          a=myFun(true);
          h[1]=a+3/4+5+6;
          putIntLn(h[1]);
        }
      """
    val expected = "22"
    assert(checkCode(input, expected, 516))
  }
  test("17. Assign an expression of float to variable of float") {
    val input =
      """
        int myFunc(){
          return 69;
        }
        void main(){
          int a;
          float b;
          b=myFunc()+4%5;
          putFloatLn(b);
        }
      """
    val expected = "73.0"
    assert(checkCode(input, expected, 517))
  }
  test("18. Assign an expression of boolean to variable of boolean") {
    val input =
      """
        void main(){
          boolean b;
          int a;
          b = (a=1) == 2;
          putBoolLn(b);
        }
      """
    val expected = "false"
    assert(checkCode(input, expected, 518))
  }
  test("19. Check BinOp +, - with number") {
      val input = """
      void main() {
        putIntLn(1 - 2 + 3);
      }
      """
      val expected = """2"""
      assert(checkCode(input,expected,519))
    }
    test("20. Check BinOp +, - with variable") {
      val input = """
      void main() {
        float f;
        f = 5.6;
        putFloatLn(1 + 2 + f);
      }
      """
      val expected = """8.6"""
      assert(checkCode(input,expected,520))
    }
    test("21. Check BinOp +, - with Check Function call") {
      val input = """
      float myFunc(float number) {
        return number;
      }
      void main() {
        float a;
        a = 5.6;
        myFunc(a);
        putFloatLn(1 + 2 - 3.4 + a);
      }
      """
      val expected = """5.2"""
      assert(checkCode(input,expected,521))
    }
    test("22. Check BinOp +, - with myArray cell") {
      val input = """
      void main() {
        int myArr[10];
        myArr[0] = myArr[1] = 2;
        putFloatLn(1 + myArr[1]);
      }
      """
      val expected = """3.0"""
      assert(checkCode(input,expected,522))
    }
    test("23. Check BinOp +, - with myArray cell, Check Function call") {
      val input = """
      int[] myFunc(int imyArr[]) {
        return imyArr;
      }
      void main() {
        int myArr[5];
        myArr[0] = myArr[1] = 10;
        putFloatLn(1 + myFunc(myArr)[1]);
      }
      """
      val expected = """11.0"""
      assert(checkCode(input,expected,523))
    }
    test("24. Check BinOp *, / with number") {
      val input = """
      void main() {
        putFloatLn(4 * 10 / 5);
      }
      """
      val expected = """8.0"""
      assert(checkCode(input,expected,524))
    }
    test("25. Check BinOp *, / with variable") {
      val input = """
      void main() {
        int f;
        f = 20;
        putFloatLn(10 * 20 * 30 / f);
      }
      """
      val expected = """300.0"""
      assert(checkCode(input,expected,525))
    }
    test("26. Check BinOp *, / with Check Function call") {
      val input = """
      float myFunc(int number) {
        return number;
      }
      void main() {
        float f;
        f = 5.6;
        f = f * 10;
      }
      """
      val expected = """"""
      assert(checkCode(input,expected,526))
    }
    test("27. Check BinOp *, / with myArray cell") {
      val input = """
      void main() {
        int myArr[10];
        myArr[0] = 5;
        putFloatLn(10 * myArr[0]);
      }
      """
      val expected = """50.0"""
      assert(checkCode(input,expected,527))
    }
    test("28. Check BinOp *, / with myArray cell, Check Function call") {
      val input = """
      int[] myFunc(int myArr[]) {
        return myArr;
      }
      int[] goo(int myArr[]) {
        return myFunc(myArr);
      }
      void main() {
        int myArr[5];
        myArr[0] = myArr[1] = 10;
        goo(myArr);
        putFloatLn(11);
      }
      """
      val expected = """11.0"""
      assert(checkCode(input,expected,528))
    }
    test("29. Check BinOp % with number") {
      val input = """
      void main() {
        putIntLn(4 % 9);
      }
      """
      val expected = """4"""
      assert(checkCode(input,expected,529))
    }
    test("30. Check BinOp % with variable") {
      val input = """
      void main() {
        int f;
        f = 5;
        putFloatLn(21 % f);
      }
      """
      val expected = """1.0"""
      assert(checkCode(input,expected,530))
    }
    test("31. Check BinOp % with Check Function call") {
      val input = """
      int myFunc(int f) {
        return f + 1;
      }
      void main() {
        putFloatLn(1 % 2 % myFunc(3));
      }
      """
      val expected = """1.0"""
      assert(checkCode(input,expected,531))
    }
    test("32. Check BinOp % with myArray cell") {
      val input = """
      void main() {
        int myArr[5];
        myArr[0] = myArr[1] = 10;
        myArr[4] = 1 % myArr[1];
      }
      """
      val expected = """"""
      assert(checkCode(input,expected,532))
    }
    test("33. Check BinOp % with myArray cell, Check Function call") {
      val input = """
      int[] myFunc(int imyArr[]) {
        return imyArr;
      }
      void main() {
        int myArr[5];
        myArr[0] = 12;
        myArr[1] = 12;
        myFunc(myArr);
        putInt(1 % myArr[1]);
      }
      """
      val expected = """1"""
      assert(checkCode(input,expected,533))
    }
    test("34. Check BinOp >") {
      val input = """
      void main() {
        int i;
        i = 1;
        i > 0;
        putBool(i > 0);
      }
      """
      val expected = """true"""
      assert(checkCode(input,expected,534))
    }
    test("35. Check BinOp <") {
      val input = """
      void main() {
        int i;
        i = 1;
        i < 0;
        putBool(i < 0);
      }
      """
      val expected = """false"""
      assert(checkCode(input,expected,535))
    }
    test("36. Check BinOp >=") {
      val input = """
      void main() {
        int i;
        i = 1;
        i >= 0;
        putBool(i >= 0);
      }
      """
      val expected = """true"""
      assert(checkCode(input,expected,536))
    }
    test("37. Check BinOp <=") {
      val input = """
      void main() {
        int i;
        i = 1;
        i <= 0;
        putBool(i <= 0);
      }
      """
      val expected = """false"""
      assert(checkCode(input,expected,537))
    }
    test("38. Check BinOp ==") {
      val input = """
      void main() {
        int i;
        i = 1;
        putBoolLn(i == 0);
      }
      """
      val expected = """false"""
      assert(checkCode(input,expected,538))
    }
    test("39. Check BinOp !=") {
      val input = """
      void main() {
        int i;
        i = 1;
        i != 0;
        putBoolLn(i != 0);
      }
      """
      val expected = """true"""
      assert(checkCode(input,expected,539))
    }
    test("40. Check BinOp &&") {
      val input = """
      void main() {
        boolean b;
        b = true;
        putBool(b && false);
      }
      """
      val expected = """false"""
      assert(checkCode(input,expected,540))
    }
    test("41. Check BinOp ||") {
      val input = """
      void main() {
        boolean b;
        b = true;
        putBool(b || false);
      }
      """
      val expected = """true"""
      assert(checkCode(input,expected,541))
    }
  test("42. Check IF stmt") {
    val input = """
      float arr[3];
      void main(){
       if((arr[2]=2)>=2) putFloat(arr[2] + arr[0]);
      }
      """
    val expected = "2.0"
    assert(checkCode(input,expected,542))
  }
  test("43. Check IF stmt else") {
    val input = """
      float k[4];
      void main(){
        if(1000 < 100) putInt(10);
        else putInt(20);
      }
      """
    val expected = "20"
    assert(checkCode(input,expected,543))
  }
    test("44. Check FOR stmt: Simple for statement") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 5; a = a + 1)
          putInt(a);

      }"""
    val expected = "1234"
    assert(checkCode(input, expected, 544))
  }

  test("45. Check FOR stmt: Check BLOCK statement in for") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 10; a = a + 1){
          int b;
          b = a * (a - 12);
          putInt(b);
         }

      }"""
    val expected = "-11-20-27-32-35-36-35-32-27"
    assert(checkCode(input, expected, 545))
  }
  test("46. Check FOR stmt: Break in loop") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 10; a = a + 1){
          if (a == 5)
            break;
         }
         putInt(a);
      }"""
    val expected = "5"
    assert(checkCode(input, expected, 546))
  }
  test("47. Check FOR stmt: Continue") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 10; a = a + 1){

          if (a == 5)
          {
            continue;
          }
          putInt(a);
         }
      }
      """
    val expected = "12346789"
    assert(checkCode(input, expected, 547))
  }
  test("48. Check FOR stmt: Nested Check BLOCK in loop") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 10; a = a + 1){
          int b;
          b = a * (a - 12);
          {
            int c;
            c = 10;
            putInt(b*c);
          }
         }
      }"""
    val expected = "-110-200-270-320-350-360-350-320-270"
    assert(checkCode(input, expected, 548))
  }
  test("49. Check FOR stmt: Return statement in loop") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 10; a = a + 1){
          if (a == 5)
            return;
          putInt(a);
         }
      }"""
    val expected = "1234"
    assert(checkCode(input, expected, 549))
  }
  test("50. Check FOR stmt: Break in deep nested Check BLOCK") {
    val input =
      """void main () {
         int a;
         for (a = 1; a < 10; a = a + 1){
          int b;
          b = a * (a - 2);
          {
            int c;
            c = 2;
            if ((b * c) < 50)
              break;
          }
          putInt(a);
         }
      }"""
    val expected = ""
    assert(checkCode(input, expected, 550))
  }
  test("51. Check DOWHILE stmt: simple expression") {
    val input = """
      boolean a; 
      void main() {
        do {
          putStringLn("Hello");
        } while (a);
      }
    """
    val expected = "Hello"
    assert(checkCode(input,expected,551))
  }
  test("552. Check DOWHILE stmt: complex expression") {
    val input = """
      boolean a; 
      void main() {
        int b; 
        b = 0; 
        do {
          b = b + 1; 
          putInt(b);
        } while (!a && (b < 10));
      }
      """
    val expected = "12345678910"
    assert(checkCode(input,expected,552))
  }
  test("553. Check DOWHILE stmt: complex expression and body") {
    val input = """
      int b[200]; 
      void main() {
        int i, sum; 
        i = 0; 
        sum = 0; 
        do {
          b[i] = i = i + 1; 
          sum = sum + b[i-1];
        } while (true && (i < 100)); 
        putIntLn(sum);
      }
      """
    val expected = "5050"
    assert(checkCode(input,expected,553))
  }
  test("554. Check DOWHILE stmt: with break statement") {
    val input = """
      int b[10]; 
      void main() {
        int i, sum; 
        i = 0; 
        sum = 0; 
        do {
          b[i] = i = i + 1; 
          sum = sum + b[i-1]; 
          break;
        } while (true && (i < 100)); 
        putIntLn(sum);}
        """
    val expected = "1"
    assert(checkCode(input,expected,554))
  }
  test("555. Check DOWHILE stmt: with if break statement") {
    val input = """
      int b[200]; 
      void main() {
        int i, sum; 
        i = 0; 
        sum = 0; 
        do {
          b[i] = i = i + 1; 
          sum = sum + b[i-1]; 
          if (i > 50) break;
        } while (true && (i < 100)); 
        putIntLn(sum);
      }"""
    val expected = "1326"
    assert(checkCode(input,expected,555))
  }
  test("56. Check DOWHILE stmt: with if continue statement") {
    val input = """
      int b[200]; 
      void main() {
        int i, sum; 
        i = 0; 
        sum = 0; 
        do {
          b[i] = i = i + 1; 
          if (i < 50) continue; 
          sum = sum + b[i-1];
        } while (true && (i < 100)); 
        putIntLn(sum);
      }"""
    val expected = "3825"
    assert(checkCode(input,expected,556))
  }
  test("57. Check DOWHILE Continue in loop") {
    val input =
      """void main () {
          int a,b;
          a = 10;
          do
           a = a + 1;
           if (a == 15)
             continue;
          while (a < 100);
          putInt(a);
      }"""
    val expected = "100"
    assert(checkCode(input, expected, 557))
  }

  test("58. Check DOWHILE Break in deep nested Check BLOCK") {
    val input =
      """void main () {
          int a,b;
          a = 10;
          do
          {
           a = a + 1;
           {
            int b;
            b = a * a;
            if (b > 1000)
              break;
           }
          }
          while (a < 10);
          putInt(a);
      }"""
    val expected = "11"
    assert(checkCode(input, expected, 558))
  }

  test("59. Check DOWHILE Continue in deep nested Check BLOCK") {
    val input =
      """void main () {
          int a,b;
          a = 10;
          do
          {
           a = a + 1;
           {
            int b;
            b = a * a;
            if (b == 121)
              continue;
           }
          }
          while (a < 10);
          putInt(a);
      }"""
    val expected = "12"
    assert(checkCode(input, expected, 559))
  }
  test("60. Check Return integer") {
      val input = """
      int myFunc(int i) {
        return i;
      }
      void main() {
        putInt(myFunc(1));
      }"""
      val expected = """1"""
      assert(checkCode(input,expected,560))
    }
    test("61. Check Return float") {
      val input = """
      float myFunc(float i) {
        return i;
      }
      void main() {
        putFloat(myFunc(11));
      }"""
      val expected = """11.0"""
      assert(checkCode(input,expected,561))
    }
    test("62. Check Return boolean") {
      val input = """
      boolean myFunc(boolean i) {
        return i;
      }
      void main() {
        putBool(myFunc(true));
      }"""
      val expected = """true"""
      assert(checkCode(input,expected,562))
    }
    test("63. Return string") {
      val input = """
      string myFunc(string i) {
        return i;
      }
      void main() {
        putString(myFunc("Hello"));
      }"""
      val expected = """Hello"""
      assert(checkCode(input,expected,563))
    }
  test("64. Check BLOCK: simple Check BLOCK") {
    val input =
      """
         void main(){
            putIntLn(10);
         }
      """
    val expected = "10"
    assert(checkCode(input, expected, 564))
  }
  test("65. Check BLOCK: nested Check BLOCK") {
    val input =
      """
         void main(){
            {
              putIntLn(1);
            }
         }
      """
    val expected = "1"
    assert(checkCode(input, expected, 565))
  }
  test("66. Check BLOCK: nested Check BLOCK with local declaration") {
    val input =
      """
         void main(){
            {
              int a;
              putIntLn(1);
              {
                int b;
                b=9;
                putIntLn(2);
                {
                  int c;
                  putIntLn(3);
                  {
                    int a[2];
                    c=2;
                    putIntLn(c+b+a[1]);
                  }
                }
              }
            }
         }
      """
    val expected = "12311"
    assert(checkCode(input, expected, 566))
  }
  test("67. Check BLOCK: Check BLOCK in if statement") {
    val input =
      """
         int myFunc(){
          if(true){
            {
              int a;
              a=2;
              return a;
            }
          }
          return 0;
         }
         void main(){
          putIntLn(myFunc());
         }
      """
    val expected = "2"
    assert(checkCode(input, expected, 567))
  }
  test("68. Check BLOCK: Check BLOCK in do while statement") {
    val input =
      """
         void main(){
            int a;
            a=2;
            do{
              int a;
              a=3;
              putIntLn(a=4);
            }
            while(!true);
         }
      """
    val expected = "4"
    assert(checkCode(input, expected, 568))
  }
  test("69. Check BLOCK: Check BLOCK in for statement") {
    val input =
      """
         void main(){
            boolean b;
            b=true;
            for(b;b;b){
              {
                putIntLn(100);
                break;
              }
            }
         }
      """
    val expected = "100"
    assert(checkCode(input, expected, 569))
  }
  test("70. Check Function call: call a simple function") {
    val input =
      """
         string foo(){
          return "Hello";
         }
         void main(){
            putStringLn(foo());
         }
      """
    val expected = "Hello"
    assert(checkCode(input, expected, 570))
  }
  test("71. Check Function call: Check Function call use as condition") {
    val input =
      """
         boolean checker(float x){
          if(x>2.08)
            return true;
          else
            return false;
          return true;
         }
         void main(){
            boolean a;
            if(a=checker(4)) putBoolLn(a);
            else putStringLn("fail");
         }
      """
    val expected = "true"
    assert(checkCode(input, expected, 571))
  }
  test("72. Check Function call: Check Function call in assign statement") {
    val input =
      """
         int assign(int a[]){
            a[1]=a[2]+1;
            a[2]=a[3]+1;
            a[3]=a[1]+1;
            return a[1]+a[2]+a[3];
         }
         void main(){
            int a[4];
            a[1]=a[3]=a[2]=1;
            a[1]=assign(a);
            putIntLn(a[1]);
         }
      """
    val expected = "7"
    assert(checkCode(input, expected, 572))
  }
  test("73. Check Function call: coercion (float & int) in parameter") {
    val input =
      """
         void fun(float v){
          return v;
         }
         void main(){
          int a;
          a=9;
          putFloatLn(a);
         }
      """
    val expected = "9.0"
    assert(checkCode(input, expected, 573))
  }
  test("74. Check Function call: coercion (array & array pointer) in parameter") {
    val input =
      """
         int moo(int a[]){return 1;}
         void main(){
            int a[2];
            putIntLn(moo(a));
         }
      """
    val expected = "1"
    assert(checkCode(input, expected, 574))
  }

  test("75. Check Function call: coercion in assign") {
    val input =
      """
         int foo(){
          if(true){
            {
              int a;
              a=2;
              return a;
            }
          }
          return 0;
         }
         void main(){
          putFloatLn(foo());
         }
      """
    val expected = "2.0"
    assert(checkCode(input, expected, 575))
  }
  test("76. Check Function call: Check Function call in expression") {
    val input =
      """
         int foo(){return 1;}
         int moo(){return 2;}
         void main(){
          int a;
          a=moo()+foo();
          putIntLn(a);
         }
      """
    val expected = "3"
    assert(checkCode(input, expected, 576))
  }
  test("77. Special program: recursive") {
    val input =
      """
        void main() {
           int num;
           int result;
           num = 5;
           result = calcSUM(num);
           putInt(result);
        }

        int calcSUM(int num) {
           int res;
           if (num == 1) {
              return 1;
           } else {
              res = num + calcSUM(num - 1);
           }
           return res;
        }
         """
    val expected = "15"
    assert(checkCode(input,expected,577))
  }
  test("78. Special program: recursive giai thua 10") {
    val input =
      """
        int recursive(int i)
        {
           if(i <= 1)
           {
              return 1;
           }
           return i * recursive(i - 1);
        } 
        void  main()
        {
            int i;
            putInt(recursive((i=10)));
            return ;
        }
         """
    val expected = "3628800"
    assert(checkCode(input,expected,578))
  }
  test("79. Special program => recursive giai thua 20") {
    val input =
      """
        int recursive( int i)
        {
           if(i <= 1)
           {
              return 1;
           }
           return i * recursive(i - 1);
        }
        void  main()
        {
            int i;
            putInt(recursive((i=20)));
            return ;
        }
         """
    val expected = "-2102132736"
    assert(checkCode(input,expected,579))
  }
  test("80. Special program : function call in exprs") {
    val input =
      """
         int foo(){return 1;}
         int moo(){return 2;}
         void main(){
          int a;
          a=(a=moo()+foo()+foo()+(a=3))+3;
          putIntLn(a);
         }
      """
    val expected = "10"
    assert(checkCode(input, expected, 580))
  }
  test("81. Test Program: Fibonacci") {
    val input = """void main() {
      int i, n1, n2, n3, sopt;
      n1 = 0;
      n2 = 1;
      n3 = 0;
      sopt = 6;
      for(i=2;i<sopt;i = i + 1)
      {  
        n3=n1+n2;  
        n1=n2;  
        n2=n3;  
      } 
      putInt(n3);
    }"""
    val expected = """5"""
    assert(checkCode(input,expected,581))
  }
  test("82. Assignment in expression") {
    val input = """
    void main() {
      int i;
      putInt(1 + (i = 2));
    }"""
    val expected = """3"""
    assert(checkCode(input,expected,582))
  }
  test("83. Array declarations in multiple blocks") {
    val input =
      """
      void main () {
        {
          int a[12];
        }
        {
          float b[12];
        }
        {
          string c[12];
          boolean d[12];
        }
      }
      """
    val expected = ""
    assert(checkCode(input,expected,583))
  }
  test("84. Array declarations in multiple block levels ") {
    val input =
      """void main () {
          int a[3];
          {
            float b[10];
            {
              string c[5];
              {
                boolean d[2];
              }
            }
          }
      }"""
    val expected = ""
    assert(checkCode(input,expected,584))
  }
  test ("85. Function call as argument") { 
    val input  = """
      int x,y,z;
      void main(){
          int a;
          float b;
          a=foo(foo(2));putInt(a);  
          b=foo1(foo1(-9.9));putFloat(b);
      }
      int foo(int i){
        return -i;
      }
      float foo1(float f){
        return -f;
      }
    """
    val expected = "2-9.9"
    assert(checkCode(input,expected,585))
  }
  test ("86. Short circut") { 
    val input  = """
      void main(){
          int a;
          putBool(((a=1) == 2) && ((a=3) == 3));
          putInt(a);
      }
    """
    val expected = "false1"
    assert(checkCode(input,expected,586))
  }
  test ("87. Short circuit logical expression (3)") { 
    val input  = """  
      int x,y,z;
      void main(){
        boolean a;
        a = (( (x>y) || (((x=y=20)!=(y=10+(y=10)))) )) ;
      } 
      """
    val expected = ""
    assert(checkCode(input,expected,587))
  }
  test ("88. Short circut ||") { 
    val input  = """
      void main(){
          int a;
          putBool(((a=2) == 2) || ((a=3) == 3));
          putInt(a);
      }
    """
    val expected = "true2"
    assert(checkCode(input,expected,588))
  }
  test("89. Short circut: Simple program") {
    val input = "void main () { int a; (a=1) ==1 && (a=3) ==3 && (a=8)!=8 &&(a=11)==11; putIntLn(a);}"
    val expected = "8"
    assert(checkCode(input,expected,589))
  }
  test("90. Short circuit: short circuit with && normal") {
    val input =
      """
        void main(){
          int a,b,c;
          a=10;
          b=10;
          c=20;
          if(a==10&&b==10)
            putIntLn(c);
          else
            putIntLn(b);
        } """
    val expected = "20"
    assert(checkCode(input,expected,590))
  }
  test("91. Program: short circuit with && normal") {
    val input =
      """
        void main(){
          int a, b;
          a = 10;
          b = 20;
          if (a > b) {
            putInt(a);
          } else {
            putInt(b);
          }
        } """
    val expected = "20"
    assert(checkCode(input,expected,591))
  }
  test ("92.Program: Simple if-else statement") { 
    val input  = """  int x,y,z;
                      void main(){
                          if(x>y){x=0;}
                          else x=1;
                          putInt(x);
                          if(x>y){x=0;}
                          else x=1;
                          putInt(x);
                      } """
    val expected = "10"
    assert(checkCode(input,expected,592))
  }

  test ("93. Program: Nested if-else statement") { 
    val input  = """  
      int x,y,z;
        void main(){
          x=0;
          if((x=2)<3){
            if (x!=2){
              x=99;putInt(x);
            }
            else{
              if (3==(x=3)){
                putInt(x);
              }
              else{
                x=100;putInt(x);
              }
              x=4;
              if (4==(x=5)) putInt(x);
              else{
                putInt(x);x=100;
              }
              putInt(x);
            }
          }
          else putString("Can't touch this");
        } 
      """
    val expected = "35100"
    assert(checkCode(input,expected,593))
  }
  test ("94. Program: Bubble sort") { 
    val input  = """  
      void bubbleSort(int arr[], int n)
      {
        int i, j;
        for (i = 0; i < n-1; i=i+i) {
          for (j = 0; j < n-i-1; j=j+1) {
            if (arr[j] > arr[j+1]) {
              arr[j] = arr[j+1];
            }     
          }            
        }            
      }
      void main(){
        
      } 
      """
    val expected = ""
    assert(checkCode(input,expected,594))
  }
  test("95. Program: For and continue statement") {
    val input = """
    void main(){
      int i;
      for(i = 0; i < 10;i = i + 2){
        if (i == 6)
          continue;
        putInt(i);
      }
    }"""
    val expected = """0248"""
    assert(checkCode(input,expected,595))
  }
  test("96. Program: Do and break statement") {
    val input = """
    void main() {
      int x;
      x = 1;
      do
        x = x + 1;
        if(x<8){x = x + 2;}
        else break;
      while(x < 10);
    }"""
    val expected = """"""
    assert(checkCode(input,expected,596))
  }
  test ("97. Program: controll loop in body") { 
    val input  = """  int x,y,z;
                      void main(){
                        for(x;x<10;x=x+1){
                          putInt(x);
                          if(x==4) x=9; 
                        } 
                        x=0;
                        for(x;x<10;x=x+1){
                          putInt(x);
                          if(x==4) x=8; 
                        }     
                      } """
    val expected = "01234012349"
    assert(checkCode(input,expected,597))
  }
  test("98. Program: Complex") {
    val input =
      """
        int foo(int a,int b){
           if(a>b) return a-b;
           do a=b+1;
           while (a<b);
           return a;
         }
         void main(){
            string s;
            s="haha";
            a[1]=1;
            a[0]=2;
            putIntLn(foo(a[1],a[0]));
            c[2]=a[1]+9/2%2+2/7-a[0]-0.2;
            putFloatLn(c[2]);
            d[1]=test(s,0);
            putStringLn(d[1]);
        }
        int a[2];
        float c[10];
        string d[5];
        boolean b[2];
        string test(string s,int t){
          if(t>1) s="hehe";
          else s="hello";
          return s;
        }

        """
    val expected = "3-1.2hello"
    assert(checkCode(input,expected,598 ))
  }
  test("99. Program: Mix complex expression as if condition") {
    val input =
      """
         void main(){
          int a;
          a=10;
          if((a=1)+2-3%4*5/6<2.0){
            putStringLn("if was excuted");
          }
          else putStringLn("else was excuted");
         }
      """
    val expected = "if was excuted"
    assert(checkCode(input, expected, 599))
  }
  test("100. Program: multiple ops in one assignment") {
    val input =
      """void main () {
          boolean d,d1,d2,d3;

          d = (1 > 2) || (4.2 > 0);
          d1 = (2.3 >= 0) && (1 > -100) && (100 > 99);
          d2 = (2 > 0.1) && ((1 > 2) || (4.2 > 0));
          putBool(d);putBool(d1);putBool(d2);
      }"""
    val expected = "truetruetrue"
    assert(checkCode(input,expected,600))
  }
  test("70. Do while: Continue in deep nested block") {
    val input =
      """void main () {
          int a,b;
          a = 10;
          do
          {
           a = a + 1;
           {
            int b;
            b = a * a;
            if (b == 121)
              continue;
           }
          }
          while (a < 10);
          putInt(a);
      }"""
    val expected = "11"
    assert(checkCode(input, expected, 601))
  }
}